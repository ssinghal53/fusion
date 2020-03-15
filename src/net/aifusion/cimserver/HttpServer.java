/**
 * Copyright 2017, Sharad Singhal, All Rights Reserved
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *    
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * Created Jan 28, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;

import net.aifusion.metamodel.ModelException;

/**
 * This class implements a small HTTP server for use in CIM services
 * @author Sharad Singhal
 */
class HttpServer implements Runnable {
	/** Logger for this class */
	private static final Logger logger = Logger.getLogger(HttpServer.class.getName());
	/** Server configuration for this server */
	private HttpConfiguration config;
	/** Primary listen socket on the server */
	private volatile ServerSocket serverSocket;
	/** Main server thread */
	private volatile Thread serverThread;
	/** timeout (in ms) for sessions. If 0, no timeout is enforced */
	private int timeout = 0;
	/** Number of currently active sessions */
	private volatile long sessionCount;
	/** Currently active sessions */
	private final HashSet<HttpSession> currentSessions = new HashSet<HttpSession>();
	/** Exception in the primary listen loop */
	private Exception listenException = null;
	/** True when the primary listen socket is ready to receive connections */
	private volatile boolean isReady = false;

	/**
	 * Create an HttpServer using the given configuration settings
	 * @param config - configuration for the server
	 * @throws IOException - if the a new socket could not be created
	 */
	public HttpServer(HttpConfiguration config) throws IOException{
		this.config = config;
		// set proxy for outbound connections from the server
		if(config.getProxyPort() > 0){
			System.setProperty("http.proxyHost", config.getProxyHost());
			System.setProperty("https.proxyHost", config.getProxyHost());
			System.setProperty("http.proxyPort", String.valueOf(config.getProxyPort()));
		}
		if(!config.isSecure()){
			// create a normal socket
			serverSocket = new ServerSocket();
		} else {
			// create a TLS socket
			System.setProperty("javax.net.ssl.keyStore", config.getKeyStore());
			System.setProperty("javax.net.ssl.keyStorePassword", config.getKeyStorePassword());
			System.setProperty("javax.net.ssl.trustStore", config.getTrustStore());
			System.setProperty("javax.net.ssl.trustStorePassword", config.getTrustStorePassword());
			// alternative paths...

			// TODO: ServerConfiguration has the server private key protected by a second password.
			// Need to understand how to pass that down...
			SSLServerSocketFactory sslServerSocketFactory = (SSLServerSocketFactory) SSLServerSocketFactory.getDefault();
			SSLServerSocket socket = (SSLServerSocket) sslServerSocketFactory.createServerSocket();
			// socket.setNeedClientAuth(true); // require client authentication
			serverSocket = socket;
			// keytool -genkey -keyalg RSA -alias selfsigned -keystore testkey.jks -storepass password -validity 360 -keysize 2048
			/* Alternate method from http://stackoverflow.com/questions/18787419/ssl-socket-connection
			 * 
			KeyStore ks = KeyStore.getInstance("JKS");
			ks.load(new FileInputStream("keystoreFile"), "keystorePassword".toCharArray());

			KeyManagerFactory kmf = KeyManagerFactory.getInstance("X509");
			kmf.init(ks, "keystorePassword".toCharArray());

			TrustManagerFactory tmf = TrustManagerFactory.getInstance("X509"); 
			tmf.init(ks);

			SSLContext sc = SSLContext.getInstance("TLS"); 
			TrustManager[] trustManagers = tmf.getTrustManagers(); 
			sc.init(kmf.getKeyManagers(), trustManagers, null); 

			// for the server side, use
			SSLServerSocketFactory ssf = sc.getServerSocketFactory(); 
			SSLServerSocket s = (SSLServerSocket) ssf.createServerSocket(serverport);
			SSLSocket c = (SSLSocket) s.accept();

			// For client side, replace the last three lines with:
			SSLSocketFactory ssf = sc.getSocketFactory(); 
			SSLSocket s = (SSLSocket) ssf.createSocket(serverip, serverport);
			s.startHandshake();
			 */
		}
		// allow the socket to be reused
		serverSocket.setReuseAddress(true);			
		return;
	}

	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		// get server hostname, port, and maximum concurrent sessions
		String hostName = config.getHostName();		// server host
		int port = config.getServerPort();			// server listen port
		// logger.info("Host: "+hostName+" Port: "+port);
		int maxSessions = config.getMaxSessions();	// maximum sessions; 0 implies no limit enforced
		// get the underlying request handler -- note that the handler instance is shared across all sessions
		HttpRequestHandler handler = HttpRequestHandler.getHandler(config);
		// bind the server socket to the host:port
		try {
			serverSocket.bind(hostName != null ? new InetSocketAddress(hostName, port) : new InetSocketAddress(port));
			isReady = true;
			// loop listening to the socket. ServerSocket will be closed by StopServer() method
			while(isReady && !serverSocket.isClosed()){
				// while we are alive, do
				if(maxSessions == 0 || sessionCount < maxSessions){
					// accept a session, and set timeouts on it
					Socket sessionSocket = serverSocket.accept();
					if (timeout > 0) {
						sessionSocket.setSoTimeout(timeout);
					}
					// create the session
					HttpSession session = new HttpSession(this,sessionSocket,handler);
					try {
						// handle the session
						Thread t = new Thread(session);
						t.setDaemon(true);
						synchronized(currentSessions){
							currentSessions.add(session);
							sessionCount++;
						}
						t.start();
						// when the session finishes, it will close the sessionSocket,
						// and update sessionCount and currentSessions
					} catch (Exception e) {
						listenException = e;
						logger.info("HttpServer: Unable to accept connection "+e.toString());
						endSession(session);
					}
				} else {
					// sessionCount >= maxSessions; wait a bit for the session queue to drain
					try {
						Thread.sleep(20L);
					} catch (InterruptedException e) {
						listenException = e;
						logger.info("HttpServer: Queue drain sleep interrupted"+e.toString());
					}
				}
			}
		} catch (Exception e) {	// exception on serverSocket.bind() or serverSocket.accept()
			this.listenException = e;
			if(isReady) logger.log(Level.INFO, "HttpServer: Unable to bind socket", e);
		} finally {
			// close all current sessions
			synchronized(currentSessions){
				if(!currentSessions.isEmpty()){
					for(HttpSession s : currentSessions){
						s.endSession();
					}
				}
				// TODO: Do we need to check if all sessions terminated gracefully?
				// once we close the server socket, everything will abort
				currentSessions.clear();
			}
			// if server socket is not closed (we terminated because isReady = false), close it
			if(!serverSocket.isClosed()){
				try {
					serverSocket.close();
				} catch (IOException e) {
					listenException = e;
					logger.log(Level.INFO, "HttpServer: Unable to close server socket", e);
				}
			}
			// shut down the request handler
			handler.shutdown();
		}
		return;
	}

	/**
	 * Callback method to mark a session complete from a running session
	 * @param httpSession - session to remove from the active session list
	 */
	protected void endSession(HttpSession httpSession) {
		// logger.info("Ending session "+Thread.currentThread().getName()+"\n");
		synchronized(currentSessions){
			if(currentSessions.contains(httpSession)){
				currentSessions.remove(httpSession);
				sessionCount--;
			}
		}
		return;
	}

	/**
	 * Start the Server. The calling thread will return once server starts
	 */
	public void startServer() {
		serverThread = new Thread(this);		// create the main thread
		serverThread.setDaemon(true);
		serverThread.setName("HttpServer-Main");
		serverThread.start();
		// start the listener
		while (!isReady && listenException == null) {
			try {
				// wait until the listener socket is bound 
				Thread.sleep(20L);
			} catch (Throwable e) {
				// nothing to do-- sleep was interrupted
			}
		}
		// check that we did not get an exception during start-up
		if (listenException != null) {
			throw new ModelException("HttpServer: Server Terminated unexpectedly ",listenException);
		}
		// return the main thread. The listener continues to listen on the server socket
		return;
	}

	/**
	 * Stop the server and free all resources
	 */
	public void stopServer(){
		try {
			// stop the server from accepting more requests
			isReady = false;
			serverSocket.close();
			// wait for the server thread to die
			if(serverThread != null) {
				serverThread.join();
			}
			if(listenException != null) logger.fine("Saw "+listenException.toString());
		} catch (Exception e) {
			logger.log(Level.SEVERE, "HttpServer: Could not stop all connections", e);
		}
		return;
	}
	
	/**
	 * Start the Http Server.
	 * @param args - the program takes the following arguments
	 * <dl>
	 * <dt>-h | -h</dt><dd>(must be only argument) print a help message and exit</dd>
	 * <dt>-c configId</dt><dd>use configuration identified by ID [defaultConfig]</dd>
	 * <dt>-cp path</dt><dd>use specified directory to locate the configuration [resources/config]</dd>
	 * <dt>-n nameSpace</dt><dd>use specified namespace to locate the configuration [cimfusion]</dd>
	 * </dl>
	 */
	public static void main(String[] args) {
		// check if we need to print out help
		if(args.length == 1 && (args[0].startsWith("-h") || args[0].startsWith("-H"))){
			System.out.println("Use:\n$ HttpServer [options]\nwhere options can be:");
			System.out.println("\t-help | -Help				# show help (this message) (must be only argument)");
			System.out.println("\t-n namespacePath			# NameSpacePath to use [cimfusion]");
			System.out.println("\t-c configurationId		# ConfigurationID to use [defaultConfig]");
			System.out.println("\t-cp configPath			# Path to the configuration directory [resources/config]");
			return;
		}
		String id = null, path = null, directory = null;
		for(int i = 0; i < args.length-1; i += 2){
			switch(args[i]){
			case "-n":
				path = args[i+1].trim();
				break;
			case "-c":
				id = args[i+1].trim();
				break;
			case "-cp":
				directory = args[i+1].trim();
				break;
			default:
				System.out.println("Unknown argument (ignored) "+args[i]+" "+args[i+1]);
				break;
			}
		}
		HttpConfiguration config = HttpConfiguration.getConfiguration(id, path, directory);
		if(config == null){
			logger.info("HttpServer: No configuration found at ("+id+","+path+","+directory+")");
			return;
		}
		HttpServer server;
		try {
			server = new HttpServer(config);
			server.startServer();
			System.out.println("Server started. Hit any key to stop server");
			System.in.read();
			System.out.println("Stopping server");
			server.stopServer();
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		System.out.println("Server stopped");
		return;
	}

}
