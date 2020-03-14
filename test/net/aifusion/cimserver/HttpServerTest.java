/**
 * Copyright 2017 Sharad Singhal, All Rights Reserved
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
 * Created Sep 6, 2017 by sharad
 */
package net.aifusion.cimserver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.utils.Java2Cim;

/**
 * Class to test HTTPServer, HttpSession
 * @author Sharad Singhal
 */
public class HttpServerTest {
	private static Logger logger = Logger.getLogger(HttpServerTest.class.getName());
	private static String repositoryLocation = "testrepository";
	private static CimServer server;
	private static String serverMof = "instance of aifusion_httpconfiguration {\n"+
			"KeyStorePassword = \"serverpass\";\n"+
			"MaxSessions = 0;\n"+
			"TrustStore = \"testrepository/serverTrustStore.jks\";\n"+
			"KeyStore = \"testrepository/serverKeyStore.jks\";\n"+
			"ServerTimeout = 5000;\n"+
			"Repository = \"testrepository\";\n"+
			"RequestHandler = \"net.aifusion.cimserver.TestHandler\";\n"+
			"Secure = false;\n"+
			"X500Principal = \"CN=localhost, OU=cimfusion.com, O=cimfusion, C=US, L=Belmont, ST=California\";\n"+
			"Id = \"serverConfig\";\n"+
			"TrustStorePassword = \"serverpass\";\n"+
			"HostName = \"localhost\";\n"+
			"ServerPort = 8085;\n"+
			"};\n"+
			"instance of aifusion_httpconfiguration {\n"+
			"KeyStorePassword = \"serverpass\";\n"+
			"MaxSessions = 0;\n"+
			"TrustStore = \"testrepository/serverTrustStore.jks\";\n"+
			"KeyStore = \"testrepository/serverKeyStore.jks\";\n"+
			"ServerTimeout = 5000;\n"+
			"Repository = \"testrepository\";\n"+
			"RequestHandler = \"net.aifusion.cimserver.TestHandler\";\n"+
			"Secure = true;\n"+
			"X500Principal = \"CN=localhost, OU=cimfusion.com, O=cimfusion, C=US, L=Belmont, ST=California IP=127.0.0.1\";\n"+
			"Id = \"secureServerConfig\";\n"+
			"TrustStorePassword = \"serverpass\";\n"+
			"HostName = \"localhost\";\n"+
			"ServerPort = 8085;\n"+
			"};\n"+
			"instance of aifusion_httpconfiguration {\n"+
			"KeyStorePassword = \"clientpass\";\n"+
			"TrustStore = \"testrepository/clientTrustStore.jks\";\n"+
			"KeyStore = \"testrepository/clientKeyStore.jks\";\n"+
			"Secure = false;\n"+
			"X500Principal = \"CN=localclient, OU=cimfusion.com, O=cimfusion, C=US, L=Belmont, ST=California\";\n"+
			"Id = \"clientConfig\";\n"+
			"TrustStorePassword = \"clientpass\";\n"+
			"};\n"+
			"#pragma namespace (\"/root/local\")\n"+
			"Enumeration test_enum : string { enumvalue};\n"+
			"Structure test_struct { test_enum v;\n string foobar; };\n"+
			"class test_class {\n\t[key]\n\tSint32 integerProperty;\n\t[write]String stringProperty;\n\tvoid reset(String propertyName);\n};\n"+
			"instance of test_class {\n\tintegerProperty = 5;\n};\n"
			;
	private static MOFParser parser;
	private static URL serverURL;
	private static HttpConfiguration serverConfig, clientConfig;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("HttpServer ");
		deleteFiles(repositoryLocation);	// clean up from prior tests
		// create the server-side cache, and server configuration
		PersistentCache cache = new PersistentCache(repositoryLocation);
		CimClass configClass = (CimClass) Java2Cim.getModelForClass(HttpConfiguration.class, cache);
		assertNotNull(configClass);
		assertTrue(cache.contains(configClass.getObjectPath()));
		// create instances at the server. Note that normally we do not mix configuration with content information in the repository
		// to prevent attacks, but for testing we don't care about that
		parser = new MOFParser(cache);
		ByteArrayInputStream in = new ByteArrayInputStream(serverMof.getBytes());
		parser.parse(in, Constants.defaultNameSpacePath);
		in.close();
		cache.shutdown();
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		assertEquals(8,elements.size());
		// create the client configuration
		clientConfig = HttpConfiguration.getConfiguration("clientConfig", null, repositoryLocation);
		assertNotNull(clientConfig);
		// create the server configuration
		serverConfig = HttpConfiguration.getConfiguration("serverConfig", null, repositoryLocation);
		assertNotNull(serverConfig);
		// create the server, and start it
		serverURL = new URL("http://localhost:8085/");
		return;
		
	}
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		// comment the following line to retain content of server-side repository after the tests complete if needed for debugging
		deleteFiles(repositoryLocation);
		System.out.println("done.");
	}
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		server = new CimServer(serverConfig);
		server.startServer();
	}
	@After
	public void tearDown() throws Exception {
		if(server != null) server.stopServer();
		System.out.print(".");
	}
	
	/**
	 * Delete a file or directory. If the input is a directory, all files and
	 * directories within that directory are deleted.
	 * @param fileOrDirectory - name of file or directory to delete
	 */
	private static void deleteFiles(String fileOrDirectory){
		File file = new File(fileOrDirectory);
		if(!file.exists()) return;
		if(file.isDirectory()){
			String [] names = file.list();
			if(names != null && names.length > 0){
				for(String n : names){
					String fName = fileOrDirectory+"/"+n;
					deleteFiles(fName);
				}
			}
		}
		if(file.exists() && !file.delete()){
			fail("Unable to delete file "+fileOrDirectory);
		}
	}
	private static int NTHREADS = 100;
	private static boolean [] done = new boolean[NTHREADS];
	@Test
	public void testHttpServer() {
		int minCount = Thread.activeCount();
		for(int i = 0; i < NTHREADS; i++){
			done[i] = true;
			Thread t = new Thread(new TestClient(100,false));
			t.setName("c-"+i);
			t.start();
		}
		while(Thread.activeCount() > minCount){
			// System.out.println(Thread.activeCount());
			try {
				Thread.sleep(10);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		for(int i = 0; i < NTHREADS; i++){
			assertTrue(done[i]);
		}
	}
	
	// TODO: Add test for Https server connections
	@Ignore
	@Test
	public void testHttpsServer(){
		fail("Not yet implemented");
	}
	
	/**
	 * Test client to emulate multiple simultaneous clients talking to the server
	 * @author Sharad Singhal
	 */
	public static class TestClient implements Runnable {
		HttpURLConnection connection;
		int iterations;
		static String [] methods = new String[]{"GET","PUT","DELETE","POST","HEAD","OPTIONS"};
		Random rand = new Random();
		long average,time;
		String httpBody = ": Some input\r\n";
		boolean secure;
		public TestClient(int iterations, boolean secure){
			this.iterations = iterations;
			this.secure = secure;
			return;
		}

		@Override
		public void run() {
			int j = Integer.parseInt(Thread.currentThread().getName().substring(2));
			// System.out.println("Starting "+j);
			for(int i = 0; i < iterations; i++){
				String httpMethod = methods[rand.nextInt(methods.length)];
				HttpURLConnection connection = getConnection(httpMethod);
				connection.setRequestProperty(HttpHeader.CONNECTION.toString(), i == iterations-1 ? "close" : "keep-alive");
				boolean doOutput = false;
				switch(httpMethod){
				case "PUT":
				case "POST":
					doOutput = true;
					connection.setDoOutput(true);
					break;
				case "GET":
				case "DELETE":
				case "HEAD":
				case "OPTIONS":
					break;
				}
				// time = System.nanoTime();
				try {
					if(doOutput){
						byte [] buffer = (Thread.currentThread().getName()+httpBody).getBytes(Constants.byteEncoding);
						connection.setRequestProperty(HttpHeader.CONTENT_TYPE.toString(), "text/mof;charset=utf-8");
						connection.setRequestProperty(HttpHeader.CONTENT_LENGTH.toString(), String.valueOf(buffer.length));
						connection.connect();
						if(buffer.length > 0){
							OutputStream out = connection.getOutputStream();
							out.write(buffer);
							out.flush();
							out.close();
						}
					} else {
						connection.connect();
					}
					int code = connection.getResponseCode();
					int contentLength = (int) connection.getContentLengthLong();
					// logger.info("Iteration "+i+" Method "+httpMethod+" doOutput "+doOutput+" Status "+code+" ContentLength "+contentLength+"\n");
					byte[] b = new byte[1024];
					if(code >= 200 && code < 300){
						if(contentLength > 0){
							InputStream in = connection.getInputStream();
							int bytesRead = 0;
							while(bytesRead < contentLength){
								int read = in.read(b, bytesRead, contentLength-bytesRead);
								bytesRead += read;
							}
							in.close();
						}
					} else if(code >= 400){
						if(contentLength > 0){
							InputStream in = connection.getErrorStream();
							int bytesRead = 0;
							while(bytesRead < contentLength){
								int read = in.read(b, bytesRead, contentLength-bytesRead);
								bytesRead += read;
							}
							in.close();
						}
						done[j] = false;
					} else {
						System.out.println("Code "+code+" not handled");
						done[j] = false;
						break;
					}
					// if(contentLength > 0) logger.info(new String(Arrays.copyOf(b, contentLength)));
				} catch (IOException e) {
					System.out.println(e.toString());
					done[j] = false;
					break;
				}
				// average += System.nanoTime() - time;
				// j++;
			}
			// logger.info("Thread "+Thread.currentThread().getName()+": "+(float)average/(float)j);
			return;
		}
		
		private HttpURLConnection getConnection(String httpMethod){
			// target URL to use
			URL url = serverURL;
			try {
				// TODO: Add code here to test HTTPS connections
				HttpURLConnection connection = (HttpURLConnection) url.openConnection();
				connection.setRequestMethod(httpMethod);
				connection.setRequestProperty(HttpHeader.HOST.toString(), url.getHost());
				connection.setRequestProperty(HttpHeader.ACCEPT.toString(), MimeType.MOF.getType()+","+MimeType.PLAINTEXT.getType());
				connection.setRequestProperty(HttpHeader.USER_AGENT.toString(), "CimClient/1.0");
				connection.setConnectTimeout(10000);
				connection.setReadTimeout(10000);
				return connection;
			} catch (IOException e) {
				throw new ModelException("CimClient-Unable to create connection to "+url,e);
			}
		}
	}

}
