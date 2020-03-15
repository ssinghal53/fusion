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
 * Created Jan 30, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Class to manage an Http Session. An Http session is a sequence of request/response pairs
 * @author Sharad Singhal
 */
class HttpSession implements Runnable {
	/** Logger for this class */
	private static final Logger logger = Logger.getLogger(HttpSession.class.getName());
	/** Server that owns this session */
	HttpServer server;
	/** Socket being used for the current session */
	Socket sessionSocket;
	/** Buffer size for reading request input */
	public static final int BUFSIZE = 16384;
	/** input buffer */
	private byte [] inputBuffer = new byte[BUFSIZE];
	/** available bytes in input buffer */
	private int inputLen = 0;
	/** Current cursor in inputBuffer */
	private int cursor = 0;
	/** Session output stream for responses */
	private final OutputStream outputStream;
	/** Session input stream for requests */
	private final BufferedInputStream inputStream;
	/** IP address of client obtained from input socket */
	private String remoteIp;
	/** Hostname of client obtained from input socket */
	private String remoteHostname;
	/** Handler to manage individual requests */
	private HttpRequestHandler requestHandler;
	/** flag to enable incoming request logging */
	private boolean logEnabled = false;
	/** Flag to indicate shutdown */
	private volatile boolean endSession = false;
	
	/**
	 * Create a new Http Session
	 * @param server - server tracking this session
	 * @param sessionSocket - session socket
	 * @param requestHandler - Request handler for this session
	 * @throws IOException - in case of errors
	 */
	public HttpSession(HttpServer server, Socket sessionSocket, HttpRequestHandler requestHandler) throws IOException {
		this.server = server;
		this.sessionSocket = sessionSocket;
		this.requestHandler = requestHandler;
		InetAddress inetAddress = sessionSocket.getInetAddress();
		this.inputStream = new BufferedInputStream(sessionSocket.getInputStream(), BUFSIZE);
		this.outputStream = sessionSocket.getOutputStream();
		remoteIp = inetAddress.isLoopbackAddress() || inetAddress.isAnyLocalAddress() ? "127.0.0.1" : inetAddress.getHostAddress().toString();
		remoteHostname = inetAddress.isLoopbackAddress() || inetAddress.isAnyLocalAddress() ? "localhost" : inetAddress.getHostName().toString();
		return;
	}
	
	/**
	 * Handle this session in a new thread
	 */
	@Override
	public void run() {
		if(logEnabled) logger.info("Starting session "+Thread.currentThread().getName()+"\n");
		try {
			// Continue to handle requests as long as the session is alive and not marked to finish by the server
			while(!endSession && !sessionSocket.isClosed() && handleNextRequest());
		} catch (Exception e) {
			logger.info("HttpSession "+Thread.currentThread().getName()+": Error in handling session "+e.toString());
		} finally {
			if(logEnabled) logger.info("Ending session "+Thread.currentThread().getName()+"\n");
			try {
				if(!sessionSocket.isClosed()){
					/*
					if(!sessionSocket.isOutputShutdown()){
						sessionSocket.shutdownOutput();
					}
					if(!sessionSocket.isInputShutdown()){
						sessionSocket.shutdownInput();
					}
					*/
					// close the session socket
					sessionSocket.close();
				}
			} catch (IOException e) {
				logger.log(Level.INFO, "HttpSession "+Thread.currentThread().getName()+": Error closing session", e);
			} finally {
				// inform the server to remove this session from its active session list
				server.endSession(this);
			}
		}
		return;
	}

	/**
	 * Mark this session to end after the current request is finished
	 */
	public void endSession() {
		endSession = true;
		return;
	}

	/**
	 * Enable logging on this session
	 * @param enable - true to enable logging, false to disable logging
	 */
	public void enableLog(boolean enable){
		logEnabled = enable;
		return;
	}
	
	/**
	 * Handle the next request from the client
	 * @return - true if successfully handled and not the last request, else false
	 */
	public boolean handleNextRequest() {
		HttpRequest request = null;
		// logger.info("Handling next request using "+Thread.currentThread().getName()+"\n");
		try {
			request = getRequest();		// get the next request in the session
			validate(request);			// validate the request
			HttpResponse response = requestHandler.serve(request);	// get the corresponding response
			// add session-specific headers
			if("close".equalsIgnoreCase(request.getHeader(HttpHeader.CONNECTION))) 
				response.addHeader(HttpHeader.CONNECTION, "close");	// if the client asked to close the session, we also return that
			
			// TODO: Set Session-specific headers in the response (e.g., cookies)
			response.send(outputStream);							// send the response to the client
			return !request.isLastRequest() && !response.isLastResponse();
		} catch (HttpException e){	
			// send an error response to the client in case we have an HttpException
			HttpResponse response = new HttpResponse(request != null ? request.getHttpMethod() : null,e.getStatus(),MimeType.PLAINTEXT, e.getMessage());
			response.send(outputStream);
			return request != null ? !request.isLastRequest() && !response.isLastResponse() : !response.isLastResponse();
		} catch (IOException e) {
			// IOExceptions mean that the client has disappeared
			// return false to terminate session
			// logger.info("Closing session "+Thread.currentThread().getName()+" because of "+e.toString()+"\n");
			return false;
		}
	}
	
	/*
	 * Notes:
	 * A server which receives an entity-body with a transfer-coding it does not understand SHOULD return 501 (Unimplemented),
	 * and close the connection.
	 * 
	 * For compatibility with HTTP/1.0 applications, HTTP/1.1 requests containing a message-body MUST include a valid
	 * Content-Length header field unless the server is known to be HTTP/1.1 compliant. If a request contains a
	 * message-body and a Content-Length is not given, the server SHOULD respond with 400 (bad request) if it
	 * cannot determine the length of the message, or with 411 (length required) if it wishes to insist on receiving a valid
	 * Content-Length.
	 * 
	 * All HTTP/1.1 applications that receive entities MUST accept the �chunked� transfer-coding (section 3.6), thus
	 * allowing this mechanism to be used for messages when the message length cannot be determined in advance.
	 * Messages MUST NOT include both a Content-Length header field and a non-identity transfer-coding. If the
	 * message does include a non-identity transfer-coding, the Content-Length MUST be ignored.
	 * When a Content-Length is given in a message where a message-body is allowed, its field value MUST exactly
	 * match the number of OCTETs in the message-body. HTTP/1.1 user agents MUST notify the user when an invalid
	 * length is received and detected.
	 * 
	 * RFC 7230 page 25 deprecates line folding except for media types
	 */

	/**
	 * Validate required information in the request
	 * @param request - request to validate
	 * @throws HttpException in case the request could not be validated
	 */
	private void validate(HttpRequest request) {
		// test for the presence of required headers
		// Host: required by RFC 2616 Section 14 for HTTP/1.1 requests
		if("HTTP/1.1".equals(request.getProtocolVersion()) && request.getHeader(HttpHeader.HOST) == null) 
			throw new HttpException(HttpStatus.BAD_REQUEST,"Host header not present");
		// Check that we support the requested character set. Assume UTF-8 if not present (see request.getCharSet())
		if(!Charset.isSupported(request.getCharset())){
			throw new HttpException(HttpStatus.BAD_REQUEST,request.getCharset()+" is not supported");
		}
		return;
	}

	/**
	 * Get the next request from the client
	 * @return - true if request successfully handled, false otherwise
	 * @throws IOException - in case the client disappears
	 */
	public HttpRequest getRequest() throws IOException {
		// mark the current position of the input stream
		inputStream.mark(BUFSIZE);

		// read in the header bytes
		int bytesRead = inputLen = cursor = 0;
		do {
			// logger.info("Input Read = "+inputLen+" Available = "+inputStream.available());
			bytesRead = inputStream.read(inputBuffer,inputLen,BUFSIZE-inputLen);
			if(bytesRead < 0){ 		// input stream was closed
				throw new IOException("Error reading input stream");
			}
			inputLen += bytesRead;
			// rfc7230 Sec 2.7.3 CRLF CRLF ends the header
			for(int headerEnd = 3; headerEnd < inputLen; headerEnd++){
				if(inputBuffer[headerEnd-3] == '\r' && inputBuffer[headerEnd -2] == '\n' && 
						inputBuffer[headerEnd -1] == '\r' && inputBuffer[headerEnd] == '\n') {
					cursor = headerEnd + 1;
					break;
				}
			}
			if(cursor > 0 || inputLen >= BUFSIZE) break;
		} while (bytesRead > 0);
		// logger.info("Found "+cursor+" bytes. Byte at cursor = "+inputBuffer[cursor]);
		// reset stream to the beginning of the body section
		if (inputLen <= BUFSIZE) {
			this.inputStream.reset();		// go back to the marked position
			this.inputStream.skip(cursor);	// skip to the body position (after the header)
		} else {
			// Have a BUFSIZE (16K) header without termination
			throw new HttpException(HttpStatus.BAD_REQUEST,"Header too large");
		}
		if(logEnabled) logger.info("HttpSession ("+Thread.currentThread().getName()+"): "+remoteHostname+"["+remoteIp+"] Read "+
				inputLen+" bytes\n"+new String(Arrays.copyOf(inputBuffer, inputLen),"UTF-8"));
		// Create a BufferedReader for parsing the header. Note that header MUST be restricted to US-ASCII characters
		BufferedReader headerIn = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(inputBuffer, 0, cursor),Charset.forName("US-ASCII")));
		// create the request, and return it
		return new HttpRequest(headerIn,inputStream);
	}
}
