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

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.GZIPOutputStream;

import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.ModelException;

/**
 * Class to represent an http response
 * @author Sharad Singhal
 */
class HttpResponse {
	/** Logger for this class */
	private static final Logger logger = Logger.getLogger(HttpResponse.class.getName());
	/** Line termination */
	private static final String CRLF = "\r\n";
	
	/** HTTP status code after processing, e.g. "200 OK", HttpStatus.OK */
	private HttpStatus status = HttpStatus.OK;
	/** Data from the request, if any */
	private InputStream data = null;
	/** message to send in the body, if any */
	private String message = null;
	/** Content length */
	private long contentLength = 0;
	/** Requested method that generated this response (default is GET) */
	private HttpMethod requestMethod = HttpMethod.GET;
	/** flag to indicate chunked transfer */
	private boolean chunkedTransfer = false;
	/** flag to indicate gzip encoding. Implies chunkedTransfer = true */
	private boolean encodeAsGzip = false;
	/** flag to indicate session keep-alive */
	private boolean keepAlive = true;
	/** headers for the response */
	private final Map<HttpHeader, String> headers = new HashMap<HttpHeader, String>();
	/** extension headers for the response */
	private final Map<String,String> xHeaders = new HashMap<String,String>();
	/** Cookies for the response */
	private final Map<String,HttpCookie> cookies = new HashMap<String,HttpCookie>();
	/** Date formatter */
	private static final SimpleDateFormat gmtFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss 'GMT'", Locale.UK);
	static {
		gmtFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
	}
	
	/**
	 * Create an HTTP response that does not require any response body
	 * @param requestMethod - Http Method that caused this response.
	 * @param status - HTTP status to return
	 */
	protected HttpResponse(HttpMethod requestMethod, HttpStatus status) {
		this.requestMethod = requestMethod;
		this.status = status;
		this.data = new ByteArrayInputStream(new byte[0]);
		this.contentLength = 0L;
		return;
	}

	/**
	 * Create an HTTP response
	 * @param requestMethod - HttpMethod that caused this response.
	 * @param status - HTTP Status to return
	 * @param mimeType - Mime Type for this response
	 * @param data - data to be returned
	 */
	protected HttpResponse(HttpMethod requestMethod, HttpStatus status, MimeType mimeType, InputStream data) {
		this.requestMethod = requestMethod;
		this.status = status;
		headers.put(HttpHeader.CONTENT_TYPE, mimeType.getType()+";charset="+Constants.byteEncoding);
		if(data != null){
			this.data = data;
			this.contentLength = -1;
		} else {
			this.data = new ByteArrayInputStream(new byte[0]);
			this.contentLength = 0;
		}
		return;
	}
	
	/**
	 * Create an HTTP response from a string to be returned as content
	 * @param requestMethod - Http Method that caused this response.
	 * @param status - HTTP status to return
	 * @param mimeType - Mime Type for this response
	 * @param message - data to return
	 */
	protected HttpResponse(HttpMethod requestMethod, HttpStatus status, MimeType mimeType, String message) {
		this.requestMethod = requestMethod;
		this.status = status;
		headers.put(HttpHeader.CONTENT_TYPE, mimeType.getType()+";charset="+Constants.byteEncoding);
		if(message == null){
			this.data = new ByteArrayInputStream(new byte[0]);
			this.contentLength = 0L;
		} else {
			this.message = message;
			byte [] bytes = message.getBytes(Constants.byteEncoding);
			this.data = new ByteArrayInputStream(bytes);
			this.contentLength = bytes.length;
		}
		return;
	}
	
	/**
	 * Add a header to this response
	 * @param name - name of the header
	 * @param value - value of the header
	 */
	public void addHeader(HttpHeader name, String value) {
		// note that Transfer_Encoding and Content_Length are managed internally in HttpResponse
		if(HttpHeader.TRANSFER_ENCODING == name){
			// RFC7230
			// A server MUST NOT send a Transfer-Encoding header field in any response with a status code of 1xx (Informational) or 204 (No Content). 
			// A server MUST NOT send a Transfer-Encoding header field in any 2xx (Successful) response to a CONNECT request (Section 4.3.6 of [RFC7231]).
			String statusCode = status.code();
			if(statusCode.startsWith("1") || statusCode.equals("204") || 
					requestMethod == HttpMethod.CONNECT && statusCode.startsWith("2")){
				return;
			}
			String [] codings = value.split(",");
			for(String coding : codings){
				switch(coding.trim().toLowerCase()){
				case "gzip":	// note that gzip transfer encoding will also set chunkedTransfer = true
					encodeAsGzip = true;
				case "chunked":
					chunkedTransfer = true;
					break;
				default:
					throw new ModelException("Transfer-Encoding "+coding+" is not implemented");
				}
			}
			return;
		} else if(HttpHeader.CONTENT_LENGTH == name){
			logger.warning("HttpResponse#addHeader() Content-Length: "+value+" ignored");
			return;
		}
		headers.put(name, value);
		return;
	}
	
	/**
	 * Add an extension header to this response
	 * @param name - name of the header
	 * @param value - value of the header
	 */
	public void addXHeader(String name, String value){
		xHeaders.put(name, value);
		return;
	}
	
	/**
	 * Add a cookie to the response
	 * @param cookie - cookie to add
	 */
	public void addCookie(HttpCookie cookie){
		cookies.put(cookie.getName(), cookie);
		return;
	}
	
	/**
	 * @return true if connection is to be closed after this response has been sent.
	 */
	public boolean isLastResponse() {
		return "close".equalsIgnoreCase(headers.get(HttpHeader.CONNECTION));
	}
	
	/*
	 * Notes:
	 * For response messages, whether or not a message-body is included with a message is dependent on both the request
	 * method and the response status code (section 6.1.1). All responses to the HEAD request method MUST NOT
	 * include a message-body, even though the presence of entity-header fields might lead one to believe they do. All 1xx
	 * (informational), 204 (no content), and 304 (not modified) responses MUST NOT include a message-body. All other
	 * responses do include a message-body, although it MAY be of zero length.
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
	 * Send this response to a given output stream
	 * @param outputStream - output Stream to use for sending the response
	 */
	protected void send(OutputStream outputStream) {
		try {
			PrintWriter pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, Constants.byteEncoding)), false);
			// send the status line
			pw.append("HTTP/1.1 ").append(status.toString()).append(CRLF);
			
			// send mandatory headers if not set otherwise
			if(!headers.containsKey(HttpHeader.DATE)) {
				printHeader(pw, HttpHeader.DATE.toString(), gmtFormat.format(new Date()));
			}
			if(!headers.containsKey(HttpHeader.CONNECTION)) {
				printHeader(pw, HttpHeader.CONNECTION.toString(), (this.keepAlive ? "keep-alive" : "close"));
			}
			
			// Rules for encoding Transfer-Encoding and Content-Length headers (RFC 7230, page 30)
			boolean sendContentIfAvailable = true;
			String statusCode = status.code();
			if(chunkedTransfer || contentLength < 0){
				if(!(statusCode.startsWith("1") || statusCode.equals("204") || requestMethod == HttpMethod.CONNECT && statusCode.startsWith("2"))){
					chunkedTransfer = true;
					printHeader(pw,HttpHeader.TRANSFER_ENCODING.toString(),encodeAsGzip ? "gzip, chunked" : "chunked");
				}
			} else if(!(requestMethod == HttpMethod.HEAD || requestMethod == HttpMethod.GET && statusCode.equals("304") || statusCode.startsWith("1") ||
					statusCode.equals("204") || requestMethod == HttpMethod.CONNECT && statusCode.startsWith("2"))){
				printHeader(pw,HttpHeader.CONTENT_LENGTH.toString(),String.valueOf(contentLength));
			} else {
				sendContentIfAvailable = false;	// we are required NOT to send any content
			}
			
			// server?
			
			// send all standard headers defined in the response.
			for (Entry<HttpHeader, String> entry : headers.entrySet()) {
				printHeader(pw, entry.getKey().toString(), entry.getValue());
			}
			
			// send all extension headers defined in the response
			for (Entry<String, String> entry : xHeaders.entrySet()) {
				printHeader(pw, entry.getKey(), entry.getValue());
			}
			
			// send all cookies in the response
			for(HttpCookie c : cookies.values()){
				printHeader(pw,HttpHeader.SET_COOKIE.toString(),c.toString());
			}
			
			pw.append(CRLF);
			pw.flush();
			if(sendContentIfAvailable){
				sendBodyWithTransferAndEncoding(outputStream, contentLength);
				outputStream.flush();
			}
		} catch (IOException ioe) {
			logger.log(Level.SEVERE, "Could not send response to the client", ioe);
		}
		return;
	}
	
	/* ************************************
	 * Helper Classes and Methods
	 * ************************************
	 */
	
	/**
	 * Output stream that will automatically send every write to the wrapped
	 * OutputStream according to chunked transfer:
	 * http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6.1
	 */
	private static class ChunkedOutputStream extends FilterOutputStream {
		public ChunkedOutputStream(OutputStream out) {
			super(out);
		}
		@Override
		public void write(int b) throws IOException {
			byte[] data = {
					(byte) b
			};
			write(data, 0, 1);
			return;
		}
		@Override
		public void write(byte[] b) throws IOException {
			write(b, 0, b.length);
			return;
		}
		@Override
		public void write(byte[] b, int off, int len) throws IOException {
			if (len == 0)
				return;
			this.out.write(String.format("%x\r\n", len).getBytes(Constants.byteEncoding));
			this.out.write(b, off, len);
			this.out.write(CRLF.getBytes(Constants.byteEncoding));
			return;
		}
		public void finish() throws IOException {
			this.out.write("0\r\n\r\n".getBytes(Constants.byteEncoding));
			return;
		}
	}

	/**
	 * Put a given {header,value} pair on a print writer
	 * @param pw - printwriter to use
	 * @param key - header key
	 * @param value - value of the header
	 */
	private void printHeader(PrintWriter pw, String key, String value){
		pw.append(key).append(": ").append(value).append(CRLF);
	}

	/**
	 * Send the body with correct transfer and encoding
	 * @param outputStream - output stream to send to
	 * @param pending - number of bytes pending
	 * @throws IOException - if something goes wrong
	 */
	private void sendBodyWithTransferAndEncoding(OutputStream outputStream, long pending) throws IOException {
		if (chunkedTransfer) {
			ChunkedOutputStream chunkedOutputStream = new ChunkedOutputStream(outputStream);
			sendBodyWithEncoding(chunkedOutputStream, -1);
			chunkedOutputStream.finish();
		} else {
			sendBodyWithEncoding(outputStream, pending);
		}
	}

	/**
	 * Send the body with correct encoding
	 * @param outputStream - the outputStream to send data to
	 * @param pending - number of bytes pending
	 * @throws IOException - if something goes wrong while sending the data
	 */
	private void sendBodyWithEncoding(OutputStream outputStream, long pending) throws IOException {
		if(encodeAsGzip) {
			GZIPOutputStream gzipOutputStream = new GZIPOutputStream(outputStream);
			sendBody(gzipOutputStream, -1);
			gzipOutputStream.finish();
		} else {
			sendBody(outputStream, pending);
		}
	}

	/**
	 * Send the response body to the specified OutputStream.
	 * @param outputStream - the OutputStream to send data to
	 * @param pending - number of bytes to send. If -1, all available data is sent
	 * @throws IOException - in case of errors
	 */
	private void sendBody(OutputStream outputStream, long pending) throws IOException {
		long BUFFER_SIZE = 16 * 1024;
		byte[] buff = new byte[(int) BUFFER_SIZE];
		boolean sendEverything = (pending == -1);
		while (pending > 0 || sendEverything) {
			long bytesToRead = sendEverything ? BUFFER_SIZE : Math.min(pending, BUFFER_SIZE);
			int read = data.read(buff, 0, (int) bytesToRead);
			if (read <= 0) break;
			outputStream.write(buff, 0, read);
			if (!sendEverything) {
				pending -= read;
			}
		}
		data.close();
	}

	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append(requestMethod).append(" ").append(status).append("\n");
		if(!headers.isEmpty()){
			for(Entry<HttpHeader,String> e : headers.entrySet()){
				b.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
			}
		}
		if(!xHeaders.isEmpty()){
			for(Entry<String,String> e : xHeaders.entrySet()){
				b.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
			}
		}
		if(message != null) b.append(message).append("\n");		
		return b.toString();
	}
}
