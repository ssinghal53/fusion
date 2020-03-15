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
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.logging.Logger;

import net.aifusion.metamodel.Constants;

/**
 * Class to handle an HTTP request at the server
 * @author Sharad Singhal
 */
class HttpRequest {
	/** Logger for this class */
	private static final Logger logger = Logger.getLogger(HttpRequest.class.getName());
	/** HTTP protocol version */
	private String protocolVersion;
	/** server URI */
	private String uri;
	/** HTTP Method for this request */
	private HttpMethod httpMethod;
	/** request Target for this request (URI) */
	private String requestTarget;
	/** Parameters from the URI */
	private HashMap<String, String> queryParams = new HashMap<String,String>();
	/** Http Headers */
	private Map<HttpHeader, String> headers = new HashMap<HttpHeader, String>();
	/** Extension Headers */
	private Map<String,String> xheaders = new HashMap<String,String>();
	/** Input stream from the client */
	private BufferedInputStream inputStream;
	/** Cookie values embedded in this request */
	private HashMap<String,HttpCookie> cookies = new HashMap<String,HttpCookie>();
	/** Accepted Mime types (if any) by the client */
	private MimeType [] acceptedTypes = null;
	/** flag to enable logging */
	private boolean logEnabled = false;
	
	/**
	 * Create an http request from an input stream (used at the server)
	 * @param header - Buffered reader containing header information
	 * @param inputStream - inputStream containing body, if any
	 */
	public HttpRequest(BufferedReader header, BufferedInputStream inputStream){
		this.inputStream = inputStream;
		// read the header and locate items from it
		try {
			// Read the request line (RFC7230 Sec. 3.1.1)
			// request-line = method SP request-target SP HTTP-version CRLF
			// TODO: NOTE: RFC 2016 Page 21 recommends that empty lines should be ignored for robustness
			String inputLine = header.readLine();
			if(inputLine == null || inputLine.trim().isEmpty()){
				throw new HttpException(HttpStatus.BAD_REQUEST,"No request line");
			}
			String [] tokens = inputLine.split(" ");
			if(tokens.length != 3){
				throw new HttpException(HttpStatus.BAD_REQUEST,"Illegal request line: "+inputLine);
			}
			// get the HTTP Method
			httpMethod = HttpMethod.lookup(tokens[0].trim());
			if(httpMethod == null) throw new HttpException(HttpStatus.NOT_IMPLEMENTED,"Method ["+tokens[0]+"] not implemented");
			
			// get the request target
			requestTarget = tokens[1];
			int queryIndex = requestTarget.indexOf('?');
			if (queryIndex >= 0) {
				getQueryParms(requestTarget.substring(queryIndex + 1));
				uri = decodeRequest(requestTarget.substring(0, queryIndex));
			} else {
				uri = decodeRequest(requestTarget);
			}
			// get the protocol version HTTP/1.1 | HTTP/1.0
			switch(tokens[2]){
			case "HTTP/1.1":
			case "HTTP/1.0":
				protocolVersion = tokens[2];
				break;
			default:
				throw new HttpException(HttpStatus.UNSUPPORTED_HTTP_VERSION,tokens[2]+" not supported"); 
			}
			
			// TODO: read header elements. Note that we currently do not allow multi-line values for headers. This is a bug
			while((inputLine = header.readLine()) != null && !inputLine.isEmpty()){
				// logger.info("Parsing header "+inputLine);
				int separator = inputLine.indexOf(':');
				if(separator > 0){
					String key = inputLine.substring(0, separator);
					HttpHeader httpHeader = HttpHeader.lookup(key);
					String value = inputLine.substring(separator+1).trim();
					if(httpHeader != null){ // have a known HTTP header
						switch(httpHeader){
						case COOKIE:
							// note that we could parse cookies here since the client only sends name=value *(;name=value), 
							// but this way, we validate the cookie as part of the cookie constructor
							HttpCookie [] cs = HttpCookie.parseCookieValues(value);
							for(HttpCookie c : cs) cookies.put(c.getName().toLowerCase(), c);
							break;
						default:
							if(!headers.containsKey(httpHeader)) headers.put(httpHeader, value);
							break;
						}
					} else {
						// have an extended header -- add it to the extended headers
						// NOTE: Currently extended headers are CASE SENSITIVE
						if(!xheaders.containsKey(key)) xheaders.put(key, value);
					}
				} else {
					throw new HttpException(HttpStatus.BAD_REQUEST,"Unknown header line "+inputLine);
				}
			}
		} catch (Exception e) {
			throw new HttpException(HttpStatus.BAD_REQUEST,"Error while reading header",e);
		}
		if(logEnabled){
			StringBuilder sb = new StringBuilder();
			sb.append(httpMethod).append(" ").append(uri).append("\n");
			if(!headers.isEmpty()){
				for(Entry<HttpHeader, String> e : headers.entrySet()){
					sb.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
				}
			}
			if(!xheaders.isEmpty()){
				for(Entry<String, String> e : xheaders.entrySet()){
					sb.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
				}
			}
			if(!queryParams.isEmpty()){
				for(Entry<String,String> e : queryParams.entrySet()){
					sb.append(e.getKey()).append("=").append(e.getValue()).append("\n");
				}
			}
			if(!cookies.isEmpty()){
				for(Entry<String,HttpCookie> e : cookies.entrySet()){
					sb.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
				}
			}
			logger.info(sb.toString());
		}
		// logger.info("Found Request "+httpMethod+" on "+uri);
		return;
	}

	/**
	 * get the content length for this request
	 * @return - content length for this request
	 */
	public long getContentLength(){
		String contentLength = headers.get(HttpHeader.CONTENT_LENGTH);
		return contentLength == null || contentLength.isEmpty() ? 0L : Long.parseLong(headers.get(HttpHeader.CONTENT_LENGTH));
	}
	
	/**
	 * Get the content type for this request
	 * @return - content type defined by the CONTENT_TYPE header. BINARY is returned if the header is not defined, or cannot be parsed.
	 * @see MimeType#BINARY
	 */
	public MimeType getContentType(){
		String h = headers.get(HttpHeader.CONTENT_TYPE);
		if(h == null) return MimeType.BINARY;
		if(h.contains(";")) h = h.substring(0,h.indexOf(';')).trim();
		return MimeType.lookup(h);
	}
	
	/**
	 * Get the content character set used in this request
	 * @return - character set associated with this request. Constants.byteEncoding returned if not defined, or cannot be parsed
	 * @see Constants#byteEncoding 
	 */
	public String getCharset(){
		String h = headers.get(HttpHeader.CONTENT_TYPE);
		if(h != null && h.contains(";")){
			String [] tokens = h.split("[;=]");
			for(int i = 0; i < tokens.length-1; i++){
				if(tokens[i].trim().equalsIgnoreCase("charset")) return tokens[i+1].trim();
			}
		}
		return Constants.byteEncoding.name();
	}
	
	/**
	 * Get the protocol version for this request e.g., "HTTP/1.1"
	 * @return - protocolVersion in this request
	 */
	public String getProtocolVersion() {
		return protocolVersion;
	}

	/**
	 * Get the HTTP Method associated with this request, e.g., GET
	 * @return - httpMethod in this request
	 */
	public HttpMethod getHttpMethod() {
		return httpMethod;
	}
	
	/**
	 * Get the request target (includes URI and query) from this request
	 * @return http request target 
	 */
	public String getRequestTarget() {
		return requestTarget;
	}

	/**
	 * Get the input stream from the client
	 * @return the (Buffered) inputStream. It is positioned at the start of the body
	 */
	public BufferedInputStream getInputStream() {
		// TODO: If we have a chunked/gzipped input stream (have a TRANFER_ENCODING), then convert the input stream
		// to take that into account. Currently we are passing the input stream as-is to the handlers. Also
		// use CONTENT_LENGTH header to read in the body, and separate out the input stream to avoid having the
		// handlers block on the input stream
		return inputStream;
	}
	
	/**
	 * Get the URI associated with this request
	 * @return - uri for this request
	 */
	public String getURI(){
		return uri;
	}
	
	/**
	 * Get the value of an http header defined in this request
	 * @param header - requested header
	 * @return - value of header. Null if no such header defined
	 */
	public String getHeader(HttpHeader header){
		return header == null ? null : headers.get(header);
	}
	
	/**
	 * Get the value of an extension header
	 * @param header - extension header to get (Case sensitive)
	 * @return - value of the extension header, or null if no such header exists
	 */
	public String getXHeader(String header) {
		return header == null ? null : xheaders.get(header);
	}
	
	/**
	 * Get the map containing all Http headers
	 * @return - map containing all Http headers
	 */
	
	public Map<HttpHeader,String> getHeaders(){
		return headers;
	}
	
	/**
	 * Get map containing extended headers
	 * @return - map containing extended headers
	 */
	public Map<String,String> getXHeaders(){
		return xheaders;
	}
	
	/**
	 * Check if the request has an extension header
	 * @param xHeader - desired extension header (Case sensitive)
	 * @return - true if the request includes this extension header, false otherwise
	 */
	public boolean hasXHeader(String xHeader) {
		return xheaders.containsKey(xHeader);
	}
	
	/**
	 * Get the value of a uri parameter
	 * @param parameterName - requested key (case insensitive)
	 * @return - value of parameter. Null if no such parameter defined
	 */
	public String getUriParameter(String parameterName){
		if(parameterName == null) return null;
		for(String parameter : queryParams.keySet()){
			if(parameterName.equalsIgnoreCase(parameter)) return queryParams.get(parameter);
		}
		return null;
	}
	
	/**
	 * Get the map containing all uri parameters
	 * @return - map containing all parameters
	 */
	public Map<String,String> getUriParameters(){
		return queryParams;
	}

	/**
	 * Check if the client has indicated that this request is the final request in the session
	 * @return - true if the client has requested this as the final request of the session, false otherwise
	 */
	public boolean isLastRequest() {
		return "close".equalsIgnoreCase(headers.get(HttpHeader.CONNECTION));
	}
	
	/**
	 * Get all cookies defined in this request
	 * @return map containing cookies
	 */
	public Map<String,HttpCookie> getCookies(){
		return cookies;
	}
	
	/**
	 * Get the value of a cookie sent by the client
	 * @param cookieName - name (case insensitive) for the cookie
	 * @return - value of the cookie, or null if no such cookie was sent by the client
	 */
	public String getCookieValue(String cookieName){
		HttpCookie c =  cookies.get(cookieName.toLowerCase());
		return c != null ? c.getValue() : null;
	}
	
	/**
	 * Check if a particular MimeType is acceptable to the client
	 * @param mimeType - mimeType desired
	 * @return - true if mimeType is acceptable to the client, false otherwise 
	 */
	public boolean accepts(MimeType mimeType) {
		if(!headers.containsKey(HttpHeader.ACCEPT)) return true;	// missing Accept => all mime types are fine RFC2616
		boolean accept = false;
		for(MimeType m : getAcceptedTypes()){
			if(m.equals(mimeType)){
				accept = true;
				break;
			}
		}
		return accept;
	}
	
	/**
	 * Parse the Accept header in the request and return acceptable mime types
	 * @return - list of acceptable types
	 **/
	public MimeType[] getAcceptedTypes(){
		if(acceptedTypes == null){
			String accept = getHeader(HttpHeader.ACCEPT);
			if(accept != null){
				String [] acceptTypes = accept.split(",");
				if(acceptTypes != null){
					Vector<Accept> a = new Vector<Accept>();
					for(String elem : acceptTypes){
						try {
							a.add(new Accept(elem));
						} catch(Exception e){
							// ignore this media type-- we do not know about it
						}
					}
					Collections.sort(a);
					acceptedTypes = new MimeType[a.size()];
					for(int i = 0; i < a.size(); i++){
						acceptedTypes[i] = a.get(i).mime;
					}
				}
			}
			if(acceptedTypes == null) acceptedTypes = new MimeType[0];
		}
		return acceptedTypes;
	}
	
	/**
	 * Parsed Accept Header element in HTTP request
	 * @author Sharad Singhal
	 */
	protected class Accept implements Comparable<Object> {
		String [] mimeType = {"text","mof"};
		MimeType mime = MimeType.MOF;
		double q = 1;
		double level = 1.5;
		
		protected Accept(String elem){
			String [] temp = elem.split(";");
			if(temp != null){
				String [] type = temp[0].trim().toLowerCase().split("/");
				if(type != null && type.length == 2){
					mimeType[0] = type[0];
					mimeType[1] = type[1];
				}
				if(temp.length > 1){
					String [] qv = temp[1].trim().split("=");
					if(qv != null && qv.length > 1 && qv[0].trim().equals("q")){
						q = Double.parseDouble(qv[1].trim());
					}
				}
			}
			if(mimeType[0].equalsIgnoreCase("text") || mimeType[0].equals("*")){
				// first parameter must be "text" or "*"
				if(mimeType[1].equalsIgnoreCase("mof")){
					mime = MimeType.MOF;
				} else if(mimeType[1].equalsIgnoreCase("plain")){
					mime = MimeType.PLAINTEXT;
				} else if(mimeType[1].equalsIgnoreCase("html")){
					mime = MimeType.HTML;
				} else if(mimeType[1].equalsIgnoreCase("*")){
					mime = MimeType.TEXT;
				} else {
					throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"Uknown Type "+elem);
				}
			} else {
				throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"Uknown Type "+elem);
			}	
			return;
		}
		@Override
		public int compareTo(Object arg0) {
			if(arg0 == null || !(arg0 instanceof Accept)) return -1;
			Accept other = (Accept) arg0;
			int i = mimeType[0].compareTo(other.mimeType[0]);
			if(i != 0) return i;
			i = mimeType[1].compareTo(other.mimeType[1]);
			if(i != 0) return i;
			if(level != other.level) return level > other.level ? 1 : -1;
			if(q != other.q) return q > other.q ? 1 : -1; 
			return 0;
		}
		@Override
		public boolean equals(Object obj) {
			if(obj == null || ! (obj instanceof Accept)) return false;
			Accept other = (Accept) obj;
			return mimeType[0].equals(other.mimeType[0]) && 
					mimeType[1].equals(other.mimeType[1]) && 
					level == other.level && q == other.q;
		}
		@Override
		public int hashCode() {
			return mimeType[0].hashCode()+mimeType[1].hashCode()+(int)(level * 17 + q * 11);
		}
	}
	
	/*
	 * *************************
	 * Helper methods
	 * *************************
	 */
	
	/**
	 * Parse a query string (part of the URI) to obtain parameters
	 * @param queryString - query string of the form {key ['=' value] *['&' key ['=' value]]}
	 */
	private void getQueryParms(String queryString) {
		if (queryString != null && !queryString.trim().isEmpty()) {
			StringTokenizer st = new StringTokenizer(queryString, "&");
			while (st.hasMoreTokens()) {
				String e = st.nextToken();
				int separator = e.indexOf('=');
				String key = separator >= 0 ? decodeRequest(e.substring(0, separator)).trim() : decodeRequest(e).trim();
				String value = separator >= 0 ? decodeRequest(e.substring(separator + 1)) : null;
				queryParams.put(key, value);
			}
		}
		return;
	}

	/**
	 * Decode the request value
	 * @param string - input request
	 * @return - %decoded value
	 */
	private String decodeRequest(String string) {
        try {
            return URLDecoder.decode(string, "UTF-16");
        } catch (UnsupportedEncodingException ignored) {
        	throw new HttpException(HttpStatus.BAD_REQUEST,"URI ["+string+"] not understood");
        }
	}
}
