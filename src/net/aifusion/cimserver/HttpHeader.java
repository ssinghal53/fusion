/**
 * Copyright 2017, Sharad Singhal. All Rights Reserved.
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
 */
package net.aifusion.cimserver;

/**
 * Predefined HttpHeaders
 * @author Sharad Singhal
 */
public enum HttpHeader {
	// Request Headers
	ACCEPT("Accept"),
	ACCEPT_CHARSET("Accept-Charset"), 
	ACCEPT_ENCODING("Accept-Encoding"),
	ACCEPT_LANGUAGE("Accept-Language"),
	AUTHORIZATION("Authorization"),
	EXPECT("Expect"),
	FROM("From"),
	HOST("Host"),
	IF_MATCH("If-Match"),
	IF_MODIFIED_SINCE("If-Modified-Since"),
	IF_NONE_MATCH("If-None-Match"),
	IF_RANGE("If-Range"),
	IF_UNMODIFIED_SINCE("If-Unmodified-Since"),
	MAX_FORWARDS("Max-Forwards"),
	PROXY_AUTHORIZATION("Proxy-Authorization"),
	RANGE("Range"),
	REFERRER("Referer"),
	TE("TE"),
	USER_AGENT("User-Agent"),
	// Response Headers
	AGE("Age"),
	ETAG("ETag"),
	LOCATION("Location"),
	PROXY_AUTHENTICATE("Proxy-Authenticate"),
	RETRY_AFTER("Retry-After"),
	SERVER("Server"),
	VARY("Vary"),
	WWW_AUTHENTICATE("WWW-Authenticate"),
	// Entity Headers
	ALLOW("Allow"),
	CONTENT_ENCODING("Content-Encoding"),
	CONTENT_LANGUAGE("Content-Language"),
	CONTENT_LENGTH("Content-Length"),
	CONTENT_LOCATION("Content-Location"),
	// CONTENT_MD5("Content-MD5"),
	// CONTENT_RANGE("Content-Range"),
	CONTENT_TYPE("Content-Type"),
	EXPIRES("Expires"),
	LAST_MODIFIED("Last-Modified"),
	
	// Others
	COOKIE("Cookie"),
	DATE("Date"),
	ACCEPT_RANGES("Accept-Ranges"),
	CACHE_CONTROL("Cache-Control"),
	CONNECTION("Connection"),
	MIME_VERSION("MIME-Version"),
	PRAGMA("Pragma"),
	SET_COOKIE("Set-Cookie"),
	TRANSFER_ENCODING("Transfer-Encoding"),
	UPGRADE("Upgrade"),
	WARNING("Warning"),
	
	;
	/** Http message header */
	private final String header;

	/**
	 * Known HTTP Headers
	 * @param header - header text
	 */
	private HttpHeader(String header){
		this.header = header;
		return;
	}
	
	/**
	 * Get the header corresponding to a headerValue
	 * @param headerValue - value to test
	 * @return corresponding header value. Null if this headerValue is not defined
	 */
	public static HttpHeader lookup(String headerValue){
		for(HttpHeader h : HttpHeader.values()){
			if(h.header.equalsIgnoreCase(headerValue)) return h;
		}
		return null;
	}

	@Override
	public String toString() {
		return header;
	}
	
	

}
