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
 * Created Jan 29, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import net.aifusion.metamodel.ModelException;

/**
 * Enumeration to manage HTTP status values
 * @author Sharad Singhal
 */
public enum HttpStatus {
	/** 100 - Continue */
	CONTINUE("100","Continue"),
	/** 101 - Switching Protocols */
	SWITCH_PROTOCOL("101","Switching Protocols"),
	/** 200 - OK */
	OK("200","OK"),
	/** 201 - Created */
	CREATED("201","Created"),
	/** 202 - Accepted */
	ACCEPTED("202","Accepted"),
	/** 203 - Non Authoritative Information */
	NON_AUTHORITATIVE_INFORMATION("203","Non-Authoritative Information"),
	/** 204 - No Content */
	NO_CONTENT("204", "No Content"),
	/** 205 - Reset Content */
	RESET_CONTENT("205","Reset Content"),
	/** 206 - Partial Content */
	PARTIAL_CONTENT("206", "Partial Content"),
	/** 301 - Moved Permanently */
	REDIRECT("301", "Moved Permanently"),
	/** 304 - Not Modified */
	NOT_MODIFIED("304", "Not Modified"),
	/** 400 - bad request */
	BAD_REQUEST("400", "Bad Request"),
	/** 401 - Unauthorized */
	UNAUTHORIZED("401","Unauthorized"),
	/** 403 - Forbidden */
	FORBIDDEN("403", "Forbidden"),
	/** 404 - Not Found */
	NOT_FOUND("404","Not Found"),
	/** 405 - Method not allowed */
	METHOD_NOT_ALLOWED("405", "Method Not Allowed"),
	/** 406 - Not acceptable */
	NOT_ACCEPTABLE("406", "Not Acceptable"),
	/** 408 - Request Timeout */
	REQUEST_TIMEOUT("408", "Request Timeout"),
	/** 409 - Conflict */
	CONFLICT("409", "Conflict"),
	/** 411 - Length required */
	LENGTH_REQUIRED("411","Length Required"),
	/** 413 - Payload too large */
	PAYLOAD_TOO_LARGE("413","Payload Too Large"),
	/** 414 - URI too long */
	URI_TOO_LONG("414","URI Too Long"),
	/** 415 - Unsupported Media Type */
	UNSUPPORTED_MEDIA_TYPE("415","Unsupported Media Type"),
	/** 416 - Range Not Satisfiable */
	RANGE_NOT_SATISFIABLE("416", "Range Not Satisfiable"),
	/** 500 - Internal Server Error */
	INTERNAL_ERROR("500","Internal Server Error"),
	/** 501 - Not Implemented */
	NOT_IMPLEMENTED("501","Not Implemented"),
	/** 503 - Service Unavailable */
	SERVICE_UNAVAILABLE("503","Service Unavailable"),
	/** 504 - Gateway Timeout */
	GATEWAY_TIMEOUT("504","Gateway Timeout"),
	/** 505 - Http version not supported */
	UNSUPPORTED_HTTP_VERSION("505", "HTTP Version Not Supported")
	;
	/*
	 +------+-------------------------------+--------------------------+
	 | Code | Reason-Phrase | Defined in... |
	 +------+-------------------------------+--------------------------+
	 | 100 | Continue | Section 6.2.1 |
	 | 101 | Switching Protocols | Section 6.2.2 |
	 | 200 | OK | Section 6.3.1 |
	 | 201 | Created | Section 6.3.2 |
	 | 202 | Accepted | Section 6.3.3 |
	 | 203 | Non-Authoritative Information | Section 6.3.4 |
	 | 204 | No Content | Section 6.3.5 |
	 | 205 | Reset Content | Section 6.3.6 |
	 | 206 | Partial Content | Section 4.1 of [RFC7233] |
	 | 300 | Multiple Choices | Section 6.4.1 |
	 | 301 | Moved Permanently | Section 6.4.2 |
	 | 302 | Found | Section 6.4.3 |
	 | 303 | See Other | Section 6.4.4 |
	 | 304 | Not Modified | Section 4.1 of [RFC7232] |
	 | 305 | Use Proxy | Section 6.4.5 |
	 | 307 | Temporary Redirect | Section 6.4.7 |
	 | 400 | Bad Request | Section 6.5.1 |
	 | 401 | Unauthorized | Section 3.1 of [RFC7235] |
	 | 402 | Payment Required | Section 6.5.2 |
	 | 403 | Forbidden | Section 6.5.3 |
	 | 404 | Not Found | Section 6.5.4 |
	 | 405 | Method Not Allowed | Section 6.5.5 |
	 | 406 | Not Acceptable | Section 6.5.6 |
	 | 407 | Proxy Authentication Required | Section 3.2 of [RFC7235] |
	 | 408 | Request Timeout | Section 6.5.7 |
	 | 409 | Conflict | Section 6.5.8 |
	 | 410 | Gone | Section 6.5.9 |
	 | 411 | Length Required | Section 6.5.10 |
	 | 412 | Precondition Failed | Section 4.2 of [RFC7232] |
	 | 413 | Payload Too Large | Section 6.5.11 |
	 | 414 | URI Too Long | Section 6.5.12 |
	 | 415 | Unsupported Media Type | Section 6.5.13 |
	 | 416 | Range Not Satisfiable | Section 4.4 of [RFC7233] |
	 | 417 | Expectation Failed | Section 6.5.14 |
	 | 426 | Upgrade Required | Section 6.5.15 |
	 | 500 | Internal Server Error | Section 6.6.1 |
	 | 501 | Not Implemented | Section 6.6.2 |
	 | 502 | Bad Gateway | Section 6.6.3 |
	 | 503 | Service Unavailable | Section 6.6.4 |
	 | 504 | Gateway Timeout | Section 6.6.5 |
	 | 505 | HTTP Version Not Supported | Section 6.6.6 |
	 +------+-------------------------------+--------------------------+
*/
	private String code;
	private String description;
	private HttpStatus(String code, String description){
		this.code = code;
		this.description = description;
		return;
	}
	/**
	 * Code associated with this status
	 * @return - code for this status
	 */
	public String code(){
		return code;
	}
	
	/**
	 * Description associated with this status
	 * @return - description for this status
	 */
	public String description(){
		return description;
	}
	
	/**
	 * Look up the status associated with a status code
	 * @param status - integer code value (e.g., 200)
	 * @return - HTTP Status
	 */
	public static HttpStatus lookup(int status){
		String statusCode = String.valueOf(status);
		for(HttpStatus s : HttpStatus.values()){
			if(s.code.equals(statusCode)) return s;
		}
		throw new ModelException("HttpStatus- code "+status+" not handled");
	}
	
	@Override
	public String toString() {
		return new StringBuilder(code).append(" ").append(description).toString();
	}
	
}
