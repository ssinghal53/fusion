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

/**
 * Class to manage known HTTP Methods. See RFC 7231 Section 4
 * @author Sharad Singhal
 */
enum HttpMethod {
	/** Transfer a current representation of the target resource */
	GET,	// required, safe, idempotent, cacheable
	/** Same as GET, but only transfer the status line and header section */ 
	HEAD,	// required, safe, idempotent, cacheable
	/** Perform resource-specific processing on the request payload */
	POST,	// optional, not safe, cacheable
	/** Replace all current representations of the target resource with the request payload */
	PUT,	// optional, not safe, idempotent
	/** Remove all current representations of the target resource */
	DELETE,	// optional, not safe, idempotent
	/** Establish a tunnel to the server identified by the target resource */
	CONNECT,
	/** Describe the communication options for the target resource */
	OPTIONS,// optional, safe, idempotent
	/** Perform a message loop-back test along the path to the target resource. */
	TRACE;	// optional, safe, idempotent
	
	/**
	 * Get an HttpMethod corresponding to the given string
	 * @param method - case insensitive string to match. Note that RFC 7231 Section 4.1 recommends all uppercase strings be used
	 * for standard methods
	 * @return - corresponding HttpMethod. Null if none found
	 */
	public static HttpMethod lookup(String method){
		for(HttpMethod m : HttpMethod.values()){
			if(m.name().equalsIgnoreCase(method)) return m;
		}
		return null;
	}
}
