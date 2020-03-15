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
 * Created Feb 5, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

/**
 * Exception used for Http Server
 * @author Sharad Singhal
 */
class HttpException extends RuntimeException {
	private static final long serialVersionUID = 1L;
	
	/** HTTP Status associated with this exception */
	private HttpStatus status;
	
	/**
	 * Generate an exception with a BAD_REQUEST(400) code
	 */
	public HttpException() {
		super();
		status = HttpStatus.BAD_REQUEST;
	}
	
	/**
	 * Generate an exception with a BAD_REQUEST(400) code and a message
	 * @param message - message to return
	 */
	public HttpException(String message) {
		super(message);
		status = HttpStatus.BAD_REQUEST;
		return;
	}
	
	/**
	 * Generate an exception with a BAD_REQUEST(400) code with a message and an underlying cause
	 * @param message - message to return
	 * @param cause - underlying cause
	 */
	public HttpException(String message, Throwable cause) {
		super(message, cause);
		status = HttpStatus.BAD_REQUEST;
	}

	/**
	 *  Generate an exception with a BAD_REQUEST(400) code with an underlying cause
	 *  @param cause - underlying cause
	 */
	public HttpException(Throwable cause) {
		super(cause);
		status = HttpStatus.BAD_REQUEST;
	}

	/**
	 * Generate an exception with a given status and a message
	 * @param status - Http Exception status
	 * @param string - associated code
	 */
	public HttpException(HttpStatus status, String string) {
		super(string);
		this.status = status;
		return;
	}

	/**
	 * Generate an http exception with a status, message, and underlying exception
	 * @param status - http exception status
	 * @param string - error message
	 * @param e - underlying exception that generated this exception
	 */
	public HttpException(HttpStatus status, String string, Exception e) {
		super(string,e);
		this.status = status;
		return;
	}

	/**
	 * Get the Http status code associated with this exception
	 * @return - http status code
	 */
	public HttpStatus getStatus() {
		return status;
	}

}
