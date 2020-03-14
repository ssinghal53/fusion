/**
 * Copyright 2013, Sharad Singhal, All Rights Reserved.
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
 * Created Dec 26, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Class to declare CIM Exceptions
 * @author Sharad Singhal
 */
public class ModelException extends RuntimeException {
	/**serial version UID required by Java */
	private static final long serialVersionUID = 1L;
	/** reason for this exception */
	private ExceptionReason reason = ExceptionReason.FAILED;

	/*
	 * ******************************************
	 * General constructors from RuntimeException
	 * ******************************************
	 */
	/**
	 * Create a FAILED CIM Exception
	 */
	public ModelException() {
		super();
		return;
	}

	/**
	 * Create a FAILED CIM Exception with a given message
	 * @param message  - message for the exception
	 */
	public ModelException(String message) {
		super(message);
		return;
	}

	/**
	 * Create a FAILED CIM Exception with an underlying cause
	 * @param cause - throwable item that caused this exception
	 */
	public ModelException(Throwable cause) {
		super(cause);
		return;
	}

	/**
	 * Create a FAILED CIM Exception with a message and an underlying cause
	 * @param message - message for the exception
	 * @param cause - underlying cause of the exception
	 */
	public ModelException(String message, Throwable cause) {
		super(message, cause);
		return;
	}
	
	/*
	 * *************************************
	 * Other constructors that take a reason
	 * *************************************
	 */
	/**
	 * Create an Exception with the given reason
	 * @param reason - reason for the exception
	 */
	public ModelException(ExceptionReason reason){
		super();
		if(reason != null) this.reason = reason;
		return;
	}
	
	/**
	 * Create an exception with a given reason, and a message string
	 * @param reason - reason for the exception
	 * @param message - accompanying message
	 */
	public ModelException(ExceptionReason reason,String message) {
		super(message);
		if(reason != null) this.reason = reason;
		return;
	}

	/**
	 * Create an exception with a given reason and the underlying cause
	 * @param reason - reason for the exception
	 * @param cause - underlying cause
	 */
	public ModelException(ExceptionReason reason,Throwable cause) {
		super(cause);
		if(reason != null) this.reason = reason;
		return;
	}

	/**
	 * Create an exception with a given reason, a message, and an underlying cause
	 * @param reason - reason for the exception
	 * @param message - accompanying message
	 * @param cause - underlying cause
	 */
	public ModelException(ExceptionReason reason,String message, Throwable cause) {
		super(message, cause);
		if(reason != null) this.reason = reason;
		return;
	}
	
	/**
	 * Get the reason provided in this exception
	 * @return - reason for this exception
	 */
	public ExceptionReason getReason(){
		return reason;
	}
	
	/**
	 * Get the message accompanying this exception. If no message is provided, and an underlying cause
	 * is defined, the message from the cause is returned. If no underlying cause is defined, then
	 * the description from the corresponding ExceptionReason is returned.
	 */
	public String getMessage() {
		String message = super.getMessage();
		// check if the message is an underlying throwable class name
		Throwable cause = getCause();
		if(cause != null && message.startsWith(cause.getClass().getName())){
			message = cause.getMessage();
		}
		return message != null ? message : reason.getDescription();
	}
	
	/**
	 * Get the message from the root cause of this exception. If this ModelException wraps a number of underlying
	 * Exception, get the message from the deepest nested exception that provides a message
	 * @return - the message from the root cause of this exception.
	 */
	public String getRootMessage(){
		// check underlying throwables and recurse down to the deepest wrapped message
		String rootMessage = null;
		Throwable cause = this;
		while(cause != null){
			String message = cause.getMessage();
			cause = cause.getCause();
			if(cause == null){
				if(message != null) rootMessage = message;
			} else {
				if(message != null && !message.equals(cause.getClass().getName())) rootMessage = message;
			}
		}
		return rootMessage;
	}
	
	/**
	 * Get the string representation that contains the ExceptionReason and message for this Exception, followed
	 * by any embedded exceptions (and their messages)
	 * @return a String representation of the exception
	 */
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append(reason);		// reason for this exception
		String message = super.getMessage();
		Throwable cause = getCause();
		if(cause == null){
			if(message != null){
				sb.append(": ");
				sb.append(message);
			}
		} else {
			if(message != null && !message.startsWith(cause.getClass().getName())){
				sb.append(": ");
				sb.append(message);
			}
		}
		while(cause != null){
			sb.append("\n");
			sb.append(cause.getClass().getName());
			sb.append(": ");
			sb.append(cause.getMessage());
			cause = cause.getCause();
		}
		return sb.toString();
	}

}
