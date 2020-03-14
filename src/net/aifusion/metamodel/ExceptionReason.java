/**
 * Copyright 2013, Sharad Singhal, All Rights Reserved
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
 * Reason for throwing a CIM Exception. See DSP0200 for definitions
 * @author Sharad Singhal
 */
public enum ExceptionReason {
	/** 1- A general error occurred that is not covered by a more specific error code */
	FAILED(1,"A general error occurred that is not covered by a more specific error code"),
	/** 2- Access to a resource is not available to the client */
	ACCESS_DENIED(2,"Access to a resource is not available to the client"),
	/** 3- The target namespace does not exist */
	INVALID_NAMESPACE(3,"The target namespace does not exist"),
	/** 4- One or more parameter values passed to the method are not valid */
	INVALID_PARAMETER(4,"One or more parameter values passed to the method are not valid"),
	/** 5- The specified class does not exist */
	INVALID_CLASS(5,"The specified class does not exist"),
	/** 6- The requested object cannot be found. See also HTTP_NOT_FOUND*/
	NOT_FOUND(6,"The requested object cannot be found"),
	/** 7- The requested operation is not supported */
	NOT_SUPPORTED(7,"The requested operation is not supported"),
	/** 8- The operation cannot be invoked on this class because it has subclasses */
	CLASS_HAS_CHILDREN(8,"The operation cannot be invoked on this class because it has subclasses"),
	/** 9- The operation cannot be invoked on this class because one or more instances of this class exist */
	CLASS_HAS_INSTANCES(9,"The operation cannot be invoked on this class because one or more instances of this class exist"),
	/** 10- The operation cannot be invoked because the specified superclass does not exist */
	INVALID_SUPERCLASS(10,"The operation cannot be invoked because the specified superclass does not exist"),
	/** 11- The operation cannot be invoked because an object already exists */
	ALREADY_EXISTS(11,"The operation cannot be invoked because an object already exists"),
	/** 12- The specified property does not exist */
	NO_SUCH_PROPERTY(12,"The specified property does not exist"),
	/** 13- The dataValue supplied is not compatible with the type */
	TYPE_MISMATCH(13,"The dataValue supplied is not compatible with the type"), 
	/** 14- The query language is not recognized or supported */
	QUERY_LANGUAGE_NOT_SUPPORTED(14,"The query language is not recognized or supported"),
	/** 15- The query is not valid for the specified query language */
	INVALID_QUERY(15,"The query language is not recognized or supported"),
	/** 16- The extrinsic method cannot be invoked because it is not available */
	METHOD_NOT_AVAILABLE(16,"The extrinsic method cannot be invoked because it is not available"),
	/** 17- The specified extrinsic method does not exist */
	METHOD_NOT_FOUND(17,"The specified extrinsic method does not exist"),
	/** 20- The specified Namespace is not empty */
	NAMESPACE_NOT_EMPTY(20,"The specified extrinsic method does not exist"),
	/** 21 The enumeration identified by the specified context cannot be found, is in a closed
	state, does not exist, or is otherwise invalid. */
	INVALID_ENUMERATION_CONTEXT(21,"The enumeration identified by the specified context cannot be found, is in a closed state, does not exist, or is otherwise invalid"),
	/** 22 The specified operation timeout is not supported by the server */
	INVALID_OPERATION_TIMEOUT(22,"The specified operation timeout is not supported by the server"),
	/** 23 The Pull operation has been abandoned due to execution of a concurrent CloseEnumeration
	 * operation on the same enumeration. */
	PULL_HAS_BEEN_ABANDONED(23,"The Pull operation has been abandoned due to execution of a concurrent CloseEnumeration operation on the same enumeration"),
	/** 24 The attempt to abandon a concurrent Pull operation on the same enumeration failed, the concurrent Pull operation proceeds normally. */
	PULL_CANNOT_BE_ABANDONED(24,"The attempt to abandon a concurrent Pull operation on the same enumeration failed, the concurrent Pull operation proceeds normally"),
	/** 25 Using a filter in the enumeration is not supported by the Server. */
	FILTERED_ENUMERATION_NOT_SUPPORTED(25,"Using a filter in the enumeration is not supported by the Server"),
	/** 26 The CIM Server does not support continuation on error. */
	CONTINUATION_ON_ERROR_NOT_SUPPORTED(26,"The CIM Server does not support continuation on error"),
	/** 27 The CIM Server has failed the operation based upon exceeding server limits. */
	SERVER_LIMITS_EXCEEDED(27,"The CIM Server has failed the operation based upon exceeding server limits"),
	/** 28 The Server is in the process of shutting down and cannot process the operation at this time. */
	SERVER_IS_SHUTTING_DOWN(28,"The Server is in the process of shutting down and cannot process the operation at this time")
	;
	
	/** Description of this description */
	private final String description;
	/** Numerical code for this description */
	private final int code;
	
	/**
	 * Create a Reason for the Exception
	 * @param code - error code
	 * @param description - string containing the description for the reason
	 */
	private ExceptionReason(int code, String description){
		this.code = code;
		this.description = description;
	}
	
	/**
	 * Get the error code corresponding to this reason
	 * @return - error code
	 */
	public int getCode(){
		return code;
	}
	
	/**
	 * Get a human-readable description for the reason
	 * @return - string containing description
	 */
	public String getDescription(){
		return description;
	}

	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append(code);
		b.append(" ");
		b.append(name());
		b.append(" [");
		b.append(description);
		b.append("]");
		return b.toString();
	}
	
	
}
