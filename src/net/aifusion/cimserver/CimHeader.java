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
 * Created Jul 16, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

/**
 * Enumeration to define known CIM Intrinsic Methods used by the server and the client.
 * @see CimXHeader#INTRINSIC
 * @author Sharad Singhal
 */
public enum CimHeader {
	/** Put a named element into the server repository */
	PUT_ELEMENT("PutElement",HttpMethod.PUT),
	/** Get a named element from the server repository */
	GET_ELEMENT("GetElement",HttpMethod.GET),
	/** Check if the server has a named element */
	HAS_ELEMENT("HasElement",HttpMethod.HEAD),
	/** Delete a named element from the server repository */
	DELETE_ELEMENT("DeleteElement",HttpMethod.DELETE),
	/** Get the list of namespaces known to the server */
	GET_NAMESPACES("GetNameSpaces",HttpMethod.GET),
	/** Get a number of elements from the server */
	GET_ELEMENTS("GetElements",HttpMethod.GET),
	/** Add a listener to the server */
	ADD_LISTENER("AddListener",HttpMethod.POST),
	/** Remove a listener from the server */
	REMOVE_LISTENER("RemoveListener",HttpMethod.POST),
	/** Check if the server has a listener registered */
	HAS_LISTENER("HasListener",HttpMethod.HEAD),
	/** Register a child provider to the server */
	REGISTER_PROVIDER("RegisterProvider",HttpMethod.POST),
	/** Unregister a child provider from the server */
	UNREGISTER_PROVIDER("UnregisterProvider",HttpMethod.POST),
	/** Get the names of the properties defined in a CIM named element */
	GET_PROPERTY_NAMES("GetPropertyNames",HttpMethod.GET),
	/** Get the property type of some property */
	GET_PROPERTY_TYPE("GetPropertyType",HttpMethod.GET),
	/** Get the value of some property */
	GET_PROPERTY_VALUE("GetPropertyValue",HttpMethod.GET),
	/** Set the value of some property */
	SET_PROPERTY_VALUE("SetPropertyValue",HttpMethod.POST),
	/** Get the names of extrinsic methods defined in some class */
	GET_METHOD_NAMES("GetMethodNames",HttpMethod.GET),
	/** Get the return type of some extrinsic method defined in some class */
	GET_METHOD_TYPE("GetMethodType",HttpMethod.GET),
	/** Get the list of parameters defined in some extrinsic method */
	GET_METHOD_PARAMETERS("GetMethodParameters",HttpMethod.GET),
	/** Invoke an extrinsic method on an instance held on the server side */
	INVOKE_METHOD("InvokeMethod",HttpMethod.POST),
	/** Execute a CIM Query to the server and retrieve results */
	EXECUTE_QUERY("ExecuteQuery",HttpMethod.POST),
	/** Send an event to the server */
	SEND_EVENT("SendEvent",HttpMethod.POST),
	/** Shut down the client, and let the server know */
	SHUT_DOWN("Bye",HttpMethod.HEAD),
	/** Filter elements at the server */
	FILTER("Filter",HttpMethod.GET)
	/** Get provider end points at the server */
	;
	
	/** Http Extension Header to be used to define the Cim header */ 
	// public static final String httpXHeader = "Cim-Intrisic";
	
	/** Cim header value */
	private final String header;
	/** Http Method to be used with this header */
	private final HttpMethod httpMethod;
	
	/**
	 * Create a Cim Header
	 * @param header - header to be inserted in http request
	 * @param httpMethod - HttpMethod to be used
	 */
	private CimHeader(String header,HttpMethod httpMethod){
		this.header = header;
		this.httpMethod = httpMethod;
		return;
	}
	
	/**
	 * Get the Http Method to use with this header
	 * @return - HttpMethod to use with the header
	 */
	public HttpMethod getHttpMethod(){
		return httpMethod;
	}
	
	/**
	 * Lookup a particular Extension header from the HTTP request
	 * @param header - string containing extension header value (case insensitive)
	 * @return - CimHeader. Null if no such header found
	 */
	public static CimHeader lookup(String header) {
		for(CimHeader h : CimHeader.values()){
			if(h.header.equalsIgnoreCase(header)) return h;
		}
		return null;
	}
	
	@Override
	public String toString() {
		return header;
	}
}
