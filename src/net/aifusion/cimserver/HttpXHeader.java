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
 * Created Jul 23, 2017 by sharad
 */
package net.aifusion.cimserver;

import java.util.ArrayList;
import java.util.List;

/**
 * Http CIM Extension Headers known to the server
 * @author Sharad Singhal
 */
public enum HttpXHeader {
	INTRINSIC("Cim-Instrinsic",null),
	OBJECT_PATH("Cim-ObjectPath",new CimHeader[] {CimHeader.GET_ELEMENT,CimHeader.HAS_ELEMENT,CimHeader.DELETE_ELEMENT,CimHeader.GET_PROPERTY_NAMES,CimHeader.GET_PROPERTY_TYPE,
			CimHeader.GET_PROPERTY_VALUE,CimHeader.SET_PROPERTY_VALUE,CimHeader.GET_METHOD_NAMES,CimHeader.GET_METHOD_TYPE,CimHeader.GET_METHOD_PARAMETERS,
			CimHeader.INVOKE_METHOD,}),
	NAMESPACE_PATH("Cim-NamespacePath",new CimHeader[] {CimHeader.PUT_ELEMENT}),
	METHOD_NAME("Cim-MethodName",new CimHeader[]{CimHeader.GET_METHOD_TYPE,CimHeader.GET_METHOD_PARAMETERS,CimHeader.INVOKE_METHOD}),
	METHOD_TYPE("Cim-MethodType",new CimHeader[]{}),
	PROPERTY_NAME("Cim-PropertyName",new CimHeader[]{CimHeader.GET_PROPERTY_TYPE,CimHeader.GET_PROPERTY_VALUE,CimHeader.SET_PROPERTY_VALUE}),
	PROPERTY_TYPE("Cim-PropertyType",new CimHeader[]{CimHeader.SET_PROPERTY_VALUE}),
	PARAMETER_NAME("Cim-ParameterName",new CimHeader[]{}),
	ELEMENT_TYPES("Cim-ElementType",new CimHeader[]{CimHeader.GET_ELEMENTS}),
	NAME_SPACES("Cim-NameSpace",new CimHeader[]{CimHeader.GET_ELEMENTS}),
	ELEMENT_NAMES("Cim-ElementName",new CimHeader[]{CimHeader.GET_ELEMENTS}),
	LOCATE_SUBCLASS("Cim-LocateSubClass",new CimHeader[]{CimHeader.GET_ELEMENTS}),
	EVENT_TYPE("Cim-EventType",new CimHeader[]{CimHeader.ADD_LISTENER,CimHeader.REMOVE_LISTENER,CimHeader.HAS_LISTENER}),
	CIM_URL("Cim-RemoteUrl",new CimHeader[]{CimHeader.HAS_LISTENER,CimHeader.ADD_LISTENER,CimHeader.REMOVE_LISTENER,CimHeader.REGISTER_PROVIDER,CimHeader.UNREGISTER_PROVIDER})
	;
	
	/** Name of this extension header */
	private final String httpXHeader;
	/** CimHeaders for which this extension applies */
	private final CimHeader [] headers;
	
	/**
	 * Construct this extension header
	 * @param httpXHeader - http extension header name
	 * @param headers - CimHeaders to which this extension applies
	 */
	private HttpXHeader(String httpXHeader, CimHeader [] headers){
		this.httpXHeader = httpXHeader;
		this.headers = headers;
		return;
	}
	
	/**
	 * Test if this parameter applies to a given header
	 * @param h - header to test
	 * @return - true if this parameter applies to the given header, false otherwise
	 */
	public boolean appliesTo(CimHeader h){
		if(headers == null) return false;
		for(CimHeader head : headers){
			if(head.equals(h)) return true;
		}
		return false;
	}
	
	/**
	 * Get all valid parameters for a given header
	 * @param h - header to test
	 * @return - parameters appropriate for that header
	 */
	public static List<HttpXHeader> getXHeaders(CimHeader h){
		ArrayList<HttpXHeader> a = new ArrayList<HttpXHeader>();
		for(HttpXHeader p : HttpXHeader.values()){
			if(p.appliesTo(h)) a.add(p);
		}
		a.trimToSize();
		return a;
	}
	
	/**
	 * Get the extension header corresponding to a string
	 * @param value - string value for the header (case insensitive)
	 * @return - corresponding extension header, null if no such header exists
	 */
	public static HttpXHeader lookup(String value){
		for(HttpXHeader p : HttpXHeader.values()){
			if(p.httpXHeader.equalsIgnoreCase(value)) return p;
		}
		return null;
	}

	@Override
	public String toString() {
		return httpXHeader;
	}
}
