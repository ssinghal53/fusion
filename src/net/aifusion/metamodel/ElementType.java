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
 * Created Dec 29, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Enumeration containing Type definitions for CIM elements
 * @author Sharad Singhal
 */
public enum ElementType {
	/** Element is a CIM Qualifier Type */
	QUALIFIERTYPE("Qualifier"),
	/** Element is a CIM Structure */
	STRUCTURE("Structure"),
	/** Element is a CIM Structure value */
	STRUCTUREVALUE(""),
	/** Element is a CIM Enumeration */
	ENUMERATION("Enumeration"),
	/** Element is a CIM Enumeration Value */
	ENUMERATIONVALUE(""),
	/** Element is a CIM Interface */
	INTERFACE("Interface"),
	/** Element is a CIM Association (only used for CimV.2 parsing) */
	ASSOCIATION("Association"),
	/** Element is a CIM Class */
	CLASS("Class"),
	/** Element is a CIM Instance */
	INSTANCE(""),
	/** Element is a CIM Qualifier */
	QUALIFIER(""),
	/** Element is a CIM Parameter */
	PARAMETER(""),
	/** Element is a CIM Method */
	METHOD(""),
	/** Element is a CIM Property */
	PROPERTY(""),
	/** Element is a CIM Reference */
	REFERENCE(""),
	/** Element is a CIM Indication (only used for CimV.2 parsing) */
	INDICATION("Indication"),
	/** Element is a CIM Schema */
	SCHEMA("");
	
	
	private String mof;
	private ElementType(String mof){
		this.mof = mof;
		return;
	}

	/**
	 * Check if this elementType represents a Named Element
	 * @return true if this element type is a named element
	 */
	public boolean isNamedElement() {
		return this == INSTANCE || this == CLASS || this == STRUCTURE || this == ENUMERATION || this == QUALIFIERTYPE || 
				this == INTERFACE || this == STRUCTUREVALUE || this == ASSOCIATION;
	}
	
	/**
	 * Return the MOF string for this element.
	 * @return - MOF string for this element. Empty for elements that are not top-level elements (Class, Association, Structure, Enumeration, Interface, Qualifier)
	 */
	public String toMOF(){
		return mof;
	}
}
