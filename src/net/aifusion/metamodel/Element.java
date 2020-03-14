
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
 * Created Dec 29, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Abstract class that is used as base class for all CIM elements
 * @author Sharad Singhal
 */
abstract class Element {
	/** Mixed case name for this element */
	private String name;
	/** Normalized (lower case) name for this element */
	private String lowerCaseName;
	/** Element Type for this element */
	private ElementType type;

	/**
	 * Create a CIM element with given element type and name
	 * @param type - type of element (must not be null)
	 * @param name - name of element (must not be null or empty)
	 */
	Element(ElementType type, String name) {
		if(type == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Element type cannot be null");
		if(name == null || name.trim().isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Element name cannot be null or empty string");
		this.type = type;
		this.name = name.trim();
		lowerCaseName = this.name.toLowerCase();
		return;
	}
	
	/**
	 * Get the name of this element
	 * @return - name of this element
	 * @see #getLowerCaseName()
	 */
	public String getName(){
		return name;
	}
	
	/**
	 * Get the case-normalized (lower case) name for this element
	 * @return - name of this element normalized to lower case
	 * @see #getName()
	 */
	public String getLowerCaseName(){
		return lowerCaseName;
	}
	
	/**
	 * Get the CIM type of this element
	 * @return - CIMType for this element
	 * @see net.aifusion.metamodel.ElementType
	 */
	public ElementType getElementType(){
		return type;
	}
	
	/**
	 * Return the MOF representation of this element
	 * @return - String containing MOF representation of this element
	 */
	public String toMOF(){
		return toMOF("");
	};
	
	/**
	 * Return the MOF representation of this element prefixed by a given string
	 * @param prefix
	 * @return - MOF representation of this element prefixed by a given prefix
	 */
	abstract String toMOF(String prefix);
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return toMOF("");
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return type.hashCode()+lowerCaseName.hashCode();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof Element)) return false;
		Element other = (Element) obj;
		return type == other.type && lowerCaseName.equals(other.lowerCaseName);
	}
}
