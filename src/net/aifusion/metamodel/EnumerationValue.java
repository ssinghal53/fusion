/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Jun 22, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.List;

/**
 * Class to represent an Enumeration Value. Enumeration values are (name, value) pairs, where
 * the name is associated with the value.
 * @author Sharad Singhal
 */
public class EnumerationValue extends QualifiedElement {
	/** dataValue of this enumeration dataValue */
	private DataValue value;
	/** Fully qualified name of the enumeration within which this value resides */
	private String enumName;

	/**
	 * Create an enumeration value
	 * @param valueName - name of the enumeration value
	 * @param enumName -- fully qualified name of the enumeration within which this value resides
	 * @param value - value associated with the name of the enumeration value
	 * @param qualifiers - qualifiers, if any, on the enumeration value
	 */
	protected EnumerationValue(String valueName, String enumName, DataValue value, List<Qualifier> qualifiers) {
		// enumeration values do not have superTypes, and do not override values defined in superTypes
		super(ElementType.ENUMERATIONVALUE, valueName, qualifiers);
		if(enumName == null || enumName.trim().isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,valueName+" must reside in an enumeration");
		this.enumName = enumName;
		this.value = value;	// data values are validated in CimEnumeration during construction
		return;
	}
	
	/**
	 * Get the literal data value embedded in this enumeration Value
	 * @return - value associated with this enumeration dataValue
	 */
	public DataValue getDataValue(){
		return value != null ? value : new DataValue(getName());
	}
	
	/**
	 * Check if this enumeration value has a data value defined
	 * @return - true if the enumeration value has a data value defined, false otherwise
	 */
	public boolean hasValue(){
		return value != null;
	}
	
	/**
	 * Get the literal data type associated with this enumeration value
	 * @return - data type associated with this Enumeration value
	 */
	public DataType getDataType(){
		return value != null ? value.getType() : DataType.STRING;
	}
	
	/**
	 * Get the fully qualified name of the enumeration within which this value resides
	 * @return - fully qualified name of the enumeration
	 */
	public String getEnumName(){
		return enumName;
	}
	
	/**
	 * Get the fully qualified name of this value (including the enumeration 
	 * hierarchy within which it resides)
	 * @return full name of this enumeration value
	 */
	public String getFullName() {
		return enumName+"."+getName();
	}

	/**
	 * Get the MOF representation of this enumeration value in {name = value} form
	 * @see #toString()
	 * @see #getFullName()
	 * @see #getDataValue()
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		if(b.length() > 0 && b.charAt(b.length()-1) == ']'){
			b.append("\n");
			b.append(prefix);
		}
		b.append(getName());
		if(value != null){
			b.append(" = ");
			b.append(value.toMOF());
		}
		return b.toString();
	}
	
	/**
	 * Get the string representation of this enumeration value in {name} form
	 * @see #toMOF()
	 * @see #getFullName()
	 * @see #getDataValue()
	 */
	public String toString(){
		return getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.QualifiedElement#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(!super.equals(o)) return false;
		EnumerationValue other = (EnumerationValue) o;
		if(!enumName.equals(other.enumName)) return false;
		if(value == null){
			return other.value == null;
		}
		return value.equals(other.value);
	}
}
