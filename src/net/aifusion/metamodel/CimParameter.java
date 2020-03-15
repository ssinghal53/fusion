/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created January 18, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.List;

/**
 * Class to represent a CIM method parameter.
 * @author Sharad Singhal
 */
public class CimParameter extends QualifiedElement {
	/** Full name of the method within which this parameter is declared */
	private String originMethod = "";
	/** data type for this parameter */
	private DataType dataType = null;
	/** Default dataValue of this parameter */
	private DataValue defaultValue = null;
	/** Current dataValue of this parameter */
	private DataValue dataValue = null;
	/** Name of referenced class (for reference parameters) */
	private String refClassName = null;
	/** reference to the corresponding structure, if the parameter is a structure parameter */
	private CimStructure structure = null;
	/** reference to the enum, if the parameter is an enum parameter */
	private CimEnumeration enumeration = null;
	
	/**
	 * Create a CIM Parameter with primitive data types
	 * @param originMethod - Full name of method within which this parameter is declared
	 * @param name - name of the parameter
	 * @param dataType - dataType of the parameter
	 * @param defaultValue - default dataValue, if any
	 * @param qualifiers - qualifiers for the parameter
	 */
	protected CimParameter(String originMethod, String name, DataType dataType, DataValue defaultValue, List<Qualifier> qualifiers) {
		super(ElementType.PARAMETER, name, qualifiers);
		if(dataType == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": no data type defined");
		if(!dataType.isPrimitive()){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,name+": Non-primitive parameters must use different constructor");
		}
		this.originMethod = originMethod;
		this.dataType = dataType;
		this.defaultValue = (this.dataValue = defaultValue);
		if(defaultValue != null && defaultValue.getType() != dataType){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,name+": parameter expected data type "+dataType+" found "+defaultValue.getType());
		}
		return;
	}
	
	/**
	 * Create a CIM reference parameter	
	 * @param originMethod - Full name of method within which this parameter is declared
	 * @param name - name of the parameter
	 * @param refClassName - referenced class or structure name
	 * @param isArray - true if the parameter is an array parameter
	 * @param defaultValue - default dataValue, if any
	 * @param qualifiers - qualifiers for this parameter
	 */
	protected CimParameter(String originMethod, String name, String refClassName, boolean isArray, DataValue defaultValue, List<Qualifier> qualifiers){
		super(ElementType.PARAMETER,name,qualifiers);
		dataType = isArray ? DataType.OBJECTPATH_ARRAY : DataType.OBJECTPATH;
		this.refClassName = refClassName;
		this.originMethod = originMethod;
		this.defaultValue = (this.dataValue = defaultValue);
		if(defaultValue != null && defaultValue.getType() != dataType){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,name+": parameter expected data type "+dataType+" found "+defaultValue.getType());
		}
		// TODO: Check that defaultValue refers to refClassName
	}
	
	/**
	 * Create a CIM Enumeration parameter
	 * @param originMethod - Full name of method within which this parameter is declared
	 * @param name - name of the parameter
	 * @param enumeration - enumeration from which parameter values must be selected
	 * @param isArray - true if this parameter is an array parameter
	 * @param defaultValue - default value, if any
	 * @param qualifiers - qualifiers for this parameter, if any
	 */
	protected CimParameter(String originMethod, String name, CimEnumeration enumeration, boolean isArray, DataValue defaultValue, List<Qualifier> qualifiers){
		super(ElementType.PARAMETER, name, qualifiers);
		dataType = isArray ? DataType.ENUMERATIONVALUE_ARRAY : DataType.ENUMERATIONVALUE;
		this.originMethod = originMethod;
		this.enumeration = enumeration;
		this.defaultValue = (this.dataValue = defaultValue);
		if(defaultValue != null && defaultValue.getType() != dataType){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,name+": parameter expected data type "+dataType+" found "+defaultValue.getType());
		}
		// TODO: Check that the defaultvalue contains one of the values from the enumeration given
		return;
	}
	
	/**
	 * Create a CIM CimStructure parameter
	 * @param originMethod - Full name of method within which this parameter is declared
	 * @param name - name of the parameter
	 * @param structure - structure from which parameter values must be selected
	 * @param isArray - true if this parameter is an array parameter, false otherwise
	 * @param defaultValue - default value if any
	 * @param qualifiers - qualifiers, if any
	 */
	protected CimParameter(String originMethod, String name, CimStructure structure, boolean isArray, DataValue defaultValue, List<Qualifier> qualifiers){
		super(ElementType.PARAMETER,name,qualifiers);
		if(structure == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null structure provided");
		switch(structure.getElementType()){
		case STRUCTURE:
			dataType = isArray ? DataType.STRUCTUREVALUE_ARRAY : DataType.STRUCTUREVALUE;
			break;
		case CLASS:
			dataType = isArray ? DataType.INSTANCEVALUE_ARRAY : DataType.INSTANCEVALUE;
			break;
		default:
			// association, interface
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": only structures and instances are currently allowed in parameters");
		}
		this.originMethod = originMethod;
		this.structure = structure;
		this.defaultValue = (this.dataValue = defaultValue);
		if(defaultValue != null && defaultValue.getType() != dataType){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,name+": parameter expected data type "+dataType+" found "+defaultValue.getType());
		}
		// TODO: Check that the defaultValue contains a value of the given structure
		return;
	}
	
	/**
	 * Get the full name of the parameter. The full name of the parameter is originClassName '#' MethodName '$' parameterName
	 * @return the full name for the parameter
	 */
	public String getFullName() {
		return originMethod != null ? new StringBuilder(originMethod).append("$").append(getName()).toString() :
			new StringBuilder("$").append(getName()).toString();
	}

	/**
	 * Get the referenced class name for this parameter
	 * @return - referenced class name. Null if this parameter is not a reference parameter
	 */
	public String getRefClassName(){
		return refClassName;
	}
	
	/**
	 * Get the enumeration associated with this parameter
	 * @return - referenced enumeration. Null if this parameter does not refer to an enumeration
	 */
	public CimEnumeration getEnum(){
		return enumeration;
	}
	
	/**
	 * Get the structure associated with this parameter
	 * @return - referenced structure. Null if this parameter does not refer to a structure
	 */
	public CimStructure getStruct(){
		return structure;
	}
	
	/**
	 * Get the data type associated with this parameter
	 * @return - data type for this parameter
	 */
	public DataType getDataType(){
		return dataType;
	}
	
	/**
	 * Check if this parameter has a default value defined in it
	 * @return - true if this parameter has a default value, false otherwise
	 */
	public boolean hasDefaultValue(){
		return defaultValue != null;
	}
	
	/**
	 * Get the default value of this parameter
	 * @return - default value of this parameter. Null if no default value is defined
	
	 */
	public DataValue getDefaultValue(){
		return defaultValue;
	}
	
	/**
	 * Check if this parameter has a value defined in it
	 * @return - true if this parameter has a value, false otherwise
	 */
	public boolean hasValue(){
		return dataValue != null;
	}
	
	/**
	 * Check if this parameter has a non-null value defined in it
	 * @return - true if the parameter has a value defined, and it contains a non-null value, false otherwise
	 */
	public boolean hasNonNullValue(){
		return dataValue != null && dataValue.getValue() != null;
	}
	/**
	 * Get the value of this parameter. If no value is defined, returns the default value of the parameter
	 * @return - value of the parameter
	 */
	public DataValue getValue(){
		return dataValue != null ? dataValue : defaultValue;
	}
	
	/**
	 * Set the value of this parameter.
	 * @param value - value to be set in this parameter
	 * @see JavaModelMapper
	 */
	public void setValue(DataValue value){
		if(value == null || dataType == value.getType()){
			dataValue = value;
			return;
		}
		throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+" expected data type "+dataType+" found "+value.getType());
	}
	
	/**
	 * Check if this parameter is an input parameter
	 * @return - true if this parameter is an input parameter
	 */
	public boolean isInput() {
		return (Boolean) getQualifierValue("IN").getValue();
	}
	
	/**
	 * Check if this parameter is an output parameter
	 * @return - true if this parameter is an output parameter
	 */
	public boolean isOutput() {
		return (Boolean) getQualifierValue("OUT").getValue();
	}

	/**
	 * Check if this parameter is a reference parameter
	 * @return - true if this parameter is a reference parameter
	 */
	public boolean isReference() {
		return refClassName != null;
	}

	/**
	 * Check if this parameter is an array parameter
	 * @return - true if this parameter is an array value, false otherwise
	 */
	public boolean isArray() {
		return dataType.isArray();
	}
	
	/**
	 * Clone this parameter for an instance method
	 * @return - cloned parameter
	 * @see #setValue(DataValue)
	 */
	protected CimParameter createInstanceParameter(){
		CimParameter instanceParameter = null;
		if(refClassName != null){
			// reference parameter
			instanceParameter = new CimParameter(originMethod,getName(),refClassName,dataType.isArray(),defaultValue, getQualifiers());
		} else if(enumeration != null){
			// enumeration parameter
			instanceParameter = new CimParameter(originMethod,getName(),enumeration,dataType.isArray(),defaultValue, getQualifiers());
		} else if(structure != null){
			// structure parameter
			instanceParameter = new CimParameter(originMethod,getName(),structure,dataType.isArray(),defaultValue, getQualifiers());
		} else {
			// primitive type parameter
			instanceParameter = new CimParameter(originMethod,getName(),dataType,defaultValue, getQualifiers());
		}
		return instanceParameter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.QualifiedElement#toMOF()
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		if(b.length() > 0 && b.charAt(b.length()-1) == ']'){
			b.append(" ");
		}
		if(dataType.isReference()){
			// reference parameter
			b.append(refClassName);
			b.append(" ref");
		} else if(dataType.isPrimitive()){
			// primitive parameter
			b.append(dataType.toMOF());
		} else {
			// structure or enum parameter
			b.append(enumeration != null ? enumeration.getName() : structure.getName());
		}
		b.append(" ");
		if(dataType.isArray()){
			b.append("[] ");
		}
		// parameterName
		b.append(getName());
		// note that unlike properties, only the default value appears in the MOF
		if(defaultValue != null){
			b.append(" = ");
			b.append(defaultValue.toMOF());
		}
		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.QualifiedElement#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(!super.equals(o) || !(o instanceof CimParameter)) return false;
		CimParameter other = (CimParameter) o;
		if(dataType != other.dataType) return false;
		// check reference class name if this is a reference
		if(getElementType() == ElementType.REFERENCE){
			if(!refClassName.equalsIgnoreCase(other.refClassName)) return false;
		}
		if(dataValue != null) return dataValue.equals(other.dataValue);
		return other.dataValue == null ? true : false;
	}
}
