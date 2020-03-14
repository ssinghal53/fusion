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
 * Created Dec 24, 2014 by Sharad Singhal
 * Last modified Nov 1, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Method;
import java.util.List;

/**
 * Class to represent a CIM Property. A property is a qualified element that can contain primitive data types,
 * references, structure values, enumeration values, CimInstances, or their arrays
 * @author Sharad Singhal
 */
public class CimProperty extends QualifiedElement {
	/** Fully qualified name of the class or structure within which this property is declared */
	private String originClass = "";
	/** DataType of this property */
	private DataType dataType = null;
	/** Default dataValue of this property */
	private DataValue defaultValue = null;
	/** Current dataValue of this property */
	private DataValue dataValue = null;
	/** Name of referenced class (for reference properties) */
	private String refClassName = null;
	/** reference to the corresponding structure, if the property is a structure property */
	private CimStructure structure = null;
	/** reference to the enum, if the property is an enum property */
	private CimEnumeration enumeration = null;
	/** java methods bound to this property */
	private Method getter = null, setter = null;
	/** java implementation object bound to this property */
	private Object javaObject = null;
	
	/**
	 * Create a CIM CimProperty with primitive data types
	 * @param originClass - fully qualified name of the class within which this property is declared
	 * @param name - name of the property
	 * @param dataType - dataType of the property
	 * @param defaultValue - default dataValue, if any
	 * @param qualifiers - qualifiers for the property
	 * @param overriddenProperty - property overridden by this property
	 */
	protected CimProperty(String originClass, String name, DataType dataType, DataValue defaultValue, List<Qualifier> qualifiers) {
		super(ElementType.PROPERTY, name, qualifiers);
		if(dataType == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": no data type defined");
		if(!dataType.isPrimitive()){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,name+": Non-primitive properties must use different constructor");
		}
		this.dataType = dataType;
		this.originClass = originClass;
		this.defaultValue = defaultValue;
		// validate the property
		validate();
		return;
	}
	
	/**
	 * Create a CIM reference property	
	 * @param originClass - fully qualified name of the class within which this property is declared
	 * @param name - name of the property
	 * @param refClassName - referenced class or structure name
	 * @param isArray - true if the property is an array property
	 * @param defaultValue - default dataValue, if any
	 * @param qualifiers - qualifiers for this property
	 * @param overriddenProperty - property overridden by this property
	 */
	protected CimProperty(String originClass, String name, String refClassName, boolean isArray, DataValue defaultValue, List<Qualifier> qualifiers){
		super(ElementType.REFERENCE,name,qualifiers);
		dataType = isArray ? DataType.OBJECTPATH_ARRAY : DataType.OBJECTPATH;
		this.originClass = originClass;
		if(refClassName == null || refClassName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty refrenced class name");
		this.refClassName = refClassName;
		this.defaultValue = defaultValue;
		// validate the property
		validate();
		return;
	}
	
	/**
	 * Create a CIM Enumeration property
	 * @param originClass - fully qualified name of the class within which this property is declared
	 * @param name - name of the property
	 * @param enumeration - enumeration from which property values must be selected
	 * @param isArray - true if this property is an array property
	 * @param defaultValue - default value, if any
	 * @param qualifiers - qualifiers for this property, if any
	 * @param overriddenProperty - property overridden by this property
	 */
	protected CimProperty(String originClass,String name, CimEnumeration enumeration, boolean isArray, DataValue defaultValue, List<Qualifier> qualifiers){
		super(ElementType.PROPERTY, name, qualifiers);
		dataType = isArray ? DataType.ENUMERATIONVALUE_ARRAY : DataType.ENUMERATIONVALUE;
		this.originClass = originClass;
		if(enumeration == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null enumeration provided");
		this.enumeration = enumeration;
		this.defaultValue = defaultValue;
		// validate the property
		validate();
		return;
	}
	
	/**
	 * Create a CIM CimStructure property
	 * @param originClass - fully qualified name of the class within which this property is declared
	 * @param name - name of the property
	 * @param structure - structure, class, interface, or association that defines the type of the property
	 * @param isArray - true if this property is an array property, false otherwise
	 * @param defaultValue - default value if any
	 * @param qualifiers - qualifiers, if any
	 * @param overriddenProperty - property overridden by this property
	 */
	protected CimProperty(String originClass, String name, CimStructure structure, boolean isArray, DataValue defaultValue, List<Qualifier> qualifiers){
		super(ElementType.PROPERTY,name,qualifiers);
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
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": only structures and instances are currently allowed in properties");
		}
		this.originClass = originClass;
		this.structure = structure;
		this.defaultValue = defaultValue;
		// validate the property
		validate();
		return;
	}
	
	/**
	 * Validate the qualifiers and data values in this property
	 */
	private void validate(){
		// this property is a key property
		if(isKey()){
			// a key property must have a scalar primitive or reference type
			if(!(dataType.isPrimitive() || dataType.isReference())|| dataType.isArray())
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+": Key properties must be scalar primitive types");
			// a key property cannot be writable
			if(isWritable())
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+": Key properties cannot have WRITE(TRUE) qualifier");
		}
		/*
		// this property overrides another property
		if(overriddenProperty != null){
			// key properties cannot be overridden
			if(overriddenProperty.isKey())
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+": Key property cannot be overridden");
			// the overridden property must have the same data type as this property
			if(dataType != overriddenProperty.dataType){
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+": DataType does not match overridden property type");
			}
			// the overridden property must reside in a superClass of this property
			if(!originClass.startsWith(overriddenProperty.originClass)){
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+": Overridden property is not part of the superTypes of this property");
			}			
		}
		*/
		// if a default value is given, check the default value for consistency
		if(defaultValue != null){
			// the data type of the property must match the data type of the default value
			if(defaultValue.getType() != dataType){
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,getFullName()+": property expected data type "+dataType+" found "+defaultValue.getType());
			}
			if(refClassName != null){
				// for reference properties, validate that the className in the object path matches the refClassName
				if(dataType.isArray()){
					ObjectPath [] pathArray = (ObjectPath []) defaultValue.getValue();
					if(pathArray != null && pathArray.length > 0){
						for(ObjectPath path : pathArray){
							if(!refClassName.equalsIgnoreCase(path.getName())) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
									": Expected path to class "+refClassName+" found "+path.toString());
						}
					}
				} else {
					ObjectPath p = (ObjectPath) defaultValue.getValue();
					if(!refClassName.equalsIgnoreCase(p.getName())) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
							": Expected path to class "+refClassName+" found "+p.toString());
				}
			} else if(enumeration != null){
				// for enumeration values, ensure that the value given comes from the defining enumeration
				String enumName = enumeration.getFullName();
				if(dataType.isArray()){
					EnumerationValue [] pa = (EnumerationValue  []) defaultValue.getValue();
					if(pa != null && pa.length > 0){
						for(EnumerationValue p : pa){
							if(!enumName.equalsIgnoreCase(p.getEnumName())) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
									": Expected enumeration value from "+enumName+" found "+p.toString());
						}
					}
				} else {
					EnumerationValue p = (EnumerationValue) defaultValue.getValue();
					// System.out.println(p);
					if(!enumName.equalsIgnoreCase(p.getEnumName())) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
							": Expected enumeration value from "+enumName+" found "+p.getEnumName());
				}
			} else if (structure != null){
				// for structure values, validate that the value corresponds to the given structure
				switch(structure.getElementType()){
				case STRUCTURE:	
					// we have a structure value. Structure values have the name of their enclosing structure
					if(dataType.isArray()){
						StructureValue [] pa = (StructureValue  []) defaultValue.getValue();
						if(pa != null && pa.length > 0){
							for(StructureValue p : pa){
								if(!structure.getName().equalsIgnoreCase(p.getName())) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
										": Expected value of "+structure.getName()+" found "+p.toString());
							}
						}
					} else {
						StructureValue p = (StructureValue) defaultValue.getValue();
						if(!structure.getName().equalsIgnoreCase(p.getName())) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
								": Expected value of "+structure.getName()+" found "+p.toString());
					}
					break;
				case CLASS:
					// for CimInstance values, ensure that the instance is an instance of the given cimClass, or one of its superTypes
					CimClass cimClass = (CimClass) structure;
					if(dataType.isArray()){
						CimInstance [] pa = (CimInstance  []) defaultValue.getValue();
						if(pa != null && pa.length > 0){
							for(CimInstance p : pa){
								if(!p.isInstanceOf(cimClass)) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
										": Expected value of "+cimClass.getFullName()+" found "+p.toMOF());
							}
						}
					} else {
						CimInstance p = (CimInstance) defaultValue.getValue();
						if(!p.isInstanceOf(cimClass)) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+
								": Expected value of "+cimClass.getFullName()+" found "+p.toMOF());
					}
					break;
				default:
					// should not happen
					throw new ModelException("Internal error in CimProperty.validate(). Found unexpected case "+structure.getElementType());	
				}
			}
		}
		return;
	}
	
	
	/**
	 * Get the full name for this property. The full name for the property is the originClass '#' featureName
	 * @return full name for the property
	 */
	public String getFullName() {
		StringBuilder b = new StringBuilder();
		if(originClass != null) b.append(originClass);
		b.append("#").append(getName());
		return b.toString();
// 		return new StringBuilder(originClass).append("#").append(getName()).toString();
	}
	
	/**
	 * Get the name of the class or structure where this property is declared
	 * @return - fully qualified name of the declaring class or structure
	 */
	public String getOriginClass(){
		return originClass;
	}

	/**
	 * Get the referenced class name for this property
	 * @return - referenced class name. Null if this property is not a reference property
	 */
	public String getRefClassName(){
		return refClassName;
	}
	
	/**
	 * Get the enumeration associated with this property
	 * @return - referenced enumeration. Null if this property does not refer to an enumeration
	 */
	public CimEnumeration getEnum(){
		return enumeration;
	}
	
	/**
	 * Get the structure associated with this property
	 * @return - referenced structure. Null if this property does not refer to a structure
	 */
	public CimStructure getStruct(){
		return structure;
	}
	
	/**
	 * Get the data type associated with this property
	 * @return - data type for this property
	 */
	public DataType getDataType(){
		return dataType;
	}
	
	/**
	 * Check if this property has a default value defined in it
	 * @return - true if this property has a default value, false otherwise
	 */
	public boolean hasDefaultValue(){
		return defaultValue != null;
	}
	
	/**
	 * Get the default value of this property
	 * @return - default value of this property. Null if no default value is defined
	
	 */
	public DataValue getDefaultValue(){
		return defaultValue;
	}
	
	/**
	 * Check if this property has a value defined in it
	 * @return - true if this property has a value, false otherwise
	 */
	public boolean hasValue(){
		return dataValue != null;
	}
	
	/**
	 * Check if this property has a non-null value defined in it
	 * @return - true if the property has a value defined, and it contains a non-null value, false otherwise
	 */
	public boolean hasNonNullValue(){
		return dataValue != null && dataValue.getValue() != null;
	}
	
	/**
	 * Check if this property is readable (i.e., has READ(True) qualifier)
	 * @return true if this property is readable
	 * @see #getValue()
	 */
	public boolean isReadable(){
		return (boolean) getQualifierValue("Read").getValue();
	}
	
	/**
	 * Check if this property is writable (i.e., has WRITE(True) qualifier)
	 * @return true if this property is writable, false otherwise
	 * @see #setValue(DataValue)
	 */
	public boolean isWritable(){
		return (boolean) getQualifierValue("Write").getValue();
	}
	
	/**
	 * Check if this property is a KEY property, i.e., has KEY(true) qualifier
	 * @return - true if this property is a key property, false otherwise
	 * @see #setValue(DataValue)
	 */
	public boolean isKey(){
		return (boolean) getQualifierValue("Key").getValue();
	}
	
	/**
	 * Check if this property is a required property
	 * @return - true if this property is Key, or has a REQUIRED qualifier on it, false otherwise
	 */
	public boolean isRequired(){
		return isKey() || (boolean) getQualifierValue("Required").getValue();
	}
	
	/**
	 * Check if this property is a static property
	 * @return - true if this property has STATIC(true) qualifier
	 */
	public boolean isStatic(){
		return hasQualifier("Static") && (boolean) getQualifierValue("Static").getValue();
	}
	
	
	/**
	 * Validate a data value against the definitions in this property
	 * @param v - data value to validate
	 */
	public void validate(DataValue v){
		// check the data type 
		if(v != null && v.getType() != getDataType()) 
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getFullName()+" expected data type "+getDataType()+" found "+v.getType());
		// TODO: Check value against qualifiers, enums, structures, etc.
		return;
	}
	
	/**
	 * Get the value of this property. If no value is defined, returns the default value of the property
	 * @return - value of the property
	 * @throws ModelException if property is not readable
	 * @see #isReadable()
	 */
	public DataValue getValue(){
		if(!isReadable()) throw new ModelException(ExceptionReason.ACCESS_DENIED,getName()+" is not readable");
		return readValue();
	}
	

	/**
	 * Set the value of this property
	 * @param value - value to set in this property
	 * @throws ModelException if property is not writable, or if the data type of the value does not match property data value
	 * @see #isWritable()
	 * @see #isKey()
	 */
	public void setValue(DataValue value){
		if(!isWritable()) throw new ModelException(ExceptionReason.ACCESS_DENIED,getName()+" is not writable");
		writeValue(value);
		return;		
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.QualifiedElement#toMOF(String)
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		if(b.length() > 0 && b.charAt(b.length()-1) == ']'){
			b.append("\n");
			b.append(prefix);
		}
		if(dataType.isReference()){
			// reference property
			b.append(refClassName);
			b.append(" ref");
		} else if(dataType.isPrimitive()){
			// primitive property
			b.append(dataType.toMOF());
		} else {
			// structure or enum property
			b.append(enumeration != null ? enumeration.getName() : structure.getName());
		}
		if(dataType.isArray()){
			b.append(" []");
		}
		b.append(" ");
		// featureName
		b.append(getName());
		if(isReadable() && hasValue()){
			b.append(" = ");
			b.append(dataValue.toMOF());
		} else if(defaultValue != null){
			b.append(" = ");
			b.append(defaultValue.toMOF());
		}
		b.append(";\n");
		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.QualifiedElement#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(!super.equals(o)) return false;
		if(!(o instanceof CimProperty)) return false;
		CimProperty other = (CimProperty) o;
		if(dataType != other.dataType) return false;
		// check reference class name if this is a reference
		if(getElementType() == ElementType.REFERENCE){
			if(!refClassName.equalsIgnoreCase(other.refClassName)) return false;
		}
		if(structure == null && other.structure != null) return false;
		if(structure != null && !structure.equals(other.structure)) return false;
		if(enumeration == null && other.enumeration != null) return false;
		if(enumeration != null && !enumeration.equals(other.enumeration)) return false;
		
		if(dataValue != null) return dataValue.equals(other.dataValue);
		return other.dataValue == null ? true : false;
	}
	
	/* *******************************************
	 * Java bindings
	 * *******************************************
	 */
	
	/**
	 * Bind this property to a java implementation.
	 * Once bound, this property will delegate its readValue() and writeValue() methods to the java implementation.
	 * @param getter - java getter method being bound. If null, the property will not be bound to a getter
	 * @param setter - java setter method being bound. If null, the property will not be bound to a setter
	 * @param implObject - underlying java implementation object. For static properties, it can be null
	 * @throws ModelException - in case of errors
	 */
	protected synchronized void bind(Method getter, Method setter, Object implObject){
		JavaModelMapper.validatePropertyBinding(this, getter, setter, implObject);
		this.getter = getter;
		this.setter = setter;
		this.javaObject = implObject;
		return;
	}
	
	/**
	 * Unbind this property. If the property is bound to a java object, it will be unbound,
	 * and will no longer delegate its read/write methods to the java object.
	 */
	protected synchronized void unbind(){
		if(getter != null) getter = null;
		if(setter != null) setter = null;
		if(javaObject != null) javaObject = null;
		return;
	}
	
	/**
	 * Read the value of a property. If no value is defined, return the default value.<p>
	 * Note that this method is used internally within the package and bypasses the check by the Read qualifier
	 * @return - the value of this property
	 */
	protected DataValue readValue(){
		if(getter != null){
			dataValue = JavaModelMapper.readPropertyValue(this, getter, javaObject);
		}
		return dataValue != null ? dataValue : defaultValue;
	}
	
	/**
	 * Write the value of this property<p>
	 * Note that this method is used internally within the package and bypasses the check by the Write qualifier
	 * @param value - value to be written
	 */
	protected void writeValue(DataValue value){
		if(value != null && value.getType() != getDataType()) throw new ModelException(ExceptionReason.TYPE_MISMATCH,getName()+
				" expected property type "+getDataType()+" received "+value.getType());
		if(setter != null){
			JavaModelMapper.writePropertyValue(this,setter,javaObject, value);
		}
		dataValue = value;
		return;		
	}
	
	/**
	 * Clone this property for an instance
	 * @param value - initial value for the instance property (ignored for static properties)
	 * @return - cloned property
	 */
	protected CimProperty createInstanceProperty(DataValue value){
		if(isStatic()) return this;
		CimProperty instanceProperty = null;
		if(refClassName != null){
			// reference property
			instanceProperty = new CimProperty(originClass,getName(),refClassName,dataType.isArray(),getDefaultValue(),getQualifiers());
		} else if(enumeration != null){
			// enumeration property
			instanceProperty = new CimProperty(originClass,getName(),enumeration,dataType.isArray(),getDefaultValue(),getQualifiers());
		} else if(structure != null){
			// structure property
			instanceProperty = new CimProperty(originClass,getName(),structure,dataType.isArray(),getDefaultValue(),getQualifiers());
		} else {
			// primitive type property
			instanceProperty = new CimProperty(originClass,getName(),getDataType(),getDefaultValue(),getQualifiers());
		}
		if(value != null) instanceProperty.writeValue(value);
		return instanceProperty;
	}
}
