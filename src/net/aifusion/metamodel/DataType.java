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
 * Last Modified Sep 15, 2020 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Vector;

/**
 * This enum contains all known CIM data types, as well as commonly used methods for manipulating them
 * @author Sharad Singhal
 */
public enum DataType {
	/* represented as typeName, isArrayType, acceptedJavaClass [], generatedJavaClass */
	/** Void (null) type */
	VOID("Void",false, new Class[]{void.class}, void.class),
	/** Boolean type (Boolean | boolean) */
	BOOLEAN("Boolean",false,new Class[]{Boolean.class,boolean.class},Boolean.class),
	/** Unsigned 8-bit Integer (UInt8) */
	UINT8("UInt8",false,new Class[]{UInt8.class},UInt8.class),
	/** Unsigned 16-bit Integer (UInt16) */
	UINT16("UInt16",false,new Class[]{UInt16.class},UInt16.class),
	/** Unsigned 32-bit Integer (UInt32) */
	UINT32("UInt32",false,new Class[]{UInt32.class},UInt32.class),
	/** Unsigned 64-bit Integer (UInt64) */
	UINT64("UInt64",false,new Class[]{UInt64.class},UInt64.class),
	/** Signed 8-bit Integer (Byte | byte) */
	SINT8("SInt8",false,new Class[]{Byte.class,byte.class},Byte.class),
	/** Signed 16-bit Integer (Short | short) */
	SINT16("SInt16",false,new Class[]{Short.class,short.class},Short.class),
	/** Signed 32-bit Integer (Integer | int) */
	SINT32("SInt32",false,new Class[]{Integer.class,int.class},Integer.class),
	/** Signed 64-bit Integer (Long | long) */
	SINT64("SInt64",false,new Class[]{Long.class,long.class},Long.class),
	/** IEEE 4-byte real number (Float | float) */
	REAL32("Real32",false,new Class[]{Float.class,float.class},Float.class),
	/** IEEE 8-byte real number (Double | double) */
	REAL64("Real64",false,new Class[]{Double.class,double.class},Double.class),
	/** 16-bit UCS-2 Character (Character | char) */
	CHAR16("Char16",false,new Class[]{Character.class,char.class},Character.class),
	/** 16-bit UCS-2 String (String) */
	STRING("String",false,new Class[]{String.class},String.class),
	/** Date time  (DateTime) */
	DATETIME("Datetime",false,new Class[]{DateTime.class},DateTime.class),
	/** OctetString value (OctetString) */
	OCTETSTRING("OctetString",false,new Class[]{OctetString.class},OctetString.class),
	/** Object Name (ObjectPath) */
	OBJECTPATH("Ref",false,new Class[]{ObjectPath.class},ObjectPath.class),
	/** Enumeration value (EnumerationValue) */
	ENUMERATIONVALUE("",false, new Class[]{EnumerationValue.class},EnumerationValue.class),
	/** CimStructure value (StructureValue) */
	STRUCTUREVALUE("",false, new Class[]{StructureValue.class},StructureValue.class),
	/** Instance value (CimInstance) */
	INSTANCEVALUE("",false, new Class[]{CimInstance.class},CimInstance.class),
	
	/** Boolean array (Boolean[] | boolean[]) */
	BOOLEAN_ARRAY("Boolean",true,new Class[]{Boolean[].class,boolean[].class},Boolean[].class),
	/** Unsigned 8-bit Integer array (UInt8[]) */
	UINT8_ARRAY("UInt8",true,new Class[]{UInt8[].class},UInt8[].class),
	/** Unsigned 16-bit Integer array (UInt16[]) */
	UINT16_ARRAY("UInt16",true,new Class[]{UInt16[].class},UInt16[].class),
	/** Unsigned 32-bit Integer array (UInt32[]) */
	UINT32_ARRAY("UInt32",true,new Class[]{UInt32[].class},UInt32[].class),
	/** Unsigned 64-bit Integer array (UInt64[]) */
	UINT64_ARRAY("UInt64",true,new Class[]{UInt64[].class},UInt64[].class),
	/** Signed 8-bit Integer array (Byte[] | byte[]) */
	SINT8_ARRAY("SInt8",true,new Class[]{Byte[].class,byte[].class},Byte[].class),
	/** Signed 16-bit Integer array (Short[] | short[]) */
	SINT16_ARRAY("SInt16",true,new Class[]{Short[].class,short[].class},Short[].class),
	/** Signed 32-bit Integer array (Integer[] | int[]) */
	SINT32_ARRAY("SInt32",true,new Class[]{Integer[].class,int[].class},Integer[].class),
	/** Signed 64-bit Integer array (Long[] | long[]) */
	SINT64_ARRAY("SInt64",true,new Class[]{Long[].class,long[].class},Long[].class),
	/** IEEE 4-byte real number array (Float[] | float[]) */
	REAL32_ARRAY("Real32",true,new Class[]{Float[].class,float[].class},Float[].class),
	/** IEEE 8-byte real number array (Double[] | double[]) */
	REAL64_ARRAY("Real64",true,new Class[]{Double[].class,double[].class},Double[].class),
	/** 16-bit UCS-2 Character array (Character[] | char[]) */
	CHAR16_ARRAY("Char16",true,new Class[]{Character[].class,char[].class},Character[].class),
	/** 16-bit UCS-2 String array (String[]) */
	STRING_ARRAY("String",true,new Class[]{String[].class},String[].class),
	/** Date time array (DateTime[]) */
	DATETIME_ARRAY("Datetime",true,new Class[]{DateTime[].class},DateTime[].class),
	/** OctetString array value (OctetString[]) */
	OCTETSTRING_ARRAY("OctetString",true,new Class[]{OctetString[].class},OctetString[].class),
	/** Object Name array (ObjectPath[]) */
	OBJECTPATH_ARRAY("Ref",true,new Class[]{ObjectPath[].class},ObjectPath[].class),
	/** Enumeration value array (EnumerationValue) */
	ENUMERATIONVALUE_ARRAY("",true, new Class[]{EnumerationValue[].class},EnumerationValue[].class),
	/** CimStructure value (StructureValue) */
	STRUCTUREVALUE_ARRAY("",true, new Class[]{StructureValue[].class},StructureValue[].class),
	/** Instance value (CimInstance) */
	INSTANCEVALUE_ARRAY("",true, new Class[]{CimInstance[].class},CimInstance[].class);
	
	/** mof representation of this data type */
	private final String mof;
	/** indication if this data type is an array data type */
	private final boolean isArray;
	/** java class types that correspond to this type */
	private final Class<?> [] accepted;
	/** java class type generated for this type */
	private final Class<?> generated;
	/** Map from Java data type to CIM DataType */
	private static HashMap<Class<?>,DataType> typeForClass = new HashMap<Class<?>,DataType>();
	static {
		for(DataType d : DataType.values()){
			for(Class<?> c : d.accepted){
				typeForClass.put(c,d);
			}
		}
	}
	
	/**
	 * Create a CIM data type dataValue 
	 * @param mof - mof representation of this data type
	 * @param isArray - flag to indicate if this data type is an array type
	 * @param accepted - acceptable java types corresponding to this data type
	 * @param generated - java type returned as the preferred java type for this type
	 */
	private DataType(String mof,boolean isArray, Class<?> [] accepted, Class<?> generated){
		this.mof = mof;
		this.isArray = isArray;
		this.accepted = accepted;
		this.generated = generated;
		return;
	}
	
	/**
	 * Return the CIM data type corresponding to some object
	 * @param o - object for which the CIM data type is required
	 * @return - DataType for the class.
	 * @throws ModelException if the object does not correspond to a valid CIM data type
	 */
	public static DataType getTypeForObject(Object o){
		if(o == null) return VOID;
		return getTypeForClass(o.getClass());
	}
	
	/**
	 * Return the CIM data type corresponding to a java class
	 * @param javaClass - java class for which the data type is needed (non null)
	 * @return - CIM data type for the class
	 * @throws ModelException if the java class does not correspond to a valid CIM data type
	 */
	public static DataType getTypeForClass(Class<?> javaClass){
		if(javaClass == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER, "Null class does not have a valid CIM data type");
		if(typeForClass.containsKey(javaClass)){
			return typeForClass.get(javaClass);
		} else {
			boolean isArray = javaClass.isArray();
			Class<?> componentClass = isArray ? javaClass.getComponentType() : javaClass;
			if(componentClass.isAnnotationPresent(Export.class)){
				if(componentClass.isEnum()) {	// annotated Enum class
					return isArray ? ENUMERATIONVALUE_ARRAY : ENUMERATIONVALUE;
				} else if(!componentClass.isInterface()){
					boolean hasNonPropertyMethod = false;
					for(Method m : componentClass.getMethods()) {
						if(!m.isAnnotationPresent(Export.class)) continue;	// ignore non-property methods
						if(JavaModelMapper.isPropertyMethod(m)) continue;
						hasNonPropertyMethod = true;
						break;
					}
					if(hasNonPropertyMethod) return isArray ? INSTANCEVALUE_ARRAY : INSTANCEVALUE;
					// we possibly have a structure; check if forceClass() is present
					Export cls = componentClass.getAnnotation(Export.class);
					if(cls.forceClass()) {
						return isArray ? INSTANCEVALUE_ARRAY : INSTANCEVALUE;
					}
					return isArray ? STRUCTUREVALUE_ARRAY : STRUCTUREVALUE;
				}
			}
		}
		throw new ModelException(ExceptionReason.NOT_FOUND,"Class "+javaClass.getName()+" does not have a valid CIM data type");
	}
	
	/**
	 * Check if a java class is a valid CIM data type
	 * @param javaClass - java class to test
	 * @return - true if the java class is exported, or has a corresponding CIM data type
	 */
	public static boolean isCimType(Class<?> javaClass){
		if(javaClass == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER, "Null class does not have a valid CIM data type");
		if(typeForClass.containsKey(javaClass)) return true;
		boolean isArray = javaClass.isArray();
		Class<?> componentClass = isArray ? javaClass.getComponentType() : javaClass;
		// check if the class is exported
		if(componentClass.isAnnotationPresent(Export.class) && !componentClass.isInterface()) return true;
		return false;
	}

	/**
	 * Return the Java class corresponding to this data type
	 * @return - Java class representing the data type. Note that primitive types are NOT returned, e.g., this method
	 * returns a Long.class, not long.class for SINT64 data type
	 */
	public Class<?> getClassForType(){
		return generated;	
	}
	
	/**
	 * Return an array containing java types that are accepted as valid java types for this CIM data type
	 * @return - array containing java classes accepted as valid types for this CIM type
	 */
	public Class<?>[] getAcceptedClasses(){
		Class<?>[] acceptedClasses = new Class<?>[accepted.length];
		for(int i=0; i<accepted.length; i++){
			acceptedClasses[i] = accepted[i];
		}
		return acceptedClasses;
	}

	/**
	 * Return the Array type corresponding to this data type
	 * @return - this type if it is already an array type, else array type corresponding this type
	 */
	public DataType getArrayType() {
		if(isArray) return this;
		if(this == VOID) throw new ModelException(ExceptionReason.NOT_FOUND,"VOID does not have an Array Type");
		return DataType.valueOf(toString()+"_ARRAY");
		
	}
	
	/**
	 * Get the component type corresponding to this data type
	 * @return - this type if it is already a component type, else the corresponding component type
	 */
	public DataType getComponentType(){
		if(!isArray) return this;
		String arrayTypeName = toString();
		return DataType.valueOf(arrayTypeName.substring(0,arrayTypeName.length()-6));
	}
	
	/**
	 * Return the MOF string for this data type
	 * @return - MOF string for this data type
	 */
	public String toMOF(){
		return mof;
	}
	
	/**
	 * Check if a java object matches this type
	 * @param o - java object to compare against this type
	 * @return - true if the java object is of this type, false otherwise
	 */
	public boolean matches(Object o){
		if(o == null) return true;	// all types match null values
		Class <?> oc = o.getClass();
		for(Class<?> c : accepted){
			if(oc == c) return true; // we do NOT use equals(). class must match exactly
		}
		// this allows annotated classes handled by getTypeForClass() to be matched
		return getTypeForClass(o.getClass()) == this ? true : false;
	}
	
	/**
	 * Convert a string dataValue to a java object corresponding to this type
	 * @param value - string containing dataValue appropriate to this type
	 * @return - java object containing the given dataValue corresponding to this type
	 */
	public Object getValueFromString(String value){
		if(value == null || "null".equalsIgnoreCase(value)) return null;
		try {
			switch(this){
			case CHAR16:
				if(value.length() != 1) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected string with single character, found "+value);
				return Character.valueOf(value.charAt(0));
			case ENUMERATIONVALUE:
			case STRUCTUREVALUE:
			case INSTANCEVALUE:
				// These require access to the repository and the mof parser
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Conversion of strings to complex types currently not supported");
			default:
				return generated.getConstructor(String.class).newInstance(value);
			}
		} catch(Exception e){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Error in converting string "+value+"to type "+toString(),e);
		}
	}
	
	/**
	 * Convert a string array to a java object containing the appropriate values
	 * @param values - string array containing the desired values
	 * @return - java array of this array type containing the desired values
	 */
	public Object [] getValuesFromStrings(String [] values){
		try {
			DataType componentType = getComponentType();
			// Note that currently, Array.newInstance() limits array dimensions to 255 in the next call.
			Object vals = Array.newInstance(componentType.generated, values.length);
			for(int i = 0; i < values.length; i++){
				Object o = componentType.getValueFromString(values[i]);
				Array.set(vals, i, o);
			}
			return (Object []) vals;
		} catch(Exception e){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Error in converting string [] to type "+toString(),e);
		}
	}
	
	/**
	 * Check if this data type is an array type
	 * @return - true if this type is an array, false otherwise
	 */
	public boolean isArray(){
		return isArray;
	}
	
	/**
	 * Check if this data type is a character type (or an array of character type)
	 * @return - true if this data type is character type, false otherwise
	 */
	
	public boolean isCharacter(){
		return this == CHAR16 || this == CHAR16_ARRAY;
	}
	
	/**
	 * Check if this data type is a string type (or an array of string type)
	 * @return - true if this data type is a string type, false otherwise
	 */
	public boolean isString(){
		return this == STRING || this == STRING_ARRAY;
	}
	
	/**
	 * Check if this data type is an integer type (or array of integer type)
	 * @return - true if this data type is integer, false otherwise
	 */
	public boolean isInteger() {
		return this == SINT32 || this == SINT64 || this == SINT8 || this == SINT16 || this == UINT8 ||
				this == UINT16 || this == UINT32 || this == UINT64 ||
				this == SINT32_ARRAY || this == SINT64_ARRAY || this == SINT8_ARRAY || this == SINT16_ARRAY || this == UINT8_ARRAY || 
				this == UINT16_ARRAY || this == UINT32_ARRAY || this == UINT64_ARRAY;
	}
	
	/**
	 * Check if this data type is an unsigned integer type (or array of unsigned integer type)
	 * @return - true if this data type is an unsigned integer
	 */
	public boolean isUnsigned() {
		return this == UINT8 || this == UINT16 || this == UINT32 || this == UINT64 ||
				this == UINT8_ARRAY || this == UINT16_ARRAY || this == UINT32_ARRAY || this == UINT64_ARRAY;
	}
	
	/**
	 * Check if this data type is a real type (or array of real type)
	 * @return - true if the data type is real, false otherwise
	 */
	public boolean isReal(){
		return this == REAL32 || this == REAL64 || this == REAL32_ARRAY || this == REAL64_ARRAY;
	}
	
	/**
	 * Check if this data type is a reference (or array of reference) type
	 * @return - true if this data type is a reference type, false otherwise
	 */
	public boolean isReference(){
		return this == OBJECTPATH || this == OBJECTPATH_ARRAY;
	}
	
	/**
	 * Check if this type is a number (real or integer) type or a number array
	 * @return - true if the type is numeric, false otherwise
	 */
	public boolean isNumeric(){
		return isReal() || isInteger();
	}

	/**
	 * Check if this data type is primitive, or array of primitive data types
	 * @return - true if the data type is primitive, false otherwise
	 */
	public boolean isPrimitive(){
		return !isComplex() && !isReference() && this != VOID;
	}
	
	/**
	 * Check if this data type is a complex value type (Instance, StructureValue, EnumerationValue or their arrays)
	 * @return - true if this data type is a complex value type 
	 */
	public boolean isComplex(){
		return isEnumerationValue() || isStructureValue() || isInstanceValue();
	}
	
	/**
	 * Check if this data type is an Enumeration type (EnumerationValue or its array)
	 * @return - true if this data type is an enumeration type
	 */
	public boolean isEnumerationValue(){
		return this == ENUMERATIONVALUE || this == ENUMERATIONVALUE_ARRAY;
	}
	
	/**
	 * Check if this data type is a structure value, or an array of structure values
	 * @return - true if this datatype is a structure value, false otherwise
	 */
	public boolean isStructureValue(){
		return this == STRUCTUREVALUE || this == STRUCTUREVALUE_ARRAY;
	}
	
	/**
	 * Check if this data type is an instance value, or array of instance values
	 * @return - true if this data type is an instance value, false otherwise
	 */
	public boolean isInstanceValue(){
		return this == INSTANCEVALUE || this == INSTANCEVALUE_ARRAY;
	}
	
	/**
	 * Check if this data type is an octetString, or array of octetString values
	 * @return - true if this data type is an octetString, or array of octetStrings, false otherwise
	 */
	public boolean isOctetString(){
		return this == OCTETSTRING || this == OCTETSTRING_ARRAY;
	}

	/**
	 * Check if this dataType is a boolean, or array of booleans
	 * @return - true if the data type is a boolean, or arry of boolean values
	 */
	public boolean isBoolean() {
		return this == BOOLEAN || this == BOOLEAN_ARRAY;
	}
	
	/**
	 * Check if this dataType is a void
	 * @return - true if the data type is void, false otherwise
	 */
	public boolean isVoid(){
		return this == VOID;
	}

	/**
	 * Check if this dataType is a DateTime, or array of DateTime
	 * @return - true if this data type is a dateTime value
	 */
	public boolean isDateTime() {
		return this == DATETIME || this == DATETIME_ARRAY;
	}
	
	// END of enumeration DataType
}
