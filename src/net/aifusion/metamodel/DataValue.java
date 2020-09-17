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
 * Last Modified Aug 15, 2016 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Array;
import java.util.Arrays;

/**
 * This class encapsulates all CIM data values
 * @author Sharad Singhal
 */
public class DataValue {
	/** dataValue encapsulated in this CIM data dataValue */
	private Object value;
	/** data type for the encapsulated dataValue */
	private DataType type;

	/**
	 * Create a data dataValue from some java object
	 * @param value - java dataValue to be encapsulated. Can be any type defined in DataType
	 * @throws ModelException if given java object is null, or does not represent a valid CIM data type
	 * @see DataType
	 */
	public DataValue(Object value){
		if(value == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Null values require data type to be defined in constructor");
		type = DataType.getTypeForObject(value);
		this.value = value;
		return;
	}

	/**
	 * Create a data dataValue from the given type and dataValue
	 * @param type - expected data type
	 * @param value - java object of the expected data type (may be null)
	 * @throws ModelException if type is null, or dataValue does not match type
	 * @see DataType
	 */
	public DataValue(DataType type, Object value){
		if(type == null)
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected DataType, found null");
		if(!type.matches(value))
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected type "+type+" found "+value.toString());
		this.type = type;
		this.value = value;
		return;
	}

	/**
	 * Create a data dataValue from a given component type and dataValue
	 * @param type - string containing expected data type
	 * @param value - string containing the expected data dataValue
	 * @throws ModelException if type is null or an array type, or if the given string value cannot be converted to the corresponding type
	 * @see DataType
	 */
	public DataValue(String type, String value){
		if(type == null){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected DataType, found null");
		}
		this.type = DataType.valueOf(type.toUpperCase());
		if(this.type.isArray()){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected non-array DataType, found "+type);
		} else {
			this.value = this.type.getValueFromString(value);
		}
		return;
	}

	/**
	 * Create a data dataValue from an array of string values
	 * @param type - string containing expected array type
	 * @param value - array containing string representations of the expected values
	 * @throws ModelException if type is null or a component type, or if dataValue cannot be converted to the given type
	 */
	public DataValue(String type, String [] value){
		if(type == null){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected array DataType, found null");
		}
		this.type = DataType.valueOf(type.toUpperCase());
		if(!this.type.isArray()){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected array DataType, found "+type);
		} else {
			this.value = this.type.getValuesFromStrings(value);
		}
		return;
	}

	/**
	 * Get the type of the data dataValue
	 * @return - CIM data type for this dataValue
	 */
	public DataType getType(){
		return type;
	}

	/**
	 * Get the java value in this dataValue. Note that for array values, a shallow copy of the underlying array is returned
	 * @return - value encapsulated in this dataValue
	 */
	public Object getValue(){
		if(!type.isArray() || value == null) return value;
		Object retValue;
		Class<?> javaClass = value.getClass().getComponentType();
		if(!javaClass.isAnnotationPresent(Export.class)){
			// native CIM data type values known to DataType
			retValue = Array.newInstance(type.getComponentType().getClassForType(), Array.getLength(value));
		} else {
			// annotated java class values
			retValue = Array.newInstance(javaClass, Array.getLength(value));
		}
		for(int i = 0; i < Array.getLength(value); i++){
			Array.set(retValue, i, Array.get(value, i));
		}
		return retValue;
	}

	/**
	 * Get the MOF representation of this data dataValue
	 * @return - string containing the MOF representation of this data dataValue
	 */
	public String toMOF(){
		if(value == null){			
			return type.isArray() ? "{ }" : "null";
		} else if(type.isArray()){
			DataType objectType = type.getComponentType();
			StringBuilder b = new StringBuilder();
			b.append("{ ");
			int length = Array.getLength(value);
			for(int i = 0; i < length; i++){
				b.append(ModelUtilities.toMOFString(objectType, Array.get(value, i)));
				b.append(", ");
			}
			b.setLength(b.length()-2);
			b.append(" }");
			return b.toString();
		}
		return ModelUtilities.toMOFString(type,value);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		if(value == null) return null;
		if(type.isComplex() || type.isArray()) return toMOF();
		return value.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return value != null ? value.hashCode() : type != null ? type.hashCode() : super.hashCode();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof DataValue)) return false;
		DataValue other = (DataValue) obj;
		if(type != other.type) return false;
		if(value != null && other.value != null){
			// use of getValue() allows cast to Object[], since primitive arrays are converted there
			return type.isArray() ? Arrays.deepEquals((Object[])getValue(), (Object[])other.getValue()) : value.equals(other.value);
		} else {
			// if both are null, we match, else we must have one null and one not null-- declare a mismatch
			return value == null && other.value == null ? true : false;
		}
	}
}
