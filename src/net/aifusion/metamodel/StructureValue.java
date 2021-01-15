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
 * Created Dec 26, 2014 by Sharad Singhal
 * Last modified November 27, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * Class to represent CimStructure values. A structure value encapsulates a set of key-value pairs. It can be created from a CimStructure
 * as a template using the factory method {@link #createStructureValue(CimStructure, Map, String)}.
 * @author Sharad Singhal
 */
public class StructureValue extends NamedElement {
	/** properties defined in this structure value */
	private Map<String,CimProperty> properties;
	/** alias value for this structure value, if any */
	private String alias = null;
	/** Structure used to define this StructureValue */
	private CimStructure struct;
	/** bound object, if any */
	private Object boundObject;

	/**
	 * Create a new StructureValue
	 * @param keys Key properties, if any in this structure value
	 * @param properties - properties exposed in this Structure Value (including key properties)
	 * @param alias - alias used for this structure
	 */
	StructureValue(CimStructure struct, Map<String, DataValue> keys, Map<String,CimProperty> properties, String alias){
		super(struct.getElementType() == ElementType.STRUCTURE ? ElementType.STRUCTUREVALUE : ElementType.INSTANCE
				,struct.getName(), null, null, struct.getNameSpacePath(), keys, alias);
		if(alias != null && !alias.startsWith("$")){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+" expected alias starting with '$', found "+alias);
		}
		this.alias = alias;
		this.properties = properties;
		this.struct = struct;
		return;
	}

	/**
	 * Create a new Structure value from a given structure, and associated property values
	 * @param struct - CimStructure to be used in creating the value
	 * @param propertyValues - property values to be used for creating the structure value
	 * @param alias - alias, if any for this Structure value
	 * @return - constructed StructureValue
	 */
	public static StructureValue createStructureValue(CimStructure struct, Map<String,DataValue> propertyValues, String alias){
		// check that the structure can be instantiated
		if(struct.getElementType() == ElementType.INTERFACE) throw new ModelException(ExceptionReason.INVALID_PARAMETER,
				struct.getName()+" is an interface and does not allow instantiation");
		if(struct.hasQualifier("Abstract") && ((Boolean)struct.getQualifierValue("Abstract").getValue()) == true) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,
					struct.getName()+" is an abstract structure and does not allow instantiation");
		}

		Map<String, CimProperty> allProperties = struct.getAllProperties();
		LinkedHashMap<String,CimProperty> structureProperties = new LinkedHashMap<String,CimProperty>();
		HashMap<String,DataValue> keyValues = new HashMap<String,DataValue>();
		
		// validate all given properties and locate keys, if any
		for(String pName : propertyValues.keySet()){
			CimProperty p = allProperties.remove(pName.toLowerCase());
			if(p == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,struct.getName()+" does not have property "+pName);
			DataValue v = propertyValues.get(pName);
			p.validate(v);
			if(p.isKey()) keyValues.put(p.getLowerCaseName(), v);
			structureProperties.put(p.getLowerCaseName(), p.createInstanceProperty(v));
		}
		// check if we have all required properties in the incoming property values
		// add any properties with default values and writable properties
		for(CimProperty p : allProperties.values()){
			if(p.isRequired()) throw new ModelException(ExceptionReason.NOT_FOUND,struct.getName()+": Required Property "+p.getFullName()+" not given");
			if(p.isKey()) throw new ModelException(ExceptionReason.NOT_FOUND,struct.getName()+": Key Property "+p.getFullName()+" not given");
			if(p.hasDefaultValue() || p.isWritable())
				structureProperties.put(p.getLowerCaseName(),p.createInstanceProperty(p.getDefaultValue()));
		}
		return new StructureValue(struct,keyValues,structureProperties, alias);
	}
	
	/**
	 * Bind this structure value to a Java implementation
	 * @return - bound java object with all properties bound
	 */
	public Object bind() {
		if(boundObject != null) return boundObject;
		Class<?> javaClass = getCreationStruct().bind();
		// Construct a java object from the structure value
		boundObject = JavaModelMapper.createJavaObjectForCim(this,javaClass);
		// bind all CIM properties (this class, or its superclasses)
		for(CimProperty p : properties.values()) {
			// validate the property in the object, and get the getter/setter methods
			Method[] javaMethods = JavaModelMapper.validatePropertyBinding(p, boundObject);
			p.bind(javaMethods[0], javaMethods[1], boundObject);
		}
		return boundObject;
	}
	
	/**
	 * Get the alias value associated with this structure value, if any
	 * @return - alias value, if any. Null if no alias was defined
	 */
	public String getAlias(){
		return alias;
	}

	/**
	 * Check if this structure value has an alias
	 * @return - true if this structure value has an alias, false otherwise
	 */
	public boolean hasAlias(){
		return alias != null;
	}

	/**
	 * Get the structure used to create this value
	 * @return - structure used to create this value
	 */
	public CimStructure getCreationStruct(){
		return struct;
	}

	/**
	 * Check if this Structure Value has a non-null property value
	 * @param propertyName - name of the property 
	 * @return - true if the structure value has a non-null property with the given name, false otherwise
	 */
	public boolean hasNonNullProperty(String propertyName) {
		DataValue v = getPropertyValue(propertyName);
		return v != null && v.getValue() != null;
	}

	/**
	 * Get the value of a property from this structure value
	 * @param propertyName - name of the property
	 * @return - value of the property
	 */
	public DataValue getPropertyValue(String propertyName){
		if(!struct.hasProperty(propertyName)){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,struct.getName()+" does not define property ["+propertyName+"]");
		}
		String pName = propertyName.toLowerCase();
		return properties.containsKey(pName) ? properties.get(pName).getValue() : struct.getProperty(pName).getDefaultValue();
	}

	/**
	 * Set the value of a property in this instance
	 * @param propertyName - name of the property
	 * @param propertyValue - value to be set in the property
	 */
	public void setPropertyValue(String propertyName,DataValue propertyValue){
		if(!struct.hasProperty(propertyName)){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,struct.getName()+" does not define property ["+propertyName+"]");
		}
		String pName = propertyName.toLowerCase();
		CimProperty p = properties.get(pName);
		if(p == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,struct.getName()+":"+propertyName+" is not a settable property");
		p.setValue(propertyValue);
		if(hasListener(CimEventType.UPDATED, null)){
			generateEvent(new CimIndication(CimEventType.UPDATED,this,p.toMOF()));
		}
		return;
	}


	/**
	 * Check if the given property is defined in this structure value
	 * @param propertyName - name of the property to be checked
	 * @return - true if the property exists in this structure value, false otherwise
	 */
	public boolean hasProperty(String propertyName){
		return properties.containsKey(propertyName.toLowerCase());
	}

	/**
	 * Get the names of all properties defined in this structure value
	 * @return - set containing names of all properties defined in this structure value
	 */
	public Set<String> getPropertyNames(){
		return struct.getAllPropertyNames();
	}

	/**
	 * Get the property type of a property in this structure value
	 * @param propertyName - name of the property to get the type
	 * @return - data type of the property
	 */
	public DataType getPropertyType(String propertyName){
		return struct.getPropertyType(propertyName);
	}
	
	/**
	 * Check if this instance/value is an instance of a given class/structure
	 * @param givenClass - class/structure to check 
	 * @return - true if this instance is an instance of the given class/structure (or one of its superTypes)
	 */
	public boolean isInstanceOf(CimStructure givenClass) {
		return struct.isSubTypeOf(givenClass.getName());
	}

	/**
	 * Check if this instance/value is an instance of a given class/structure
	 * @param className - name of the class/structure to check
	 * @return - true if the instance/value is an instance of the given class/structure, false otherwise
	 */
	public boolean isInstanceOf(String className){
		return struct.isSubTypeOf(className);
	}


	/**
	 * Return the MOF value for this Structure value
	 * @param prefix - prefix for the MOF
	 * @param showAlias - if true, alias for this value, if any, is returned as part of the MOF value
	 * @return - String containing MOF representation
	 */
	protected String toMOF(String prefix, boolean showAlias) {
		StringBuilder b = new StringBuilder(prefix);
		boolean isStruct = struct.getElementType() == ElementType.STRUCTURE;
		b.append(isStruct ? "value of " : "instance of ");
		b.append(struct.getName());
		if(alias != null && showAlias) {
			b.append(" as ");
			b.append(alias);
		}
		b.append(" {\n");
		if(!properties.isEmpty()){
			String prefix1 = prefix+"\t";
			for(CimProperty p : properties.values()){
				if(p.isReadable() && p.hasValue()){
					b.append(prefix1);
					b.append(p.getName());
					b.append(" = ");
					String val = properties.get(p.getLowerCaseName()).getValue().toMOF();
					if(val == null){
						b.append("null");
					} else {
						for(int i = 0; i < val.length(); i++){
							char c = val.charAt(i);
							b.append(c);
							if(c == '\n') b.append(prefix1);
						}
					}
					b.append(";\n");
				}
			}
		}
		b.append(prefix);
		b.append("}");
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Element#toMOF(java.lang.String)
	 */
	@Override
	protected String toMOF(String prefix) {
		return toMOF(prefix,true);
	}

	@Override
	public boolean equals(Object obj) {
		if(!super.equals(obj) || !(obj instanceof StructureValue)) return false;
		StructureValue other = (StructureValue) obj;
		if(!other.struct.equals(struct)) return false;

		// check all properties. Note that it is possible that default values may be specified
		// in one but not the other set, so we need to use the proper comparisons
		HashSet<String> seen = new HashSet<String>();
		if(properties != null){
			for(String pName : properties.keySet()){
				DataValue mValue = properties.get(pName).getValue();
				DataValue oValue = other.getPropertyValue(pName);
				if(mValue != null){
					if(oValue == null || !mValue.equals(oValue)) return false;
				} else {
					if(oValue != null) return false;
				}
				seen.add(pName);
			}
		}
		if(other.properties != null){
			for(String pName : other.properties.keySet()){
				if(seen.contains(pName)) continue;
				DataValue oValue = other.properties.get(pName).getValue();
				DataValue mValue = getPropertyValue(pName);
				if(oValue != null){
					if(mValue == null || !oValue.equals(mValue)) return false;
				} else {
					if(mValue != null) return false;
				}
			}
		}
		return true;
	}
}
