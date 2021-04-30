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

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

/**
 * Class to represent a CIM Enumeration. CIM Enumerations are constructed by the {@link MOFParser}.
 * @author Sharad Singhal
 *
 */
public class CimEnumeration extends NamedElement {
	/** Data type for this enumeration */
	private DataType type;
	/** Values for this enumeration. Indexed by lower case names */
	private LinkedHashMap<String,EnumerationValue> values = new LinkedHashMap<String,EnumerationValue>();
	/** bound java Enum, if any */
	Class<?> boundJavaEnum;

	/**
	 * Create a CIM Enumeration
	 * @param name - name of this enumeration
	 * @param superType - superType for this Enumeration, if any
	 * @param qualifiers - qualifiers for this enumeration
	 * @param path - NameSpace Path for this enumeration
	 * @param type - data type associated with this enumeration (must be integer or string)
	 * @param values - list of enumeration values associated with this enumeration
	 */
	protected CimEnumeration(String name, CimEnumeration superType, List<Qualifier> qualifiers, NameSpacePath path, DataType type, List<EnumerationValue> values) {
		super(ElementType.ENUMERATION, name, superType, qualifiers, path, null, null);
		// validate that enumeration is either string or integer singleton
		if(type.isArray() || (!type.isString() && !type.isInteger())){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Enumeration "+name+" expected Integer or String, found "+type);
		}
		this.type = type;
		String myName = getFullName();
		// validate all values against the enumeration type and enumeration name
		for(EnumerationValue v : values){
			if(v.getDataType() != type){
				throw new ModelException(ExceptionReason.INVALID_PARAMETER, name+": Enumeration Value "+v.toMOF()+" does not match Enumeration type "+type);
			}
			String expectedValueName = myName + "." + v.getName();
			if(!(v.getFullName().equals(expectedValueName))){
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,name+": Enumeration value "+v.toMOF()+" does not match names, expected "+expectedValueName+
						" found "+v.getFullName());
			}
			v.setEnumeration(this);
			this.values.put(v.getLowerCaseName(), v);
		}
		// validate that inherited keys are not being re-declared
		if(superType != null){
			Set<String> keysPresent = superType.getKeys();
			for(EnumerationValue v : values){
				if(keysPresent.contains(v.getName().toLowerCase()))
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": declares duplicate Enum dataValue "+v.getName());
			}
		}
		return;
	}

	/**
	 * Get the primitive data type for all values in this enumeration
	 * @return - data type for all values in this enumeration
	 */
	public DataType getDataType(){
		return type;
	}

	/**
	 * Get the set of keys defined in this enumeration (or its superTypes)
	 * @return - set of keys (mixed case) in the enumeration
	 */
	public Set<String> getKeys(){
		HashSet<String> keys = new HashSet<String>();
		for(EnumerationValue v : values.values()) {
			keys.add(v.getName());
		}
		CimEnumeration superType = (CimEnumeration) getSuperType();
		if(superType != null) keys.addAll(superType.getKeys());
		return keys;
	}

	/**
	 * Check if this enumeration (or one of its superTypes) declares a given key
	 * @param key - key to check
	 * @return - true if the given key is declared, false otherwise
	 */
	public boolean hasKey(String key){
		if(key == null) return false;
		if(values.containsKey(key.toLowerCase())) return true;
		CimEnumeration superType = (CimEnumeration) getSuperType();
		if(superType != null) return superType.hasKey(key);
		return false;
	}

	/**
	 * Get the primitive dataValue corresponding to a given key
	 * @param key - name of the enumeration element
	 * @return - dataValue corresponding to the enumeration element. Null if no such element exists
	 */
	public DataValue getDataValue(String key){
		if(key == null) return null;
		EnumerationValue v = getValue(key);
		return v != null ? v.getDataValue() : null;
	}

	/**
	 * Get the enumeration value corresponding to a given key
	 * @param key - name of the enumeration element
	 * @return - enumerationValue corresponding to the element. Null if no such element exists
	 */
	public EnumerationValue getValue(String key){
		if(key == null) return null;
		EnumerationValue v =  values.get(key.toLowerCase());
		if(v != null) return v;
		CimEnumeration superType = (CimEnumeration) getSuperType();
		return superType == null ? null : superType.getValue(key);
	}

	/**
	 * Check if this enumeration has explicitly defined values
	 * @return - true if the enumeration has explicit values defined
	 */
	public boolean hasDefinedValues(){
		for(EnumerationValue v : values.values()){
			if(v.hasValue()) return true;
		}
		return false;
	}

	/**
	 * Bind this CimEnum to a java Enum
	 * @return - bound java Enum
	 */
	public Class<?> bind() {
		if(boundJavaEnum != null) return boundJavaEnum;
		// locate the java class based on the MappingStrings qualifier
		String boundClassName = null;
		DataValue mappingString = getQualifierValue("MappingStrings");
		if(mappingString != null && mappingString.getValue() != null) {
			String [] mappings = (String []) mappingString.getValue();
			for(String c : mappings) {
				if(c.startsWith(Constants.fusionMap)) {
					boundClassName = c.substring(Constants.fusionMap.length());
					break;
				}
			}
		}
		if(boundClassName == null) {
			throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Qualifier MappingStrings not declared in "+getName());
		}
		ClassLoader loader = CimStructure.class.getClassLoader();
		try {
			Class<?> javaClass = loader.loadClass(boundClassName);
			if(!javaClass.isEnum()) throw new ModelException(ExceptionReason.INVALID_CLASS,getName()+": "+javaClass.getName()+" is not an Enum class");
			JavaModelMapper.validateEnumBinding(this, javaClass);
			boundJavaEnum = javaClass;
		} catch (ClassNotFoundException e) {
			throw new ModelException(ExceptionReason.NOT_FOUND,getName()+" : Could not locate Java Class "+boundClassName);
		}
		return boundJavaEnum;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Element#toMOF()
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		if(b.length() > 0 && b.charAt(b.length()-1) == ']'){
			b.append("\n");
			b.append(prefix);
		}
		b.append("Enumeration ");
		b.append(getName());
		b.append(" : ");
		CimEnumeration superType = (CimEnumeration) getSuperType();
		b.append(superType != null ? superType.getName() : type.toMOF());
		b.append(" {\n");
		if(values != null){
			for(EnumerationValue v : values.values()){
				b.append(prefix);
				b.append("\t");
				b.append(v.toMOF(""));
				b.append(",\n");
			}
			b.setLength(b.length()-2);
			b.append("\n");
			b.append(prefix);
			b.append("};\n");
		}
		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.NamedElement#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(!super.equals(obj)) return false;
		CimEnumeration other = (CimEnumeration) obj;
		if(type != other.type) return false;
		if(values.size() != other.values.size()) return false;
		if(values.size() > 0){
			for(String key : values.keySet()){
				if(!values.get(key).equals(other.values.get(key))) return false;
			}
		}
		return true;
	}
}
