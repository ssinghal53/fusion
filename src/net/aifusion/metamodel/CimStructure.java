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
 * Last Modified Jan 2, 2020 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import java.util.Set;
import java.util.Vector;

/**
 * Class to model a CIM CimStructure. A CIM Structure is a template for a named value containing other name-value pairs.
 * @see StructureValue
 * @author Sharad Singhal
 */
public class CimStructure extends NamedElement {
	/** Properties defined in this structure. Indexed by lower case name */
	private LinkedHashMap<String,CimProperty> properties = new LinkedHashMap<String,CimProperty>();
	/** Local Structures defined in this structure. Indexed by lower case name */
	private LinkedHashMap<String,CimStructure> structures = new LinkedHashMap<String,CimStructure>();
	/** Local Enumerations defined in this structure. Indexed by lower case name */
	private LinkedHashMap<String,CimEnumeration> enumerations = new LinkedHashMap<String,CimEnumeration>();
	/** Interfaces implemented by this structure. Indexed by lower case name */
	private LinkedHashMap<String,CimInterface> interfaces = new LinkedHashMap<String,CimInterface>();
	/** Bound java class, if any */
	private Class<?> boundJavaClass = null;

	/**
	 * Create a CimClass, CimStructure, or CimInterface
	 * @param elementType - type of this structure. Must be one of STRUCTURE, CLASS, INTERFACE 
	 * @param name - name of this class or structure
	 * @param superType - superType, if any for this class or structure
	 * @param qualifiers - qualifiers for this class or structure
	 * @param path - nameSpace path for this class or structure
	 * @param structureFeatures - features (local properties, structures, enums) defined within this class or structure
	 */
	protected CimStructure(ElementType elementType, String name, CimStructure superType, List<Qualifier> qualifiers, NameSpacePath path, List<? extends QualifiedElement> structureFeatures){
		super(elementType, name,superType,qualifiers, path, null, null);
		ElementType type = getElementType();
		// validate the elementType
		if(!(type == ElementType.STRUCTURE || type == ElementType.CLASS || 
				type == ElementType.INTERFACE)){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+" must be of type STRUCTURE, CLASS, INTERFACE");
		}
		// copy all elements needed
		for(QualifiedElement feature : structureFeatures){
			// TODO: check on overriding properties, enumerations and structures from superTypes and interfaces to validate qualifiers?
			ElementType fType = feature.getElementType();
			switch(fType){
			case PROPERTY:
			case REFERENCE:
				properties.put(feature.getName().toLowerCase(), (CimProperty)feature);
				break;
			case ENUMERATION:
				enumerations.put(feature.getName().toLowerCase(), (CimEnumeration)feature);
				break;
			case STRUCTURE:
				structures.put(feature.getName().toLowerCase(), (CimStructure)feature);
				break;
			case INTERFACE:
				interfaces.put(feature.getLowerCaseName(), (CimInterface)feature);
				addInterface((CimInterface)feature);	// used to check sub-type relations by NamedElement
				break;
			case METHOD:
				break;	// methods may be part of a CimClass or Interface-- ignore
			default:
				throw new ModelException("CimStructure - Unexpected feature type "+fType+" for feature "+feature.getName());
			}
		}
		// TODO: Validate that the IMPLEMENTS qualifier matches values in interfaces
		return;
	}

	/* 
	 * **********************************
	 * Embedded Interface related methods
	 * **********************************
	 */

	/**
	 * Get the names of all interfaces locally defined in this structure
	 * @return - set containing names of all defined interfaces. Empty if none defined
	 * @see #getAllInterfaceNames()
	 */
	public Set<String> getInterfaceNames(){
		LinkedHashSet<String> names = new LinkedHashSet<String>();
		if(!interfaces.isEmpty()){
			for(CimInterface i : interfaces.values()){
				if(!names.contains(i.getName())) names.add(i.getName());
			}
		}
		return names;
	}

	/**
	 * Get names of interfaces defined in this structure or its superTypes.
	 * @return - set containing names of defined interfaces. Empty if none defined
	 * @see #getInterfaceNames()
	 */
	public Set<String> getAllInterfaceNames(){
		Set<String> names = getInterfaceNames();
		CimStructure s = (CimStructure) getSuperType();
		// note that we add all interfaces in the interface hierarchy
		if(s != null) names.addAll(s.getAllInterfaceNames());
		return names;
	}

	/**
	 * Check if this structure or it's superTypes implement a given interface
	 * @param interfaceName - name of the property
	 * @return - true if the structure implements the interface, false otherwise
	 */
	public boolean implementsInterface(String interfaceName){
		if(interfaceName == null || interfaceName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty interface requested");
		String pName = interfaceName.toLowerCase();
		// check interfaces declared on this structure
		if(interfaces.containsKey(pName)) return true;
		// check supertypes of all declared interfaces
		for(CimInterface intf : interfaces.values()) {	
			if(intf.implementsInterface(interfaceName)) return true;
		}
		// check interfaces declared on supertypes
		CimStructure s = (CimStructure)getSuperType();
		return s != null ? s.implementsInterface(interfaceName) : false;
	}

	/**
	 * get an interface defined within this structure or one of its supertypes
	 * @param interfaceName - name of the local interface
	 * @return - the given Interface, null if no such interface exists
	 */
	public CimInterface getInterface(String interfaceName){
		if(interfaceName == null || interfaceName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty structure requested");
		String pName = interfaceName.toLowerCase();
		if(interfaces.containsKey(pName)) return interfaces.get(pName);
		if(!interfaces.isEmpty()){
			for(CimInterface i : interfaces.values()){
				CimInterface e = i.getInterface(interfaceName);
				if(e != null) return e;
			}
		}
		CimStructure s = (CimStructure) getSuperType();
		return s != null ? s.getInterface(interfaceName) : null;
	}

	/* 
	 * **********************************
	 * Embedded Structure related methods
	 * **********************************
	 */

	/**
	 * Get the names of structures locally defined in this structure or interfaces implemented by it
	 * @return - set containing names of all locally defined structures. Empty if none defined
	 */
	public Set<String> getStructureNames(){
		HashSet<String> names = new HashSet<String>();
		names.addAll(structures.keySet());
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				// note that we look up the entire hierarchy for the interface
				names.addAll(i.getAllStructureNames());
			}
		}
		return names;
	}

	/**
	 * Get the names of locally defined structures in this structure, or its supertypes
	 * @return - names of locally defined structures
	 */
	public Set<String> getAllStructureNames(){
		Set<String> names = getStructureNames();
		CimStructure s = (CimStructure) getSuperType();
		if(s != null) names.addAll(s.getAllStructureNames());
		return names;
	}

	/**
	 * Check if this structure (or one of its supertypes) defines a structure
	 * @param structureName - name of the structure
	 * @return - true if the structure defines the given structure, false otherwise
	 */
	public boolean hasStructure(String structureName){
		if(structureName == null || structureName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty structure requested");
		String pName = structureName.toLowerCase();
		if(structures.containsKey(pName)) return true;
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				if(i.hasStructure(structureName)) return true;
			}
		}
		CimStructure s = (CimStructure) getSuperType();
		return s != null ? s.hasStructure(structureName) : false;
	}

	/**
	 * get a structure defined within this structure, or one of its supertypes
	 * @param structureName - name of the structure
	 * @return - the given structure, null if no such structure exists
	 */
	public CimStructure getStructure(String structureName){
		if(structureName == null || structureName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty structure requested");
		String pName = structureName.toLowerCase();
		if(structures.containsKey(pName)) return structures.get(pName);
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				CimStructure e = i.getStructure(structureName);
				if(e != null) return e;
			}
		}
		CimStructure s = (CimStructure) getSuperType();
		return s != null ? s.getStructure(structureName) : null;
	}

	/* 
	 * ************************************
	 * Embedded Enumeration related methods
	 * ************************************
	 */

	/**
	 * Get the names of all enumerations locally defined in this structure
	 * @return - name of all defined enumerations. Empty if none defined
	 */
	public Set<String> getEnumerationNames(){
		HashSet<String> names = new HashSet<String>();
		names.addAll(enumerations.keySet());
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				// note that we add all enumerations in the interface
				names.addAll(i.getAllEnumerationNames());
			}
		}
		return names;
	}

	/**
	 * Get the names of all enumerations defined in this structure (or its supertypes)	
	 * @return - name of all defined enumerations. Empty if none defined
	 */
	public Set<String> getAllEnumerationNames(){
		Set<String> names = getEnumerationNames();
		CimStructure s = (CimStructure) getSuperType();
		if(s != null) names.addAll(s.getAllEnumerationNames());
		return names;
	}

	/**
	 * Check if this structure defines a local Enumeration
	 * @param enumerationName - name of the local Enumeration
	 * @return - true if the structure defines the given local enumeration, false otherwise
	 */
	public boolean hasEnumeration(String enumerationName){
		if(enumerationName == null || enumerationName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty enumeration requested");
		String pName = enumerationName.toLowerCase();
		if(enumerations.containsKey(pName)) return true;
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				if(i.hasEnumeration(enumerationName)) return true;
			}
		}
		CimStructure s = (CimStructure) getSuperType();
		return s != null ? s.hasEnumeration(enumerationName) : false;
	}

	/**
	 * get a local Enumeration defined in this structure
	 * @param enumerationName - name of the local Enumeration
	 * @return - the given local enumeration, null if no such Enumeration
	 */
	public CimEnumeration getEnumeration(String enumerationName){
		if(enumerationName == null || enumerationName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty enumeration requested");
		String pName = enumerationName.toLowerCase();
		if(enumerations.containsKey(pName)) return enumerations.get(pName);
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				CimEnumeration e = i.getEnumeration(enumerationName);
				if(e != null) return e;
			}
		}
		CimStructure s = (CimStructure) getSuperType();
		return s != null ? s.getEnumeration(enumerationName) : null;
	}

	/* 
	 * *********************************
	 * Embedded Property related methods
	 * *********************************
	 */

	/**
	 * Get names of all properties defined in this structure
	 * @return - mixed case names of all properties defined locally in this structure
	 */
	public Set<String> getPropertyNames(){
		LinkedHashSet<String> names = new LinkedHashSet<>();
		for(CimProperty p : properties.values()) {
			names.add(p.getName());
		}
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				// note that we traverse the entire interface hierarchy for property names
				names.addAll(i.getAllPropertyNames());
			}
		}
		return names;
	}

	/**
	 * Get names of all properties defined in this structure or its supertypes
	 * @return - mixed case names of all properties defined in this structure
	 */
	public Set<String> getAllPropertyNames(){
		Set<String> names = getPropertyNames();
		CimStructure s = (CimStructure) getSuperType();
		if(s != null) names.addAll(s.getAllPropertyNames());
		return names;
	}

	/**
	 * Check if this structure has a given property
	 * @param propertyName - name of the property
	 * @return - true if the structure has the given property, false otherwise
	 */
	public boolean hasProperty(String propertyName){
		if(propertyName == null || propertyName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty property requested");
		String pName = propertyName.toLowerCase();
		if(properties.containsKey(pName)) return true;
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				if(i.hasProperty(propertyName)) return true;
			}
		}
		CimStructure s = (CimStructure)getSuperType();
		return s != null ? s.hasProperty(propertyName) : false;
	}

	/**
	 * Get the origin class for a property defined in this structure
	 * @param propertyName - name of the property
	 * @return - class where this property is defined. Null if no such property exists
	 */
	public String getPropertyOriginClass(String propertyName) {
		CimProperty p = getProperty(propertyName);
		return p == null ? null : p.getOriginClass();
	}

	/**
	 * Get the qualifiers declared on a property
	 * @param propertyName - name of the property
	 * @return - all qualifiers on the property. Empty if none declared
	 */
	public List<Qualifier> getPropertyQualifiers(String propertyName){
		Vector<Qualifier> quals = new Vector<Qualifier>();
		CimProperty p = getProperty(propertyName);
		if(p == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": No such property ("+propertyName+")");
		quals.addAll(p.getQualifiers());
		return quals;
	}

	/**
	 * get a property defined in this structure
	 * @param propertyName - name of the property
	 * @return - the given property, null if no such property exists
	 */
	protected CimProperty getProperty(String propertyName){
		if(propertyName == null || propertyName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty property requested");
		String pName = propertyName.toLowerCase();
		if(properties.containsKey(pName)) return properties.get(pName);
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				CimProperty p = i.getProperty(propertyName);
				if(p != null) return p;
			}
		}
		CimStructure s = (CimStructure)getSuperType();
		return s != null ? s.getProperty(propertyName) : null;
	}

	/**
	 * Get all properties exposed by this Cim Structure
	 * @return - all properties defined in (or inherited by) this CIM Structure (indexed by lowercase name)
	 */
	protected Map<String,CimProperty> getAllProperties(){
		return getProperties(null);
	}

	/**
	 * Get all exposed properties for this structure, and its superTypes if any
	 * @param allProperties - collection containing all properties for this structure, and its superTypes
	 * @return - map containing properties
	 */
	private Map<String,CimProperty> getProperties(Map<String,CimProperty> allProperties){
		if(allProperties == null) allProperties = new LinkedHashMap<String,CimProperty>();
		// add locally defined properties
		for(Entry<String,CimProperty> nameValue : properties.entrySet()){
			if(allProperties.containsKey(nameValue.getKey())) continue;
			allProperties.put(nameValue.getKey(), nameValue.getValue());
		}
		// add properties from the interfaces
		if(!interfaces.isEmpty()){
			for(CimStructure i : interfaces.values()){
				i.getProperties(allProperties);
			}
		}
		// if a superType is defined, collect properties from the superType
		CimStructure superType = (CimStructure) getSuperType();
		if(superType != null){
			superType.getProperties(allProperties);
		}
		return allProperties;
	}

	/*
	 * ****************************************
	 * Delegate methods for property operations
	 * ****************************************
	 */

	/**
	 * Check if this structure (or its superType hierarchy) has at least one key property defined
	 * @return - true if this structure has a key property in it, false otherwise
	 */
	public boolean hasKeys(){
		for(CimProperty p : properties.values()){
			if(p.isKey()) return true;
		}
		if(!interfaces.isEmpty()){
			for(CimStructure intf : interfaces.values()){
				if(intf.hasKeys()) return true;
			}
		}
		CimStructure superType = (CimStructure) getSuperType();
		return superType != null ? superType.hasKeys() : false;
	}

	/**
	 * Return the value of a property defined in this structure
	 * @param propertyName - name of the property to be checked
	 * @return - value of the property. Null if no such property
	 * @throws ModelException if a non-static property is read
	 */
	public DataValue getPropertyValue(String propertyName){
		CimProperty p = getProperty(propertyName);
		if(p == null) return null;
		if(!p.isStatic()) throw new ModelException(ExceptionReason.ACCESS_DENIED,getName()+": Property "+propertyName+
				" is not a static property, and cannot be read at class/structure level");
		return p.isReadable() ? p.getValue() : p.getDefaultValue();
	}

	/**
	 * Get the default value of a property
	 * @param propertyName - name of the property to be checked
	 * @return - value of the property. Null if no such property, or no default is present
	 */
	public DataValue getDefaultPropertyValue(String propertyName) {
		CimProperty p = getProperty(propertyName);
		if(p == null) return null;
		return p.getDefaultValue();
	}

	/**
	 * Set the value of a property in this structure
	 * @param propertyName - name of the property
	 * @param propertyValue - value to be set in the property
	 * @throws ModelException if property does not exist or is not static
	 */
	public void setPropertyValue(String propertyName,DataValue propertyValue){
		if(!hasProperty(propertyName)){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+" does not define property ["+propertyName+"]");
		}
		CimProperty p = getProperty(propertyName);
		if(!p.isStatic()) throw new ModelException(ExceptionReason.ACCESS_DENIED,getName()+": Property "+propertyName+
				" is not a static property, and cannot be set at class level");
		p.setValue(propertyValue);
		if(hasListener(CimEventType.UPDATED, null)){
			generateEvent(new CimIndication(CimEventType.UPDATED,this,p.toMOF()));
		}
		return;
	}

	/**
	 * Get the structure associated with a structureValue property
	 * @param propertyName - name of the property
	 * @return - structure associated with the property. Null if the property does not exist, or is not a structure property
	 */
	public CimStructure getReferencedStructure(String propertyName){
		CimProperty p = getProperty(propertyName);
		return p != null && (p.getDataType().isStructureValue() || p.getDataType().isInstanceValue()) ? p.getStruct() : null;
	}

	/**
	 * Get the Enumeration associated with a EnumerationValue property
	 * @param propertyName - name of the properfy
	 * @return - Enumeration associated with the property. Null if the property does not exist, or is not an enum property
	 */
	public CimEnumeration getReferencedEnum(String propertyName){
		CimProperty p = getProperty(propertyName);
		return p != null && p.getDataType().isEnumerationValue() ? p.getEnum() : null;
	}

	/**
	 * Get the name of the class referenced by a reference property
	 * @param propertyName - name of the property
	 * @return - name of the class referenced by this property. Null if the property does not exist, or is not an object reference
	 */
	public String getReferencedClass(String propertyName){
		CimProperty p = getProperty(propertyName);
		return p != null && p.getDataType().isReference() ? p.getRefClassName() : null;
	}

	/**
	 * Get the data type associated with a given property
	 * @param propertyName - name of the property
	 * @return - data type associated with that property. Null if no such property exists
	 */
	public DataType getPropertyType(String propertyName){
		CimProperty p = getProperty(propertyName);
		return p == null ? null : p.getDataType();
	}

	/**
	 * Get the value of a property qualifier 
	 * @param propertyName - name of the property
	 * @param qualifierName - name of the qualifier
	 * @return - value of the qualifier. Null if the property does not exist, or the qualifier does not exist
	 */
	public DataValue getPropertyQualifierValue(String propertyName, String qualifierName){
		CimProperty p = getProperty(propertyName);
		if(p == null) return null;
		return p.getQualifierValue(qualifierName);
	}

	/**
	 * Test if a property has a given qualifier
	 * @param propertyName - name of the property to test
	 * @param qualifierName - name of the qualifier to test
	 * @return - true of the property has the given qualifier. False otherwise
	 */
	public boolean hasPropertyQualifier(String propertyName, String qualifierName){
		CimProperty p = getProperty(propertyName);
		if(p == null) return false;
		return p.hasQualifier(qualifierName);
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.QualifiedElement#toMOF()
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		if(b.length() > 0 && b.charAt(b.length()-1) == ']'){
			b.append("\n");
			b.append(prefix);
		}
		switch(getElementType()){
		case STRUCTURE:
			b.append("Structure ");
			break;
		case CLASS:
			b.append("Class ");
			break;
		case INTERFACE:
			b.append("Interface ");
			break;
		default:
			throw new ModelException("Internal Error in CimStructure");
		}
		// b.append(getElementType() == ElementType.STRUCTURE ? "Structure " : "Class ");
		b.append(getName());
		CimStructure sup = (CimStructure) getSuperType();
		if(sup != null){
			b.append(" : ");
			b.append(sup.getName());
		}
		b.append(" {\n");
		String prefix1 = prefix + "\t";
		if(enumerations != null){
			for(CimEnumeration e : enumerations.values()){
				b.append(e.toMOF(prefix1));
			}
		}
		if(structures != null){
			for(CimStructure s : structures.values()){
				b.append(s.toMOF(prefix1));
			}
		}
		if(properties != null){
			for(CimProperty p : properties.values()){
				b.append(p.toMOF(prefix1));
			}
		}
		// note that definitions other than structures are terminated in the corresponding subtype
		if(getElementType() == ElementType.STRUCTURE){
			b.append(prefix);
			b.append("};\n");
		}
		return b.toString();
	}

	/*
	 * *************************
	 * Bindings to java objects
	 * *************************
	 */

	/**
	 * Bind a java class to this Cim Structure, using the defined values as the key values
	 * @param values - structureValues containing keys during construction
	 * @return - java class bound to this CimStructure
	 */
	protected Class<?> bind(StructureValue value) {
		
		// TODO: May need to recurse up to the superclasses here
		
		if(value == null || value.getCreationStruct() != this) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+" requires a non-null matching value, found "+value);
		}
		if(boundJavaClass != null) return boundJavaClass;	// Structure/Instance has already been bound

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
			// the corresponding method and property bindings
			switch(getElementType()) {
			case CLASS:
				// bind methods. All methods are static, so an implementation object is not needed.
				Map<String,CimMethod> allMethods = ((CimClass)this).getAllMethods();
				for(CimMethod m : allMethods.values()) {
					if(!m.isStatic()) continue;
					Method accessMethod = JavaModelMapper.validateStaticMethodBinding(m, javaClass);
					m.bind(accessMethod, null);
				}
			case STRUCTURE:
				// bind properties. Note that all properties are static, so an implementation object is not needed.
				Map<String, CimProperty> allProperties = getAllProperties();
				for(CimProperty p : allProperties.values()) {
					if(!p.isStatic()) continue;
					Method [] accessMethods = JavaModelMapper.validateStaticPropertyBinding(p, javaClass);
					p.bind(accessMethods[0], accessMethods[1], null);
				}
				break;
			default:
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Only CLASS or STRUCTURE can be bound: found "+getElementType());
			}
			boundJavaClass = javaClass;
		} catch (ClassNotFoundException e) {
			throw new ModelException(ExceptionReason.NOT_FOUND,getName()+" : Could not locate Java Class "+boundClassName);
		}
		return boundJavaClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.NamedElement#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(!super.equals(obj)) return false;
		if(!(obj instanceof CimStructure)) return false;
		CimStructure other = (CimStructure) obj;
		// check enumerations
		if(enumerations != null){
			if(other.enumerations == null) return false;
			if(enumerations.size() != other.enumerations.size()) return false;
			for(String name : enumerations.keySet()){
				if(!enumerations.get(name).equals(other.enumerations.get(name))) return false;
			}
			return true;
		} else if(other.enumerations != null) return false;
		// check structures
		if(structures != null){
			if(other.structures == null) return false;
			if(structures.size() != other.structures.size()) return false;
			for(String name : structures.keySet()){
				if(!structures.get(name).equals(other.structures.get(name))) return false;
			}
			return true;
		} else if(other.structures != null) return false;
		// check interfaces
		if(interfaces != null){
			if(other.interfaces == null) return false;
			if(interfaces.size() != other.interfaces.size()) return false;
			for(String name : interfaces.keySet()){
				if(!interfaces.get(name).equals(other.interfaces.get(name))) return false;
			}
			return true;
		} else if(other.interfaces != null) return false;
		// check properties
		if(properties != null){
			if(other.properties == null) return false;
			if(properties.size() != other.properties.size()) return false;
			for(String name : properties.keySet()){
				if(!properties.get(name).equals(other.properties.get(name))) return false;
			}
			return true;
		} else return other.properties == null;

		// TODO: This only compares locally defined elements. Do we need to check interface hierarchies as well?
	}
}
