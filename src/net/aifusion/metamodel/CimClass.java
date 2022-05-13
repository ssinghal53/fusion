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
 * Created Dec 29, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Vector;

/**
 * Class to represent a CIM Class definition. A CIM Class can declare methods (operations) in addition to properties. Classes are
 * instantiated by CIM Instances. Classes are constructed by the {@link MOFParser} using MOF definitions.
 * @see CimInstance
 * @author Sharad Singhal
 */
public class CimClass extends CimStructure {
	/** Locally defined methods. Indexed by lower case name */
	private LinkedHashMap<String, CimMethod> localMethods = new LinkedHashMap<String, CimMethod>();

	/**
	 * Create the definition of a CIM class
	 * @param elementType - type of this class. Must be one of CLASS or INTERFACE
	 * @param name - name of this class
	 * @param superType - superType of this class, if any
	 * @param qualifiers - Qualifiers specified on this class
	 * @param path - namespace path for this class
	 * @param classFeatures - features (properties, structures, enumerations, methods) defined in this class
	 */
	protected CimClass(ElementType elementType, String name, CimStructure superType, List<Qualifier> qualifiers, NameSpacePath path, List<? extends QualifiedElement> classFeatures) {
		super(elementType, name, superType, qualifiers, path, classFeatures);
		if(!name.matches("([a-zA-Z0-9])+_([a-zA-Z0-9_])+")) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected name in form 'Schema' '_' 'ClassName', found "+name);
		for(QualifiedElement classFeature : classFeatures){
			switch(classFeature.getElementType()){
			case METHOD:
				localMethods.put(classFeature.getName().toLowerCase(),(CimMethod)classFeature);
				break;
			default:
				break;
			}
		}
		return;
	}
	
	/**
	 * Get the names of all methods defined in this class
	 * @return - set containing method names defined in this class
	 */
	public Set<String> getMethodNames(){
		LinkedHashSet<String> methodNames = new LinkedHashSet<String>();
		for(CimMethod m : localMethods.values()) {
			methodNames.add(m.getName());
		}
		for(String iName : getInterfaceNames()) {
			CimInterface intf = getInterface(iName);
			methodNames.addAll(intf.getAllMethodNames());
		}
		return methodNames;
	}
	
	/**
	 * Get the names of all methods defined in this class, or its superTypes
	 * @return - names of all methods in this class or its superclass hierarchy
	 */
	public Set<String> getAllMethodNames(){
		Set<String> methodNames = getMethodNames();
		NamedElement s = getSuperType();
		if(s != null) {
			// note that NamedElement enforces that the superType for a CLASS is a CLASS or a STRUCTURE
			// and that the superType for an INTERFACE is always an INTERFACE
			if(s.getElementType() == ElementType.CLASS || s.getElementType() == ElementType.INTERFACE) {
				methodNames.addAll(((CimClass)s).getAllMethodNames());
			} else if(s.getElementType() == ElementType.STRUCTURE) {
				for(String iName : ((CimStructure)s).getInterfaceNames()) {
					CimInterface intf = getInterface(iName);
					methodNames.addAll(intf.getAllMethodNames());
				}
			}
		}
		return methodNames;
	}
	
	/**
	 * Check if this class or one of its supertypes contains a given method
	 * @param methodName - name of the method to check
	 * @return - true if the class contains a method with the given name, false otherwise
	 */
	public boolean hasMethod(String methodName){
		if(methodName == null || methodName.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null or empty method requested");
		String mName = methodName.toLowerCase();
		// check locally defined methods
		if(localMethods.containsKey(mName)) return true;
		for(String iName : getInterfaceNames()) {
			CimInterface intf = getInterface(iName);
			if(intf.hasMethod(methodName)) return true;
		}
		// check methods defined in superType
		NamedElement e = getSuperType();
		if(e != null ) {
			// superType is CLASS or INTERFACE
			if(e.getElementType() == ElementType.CLASS || e.getElementType() == ElementType.INTERFACE) {	
				return ((CimClass)e).hasMethod(methodName);
			}
			// superType is STRUCTURE - check its interface hierarchy. Note that Structures do not provide method-related access
			CimStructure s = (CimStructure)e;	
			for(String iName : s.getAllInterfaceNames()) {
				CimInterface intf = getInterface(iName);
				if(intf.hasMethod(methodName)) return true;
			}
		}
		return false;
	}
	
	/**
	 * Get the return type of a method defined in this class
	 * @param methodName - name of the method desired
	 * @return - data type returned by the method. Null if no such method exists
	 */
	public DataType getMethodReturnType(String methodName) {
		CimMethod m = getMethod(methodName);
		return m == null ? null : m.getReturnedType();
	}

	/**
	 * Get the parameters associated with a given method
	 * @param methodName - name of the method desired
	 * @return - list containing method parameters. Null if no such method defined
	 */
	public List<CimParameter> getMethodParameters(String methodName) {
		CimMethod m = getMethod(methodName);
		return m == null ? null : m.getParameters();
	}
	
	/**
	 * Get the qualifiers declared on a method
	 * @param methodName - name of the method
	 * @return - all qualifiers on the method. Empty if none declared
	 */
	public List<Qualifier> getMethodQualifiers(String methodName){
		Vector<Qualifier> quals = new Vector<Qualifier>();
		CimMethod m = getMethod(methodName);
		if(m == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": No such method ("+methodName+")");
		quals.addAll(m.getQualifiers());
		return quals;
	}
	
	/**
	 * Check if a given qualifier is declared on a method
	 * @param methodName - name of the method
	 * @param qualifierName - name of the qualifier
	 * @return - true if the method has the given qualifier, false otherwise
	 */
	public boolean hasMethodQualifier(String methodName,String qualifierName){
		CimMethod m = getMethod(methodName);
		if(m == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": No such method ("+methodName+")");
		return m.hasQualifier(qualifierName);
	}
	
	/**
	 * Get the value of a method qualifier in this class
	 * @param methodName - name of the method
	 * @param qualifierName - name of the qualifier
	 * @return - value of the qualifier
	 */
	public DataValue getMethodQualifierValue(String methodName, String qualifierName) {
		CimMethod m = getMethod(methodName);
		if(m == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": No such method ("+methodName+")");
		return m.getQualifierValue(qualifierName);
	}
	
	/**
	 * Get the name of the class or interface where this property or method is defined
	 * @param methodOrPropertyName - name of the method
	 * @return - name of the origin class. Null if no such property or method exists
	 */
	public String getMethodOriginClass(String methodOrPropertyName) {
		CimMethod m = getMethod(methodOrPropertyName);
		return m == null ? null : m.getOriginClass();
		
	}
	
	/**
	 * Get all Methods exposed from this class, its interfaces, or its supertypes
	 * @return - map containing all methods exposed by this class
	 */
	protected Map<String, CimMethod> getAllMethods() {
		// order is important in the search, so we use a linkedHashMap
		LinkedHashMap<String,CimMethod> methods = new LinkedHashMap<String,CimMethod>();
		getMethods(methods);
		return methods;
	}
	
	/**
	 * Get all methods exposed by this class
	 * @param methods - collection seen so far
	 */
	private void getMethods(Map<String,CimMethod> methods){
		// first add locally defined methods
		for(Entry<String,CimMethod> nameValue : localMethods.entrySet()){
			if(methods.containsKey(nameValue.getKey())) continue;
			methods.put(nameValue.getKey(), nameValue.getValue());
		}
		// add methods defined in any declared interfaces
		for(String intfName : getInterfaceNames()) {
			CimClass intf = getInterface(intfName);
			intf.getMethods(methods);
		}
		// if a superType is defined, collect methods from the superType
		NamedElement superType = getSuperType();
		if(superType != null && superType instanceof CimClass){
			((CimClass)superType).getMethods(methods);
		}
		return;
	}
	
	/**
	 * Get the Structure returned by the method or property
	 * @param methodOrPropertyName - name of the method or property
	 * @return - structure returned by method or property, null if the method or property does not return one
	 */
	@Override
	public CimStructure getReferencedStructure(String methodOrPropertyName) {
		CimMethod m = getMethod(methodOrPropertyName);
		return m == null? super.getReferencedStructure(methodOrPropertyName) : m.getStruct();
	}

	/**
	 * Get the Enumeration returned by the method or property
	 * @param methodOrPropertyName - name of the method or property
	 * @return - Enumeration returned by method or property, null if the method or property does not return one
	 */
	@Override
	public CimEnumeration getReferencedEnum(String methodOrPropertyName) {
		CimMethod m = getMethod(methodOrPropertyName);
		return m == null ? super.getReferencedEnum(methodOrPropertyName) : m.getEnum();
	}

	/**
	 * Get the name of the referenced class returned by the method or property
	 * @param methodOrPropertyName - name of the method or property
	 * @return - referenced class returned by method or property, null if the method or property does not return one
	 */
	@Override
	public String getReferencedClass(String methodOrPropertyName) {
		CimMethod m = getMethod(methodOrPropertyName);
		return m == null ? super.getReferencedClass(methodOrPropertyName) : m.getRefClassName();
	}

	/**
	 * Get a method with a given name
	 * @param methodName - name of the method
	 * @return - method with the given name. Null if no such method
	 */
	protected CimMethod getMethod(String methodName){
		String mName = methodName.toLowerCase();
		// check locally defined methods
		if(localMethods.containsKey(mName)) return localMethods.get(mName);
		for(String iName : getInterfaceNames()) {
			CimInterface intf = getInterface(iName);
			if(intf.hasMethod(methodName)) return intf.getMethod(methodName);
		}
		// if a superType is defined, get methods from the superType
		NamedElement superType = getSuperType();
		if(superType != null) {
			// superType is a CLASS or INTERFACE
			if(superType.getElementType() == ElementType.CLASS || superType.getElementType() == ElementType.INTERFACE) {
				return ((CimClass)superType).getMethod(methodName);
			}
			// superType is a STRUCTURE. Check its interface hierarchy
			for(String iName : ((CimStructure)superType).getAllInterfaceNames()) {
				CimInterface intf = ((CimStructure)superType).getInterface(iName);
				if(intf.hasMethod(methodName)) return intf.getMethod(methodName);
			}
		}
		return null;
	}
	
	/**
	 * Invoke a method on this instance
	 * @param methodName - name of the method to invoke
	 * @param cimParameters - parameters required by the method
	 * @return - returned value from the method
	 */
	public DataValue invokeMethod(String methodName,List<CimParameter> cimParameters){
		CimMethod m = getMethod(methodName);
		if(m == null) throw new ModelException(ExceptionReason.METHOD_NOT_FOUND,getName()+" Method "+methodName+" not found");
		// Note that only static methods can be invoked on a class
		if(!m.isStatic()) throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,getName()+" Method "+methodName+" is not a static method");
		DataValue returnedValue =  m.invoke(cimParameters);
		if(hasListener(CimEventType.INVOKED, null)){
			generateEvent(new CimIndication(CimEventType.INVOKED,this,m.toMOF()));
		}
		return returnedValue;
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.CimStructure#toMOF()
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		String prefix1 = prefix+"\t";
		for(CimMethod m : localMethods.values()){
			b.append(m.toMOF(prefix1));
		}
		b.append(prefix);
		b.append("};\n");
		return b.toString();
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.CimStructure#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(!super.equals(obj)) return false;
		if(!(obj instanceof CimClass)) return false;
		CimClass other = (CimClass) obj;
		// check methods
		if(localMethods.size() != other.localMethods.size()) return false;
		for(String name : localMethods.keySet()){
				if(!localMethods.get(name).equals(other.localMethods.get(name))) return false;
		}
		return true;
	}
}
