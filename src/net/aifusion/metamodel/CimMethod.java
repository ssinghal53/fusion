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
 * Created Dec 28, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Vector;

/**
 * Class to represent a CIM Method
 * @author Sharad Singhal
 */
public class CimMethod extends QualifiedElement {
	/** Fully qualified name of the class or structure within which this method is declared */
	private String originClass = "";
	/** Return type for this method */
	private DataType returnType;
	/** Parameters for this method */
	private List<CimParameter> parameters;
	
	/** Name of referenced class returned (for methods returning object paths) */
	private String refClassName = null;
	/** reference to the returned structure (for method returning structure values) */
	private CimStructure structure = null;
	/** reference to the returned enumeration (for methods returning enumeration values) */
	private CimEnumeration enumeration = null;
	/** Java object bound to this method */
	private Object boundObject;
	/** Java method bound to this method */
	private Method boundMethod;
	
	/**
	 * Create a CIM method returning primitive types, or arrays of primitive types
	 * @param originClass - full name of the class within which this method resides
	 * @param name - name of the method
	 * @param returnType - data type returned
	 * @param qualifiers - qualifiers for the method
	 * @param parameters - parameter list for the method
	 */
	protected CimMethod(String originClass, String name, DataType returnType, List<Qualifier> qualifiers, List<CimParameter> parameters) {
		// methods do not have superTypes
		super(ElementType.METHOD, name, qualifiers);
		if(!(returnType.isPrimitive() || returnType.isVoid())){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,name+": Non-primitive methods must use different constructor");
		}
		this.returnType = returnType;
		this.parameters = parameters;
		this.originClass = originClass;
		return;
	}
	
	/**
	 * Create a CIM method returning object references (or their arrays)	
	 * @param originClass - full name of the class within which this method resides
	 * @param name - name of the method
	 * @param refClassName - name of the referenced class or structure returned
	 * @param isArray - true if the method returns an array
	 * @param qualifiers - qualifiers for this method
	 * @param parameters - parameter list for the method
	 * @param defaultValue - default returnValue, if any
	 */
	protected CimMethod(String originClass, String name, String refClassName, boolean isArray, List<Qualifier> qualifiers, List<CimParameter> parameters){
		super(ElementType.METHOD,name,qualifiers);
		returnType = isArray ? DataType.OBJECTPATH_ARRAY : DataType.OBJECTPATH;
		this.refClassName = refClassName;
		this.parameters = parameters;
		this.originClass = originClass;
		return;
	}
	
	/**
	 * Create a CIM method returning enumeration values (or their arrays)
	 * @param originClass - full name of the class within which this method resides
	 * @param name - name of the method
	 * @param enumeration - enumeration from which return values must be selected
	 * @param isArray - true if this method returns an array
	 * @param qualifiers - qualifiers for this method, if any
	 * @param parameters - parameter list for the method
	 */
	protected CimMethod(String originClass, String name, CimEnumeration enumeration, boolean isArray, List<Qualifier> qualifiers, List<CimParameter> parameters){
		super(ElementType.METHOD, name, qualifiers);
		returnType = isArray ? DataType.ENUMERATIONVALUE_ARRAY : DataType.ENUMERATIONVALUE;
		this.enumeration = enumeration;
		this.parameters = parameters;
		this.originClass = originClass;
		return;
	}
	
	/**
	 * Create a CIM method returning CimStructure values (or their arrays)
	 * @param originClass - full name of the class within which this method resides
	 * @param name - name of the method
	 * @param structure - structure whose value must be returned
	 * @param isArray - true if this method returns an array
	 * @param qualifiers - qualifiers, if any
	 * @param parameters - parameter list for the method
	 */
	protected CimMethod(String originClass, String name, CimStructure structure, boolean isArray, List<Qualifier> qualifiers, List<CimParameter> parameters){
		super(ElementType.METHOD,name,qualifiers);
		if(structure == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": null structure provided");
		switch(structure.getElementType()){
		case STRUCTURE:
			returnType = isArray ? DataType.STRUCTUREVALUE_ARRAY : DataType.STRUCTUREVALUE;
			break;
		case CLASS:
			returnType = isArray ? DataType.INSTANCEVALUE_ARRAY : DataType.INSTANCEVALUE;
			break;
		default:
			// association, interface
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": only structures and instances are currently allowed in parameters");
		}
		this.structure = structure;
		this.parameters = parameters;
		this.originClass = originClass;
		return;
	}
	
	/**
	 * Get the referenced class name for this method
	 * @return - referenced class name. Null if this method does not return an objectPath
	 */
	public String getRefClassName(){
		return refClassName;
	}
	
	/**
	 * Get the enumeration associated with this method
	 * @return - referenced enumeration. Null if this method does not return an enumeration value
	 */
	public CimEnumeration getEnum(){
		return enumeration;
	}
	
	/**
	 * Get the structure associated with this method
	 * @return - referenced structure. Null if this method does not return a structure value
	 */
	public CimStructure getStruct(){
		return structure;
	}
	
	/**
	 * Get the return type associated with this method
	 * @return - data type returned by this method
	 */
	public DataType getReturnedType(){
		return returnType;
	}

	/**
	 * Get the name of the class where this method is declared
	 * @return - fully qualified name of the declaring class or structure
	 */
	public String getOriginClass(){
		return originClass;
	}
	
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		if(b.length() > 0 && b.charAt(b.length()-1) == ']'){
			b.append("\n");
			b.append(prefix);
		}
		if(returnType.isReference()){
			// reference method
			b.append(refClassName);
			b.append(" ref");
		} else if(returnType.isPrimitive() || returnType.isVoid()){
			// primitive or void method
			b.append(returnType.toMOF());
		} else {
			// structure or enum method
			b.append(enumeration != null ? enumeration.getName() : structure != null ? structure.getName() : "**ERROR**");
		}
		b.append(" ");
		if(returnType.isArray()){
			b.append("[] ");
		}
		// methodName
		b.append(getName());
		b.append("(");
		if(parameters != null && !parameters.isEmpty()){
			for(CimParameter p : parameters){
				b.append(p.toMOF(""));
				b.append(", ");
			}
			b.setLength(b.length()-2);
		}
		b.append(");\n");
		return b.toString();
	}

	/**
	 * Get the list of parameters defined in this method
	 * @return a shallow copy of the parameter list
	 */
	public List<CimParameter> getParameters() {
		Vector<CimParameter> params = new Vector<CimParameter>();
		// copy parameters to prevent updates in this class
		if(parameters != null){
			for(CimParameter p : parameters){
				params.add(p.createInstanceParameter());
			}
		}
		return params;
	}
	
	/**
	 * Invoke this method with the given parameters.
	 * @param cimParameters - parameters to invoke this method
	 * @return - value returned by this method
	 */
	public DataValue invoke(List<CimParameter> cimParameters ){
		if(boundMethod == null) throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,getName()+" is not available because it is not bound");
		// invoke the method and return the results
		return JavaModelMapper.invokeMethod(this, boundObject, boundMethod, cimParameters);
	}
	
	/**
	 * Clone this method for an instance
	 * @return - cloned method
	 */
	protected CimMethod createInstanceMethod(){
		// static methods are shared between instances and the class
		if(isStatic()) return this;
		CimMethod instanceMethod = null;
		List<CimParameter> methodParameters = null;
		if(parameters != null){
			methodParameters = new Vector<CimParameter>();
			for(CimParameter p : parameters){
				methodParameters.add(p.createInstanceParameter());
			}
		}
		if(refClassName != null){
			// reference method
			instanceMethod = new CimMethod(originClass,getName(),refClassName,returnType.isArray(),getQualifiers(), methodParameters);
		} else if(enumeration != null){
			// enumeration method
			instanceMethod = new CimMethod(originClass,getName(),enumeration,returnType.isArray(),getQualifiers(), methodParameters);
		} else if(structure != null){
			// structure method
			instanceMethod = new CimMethod(originClass,getName(),structure,returnType.isArray(),getQualifiers(), methodParameters);
		} else {
			// primitive type method
			instanceMethod = new CimMethod(originClass,getName(),returnType,getQualifiers(), methodParameters);
		}
		return instanceMethod;
	}

	/**
	 * Check if this method is a static method
	 * @return - true if the method is static, false otherwise
	 */
	public boolean isStatic() {
		return hasQualifier("STATIC") && (boolean) getQualifierValue("STATIC").getValue();
	}
	
	/**
	 * Get the full name for this method. The full name for the method is the originClass '#' featureName
	 * @return full name for the method
	 */
	public String getFullName() {
		return originClass != null ? new StringBuilder(originClass).append("#").append(getName()).toString()
				: new StringBuilder("#").append(getName()).toString();
	}
	
	/*
	 * *************************************
	 * Java binding methods for this method
	 * *************************************
	 */
	
	/**
	 * Bind this CIM Method to a java method
	 * @param javaMethod - Java method to be bound
	 * @param implObject - Java implementation object to be bound (may be null in case of static methods)
	 */
	protected synchronized void bind(Method javaMethod, Object implObject){
		// save the method and the object, after validating that this method can be bound to the java method
		JavaModelMapper.validateMethodBinding(this,javaMethod,implObject);
		this.boundMethod = javaMethod;
		this.boundObject = implObject;
		return;		
	}
}
