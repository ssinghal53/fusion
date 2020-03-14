/**
 * Copyright 2016, Sharad Singhal, All Rights Reserved
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
 * Created May 1, 2016 by Sharad Singhal
 * Last Edited June 25, 2016 by Sharad Singhal
 */
package net.aifusion.utils;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.List;
import java.util.Vector;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.JavaModelMapper;
import net.aifusion.metamodel.ModelException;

/**
 * Class to represent a single CIM Feature bound to a method
 * This class is used in Java2Cim
 * @author Sharad Singhal
 */
public class CimFeature {
	private Method javaMethod;		// java method linked to this feature
	private Class<?> returnType;	// return type for the java method
	private Class<?> javaType;		// java type for this feature (may be different from method returnType);
	private DataType cimType;		// CIM type for the feature
	private String name;			// name of the feature
	private Export annotation;		// annotation for this feature
	private boolean isProperty;		// feature is a property
	private boolean isGetter;		// feature is a getter
	private boolean isSetter;		// feature is a setter
	private boolean isIsGetter;		// feature is an isGetter
	private String refCimClass;		// CIM class referenced by this feature (for complex features)
	private Class<?> refJavaClass = null;	// java class referenced by this feature (for complex features)
	/**
	 * Create a feature corresponding to an annotated Java Method
	 * @param m - java method to introspect
	 */
	public CimFeature(Method m){
		// java method for this feature
		javaMethod = m;
		// java return type for the method
		returnType = m.getReturnType();
		
		// test if this feature is a getter or a setter
		Parameter [] parameters = javaMethod.getParameters();
		// method is a getter if it has no parameters, and it returns a valid non-void cimType (or annotated class)
		isGetter = parameters.length == 0 && returnType != void.class && DataType.isCimType(returnType) ? true : false;
		// method is a setter if it returns void, has a single valid Cim data type as a parameter
		isSetter = returnType == void.class && parameters.length == 1 && DataType.isCimType(parameters[0].getType()) ? true : false;
		
		// feature is a property if the java method is a getter or setter
		isProperty = isGetter || isSetter;
		
		// java and cim data types corresponding to the feature
		javaType = isSetter ? parameters[0].getType() : returnType;
		cimType = DataType.getTypeForClass(javaType);
		refCimClass = JavaModelMapper.getRefCimClass(m);
		String refJavaName = JavaModelMapper.getMappedJavaClassName(m);
		if(!refJavaName.isEmpty()){
			try {
				refJavaClass = Class.forName(refJavaName);
			} catch (ClassNotFoundException e) {
				throw new ModelException(ExceptionReason.INVALID_CLASS,"CimFeature: Could not locate "+refJavaClass,e);
			}
		}
		
		// java method is an isGetter if a getter returns boolean & the java method name starts with "is"
		// note that isIsGetter == true ==> isGetter == true
		isIsGetter = isGetter && cimType.isBoolean() && javaMethod.getName().startsWith("is") ? true : false;
		
		// get the name of the feature
		annotation = m.getAnnotation(Export.class); // get the annotation
		name = annotation.name();	// name is in the annotation
		if(name.isEmpty()){
			name = javaMethod.getName();	// if not, get the method name
			if(isIsGetter){
				name = name.substring(2);	// method is is{CIMName}
			} else if(isGetter && name.startsWith("get") || isSetter && name.startsWith("set")){	// method is (get|set){CIMName}
				name = name.substring(3);
			}
		}
		return;
	}
	
	/**
	 * Check if this feature represents a property getter
	 * @return - true if this feature is a getter, false otherwise
	 */
	public boolean isGetter() {
		return isGetter;
	}
	
	/**
	 * Check if this feature represents a property setter
	 * @return - true if this feature is a property setter, false otherwise
	 */
	public boolean isSetter() {
		return isSetter;
	}
	
	/**
	 * Check if this feature is an isGetter
	 * @return - true if this feature is an isGetter, false otherwise
	 */
	public boolean isIsGetter() {
		return isIsGetter;
	}
	
	/**
	 * Get the name of this feature
	 * @return - name of the feature
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Check if this feature can represent a property
	 * @return - true if this feature can represent a property
	 */
	public boolean isProperty(){
		return isProperty;
	}
	
	/**
	 * Check if this feature represents a CIM Method
	 * @return - true if this feature represents a method, false otherwise
	 */
	public boolean isMethod(){
		return !isProperty;
	}
	
	/**
	 * Get the CIM type of this feature
	 * @return data type of this feature
	 */
	public DataType getCimType(){
		return cimType;
	}
	
	/**
	 * Check if this feature is a static feature
	 * @return - true if this feature is static, false otherwise
	 */
	public boolean isStatic(){
		return Modifier.isStatic(javaMethod.getModifiers());
	}
	
	/**
	 * Get the Java type used within this feature. This is the java class returned by methods or getters,
	 * or is the java class passed to the setters
	 * @return - java Class used in the method for this feature.
	 */
	public Class<?> getJavaType(){
		return javaType;
	}
	
	/**
	 * Get the qualifiers declared on this feature 
	 * @return - qualifiers from this feature (declared using qualifiers="" annotation)
	 */
	public String getQualifiers(){
		StringBuilder b = new StringBuilder();
		if(!annotation.qualifiers().isEmpty()) b.append(annotation.qualifiers());
		if(b.length() > 0 && b.charAt(b.length()-1) == ',') b.setLength(b.length()-1);
		return b.toString();
	}
	
	/**
	 * Get the MOF type declaration for this feature type
	 * @return - string containing the MOF type declaration, e.g., "String propertyName []" or "XYZ Ref MethodName" etc. 
	 */
	public String toMOF() {
		StringBuilder b = new StringBuilder();
		switch(cimType){
		case INSTANCEVALUE:
		case INSTANCEVALUE_ARRAY:
		case STRUCTUREVALUE:
		case STRUCTUREVALUE_ARRAY:
		case ENUMERATIONVALUE:
		case ENUMERATIONVALUE_ARRAY:
		case OBJECTPATH:
		case OBJECTPATH_ARRAY:
			// get the name of the referenced class
			String refClass = JavaModelMapper.getCimClassName(javaType);
			if(refClass == null || refClass.isEmpty()){
				// un-annotated class, check if we have the refClass() annotation
				refClass = annotation.refClass().isEmpty() ? "ERROR" : JavaModelMapper.getCimClassName(annotation.refClass());
			}
			b.append(refClass);
			if(cimType.isReference()) b.append(" Ref");
			break;
		default:
			b.append(cimType.toMOF());
			break;
		}
		b.append(cimType.isArray() ? " [] " : " ");
		b.append(getName());
		return b.toString();
	}
	
	/**
	 * Get the parameter values for this feature
	 * @return - string containing the parameters for this method. Empty if no parameters declared
	 * or if it is a property method
	 */
	public String getCimParameters() {
		if(isProperty || javaMethod.getParameterCount() == 0) return "";
		Parameter [] parameters = javaMethod.getParameters();
		StringBuilder b = new StringBuilder();
		for(int i = 0; i < parameters.length; i++){
			Class<?> jType = parameters[i].getType();
			DataType cType = DataType.getTypeForClass(jType);
			Export pa = parameters[i].getAnnotation(Export.class);
			String quals = pa != null ? pa.qualifiers() : "";
			String pName = pa != null && !pa.name().isEmpty() ? pa.name() : parameters[i].getName();
			String pValue = pa != null ? pa.defaultValue() : "";
			if(!quals.isEmpty()){
				b.append("[");
				b.append(quals);
				b.append("]");
			}
			switch(cType){
			case INSTANCEVALUE:
			case INSTANCEVALUE_ARRAY:
			case STRUCTUREVALUE:
			case STRUCTUREVALUE_ARRAY:
			case ENUMERATIONVALUE:
			case ENUMERATIONVALUE_ARRAY:
			case OBJECTPATH:
			case OBJECTPATH_ARRAY:
				// get the name of the referenced class
				String refClass = JavaModelMapper.getCimClassName(jType);
				if(refClass == null || refClass.isEmpty()){
					// un-annotated class, check if we have the refClass() annotation
					refClass = pa == null || pa.refClass().isEmpty() ? "ERROR" : JavaModelMapper.getCimClassName(pa.refClass());
				}
				b.append(refClass);
				if(cimType.isReference()) b.append(" Ref");
				break;
			default:
				b.append(cimType.toMOF());
				break;
			}
			b.append(" ");
			if(cType.isArray()) b.append("[]");
			b.append(pName);
			if(!pValue.isEmpty()){
				b.append(" = ");
				b.append(pValue);
			}
			b.append(",");
		}
		if(b.length() > 0 && b.charAt(b.length()-1) == ',') b.setLength(b.length()-1);
		return b.toString();
	}
	
	/**
	 * Get the java types corresponding to parameters for this feature
	 * @return - list containing java types of the parameters for this method. Empty if no parameters declared, or if feature is a property
	 */
	public List<Class<?>> getJavaParameters(){
		Vector<Class<?>> params = new Vector<Class<?>>();
		if(!isProperty){
			Parameter[] javaParams = javaMethod.getParameters();
			for(Parameter p : javaParams){
				String mappedClass = JavaModelMapper.getMappedJavaClassName(p);
				if(!mappedClass.isEmpty()){
					try {
						params.add(Class.forName(mappedClass));
					} catch (ClassNotFoundException e) {
						throw new ModelException(ExceptionReason.INVALID_CLASS,"CimFeature: Could not locate "+mappedClass,e);
					}
				} else {
					params.add(p.getType());
				}
			}
		}
		return params;
	}
	
	/**
	 * Get the default value, if any, declared on this feature. Any empty string is returned if no default value is declared
	 * @return - default value for this feature
	 */
	public String getDefaultValue(){
		return annotation.defaultValue();
	}

	/**
	 * Get the referenced CIM class for this feature.
	 * @return the referenced class, if the feature is complex. Empty String otherwise
	 */
	public String getRefClass() {
		return refCimClass;
	}

	/**
	 * Get the mapping String needed for this feature
	 * @return the mapping string needed for this feature, Empty string if the feature is not complex
	 */
	public String getMappingString() {
		return JavaModelMapper.getMappingString(javaMethod);
	}
	
	/**
	 * Get the referenced Java class for this feature
	 * @return the referenced java class for complex features. Null if feature is not complex
	 */
	public Class<?> getRefJavaClass(){
		return refJavaClass;
	}
}
