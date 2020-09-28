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
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

/**
 * Class to represent a CIM Instance. CIM Instances are active elements that can have properties and operations, and provide
 * the ability to model control plane elements. Instances can be bound to java classes that can perform the corresponding actions
 * within the managed environment. Instances can be created from corresponding class templates.
 * @see #createInstance(CimClass, Map, String)
 * @author Sharad Singhal
 */
public class CimInstance extends StructureValue {
	/** Methods defined in this instance */
	private LinkedHashMap<String, CimMethod> methods = new LinkedHashMap<>();

	/**
	 * Create an instance of a class - Note that instances should be created by the 
	 * static method {@link CimInstance#createInstance(CimClass, Map, String)}
	 * @param creationClass - CimClass to use as a template for this instance
	 * @param properties - properties exposed by this instance (including keys)
	 * @param methods - methods exposed by this instance
	 * @param keys - {name,value} pairs defining keys for this instance
	 * @param alias - alias for the instance, if any
	 */
	private CimInstance(CimClass creationClass, Map<String, CimProperty> properties, List<CimMethod> methods,
			HashMap<String, DataValue> keys, String alias) {
		super(creationClass, keys, properties, alias);
		if(methods != null){
			for(CimMethod m : methods){
				this.methods.put(m.getLowerCaseName(),m);
			}
		}
		return;
	}

	/**
	 * Create a new CimInstance
	 * @param creationClass - class to be used as a template for this instance
	 * @param propertyValues - properties for this instance
	 * @param alias - alias, if any for this instance
	 * @return - newly constructed CIMInstance
	 */
	public static CimInstance createInstance(CimClass creationClass, Map<String,DataValue> propertyValues, String alias){
		// check that the class can be instantiated
		if(creationClass.getElementType() == ElementType.INTERFACE) throw new ModelException(ExceptionReason.INVALID_PARAMETER,
				creationClass.getName()+" is an interface class and does not allow instantiation");
		if(creationClass.hasQualifier("Abstract") && ((Boolean)creationClass.getQualifierValue("Abstract").getValue()) == true) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,
					creationClass.getName()+" is an abstract class and does not allow instantiation");
		}
		
		Map<String, CimProperty> allProperties = creationClass.getAllProperties();
		LinkedHashMap<String,CimProperty> instanceProperties = new LinkedHashMap<String,CimProperty>();
		HashMap<String,DataValue> keyValues = new HashMap<String,DataValue>();

		// validate all given properties and locate keys, if any
		for(String pName : propertyValues.keySet()){
			CimProperty p = allProperties.remove(pName.toLowerCase());
			if(p == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,creationClass.getName()+" does not have property "+pName);
			DataValue v = propertyValues.get(pName);
			p.validate(v);
			if(p.isKey()){
				if(v == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,creationClass.getName()+" Key property "+
						p.getName()+" must have a value");
				keyValues.put(p.getLowerCaseName(), v);
			}
			instanceProperties.put(p.getLowerCaseName(), p.createInstanceProperty(v));
		}
		// check if we have all required properties in the incoming property values
		for(CimProperty p : allProperties.values()){
			if(p.isRequired()) throw new ModelException(ExceptionReason.NOT_FOUND,creationClass.getName()+": Required Property "+p.getFullName()+" not given");
			if(p.isKey()) throw new ModelException(ExceptionReason.NOT_FOUND,creationClass.getName()+": Key Property "+p.getFullName()+" not given");
			if(p.hasDefaultValue() || p.isWritable())
				instanceProperties.put(p.getLowerCaseName(),p.createInstanceProperty(p.getDefaultValue()));
		}
		// add class methods to the instance
		List<CimMethod> instanceMethods = new Vector<CimMethod>();
		for(CimMethod m : creationClass.getAllMethods().values()){
			instanceMethods.add(m.createInstanceMethod());
		}
		CimInstance instance = new CimInstance(creationClass,instanceProperties,instanceMethods,keyValues,alias);
		// notify class listeners, if any
		if(creationClass.hasListener(CimEventType.CREATED, null)){
			creationClass.generateEvent(new CimIndication(CimEventType.CREATED,creationClass,instance.toMOF()));
		}
		return instance;
	}
	
	/**
	 * Bind this Cim Instance to a java object, and return the corresponding java object
	 * @return Java Object bound to this CIM Instance
	 */
	@Override
	public Object bind(Object implObject) {
		// bind any properties defined in this class or its superclasses
		implObject = super.bind(implObject);
		
		// bind methods defined in this CimInstance
		for(CimMethod cimMethod : methods.values()) {
			// validate the method against the object
			Method javaMethod = JavaModelMapper.validateMethodBinding(cimMethod, implObject);
			cimMethod.bind(javaMethod, implObject);
		}
		// return the implementation object
		return implObject;
	}

	/**
	 * Get the underlying Cimclass used to create this instance
	 * @return - underlying CimClass
	 */
	public CimClass getCreationClass() {
		return (CimClass) super.getCreationStruct();
	}

	/**
	 * Get the names of all methods defined in this instance
	 * @return - names of all methods in this instance
	 */
	public Set<String> getMethodNames(){
		HashSet<String> methodNames = new HashSet<String>();
		Map<String, CimMethod> methods = getCreationClass().getAllMethods();
		for(CimMethod m : methods.values()) {
			methodNames.add(m.getName());
		}
		return methodNames;
	}

	/**
	 * Get the return type for a declared method in this instance
	 * @param methodName - name of the desired method
	 * @return return type for the method. Null if no such method defined
	 */
	public DataType getMethodReturnType(String methodName) {
		String mName = methodName.toLowerCase();
		return methods.containsKey(mName) ? methods.get(mName).getReturnedType() : null;
	}

	/**
	 * Get the parameters associated with a declared method in this instance
	 * @param methodName - name of the desired method
	 * @return - paramters associated with the method. Null if no such method defined
	 */
	public List<CimParameter> getMethodParameters(String methodName) {
		String mName = methodName.toLowerCase();
		return methods.containsKey(mName) ? methods.get(mName).getParameters() : null;
	}

	/**
	 * Invoke a method on this instance
	 * @param methodName - name of the method to invoke
	 * @param cimParameters - parameters required by the method
	 * @return - returned value from the method
	 */
	public DataValue invokeMethod(String methodName,List<CimParameter> cimParameters){
		String mName = methodName.toLowerCase();
		CimMethod m = methods.get(mName);
		if(m == null) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getCreationClass().getName()+" does not define method ["+methodName+"]");
		}
		DataValue returnedValue = m.invoke(cimParameters);
		if(hasListener(CimEventType.INVOKED, null)){
			generateEvent(new CimIndication(CimEventType.INVOKED,this,m.toMOF()));
		}
		return returnedValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.StructureValue#toMOF(java.lang.String)
	 */
	@Override
	protected String toMOF(String prefix) {
		return toMOF(prefix,true);
	}

	/**
	 * Return the MOF value for this instance value
	 * @param prefix - prefix for the MOF
	 * @param showAlias - if true, alias for this value, if any, is returned as part of the MOF value
	 * @return - String containing MOF representation
	 */
	protected String toMOF(String prefix, boolean showAlias) {
		StringBuilder b = new StringBuilder(prefix);
		b.append(super.toMOF(prefix, showAlias));
		b.append(";");
		return b.toString();
	}

	@Override
	public boolean equals(Object obj) {
		// TODO: This needs fixing
		// check superclass equals
		if(!super.equals(obj) || !(obj instanceof CimInstance)) return false;
		CimInstance otherInstance = (CimInstance) obj;
		return toMOF().equals(otherInstance.toMOF());
	}
}
