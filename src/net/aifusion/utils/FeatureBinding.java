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
 */
package net.aifusion.utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.JavaModelMapper;
import net.aifusion.metamodel.ModelException;

/**
 * Class to represent a binding between multiple features to
 * create a CIM property or method
 * @author Sharad Singhal
 *
 */
public class FeatureBinding {
	/** Name of the feature */
	private String featureName = null;
	/** features linked to this name */
	private CimFeature method, getter, setter, isGetter;
	/** binding configuration */
	private int conf = 0;
	
	/**
	 * Create a named CIM Feature
	 * @param name - name of the feature
	 */
	public FeatureBinding(String name){
		this.featureName = name;
		return;
	}
	
	/**
	 * Get the name for this binding	
	 * @return - name of the binding
	 */
	public String getName(){
		return featureName;
	}
	
	/**
	 * Get a map &lt;cim_className,javaClass&gt; used in this binding
	 * @return - map containing types used within this binding. Empty if no complex types are used
	 */
	public Map<String,Class<?>> getReferencedTypes(){
		HashMap<String,Class<?>> ref = new HashMap<String,Class<?>>();
		boolean isMethod = false;
		Class<?> refType = null;
		if(getter != null){
			refType = getter.getRefJavaClass();
		} else if(setter != null){
			refType = setter.getRefJavaClass();
		} else if(isGetter != null){
			refType = isGetter.getRefJavaClass();
		} else {
			refType = method.getRefJavaClass();
			isMethod = true;
		}
		if(refType != null){
			String cimClass = JavaModelMapper.getCimClassName(refType);
			ref.put(cimClass,refType);
		}
		// if we have a method, check all parameter types
		if(isMethod){
			List<Class<?>> params = method.getJavaParameters();
			for(Class<?> c : params){
				DataType dt = DataType.getTypeForClass(c);
				if(dt.isPrimitive() || dt == DataType.VOID) continue;
				String cimClass = JavaModelMapper.getCimClassName(c);
				if(!ref.containsKey(cimClass)) ref.put(cimClass,c);
			}
		}
		return ref;
	}
	
	/**
	 * Get the MOF definition of this feature binding
	 * @return - string representation of this feature binding
	 */
	public String toMOF() {
		StringBuilder b = new StringBuilder();
		switch(conf){
		case 1: // single isGetXXX boolean property
			b.append(getPropertyDefinition(isGetter, null));
			break;
		case 2: // single setter
			b.append(getPropertyDefinition(null,setter));
			break;
		case 3: // isGetter + setter
			b.append(getPropertyDefinition(isGetter, setter));
			break;
		case 4: // single getter
			b.append(getPropertyDefinition(getter, null));
			break;
		case 6: // getter + setter
			b.append(getPropertyDefinition(getter, setter));
			break;
			
		// TODO: Check if cases 5 and 7 should be illegal. We currently silently ignore the get() method
			
		case 7: // getter + setter + isGetter
			// Use isGetter+setter as property, and generate a get() method
			b.append(getPropertyDefinition(isGetter,setter));
			// b.append(getMethodDefinition(getter));
			break;
		case 5: // getter + isGetter
			// use isGetter as property, and generate a get() method
			b.append(getPropertyDefinition(isGetter,null));
			// b.append(getMethodDefinition(getter));
			break;
			
		default:	// no getter, setter, isGetter - generate a method
			b.append(getMethodDefinition(method));
			break;
		}
		return b.toString();
	}
	/**
	 * Get a method definition
	 * @param meth - feature exposed as method
	 * @return - definition for method
	 */
	private Object getMethodDefinition(CimFeature meth) {
		StringBuilder b = new StringBuilder();
		String quals = getQualifiers(meth,null);
		if(!quals.isEmpty()){
			b.append(quals);
		}
		b.append(meth.toMOF());
		b.append("(");
		b.append(meth.getCimParameters());
		b.append(")");
		b.append(";\n");
		return b.toString();
	}

	/**
	 * Validate that the getter and setter match
	 * @param get - getter feature
	 * @param set - setter feature
	 */
	private void validateProperty(CimFeature get, CimFeature set) {
		// Note that the feature names match by definition
		if(get == null || set == null) return;
		
		// isStatic() must match
		if(get.isStatic() != set.isStatic()){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,getName()+": Getter and Setter must be both static ["+get.isStatic()+","+set.isStatic()+"]");
		}
		
		// CIM DataType must match
		if(get.getCimType() != set.getCimType())
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,getName()+": Getter and Setter must have same data type ["+get.getCimType()+","+set.getCimType()+"]");
		
		// for non-primitive types, the referenced class must match
		if(!get.getCimType().isPrimitive()){
			String getRef = get != null ? get.getRefClass() : "";
			String setRef = set != null ? set.getRefClass() : "";
			if(!setRef.isEmpty() && !getRef.isEmpty() && !getRef.equals(setRef))
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,
						getName()+": Getter and Setter must refer to same class ["+get.getRefClass()+","+set.getRefClass()+"]");	
		}
		return;
	}
	
	/**
	 * Get the combined qualifiers on the getter and setter
	 * @param get - getter feature (or single method)
	 * @param set - setter feature
	 * @return string containing the feature qualifiers
	 */
	private String getQualifiers(CimFeature get, CimFeature set){
		StringBuilder b = new StringBuilder("[");
		if(get != null && get.isStatic() || set != null && set.isStatic()){
			b.append("Static,");
		}
		if(set != null){
			// add write(true) qualifier
			b.append("Write,");
			b.append(set.getQualifiers());
		}
		if(get != null){
			if(b.length() > 1 && b.charAt(b.length()-1) != ',') b.append(",");
			b.append(get.getQualifiers());
		} else {
			// add read(false) qualifier
			if(b.length() > 1 && b.charAt(b.length()-1) != ',') b.append(",");
			b.append("Read(false),");
		}
		if(b.length() > 1 && b.charAt(b.length()-1) == ',') b.setLength(b.length()-1);
		if(b.length() > 1){
			b.append("]\n");
			return b.toString();
		}
		return "";
	}
	
	/**
	 * Get the combined property definition based on the getter and setter
	 * @param get - getter feature
	 * @param set - setter feature
	 */
	private String getPropertyDefinition(CimFeature get, CimFeature set){
		StringBuilder b = new StringBuilder();
		// validate property methods, and get qualifiers
		validateProperty(get,set);
		String quals = getQualifiers(get,set);
		if(!quals.isEmpty()){
			b.append(quals);
		}
		// get property definition and default value
		String def = null;
		String value = null;
		if(get != null){		// getter is not null
			def = get.toMOF();
			if(def.startsWith("ERROR")) def = null;
			value = get.getDefaultValue();
			if(value.isEmpty()) value = null;
		}
		
		if(set != null){	// setter is not null
			String sdef = set.toMOF();
			if(def == null){
				def = sdef;
			} else if(!sdef.startsWith("ERROR") && def != null && !def.equals(sdef)){
				// getter and setter definitions do not match
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,getName()+": Getter and Setter must have same defintion ["+get.toMOF()+","+set.toMOF()+"]");
			}
			String svalue = set.getDefaultValue();
			if(value == null){
				value = svalue;
			} else {
				// check that either sValue is null, or value and sValue are the same
				if(svalue != null && !svalue.isEmpty() && !svalue.equals(value)){
					// getter and setter definitions do not match
					throw new ModelException(ExceptionReason.TYPE_MISMATCH,getName()+": Getter and Setter have different default values ["
					+value+","+svalue+"]");
				}
			}
		}
		b.append(def);
		if(value != null && !value.isEmpty()){
			b.append(" = ");
			b.append(value);
		}
		b.append(";\n");
		return b.toString();
	}

	/**
	 * Add a Feature to this feature binding
	 * @param f - feature to add to this binding
	 */
	public void addFeature(CimFeature f){
		if(!f.getName().equals(featureName)){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Feature name "+f.getName()+" does not match binding "+featureName);
		}
		if(f.isIsGetter()){
			if(isGetter == null){
				isGetter = f;
				conf |= 1;
				return;
			}
		} else if(f.isGetter()){
			if(getter == null){
				getter = f;
				conf |= 4;
				return;
			}
		} else if(f.isSetter()){
			if(setter == null){
				setter = f;
				conf |= 2;
				return;
			}
		} else if(f.isMethod()){
			if(method == null){
				method = f;
				return;
			}
		}
		throw new ModelException(ExceptionReason.ALREADY_EXISTS,"Feature "+f.getName()+" duplicated");
	}
}
