/**
 * Copyright 2021, Sharad Singhal, All Rights Reserved
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
 * Created Sept 19, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Array;
import java.util.List;
import java.util.Vector;

/**
 * Node to represent a property
 * @author Sharad Singhal
 */
class FQLPropertyName extends FQLNode {
	/** Local Path associated with the class within which the property resides */
	private String classPath;
	/** Class name associated with this property name */
	private String className;
	/** Name of the property */
	private String propertyName;
	/**
	 * Class to represent a property name
	 * @param name - name of the property
	 */
	FQLPropertyName(String name) {
		super(FQLOperator.PROPERTY_NAME, name, null);
		int cpi = name.lastIndexOf(":");
		if(cpi > 0){
			classPath = name.substring(0, cpi);
			name = name.substring(cpi+1);
		}
		int index = name.indexOf(".");
		if(index > 0){
			className = name.substring(0, index);
			propertyName = name.substring(index+1);
		} else {
			propertyName = name;
		}
		return;
	}

	/**
	 * Get the name of the class within which this property is defined.
	 * @return - name of the class. Null if no class was defined
	 */
	String getClassName(){
		return className;
	}

	/**
	 * Get the class path for the class within which this property is defined
	 * @return - local path for the class. Null if no class path was defined
	 */
	String getLocalPath(){
		return classPath;
	}

	/**
	 * Get the name of the property
	 * @return - name of the property
	 */
	String getPropertyName(){
		return propertyName;
	}

	@Override
	public void evaluate(StructureValue instance) {
		if(debug) System.out.println(toString()+"(Instance) Enter");
		List<FQLNode> children = getChildren();
		for(FQLNode child : children){
			child.evaluate(instance);
		}

		if(className == null || instance.isInstanceOf(className)){
			// no class defined here, or instance is an instance with the given name
			if(propertyName != null && instance.hasProperty(propertyName)){
				// property defined here, and instance has that property
				DataValue pv = instance.getPropertyValue(propertyName);
				setValue(pv != null ? pv : new DataValue(instance.getPropertyType(propertyName),null));
				// handle property[index]
				if(hasChildren()){
					// TODO: Note that Identifiers only have one index node, so the loop may not be needed.
					// also, the current logic means that the last index node will override previous index node values
					for(FQLNode child : children){
						switch(child.getOperator()){
						case INDEX:
							if(!getType().isArray()) throw new ModelException(toString()+" requires array value to compute index");
							setValueForIndex(child);
							break;
						default:
							throw new ModelException(child.getOperator()+" operator not yet implemented as child node for IDENTIFIER operators");
						}
					}
				}
			} else {
				setValue(VoidValue);
			}
		} else {
			setValue(VoidValue);
		}
		if(debug) System.out.println(toString()+"(Instance) Exit = "+getValue());
		return;
	}

	/**
	 * set value(s) for the given index node
	 * @param child - index node for this identifier
	 */
	private void setValueForIndex(FQLNode child) {
		try {
			Object [] value = (Object[]) getValue().getValue();	// values of the current object
			Vector<Object> selected = new Vector<Object>();		// selected values
			Integer [] index = (Integer[]) child.getValue().getValue();	// selection index
			for(int i = 0; i < index.length; i++){
				Integer in = index[i];
				if(in != null){
					selected.add(value[in]);
				} else {
					for(int j = i; j < value.length; j++){
						selected.add(value[j]);
					}
					break;
				}
			}
			DataType type = getType();
			Class<?> javaClass = type.getComponentType().getClassForType();
			if(index.length == 1){
				// have a singleton index, extract the component value
				setValue(new DataValue(type.getComponentType(),selected.get(0)));
			} else {
				// have multiple index values, extract the value array
				Object newValue = Array.newInstance(javaClass, selected.size());
				for(int i = 0; i < selected.size(); i++){
					Array.set(newValue, i, selected.get(i));
				}
				setValue(new DataValue(type,newValue));
			}
		} catch (Exception ex){
			throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" unable to resolve index",ex);
		}
		return;
	}

	@Override
	public String toString() {
		StringBuilder b = new StringBuilder(super.toString());
		b.append( " [path = ").append(classPath).append(" class = ").append(className).append(" property = ").append(propertyName).append("]");
		return b.toString();
	}


}
