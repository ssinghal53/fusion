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
import java.util.Vector;

/**
 * Class to manage Identifiers
 * @author Sharad Singhal
 */
class FQLIdentifier extends FQLNode {
	/** Class path associated with this identifier, if any */
	private String classPath = null;
	/** Class name associated with this identifier, if any */
	private String className = null;
	/** Property name associated with this qualifier, if any */
	private String propertyName = null;
	/**
	 * Create an IDENTIFIER node
	 * @param name - name of the identifier
	 */
	FQLIdentifier(String name) {
		super(FQLOperator.IDENTIFIER, name,null);
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
	 * Get the name of the class associated with this identifier
	 * @return - name of the class. Null if no class was defined.
	 */
	String getClassName(){
		return className;
	}
	
	/**
	 * Get the class path for the class associated with this identifier
	 * @return - local path for the class. Null if no class path was defined
	 */
	String getLocalPath(){
		return classPath;
	}
	
	/**
	 * Get the name of the property associated with this identifier
	 * @return - name of the property.
	 */
	String getPropertyName(){
		return propertyName;
	}

	@Override
	void evaluate(StructureValue instance) {
		if(debug) System.out.println(toString()+"(Instance) - Enter");
		setValue(VoidValue);
		for(FQLNode child : getChildren()){
			child.evaluate(instance);
		}
		// System.out.print("** Evaluate "+toString()+" against "+header+" - ");
		// System.out.println(instance.toMOF());
		
		// match classPath if given
		if(classPath == null || classPath.equalsIgnoreCase(instance.getObjectPath().getLocalPath())){
			// match className if not null for this identifier
			if(className == null || instance.isInstanceOf(className)){
				// check if a propertyName matches this identifier
				if(instance.hasProperty(propertyName)){
					// property defined here, and instance has that property
					DataValue pv = instance.getPropertyValue(propertyName);
					setValue(pv != null ? pv : new DataValue(instance.getPropertyType(propertyName),null));
					// handle property[index]
					if(hasChildren()){
						for(FQLNode child : getChildren()){
							switch(child.getOperator()){
							case INDEX:
								if(!getType().isArray()){
									throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" must be an array property");
								}
								setValueForIndex(child);
								break;
							default:
								throw new ModelException(child.getOperator()+" operator not yet implemented as child node for IDENTIFIER operators");
							}
						}
					}
					if(debug) System.out.println(toString()+"(Instance) - Exit "+getValue());
					// System.out.println("** Returning "+toString());
					return;
				}
			}
		}
		setValue(VoidValue);
		if(debug) System.out.println(toString()+"(Instance) - Exit "+getValue());
		// System.out.println("** Returning "+toString());
		return;
	}


	/**
	 * set value(s) for the given index node
	 * @param child - index node for this identifier
	 */
	private void setValueForIndex(FQLNode child) {
		try {
			Object [] value = (Object[]) getValue().getValue();			// values of the current object
			Vector<Object> selected = new Vector<Object>();				// selected values
			Integer [] index = (Integer[]) child.getValue().getValue();	// selection index
			for(int i = 0; i < index.length; i++){
				Integer in = index[i];
				if(in != null){
					selected.add(value[in]);
				} else {
					Integer lastIn = index[i-1];
					for(int j = lastIn+1; j < value.length; j++){
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

	/* (non-Javadoc)
	 * @see net.aifusion.cql.Node#toString()
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder(super.toString());
		b.append( " [path = ").append(classPath).append(" class = ").append(className).append(" property = ").append(propertyName).append("]");
		return b.toString();
	}
	
}
