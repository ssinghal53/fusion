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
 * Created Oct 9, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Vector;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimStructure;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;

/**
 * Class to manage SELECT_LIST
 * @author Sharad Singhal
 */
class SelectList extends Node {
	/** template class to be returned */
	private CimClass template = null;
	/** Properties used to create instance values */
	private HashMap<String,DataValue> properties = new HashMap<String,DataValue>();
	/** Row ID */
	int rowID = 0;

	/**
	 * Create a SELECT_LIST
	 */
	SelectList() {
		super(Operator.SELECT_LIST, null, null);
		return;
	}


	@Override
	void evaluate(List<String> headers, List<StructureValue> instances) {
		if(debug) System.out.println(toString()+"(instances) - Enter");
		// handle special case of "select * from class_name where ..."
		// In this case, we simply return the actual instances, and do not construct
		// the result set
		if(headers.size() == 1 && getChildren().size() == 1 && template == null){
			Node child = getChildren().get(0);
			if(Operator.PROPERTY_NAME.equals(child.getOperator()) && "*".equals(child.getName())){
				StructureValue val = instances.get(0);
				if(val.getElementType() == ElementType.STRUCTUREVALUE)
					setValue(new DataValue(DataType.STRUCTUREVALUE,val));
				else
					setValue(new DataValue(DataType.INSTANCEVALUE,(CimInstance)val));	
				return;
			}
		}

		// construct the result set instance to return
		properties.clear();
		// locate all property values
		for(int i = 0; i < headers.size(); i++){
			String header = headers.get(i);
			StructureValue inst = instances.get(i);
			if(inst == null) continue;
			for(String p : template.getLowerCasePropertyNames()){
				DataValue q = template.getPropertyQualifierValue(p, "ModelCorrespondence");
				if(q == null) continue;
				String [] v = (String[]) q.getValue();
				if(!header.equals(v[0])) continue;
				ObjectPath pc = new ObjectPath(v[1]);
				if(!inst.isInstanceOf(pc.getName())) continue;
				if(inst.hasNonNullProperty(v[2])) properties.put(p,inst.getPropertyValue(v[2]));
			}
		}
		// locate any expressions
		// System.out.println("SelectList.Evaluate Children");
		for(Node child : getChildren()){
			switch(child.getOperator()){
			case PROPERTY_NAME:
				break;	// handled above
			default:
				child.evaluate(headers, instances);
				// System.out.println("\tEvaluation Result " + child.toString());
				if(child.hasNonNullValue()){
					String aliasValue = child.getAlias();
					DataValue value = child.getValue();
					if(aliasValue != null && template.hasProperty(aliasValue)){
						properties.put(aliasValue, value);
					} else {
						throw new ModelException("SelectList.Evaluate - Could not find property for "+child.toString());
					}
				}
				break;
			}
		}

		// set the value of the SELECT_LIST as the constructed instance
		if(properties.isEmpty()){
			setValue(new DataValue(DataType.INSTANCEVALUE,null));
			if(debug) System.out.println(toString()+"(instances) - Exit (null)");
		} else {
			properties.put("rowId", new DataValue(rowID++));
			StructureValue row = StructureValue.createStructureValue(template, properties, null);
			setValue(new DataValue(row));
			if(debug) System.out.println(toString()+"(instances) - Exit " +row.toMOF());
		}
		return;
	}

	/**
	 * Reset the select list, and construct the output class
	 * @param cache - buffered cache for looking up class names
	 * @param headers - column headers
	 * @param alias - aliases in the query
	 * @param resultClassName - name of the result class from select statement alias [Result_Class by default]
	 */
	void reset(BufferedCache cache, Vector<String> headers, Alias alias, String resultClassName) {
		// if we only have "select * from class_name [where...]" then we do not need to create a result_class
		if(headers.size() == 1 && getChildren().size() == 1 && resultClassName == null){
			Node child = getChildren().get(0);
			if(Operator.PROPERTY_NAME.equals(child.getOperator()) && "*".equals(child.getName())){
				return;
			}
		}

		// collect referenced classes in classList {columnName,Vector<CimClass>}
		HashMap<String,Vector<CimStructure>> ccMap = new HashMap<String, Vector<CimStructure>>();
		for(String header : headers){
			String className = header;
			if(alias != null){
				Node aliasNode = alias.locateAliasNode(header);
				if(aliasNode != null && aliasNode.getName() != null) className = aliasNode.getName();
			}
			// System.out.println("SelectList.reset(): Found className "+className+" for header "+header);
			String classPath = null;	// classPath if any
			if(className.contains("/")){
				int i = className.lastIndexOf(":");
				classPath = className.substring(0, i);
				className = className.substring(i+1);
			}
			List<NamedElement> candidates = cache.getAllElements("structure", classPath, className, true);
			if(candidates.isEmpty())
				throw new ModelException(ExceptionReason.NOT_FOUND,"Could not locate definition for ["+classPath+"]"+className);
			Vector<CimStructure> ccv = new Vector<CimStructure>();
			for(NamedElement e : candidates){
				ccv.add((CimStructure)e);
			}
			ccMap.put(header, ccv);
			// System.out.println("SelectList.reset(): Candidate classes for header "+header+" are");
			if(debug) {
				for(CimStructure c : ccv){
					System.out.println(c.toMOF());
				}
			}
		}

		// initialize result class template, rowID, and name of result class
		template = null;
		rowID = 0;
		if(resultClassName == null) resultClassName = "Result_Class";
		// create the result class definition 
		StringBuilder b = new StringBuilder("class ").append(resultClassName)
				.append(" {\n\t[key] SINT32 rowId;\n");
		
		// scan all properties defined in selectLiast
		for(Node child : getChildren()){
			// System.out.println("SelectList.reset(): Check Child "+child.toString());
			boolean found = false;
			switch(child.getOperator()){
			case PROPERTY_NAME:	// select propertyName ...
				PropertyName n = (PropertyName) child;		// get the property definition
				String classProperty = n.getPropertyName();	// property name within the class
				String outputAlias = n.getAlias();			// output alias for the property, if any
				String className = n.getClassName();		// class name for the property, if any
				String columnName = null;					// input column where the property name matches
				if(outputAlias == null) outputAlias = classProperty;
				HashSet<String> seenProperty = new HashSet<String>();	// hashset to hold found propertyNames
				// scan the candidate class set
				for(Entry<String,Vector<CimStructure>> e : ccMap.entrySet()){
					columnName = e.getKey();
					Vector<CimStructure> ccv = e.getValue();
					seenProperty.clear();
					for(CimStructure c : ccv){
						if(className == null){
							// select ['*' | classProperty [as outputAlias]] from ...
							if("*".equals(classProperty)){
								// select '*' from ...
								for(String pName : c.getLowerCasePropertyNames()){
									if(!seenProperty.contains(pName)){
										addProperty(columnName,pName,pName,c,b);
										seenProperty.add(pName);
										if(!found) found = true;
									}
								}
							} else {
								// select classProperty [as outputAlias] from ....
								if(!c.hasProperty(classProperty)) continue;
								if(seenProperty.contains(classProperty.toLowerCase()))
									throw new ModelException(ExceptionReason.INVALID_QUERY,
											"SelectList.reset(): Name "+classProperty+"is multiply defined");
								addProperty(columnName,outputAlias,classProperty,c,b);
								seenProperty.add(classProperty.toLowerCase());
								if(!found) found = true;
							}
						} else {
							// select className.['*' | classProperty [as outputAlias]] from ...
							if(!className.equalsIgnoreCase(c.getName())) continue;
							if("*".equals(classProperty)){
								// select className.* from ...
								for(String pName : c.getLowerCasePropertyNames()){
									if(!seenProperty.contains(pName)){
										addProperty(columnName,pName,pName,c,b);
										seenProperty.add(pName);
										if(!found) found = true;
									}
								}
							} else {
								// select className.classProperty [AS outputAlias] from ...
								if(!c.hasProperty(classProperty)) throw new ModelException(ExceptionReason.INVALID_QUERY,
										"SelectList.reset(): "+className+" does not have property "+classProperty);
								addProperty(columnName,outputAlias,classProperty,c,b);
								seenProperty.add(classProperty.toLowerCase());
								if(!found) found = true;
							}
						}
					}
				}
				break;
			case FUNCTION:	// select function as pName ...
			default:		// select expression as pName ...
				String pName = child.getAlias();
				if(pName == null) throw new ModelException(child.getOperator()+" must have an alias in SelectList");
				DataType t = child.getType();
				if(t == null || DataType.VOID.equals(t)) throw new ModelException(child.getOperator()+" does not have a pre-defined data type");
				if(t.isPrimitive()){
					b.append("\t").append(t.toMOF()).append(" ");
					if(t.isArray()) b.append("[] ");
					b.append(pName).append(";\n");
					if(!found) found = true;
				} else if(Operator.FUNCTION.equals(child.getOperator()) && t.isReference()){
					// functions returning object path
					Function f = (Function) child;
					b.append("\t").append(f.getRefClass()).append(" ref ");
					if(t.isArray()) b.append("[] ");
					b.append(pName).append(";\n");
					if(!found) found = true;
				} else {
					// property contains an objectPath or a structure or enumname
					// TODO: Need to have a generic mechanism/CQL way of getting to the referenced struct or enum name
					b.append("\t").append("** Structure or EnumName *** ");
					if(t.isArray()) b.append("[] ");
					b.append(pName).append(";\n");
					throw new ModelException(ExceptionReason.NOT_SUPPORTED,toString()+" does not currently support complex valued property expressions");
				}
				break;
			}
			if(!found){
				throw new ModelException(ExceptionReason.INVALID_QUERY,"SelectList.reset() could not find property for "+child.toString());
			}
		}
		// complete definition, and parse
		b.append("};\n");
		if(debug) System.out.println("Constructed Result Class Definition:\n"+b.toString());
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(b.toString().getBytes()), Constants.defaultNameSpacePath);
		// save the template
		template = (CimClass) cache.get(new ObjectPath(ElementType.CLASS,resultClassName,Constants.defaultNameSpacePath,null, null));
		// if(debug){
		if(debug && template != null) System.out.println("OutputClass:\n"+template.toMOF());
		//	System.out.println(toString()+" exit reset");
		// }
		return;
	}
	
	/**
	 * Add a property to the definition buffer
	 * @param columnName - name of the column
	 * @param outputPropertyName - name of the property in the definition buffer
	 * @param classPropertyName - name of the property in the defining class
	 * @param cc - defining class
	 * @param b - definition buffer
	 * @return true if the property was added, false otherwise
	 */
	private boolean addProperty(String columnName, String outputPropertyName, String classPropertyName, CimStructure cc, StringBuilder b){
		// System.out.println("SelectList.addProperty(Col: "+columnName+", outProp: "+outputPropertyName+", classProp: "+classPropertyName+")");
		if(cc.hasProperty(classPropertyName)){
			// ModelCorrespondence contains columnName, ObjectPath(cc), cc.propertyName, to enable value mapping
			b.append("\t[ModelCorrespondence{\"").append(columnName).append("\",\"")
			.append(cc.getObjectPath().toString()).append("\",\"").append(classPropertyName).append("\"}]\n");
			DataType t = cc.getPropertyType(classPropertyName);
			if(t.isPrimitive()){
				b.append("\t").append(t.toMOF()).append(" ");
				if(t.isArray()) b.append("[] ");
			} else if(t.isReference()){
				b.append("\t").append(cc.getReferencedClass(classPropertyName)).append(" ").append(t.toMOF()).append(" ");
				if(t.isArray()) b.append("[] ");
			} else if(t.isEnumerationValue()){
				b.append("\t").append(cc.getReferencedEnum(classPropertyName).getName()).append(" ");
				if(t.isArray()) b.append("[] ");
			} else if(t.isStructureValue()){
				b.append("\t").append(cc.getReferencedStructure(classPropertyName).getName()).append(" ");
				if(t.isArray()) b.append("[] ");
			} else {
				throw new ModelException("Type "+t+" is not yet implemented in SelectClass#addProperty()");
			}
			b.append(outputPropertyName != null ? outputPropertyName : classPropertyName).append(";\n");
			return true;
		}
		return false;
	}

}
