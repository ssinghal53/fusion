/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Dec 18, 2015 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.DateTime;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.metamodel.UInt16;
import net.aifusion.metamodel.UInt32;
import net.aifusion.metamodel.UInt64;
import net.aifusion.metamodel.UInt8;

/**
 * Class to represent a node in the CQL Parsed Tree
 * @author Sharad Singhal
 */
class Node {
	/** Children of this node, if any */
	private Vector<Node> children = new Vector<Node>();
	/** Name for this node, if any */
	private String name = null;
	/** Operator for this node (required) */
	private Operator operator = null;
	/** Value for this node, if any */
	private DataValue value = null;
	/** Alias for this node, if any */
	private String alias = null;
	/** True value */
	static final DataValue TrueValue = new DataValue(Boolean.valueOf(true));
	/** False value */
	static final DataValue FalseValue = new DataValue(Boolean.valueOf(false));
	/** Void value */
	static final DataValue VoidValue = new DataValue(DataType.VOID,null);
	/** Boolean NULL value */
	static final DataValue BooleanNullValue = new DataValue(DataType.BOOLEAN,null);
	/** debugging flag */
	final boolean debug = false;

	/**
	 * Create a node with a given operator, name, and value
	 * @param operator - operator to be used at this node
	 * @param name - name of the node (null if no name defined)
	 * @param value - value of the node (null if no value defined)
	 */
	Node(Operator operator, String name, DataValue value){
		if(operator == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Non-null Operator is required when instantiating node");
		this.operator = operator;
		this.name = name;
		this.value = value;
		return;
	}

	/**
	 * Add a child to this node
	 * @param child - node to be added
	 */
	void addChild(Node child){
		children.add(child);
		return;
	}

	/**
	 * Test if this node has any children
	 * @return - true if this node has children, false otherwise
	 */
	boolean hasChildren(){
		return !children.isEmpty();
	}

	/**
	 * Get the children of this node
	 * @return - list containing children. Empty if this node does not have children
	 */
	List<Node> getChildren(){
		return children;
	}

	/**
	 * Get the value of this node, if any
	 * @return - data value. Null if none defined
	 */
	DataValue getValue(){
		return value;
	}
	
	/**
	 * Check if this node has a non-null value.
	 * @return - true if this node has a data value with non-null content
	 */
	boolean hasNonNullValue(){
		return value != null && value.getValue() != null;
	}
	
	/**
	 * Check if this node has a null value
	 * @return - true if this node has a null data value, or a data value with null content
	 */
	boolean hasNullValue(){
		return value == null || value.getValue() == null;
	}
	
	/**
	 * Get the value of this node as a long value
	 * @return - value of the node as a Long. Null is returned if the node does not have a numerical value
	 */
	Long getLongValue(){
		if(value == null || value.getValue() == null) return null;
		return getLongValue(value.getType(),value.getValue());
	}
	
	/**
	 * Get the value of this node as an integer value
	 * @return - value of the node as an integer. Null is returned if the node does not have a numerical value
	 */
	Integer getIntegerValue(){
		if(value == null || value.getValue() == null) return null;
		return getIntegerValue(value.getType(),value.getValue());
	}
	
	/**
	 * Get the value at this node as a Boolean value
	 * @return - Boolean value. Null is returned if the value is not a Boolean
	 */
	Boolean getBooleanValue(){
		if(value == null || value.getValue() == null || value.getType() != DataType.BOOLEAN) return null;
		return (Boolean) value.getValue();
	}
	
	/**
	 * Get the value at this node as a Boolean array
	 * @return - Boolean Array value. Null is returned if the value is not a Boolean Array
	 */
	Boolean [] getBooleanArrayValue(){
		if(value == null || value.getValue() == null || value.getType() != DataType.BOOLEAN_ARRAY) return null;
		return (Boolean []) value.getValue();
	}
	
	/**
	 * Get the value of this node as a double value
	 * @return - value of this node. Null is returned if the value is not numerical
	 */
	Double getDoubleValue(){
		if(value == null || value.getValue() == null) return null;
		return getDoubleValue(value.getType(), value.getValue());
	}
	
	/**
	 * Get the value at this node as a string value.
	 * @return - node value as a string. If node does not contain a value, returns null
	 */
	String getStringValue(){
		if(value != null && value.getValue() != null) return value.getValue().toString();
		// if(name != null) return name;
		return null;
	}

	/**
	 * Get the data type for the value in this node. If the node does not have a value, VOID is returned
	 * @return - data type for the value stored at this node
	 */
	DataType getType(){
		return value != null ? value.getType() : DataType.VOID;
	}

	/**
	 * Get the operator at this node
	 * @return - value of operator
	 */
	Operator getOperator(){
		return operator;
	}

	/**
	 * Get the name of this node, if any	
	 * @return - name of the node. Null if none defined
	 */
	String getName(){
		return name;
	}
	
	/**
	 * Set a variable (delayed constant) within the subtree starting at this node
	 * @param name - name of the variable to set
	 * @param value - value of the variable to set
	 */
	void setVariable(String name, DataValue value){
		// recurse down to set the variable nodes in our children. Variable Nodes will over-ride this method
		for(Node n : children){
			n.setVariable(name,value);
		}
		return;
	}
	
	/**
	 * Set the value for this node
	 * @param value - value for the node
	 */
	void setValue(DataValue value){
		if(value == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": value must be a typed DataValue");
		this.value = value;
		return;
	}

	/**
	 * Set the name of this node (if not already set)
	 * @param alias - name to set on this node
	 * @throws ModelException if alias has already been set
	 */
	void setAlias(String alias){
		if(this.alias != null && !this.alias.equalsIgnoreCase(alias))
			throw new ModelException(ExceptionReason.INVALID_PARAMETER, "Node#setAlias "+this.name+" cannot re-set alias "+this.alias+" to "+alias);
		this.alias = alias;
		return;
	}

	/**
	 * Get the alias for this node, if any
	 * @return - alias for this node. Null if none defined
	 */
	String getAlias(){
		return alias;
	}
	
	/**
	 * Resolve aliases within this subtree
	 * @param aliases - aliases defined for this subtree
	 */
	void resolve(Alias aliases){
		for(Node child : getChildren()){
			child.resolve(aliases);
		}
		return;
	}

	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append(operator);
		if(name != null){
			b.append(" (").append(name).append(")");
		}
		if(value != null){
			b.append("[").append(value.getType()).append(" ").append(value.toString()).append("]");
		}
		if(alias != null){
			b.append("{").append(alias).append("}");
		}
		return b.toString();
	}
	
	/*
	 * *********************************************************
	 * Evaluation methods - subclasses should override as needed
	 * *********************************************************
	 */
	
	/**
	 * Evaluate a node based on the contents of a repository. Used by Select Nodes
	 * @param cache - Buffered cache to use. The buffer will contain additional class definitions, if any, created during query evaluation
	 * @return - List containing query results from the evaluation
	 */
	List<StructureValue> evaluate(BufferedCache cache){
		if(debug) System.out.println(toString()+"(Repository) - Enter");
		List<StructureValue> results = new Vector<StructureValue>();
		for(Node child : getChildren()){
			results.addAll(child.evaluate(cache));
		}
		evaluate();
		if(debug) System.out.println(toString()+"(Repository) - Exit");
		return results;
	}
	
	/**
	 * Evaluate nodes based on a repository and working set. The working set is created/modified based on the evaluation. Used by class_list nodes
	 * @param repository - Buffered cache to use. The buffer will contain any additional classes created during evaluation 
	 * @param workingSet - current working set. Contains {column header, column}
	 */
	void evaluate(BufferedCache repository, HashMap<String, List<StructureValue>> workingSet){
		if(debug) System.out.println(toString()+"(Repository, workingSet) - Enter");
		for(Node child : getChildren()){
			child.evaluate(repository, workingSet);
		}
		evaluate();
		if(debug) System.out.println(toString()+"(Repository, workingSet) - Exit");
	}
	
	/**
	 * Evaluation of a join. Node value depends on the evaluation
	 * @param headers - list of headers for the instances
	 * @param instances - list of instances to use in the join
	 */
	void evaluate(List<String> headers, List<StructureValue> instances) {
		if(debug) System.out.println(toString()+"(instances) - Enter");
		for(Node child : getChildren()){
			child.evaluate(headers, instances);
		}
		evaluate();
		if(debug) System.out.println(toString()+"(instances) - Exit");
		return;
	}
	
	/**
	 * Evaluate a node based on an instance. Node value depends on the evaluation
	 * @param header - header associated with the instance
	 * @param instance - instance to test against this node
	 */
	void evaluate(String header, StructureValue instance) {
		if(debug) System.out.println(toString()+"(Instance) - Enter");
		for(Node child : getChildren()){
			child.evaluate(header, instance);
		}
		evaluate();
		if(debug) System.out.println(toString()+"(instance) - Exit");
		return;
	}

	/**
	 * Evaluate a node that does not depend on any instance value.
	 * Result of evaluation is saved in value
	 * @see #getValue()
	 */
	void evaluate(){
		if(debug) System.out.println(toString()+"() - Enter");
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,toString()+" does not implement Evaluate()");
	}
	
	/* *********************
	 * Static Helper Methods
	 * *********************
	 */
	
	/**
	 * Get the value of a typed object as a Long value
	 * @param type - data type
	 * @param v - value object
	 * @return - return a long value corresponding to the object. Null is returned if the type cannot be converted to long
	 */
	static Long getLongValue(DataType type, Object v){
		switch(type){
		case DATETIME:
			return ((DateTime) v).getLowerBound();
		case REAL32:
			return ((Float)v).longValue();
		case REAL64:
			return ((Double)v).longValue();
		case SINT16:
			return ((Short)v).longValue();
		case SINT32:
			return ((Integer)v).longValue();
		case SINT64:
			return ((Long)v);
		case UINT16:
			return ((UInt16)v).longValue();
		case UINT32:
			return ((UInt32)v).longValue();
		case UINT64:
			return ((UInt64)v).longValue();
		case SINT8:
			return ((Byte)v).longValue();
		case UINT8:
			return ((UInt8)v).longValue();
		default:
			return null;
		}		
	}
	
	/**
	 * Get the value of an object as an Integer value
	 * @param type - Data type for the object
	 * @param v - object to be converted
	 * @return - integer value. Null is returned if the type cannot be converted to integer
	 */
	static Integer getIntegerValue(DataType type, Object v){
		switch(type){
		case REAL32:
			return ((Float)v).intValue();
		case REAL64:
			return ((Double)v).intValue();
		case SINT16:
			return ((Short)v).intValue();
		case SINT32:
			return ((Integer)v);
		case SINT64:
			return ((Long)v).intValue();
		case UINT16:
			return ((UInt16)v).intValue();
		case UINT32:
			return ((UInt32)v).intValue();
		case UINT64:
			return ((UInt64)v).intValue();
		case SINT8:
			return ((Byte)v).intValue();
		case UINT8:
			return ((UInt8)v).intValue();
		default:
			return null;
		}		
	}
	
	/**
	 * Get the value of a numerical type in double
	 * @param type - data type
	 * @param v - object of the corresponding type
	 * @return - double value (or null)
	 */
	static Double getDoubleValue(DataType type, Object v){
		switch(type){
		case DATETIME:
			return (double) ((DateTime)v).getLowerBound();
		case REAL32:
			return ((Float)v).doubleValue();
		case REAL64:
			return ((Double)v);
		case SINT64:
			return ((Long)v).doubleValue();
		case SINT16:
			return ((Short)v).doubleValue();
		case SINT32:
			return ((Integer)v).doubleValue();
		case UINT16:
			return ((UInt16)v).doubleValue();
		case UINT32:
			return ((UInt32)v).doubleValue();
		case UINT64:
			return ((UInt64)v).doubleValue();
		case SINT8:
			return ((Byte)v).doubleValue();
		case UINT8:
			return ((UInt8)v).doubleValue();
		default:
			return null;
		}
	}
}
