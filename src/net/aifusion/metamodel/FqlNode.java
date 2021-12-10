/**
 * Copyright 2021 Hewlett Packard Enterprise Development LP
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
 * Created Dec 4, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Array;
import java.util.List;
import java.util.Vector;

/**
 * Class to represent a FQL node
 * @author Sharad Singhal
 */
class FqlNode {
	/** Name of the node, if any */
	private String name;
	/** Operator at this node (mandatory) */
	private FqlOperator operator;
	/** Children of this node, if any */
	private Vector<FqlNode> children = new Vector<FqlNode>();
	/** Value of this node */
	private DataValue value = null;
	/** Debugging flag */
	boolean debug = true;
	/** True value */
	static final DataValue TrueValue = new DataValue(Boolean.valueOf(true));
	/** False value */
	static final DataValue FalseValue = new DataValue(Boolean.valueOf(false));
	/** Void value */
	static final DataValue VoidValue = new DataValue(DataType.VOID,null);
	/** Boolean NULL value */
	static final DataValue BooleanNullValue = new DataValue(DataType.BOOLEAN,null);

	/**
	 * Create a parse tree node with the given operator
	 * @param operator - operator to use
	 */
	FqlNode(FqlOperator operator) {
		if(operator == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Operator cannot be null");
		this.operator = operator;
		return;
	}

	/**
	 * Create a parse tree node with the given operator and value
	 * @param operator - operator to use
	 * @param value - node value to use
	 */
	FqlNode(FqlOperator operator, DataValue value) {
		this(operator);
		this.value = value;
		return;
	}

	/**
	 * Create a parse tree node with given operator, node name, and value
	 * @param operator - operator at this node
	 * @param name - name of the node
	 * @param value - value at this node
	 */
	FqlNode(FqlOperator operator, String name, DataValue value) {
		this(operator,value);
		this.name = name;
		return;
	}
	/**
	 * Get the operator at this node
	 * @return - FqlOperator at this node
	 */
	FqlOperator getOperator() {
		return operator;
	}

	/**
	 * Get the value of this node
	 * @return - value of the node
	 */
	DataValue getValue() {
		return value;
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
	 * Get the name at this node
	 * @return - name of the node
	 */
	String getName() {
		return name;
	}

	/**
	 * Add a child at this node
	 * @param n - child to add
	 */
	void addChild(FqlNode n) {
		children.add(n);
		return;
	}

	/**
	 * Check if the node has children
	 * @return true if the node has children, false otherwise
	 */
	boolean hasChildren() {
		return !children.isEmpty();
	}

	/**
	 * Get the children of this node
	 * @return - list of children at the node
	 */
	List<FqlNode> getChildren() {
		return children;
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

	/**
	 * Get the data type for the value in this node. If the node does not have a value, VOID is returned
	 * @return - data type for the value stored at this node
	 */
	DataType getType(){
		return value != null ? value.getType() : DataType.VOID;
	}

	/**
	 * Set the value of a delayed constant at this node
	 * @param name - name of the delayed constant
	 * @param value - value of the constant
	 */
	void setVariable(String name, DataValue value) {
		for(FqlNode c : children) {
			c.setVariable(name, value);
		}
		if(operator == FqlOperator.VARIABLE && this.name.equals(name)) {
			this.value = value;
		}
		return;
	}

	/**
	 * Evaluate the filter at the subtree rooted at this node
	 * @param sv - structure value to test
	 * @param repository - repository to use to fetch other references
	 */
	void evaluate(StructureValue sv, Repository repository) {
		for(FqlNode c : children) {
			c.evaluate(sv,repository);
		}
		switch(operator) {
		case VARIABLE:
		case CONSTANT:
			break;
		case ADD:
		case SUBTRACT:
		case DIVIDE:
		case MULTIPLY:
			arith();
			break;
		case CONCAT:
			StringBuilder b = new StringBuilder();
			for(FqlNode child : getChildren()){
				b.append(child.getStringValue());
			}
			setValue(new DataValue(b.toString()));
			break;
		case SIGN:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only 1 child expected, found "+children.size());
			DataType leftType = children.get(0).getType();
			if(leftType.isArray()){
				throw new ModelException(ExceptionReason.INVALID_QUERY, toString()+": Array arithmetic not supported");
			}
			int sign = getName() == "-" ? -1 : 1;
			if(leftType.isNumeric()){
				if(leftType.isReal()){
					Double dv = children.get(0).getDoubleValue();
					setValue(dv == null ? new DataValue(DataType.REAL64,null) : new DataValue(DataType.REAL64, sign * dv));
				} else {
					Long lv = children.get(0).getLongValue();
					setValue(lv == null ? new DataValue(DataType.SINT64,null) : new DataValue(DataType.SINT64, sign * lv));
				}
			} else {
				throw new ModelException(ExceptionReason.INVALID_QUERY,"Only numeric values permit sign. Found "+children.get(0).toString());
			}
			break;
		case LIKE:
		case NOTLIKE:
			if(children.size() != 2) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only 2 children expected, found "+children.size());
			String lv = children.get(0).getStringValue();
			String rv = children.get(1).getStringValue();
			if(lv != null && rv != null){
				if(operator == FqlOperator.LIKE)
					setValue(new DataValue(DataType.BOOLEAN,lv.matches(rv)));
				else setValue(new DataValue(DataType.BOOLEAN,!lv.matches(rv)));
			} else {
				setValue(new DataValue(DataType.BOOLEAN,null));
			}
			break;
		case ISNOTNULL:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,"IS NOT NULL requires 1 child, found "+children.size());
			setValue(children.get(0).hasNonNullValue() ? TrueValue : FalseValue);
			break;
		case ISNULL:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,"ISNULL requires 1 child, found "+children.size());
			setValue(children.get(0).hasNullValue() ? TrueValue : FalseValue);
			break;
		case EVERY:
		case AND:
		case OR:
		case ANY:
			Boolean currentValue = (operator == FqlOperator.AND || operator == FqlOperator.EVERY) ? Boolean.TRUE : Boolean.FALSE;
			for(FqlNode c : getChildren()){
				if(c.getType().isArray()){
					Boolean [] childValues = c.getBooleanArrayValue();
					if(childValues != null){
						for(int i = 0; i < childValues.length; i++){
							currentValue = getValue(currentValue, childValues[i]);
						}
					} else {
						currentValue = getValue(currentValue,null);
					}
				} else {
					currentValue = getValue(currentValue, c.getBooleanValue());
				}
			}
			setValue(new DataValue(DataType.BOOLEAN,currentValue));
			break;
		case EQ:
		case GE:
		case GT:
		case LE:	
		case LT:
		case NE:
			if(children.size() != 2) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only 2 children expected, found "+children.size());
			leftType = children.get(0).getType();
			DataType rightType = children.get(1).getType();
			DataValue leftValue = children.get(0).getValue();
			DataValue rightValue = children.get(1).getValue();
			if(debug) System.out.println("\tCompare "+leftValue+" "+getOperator()+" "+rightValue);
			if(leftValue == null || rightValue == null || leftValue.getValue() == null || rightValue.getValue() == null){
				setValue(BooleanNullValue);
			} else if(leftType.isArray() && rightType.isArray()){
				// array COMP array
				Object [] leftValues = (Object [] ) children.get(0).getValue().getValue();
				Object [] rightValues = (Object [] ) children.get(1).getValue().getValue();
				if(leftValues.length != rightValues.length){
					setValue(BooleanNullValue);
				} else {
					leftType = leftType.getComponentType();
					rightType = rightType.getComponentType();
					Boolean [] result = new Boolean[leftValues.length];
					for(int i = 0; i < leftValues.length; i++){
						result[i] = compare(leftType,leftValues[i],rightType,rightValues[i]);
					}
					setValue(new DataValue(DataType.BOOLEAN_ARRAY,result));
				}
			} else if(leftType.isArray()){
				// array COMP scalar
				Object [] leftValues = (Object [] ) children.get(0).getValue().getValue();
				Object rightValues = children.get(1).getValue().getValue();
				leftType = leftType.getComponentType();
				Boolean [] result = new Boolean[leftValues.length];
				for(int i = 0; i < leftValues.length; i++){
					result[i] = compare(leftType,leftValues[i],rightType,rightValues);
				}
				setValue(new DataValue(DataType.BOOLEAN_ARRAY,result));
			} else if(rightType.isArray()){
				// scalar COMP array
				Object leftValues = children.get(0).getValue().getValue();
				Object [] rightValues = (Object [] ) children.get(1).getValue().getValue();
				rightType = rightType.getComponentType();
				Boolean [] result = new Boolean[rightValues.length];
				for(int i = 0; i < rightValues.length; i++){
					result[i] = compare(leftType,leftValues,rightType,rightValues[i]);
				}
				setValue(new DataValue(DataType.BOOLEAN_ARRAY,result));
			} else {
				// scalar COMP scalar
				setValue(new DataValue(DataType.BOOLEAN,compare(leftType,leftValue.getValue(),rightType,rightValue.getValue())));
			}
			break;
		case NOT:
			// NOT has one Boolean-valued child, and the value of this node is the negation of that node
			Boolean bv = getChildren().get(0).getBooleanValue();
			setValue( (bv == null) ? new DataValue(DataType.BOOLEAN,null) : new DataValue(DataType.BOOLEAN,!bv));
			break;
		case RANGE:
			if(children.size() != 2) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+"must have two children defined");
			Integer [] bounds = new Integer[2];
			bounds[0] = children.get(0).getIntegerValue();
			if(bounds[0] == null){
				if(!children.get(0).getStringValue().equals("*")) 
					throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+"Expected * found "+children.get(0).toString());
				bounds[0] = 0;
			}
			bounds[1] = children.get(1).getIntegerValue();
			if(bounds[1] == null){
				if(!children.get(1).getStringValue().equals("*")) 
					throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+"Expected * found "+children.get(1).toString());
			} else {
				if(bounds[1] < bounds[0]) throw new ModelException(ExceptionReason.INVALID_QUERY,
						toString()+": Upper bound < lower bound ["+bounds[0]+","+bounds[1]+"]");
			}
			setValue(new DataValue(DataType.SINT32_ARRAY,bounds));
			break;
		case IDENTIFIER:
			if(name.contains(".")) {
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Property traversal not yet supported");
			}
			if(sv.hasProperty(name)) {
				DataValue pv = sv.getPropertyValue(name);
				setValue(pv != null ? pv : new DataValue(sv.getPropertyType(name),null));
				// handle property[index]
				if(hasChildren()){
					for(FqlNode child : getChildren()){
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
			} else {
				setValue(VoidValue);
			}
			break;
		case INDEX:
			Vector<Integer> values = new Vector<Integer>();
			for(FqlNode c : children){
				switch(c.getOperator()){
				case RANGE:	// Expect an array of length 2, the upper bound may be null indicating max value
					bounds = (Integer []) c.getValue().getValue();
					if(bounds[0] < 0 || (bounds[1] != null && bounds[1] < bounds[0])) 
						throw new ModelException(ExceptionReason.INVALID_QUERY,c.toString()+": Range must positive integers [L,U] with U>=L");
					if(bounds[1] != null){
						for(int i = bounds[0]; i <= bounds[1]; i++){
							values.add(i);
						}
					} else {
						values.add(bounds[0]);
						values.add(null);
					}
					break;
				case CONSTANT:
					if("*".equals(c.getStringValue())){	// single '*' is treated as 0..*
						values.add(0);
						values.add(null);
					} else {
						Integer v = c.getIntegerValue();
						if(v == null || v < 0) throw new ModelException(ExceptionReason.INVALID_QUERY,c.toString()+": index must be a positive integer value");
						values.add(v);
					}
					break;
				default:
					throw new ModelException("Index does not yet handle children of type "+c.toString());
				}
			}
			Integer[] newValue = new Integer[values.size()];
			for(int i = 0; i < values.size(); i++){
				newValue[i] = values.get(i);
			}
			setValue(new DataValue(DataType.SINT32_ARRAY,newValue));
			break;
		case FUNCTION:
			function();
			break;
		case SATISFIES:
		case SCOPE:
		case ISA:
		case CLASS_PATH:	
		case ENUM:
		default:
			throw new ModelException(ExceptionReason.NOT_SUPPORTED,operator+" is not yet implemented");
		}
		return;
	}
	
	/**
	 * Evaluate defined functions
	 */
	private void function() {
		int size = children.size();
		String n = getName().toLowerCase();
		if("datetime".equals(n)) {
			if(size != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+size);
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(DataType.DATETIME,new DateTime(children.get(0).getStringValue())));
		} else if("objectpath".equals(n)) {
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(DataType.OBJECTPATH,new ObjectPath(children.get(0).getStringValue())));
		} else {
			throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Function "+getName()+" is not supported");
		}
	}

	/**
	 * Evaluate arithmetic functions
	 */
	private void arith() {
		// Arithmetic includes +, -, *, /
		if(debug) System.out.println(toString()+"() - Enter");
		List<FqlNode> children = getChildren();
		// arithmetic requires two children
		if(children.size() != 2)throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only 2 children expected, found "+children.size());
		DataType leftType = children.get(0).getType();
		DataType rightType = children.get(1).getType();

		// array arithmetic currently not supported
		if(leftType.isArray() || rightType.isArray()){
			throw new ModelException(ExceptionReason.INVALID_QUERY, toString()+": Array arithmetic not supported");
		}
		// two numeric types
		if(leftType.isNumeric() && rightType.isNumeric()){
			// two numeric types (REAL or INTEGER)
			if(leftType.isReal() || rightType.isReal()){
				// either type is real, do real-valued arithmetic
				Double lv = children.get(0).getDoubleValue();
				Double rv = children.get(1).getDoubleValue();
				if(lv == null || rv == null){
					setValue(new DataValue(DataType.REAL64, null));
				} else {
					switch(getOperator()){
					case ADD:
						setValue(new DataValue(DataType.REAL64,lv+rv));
						break;
					case SUBTRACT:
						setValue(new DataValue(DataType.REAL64,lv-rv));
						break;
					case MULTIPLY:
						setValue(new DataValue(DataType.REAL64,lv*rv));
						break;
					case DIVIDE:
						setValue(new DataValue(DataType.REAL64,lv/rv));
						break;
					default:
						throw new ModelException("Internal error -- expected arithmetic operator, found "+getOperator());
					}
				}
			} else if(leftType.isInteger() && rightType.isInteger()){
				// both are integers, do integer-valued arithmetic
				Long lv = children.get(0).getLongValue();
				Long rv = children.get(1).getLongValue();
				if(lv == null || rv == null){
					setValue(new DataValue(DataType.SINT64,null));
				} else {
					switch(getOperator()){
					case ADD:
						setValue(new DataValue(DataType.SINT64,lv+rv));
						break;
					case SUBTRACT:
						setValue(new DataValue(DataType.SINT64,lv-rv));
						break;
					case MULTIPLY:
						setValue(new DataValue(DataType.SINT64,lv*rv));
						break;
					case DIVIDE:
						setValue(new DataValue(DataType.SINT64,lv/rv));
						break;
					default:
						throw new ModelException("Internal error -- expected arithmetic operator, found "+getOperator());
					}
				}
			}
		} else if(leftType.isDateTime() && rightType.isDateTime()){
			// two datetime values-- these support addition and subtraction
			DateTime lv = (DateTime) children.get(0).getValue().getValue();
			DateTime rv = (DateTime) children.get(1).getValue().getValue();
			if(lv == null || rv == null){
				setValue(new DataValue(DataType.DATETIME,null));
			} else {
				switch(getOperator()){
				case ADD:
					setValue(new DataValue(DataType.SINT64,lv.add(rv)));
					break;
				case SUBTRACT:
					setValue(new DataValue(DataType.SINT64,lv.subtract(rv)));
					break;
				default:
					throw new ModelException(ExceptionReason.INVALID_QUERY,"DateTime values do not support "+getOperator());
				}
			}
		} else if(leftType.isDateTime() && rightType.isNumeric() || leftType.isNumeric() && rightType.isDateTime()){
			// a datetime value and a numeric-- support multiplication and division
			switch(leftType){
			case DATETIME:	// Interval ['*' | '/'] factor
				DateTime dt = (DateTime) children.get(0).getValue().getValue();
				if(!dt.isInterval()) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": multiplication/division requires interval arguments");
				Double factor = children.get(1).getDoubleValue();
				if(factor == null){
					setValue(new DataValue(DataType.DATETIME,null));
				} else {
					switch(getOperator()){
					case MULTIPLY:
						setValue(new DataValue(dt.multiply(factor)));
						break;
					case DIVIDE:
						setValue(new DataValue(dt.divide(factor)));
						break;
					default:
						throw new ModelException(ExceptionReason.INVALID_QUERY,"DateTime values do not support "+getOperator());
					}
				}
				break;
			default:	// factor '*' Interval
				dt = (DateTime) children.get(1).getValue().getValue();
				if(!dt.isInterval()) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": multiplication requires interval arguments");
				if(getOperator() != FqlOperator.MULTIPLY)
					throw new ModelException(ExceptionReason.INVALID_QUERY,"DateTime does not support "+getOperator()+" by a factor");
				factor = children.get(0).getDoubleValue();
				setValue(factor == null ? new DataValue(DataType.DATETIME,null) : new DataValue(dt.multiply(factor)));
				break;
			}
		} else {
			throw new ModelException(ExceptionReason.INVALID_QUERY,"Only numeric or DateTime values permit arithmetic operators");
		}
		if(debug) System.out.println(toString()+"() - Exit = "+getValue());

	}

	/**
	 * Get the logical operation value based on current value and new child value
	 * @param op - logical operator (AND/OR/ANY/EVERY)
	 * @param currentValue - current expression value
	 * @param childValue - new value received from child
	 * @return - new current value
	 */
	private Boolean getValue(Boolean currentValue, Boolean childValue){
		switch(operator){
		case AND:
		case EVERY:
			if(currentValue != null && childValue != null){
				// T AND T := T; T AND F := F; F AND F := F;
				currentValue = currentValue & childValue;
			} else if(currentValue != null && childValue == null){
				// T AND NULL := NULL; F AND NULL := F;
				currentValue = currentValue ? null : Boolean.FALSE;
			} else if(currentValue == null && childValue != null){
				// NULL AND T := NULL; NULL AND F := F;
				currentValue = childValue ? null : Boolean.FALSE;
			} else {
				// NULL AND NULL := NULL;
				currentValue = null;
			}
			break;
		case OR:
		case ANY:
			if(currentValue != null && childValue != null){
				// T OR T := T; T OR F := T; F OR F := F;
				currentValue = currentValue  | childValue;
			} else if(currentValue != null && childValue == null){
				// T OR NULL := T; F OR NULL := NULL;
				currentValue = currentValue ? Boolean.TRUE : null;
			} else if(currentValue == null && childValue != null){
				// NULL OR T := T; NULL OR F := NULL;
				currentValue = childValue ? Boolean.TRUE : null;
			} else {
				// NULL OR NULL := NULL;
				currentValue = null;
			}
			break;
		default:
			throw new ModelException("Internal error - "+getOperator()+" is not implemented in Logical operators");
		}
		return currentValue;

	}

	/**
	 * Compare two values and return the result of their comparison
	 * @param leftType - data type for left value
	 * @param leftValue - left value
	 * @param rightType - data type for right value
	 * @param rightValue - right value
	 * @return
	 */
	private Boolean compare(DataType leftType, Object leftValue, DataType rightType, Object rightValue){
		if(leftValue != null && rightValue != null){
			// neither value is null, compare
			if(leftType.isNumeric() && rightType.isNumeric()){
				if(leftType.isInteger() && rightType.isInteger()){
					// do an integer compare
					Long li = getLongValue(leftType,leftValue);
					Long ri = getLongValue(rightType,rightValue);
					int comp = li.compareTo(ri);
					switch(operator){
					case EQ:
						return li.equals(ri);
					case NE:
						return !li.equals(ri);
					case LT:
						return comp < 0;
					case LE:
						return comp <= 0;
					case GT:
						return comp > 0;
					case GE:
						return comp >= 0;
					default:
						throw new ModelException(toString()+" does not support operator "+getOperator());
					}

				} else {
					// do a double compare
					Double lr = getDoubleValue(leftType,leftValue);
					Double rr = getDoubleValue(rightType,rightValue);
					int comp = lr.compareTo(rr);
					switch(operator){
					case EQ:
						return lr.equals(rr);
					case NE:
						return !lr.equals(rr);
					case LT:
						return comp < 0;
					case LE:
						return comp <= 0;
					case GT:
						return comp > 0;
					case GE:
						return comp >= 0;
					default:
						throw new ModelException(toString()+" does not support operator "+getOperator());
					}
				}
			} else if(leftType == DataType.STRING && rightType == DataType.STRING){
				String ls = leftValue.toString();
				String rs = rightValue.toString();
				int comp = ls.compareTo(rs);
				switch(operator){
				case EQ:
					return comp == 0;
				case NE:
					return comp != 0;
				case LT:
					return comp < 0;
				case LE:
					return comp <= 0;
				case GT:
					return comp > 0;
				case GE:
					return comp >= 0;
				default:
					throw new ModelException(toString()+" does not support operator "+getOperator());
				}
			} else if(leftType == DataType.DATETIME && rightType == DataType.DATETIME) {
				DateTime ld = (DateTime) leftValue;
				DateTime rd = (DateTime) rightValue;
				if(debug) System.out.println(ld.toString()+" "+rd.toString());
				Integer comp = ld.compareTo(rd);
				switch(operator){
				case EQ:
					return ld.equals(rd);
				case NE:
					return !ld.equals(rd);
				case LT:
					return comp < 0;
				case LE:
					return comp <= 0;
				case GT:
					return comp > 0;
				case GE:
					return comp >= 0;
				default:
					throw new ModelException(toString()+" does not support operator "+getOperator());
				}
			} else if(leftType == DataType.BOOLEAN && rightType == DataType.BOOLEAN){
				Boolean lb = (Boolean) leftValue;
				Boolean rb = (Boolean) rightValue;
				switch(operator){
				case EQ:
					return lb.equals(rb);
				case NE:
					return !lb.equals(rb);
				default:
					throw new ModelException(toString()+" does not support operator "+getOperator());
				}
			} else if(leftType == DataType.OBJECTPATH && rightType == DataType.OBJECTPATH) {
				ObjectPath lp = (ObjectPath) leftValue;
				ObjectPath rp = (ObjectPath) rightValue;
				switch(operator) {
				case EQ:
					return lp.equals(rp);
				default:
					throw new ModelException(toString()+" does not support operator "+getOperator());
				}
			}
		}
		// we cannot determine the value
		return null;
	}
	
	/**
	 * set value(s) for the given index node
	 * @param child - index node for this identifier
	 */
	private void setValueForIndex(FqlNode child) {
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
		return b.toString();
	}
}
