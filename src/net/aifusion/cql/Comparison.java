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
 * Created Oct 19, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.List;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.DateTime;
import net.aifusion.metamodel.EnumerationValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.ObjectPath;

/**
 * Class to manage arithmetic comparisons. Takes two children, and does a value comparison. Has a Boolean[Array] value
 * @author Sharad Singhal
 */
class Comparison extends Node {
	private static final DataValue NullValue = new DataValue(DataType.BOOLEAN,null);
	
	/**
	 * Create a comparison Boolean := Arith {=,>,>=,<,<=,<>} Arith
	 * @param operator - comparison operator.
	 */
	Comparison(Operator operator){
		super(operator,null,null);
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		List<Node> children = getChildren();
		if(children.size() != 2)throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only 2 children expected, found "+children.size());
		DataType leftType = children.get(0).getType();
		DataType rightType = children.get(1).getType();
		DataValue leftValue = children.get(0).getValue();
		DataValue rightValue = children.get(1).getValue();
		if(debug) System.out.println("\tCompare "+leftValue+" "+getOperator()+" "+rightValue);
		if(leftValue == null || rightValue == null || leftValue.getValue() == null || rightValue.getValue() == null){
				setValue(NullValue);
		} else if(leftType.isArray() && rightType.isArray()){
			// array COMP array
			Object [] leftValues = (Object [] ) children.get(0).getValue().getValue();
			Object [] rightValues = (Object [] ) children.get(1).getValue().getValue();
			if(leftValues.length != rightValues.length){
				setValue(NullValue);
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
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
		return;
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
					switch(getOperator()){
					case EQUALS:
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
					switch(getOperator()){
					case EQUALS:
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
				switch(getOperator()){
				case EQUALS:
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
				switch(getOperator()){
				case EQUALS:
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
				switch(getOperator()){
				case EQUALS:
					return lb.equals(rb);
				case NE:
					return !lb.equals(rb);
				default:
					throw new ModelException(toString()+" does not support operator "+getOperator());
				}
			} else if(leftType == DataType.OBJECTPATH && rightType == DataType.OBJECTPATH) {
				ObjectPath lp = (ObjectPath) leftValue;
				ObjectPath rp = (ObjectPath) rightValue;
				switch(getOperator()) {
				case EQUALS:
					return lp.equals(rp);
				default:
					throw new ModelException(toString()+" does not support operator "+getOperator());
				}
			} else if(leftType == DataType.ENUMERATIONVALUE || rightType == DataType.ENUMERATIONVALUE) {
				return enumCompare(leftType, leftValue, rightType, rightValue);
			}
		}
		// we cannot determine the value
		return null;
	}

	/**
	 * Compare enum types for equality
	 * @param leftType - left child type
	 * @param leftValue - value of left child
	 * @param rightType - right child type
	 * @param rightValue - value of right child
	 * @return - true if equals, false if not equals
	 */
	private Boolean enumCompare(DataType leftType, Object leftValue, DataType rightType, Object rightValue) {
		if(!(getOperator().equals(Operator.EQUALS) || getOperator().equals(Operator.NE))) {
			throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" Enumeration comparison does not support operator "+getOperator());
		}
		if(leftType == DataType.ENUMERATIONVALUE && rightType == DataType.ENUMERATIONVALUE) {
			return leftValue.equals(rightValue);	// enumValue comparison
		} else if(leftType == DataType.ENUMERATIONVALUE) {
			// enumValue comparison with string or integers, where left value is enumValue
			return ecomp((EnumerationValue) leftValue,rightValue);
		} else {
			// enumValue comparison with string or integers, where right value is enumValue
			return ecomp((EnumerationValue) rightValue,leftValue);
		}
	}
	/**
	 * Compare an enum value to an integer or string
	 * @param e - enum to compare
	 * @param v - value to compare (string or integer)
	 * @return - true if equals, false otherwise
	 */
	private Boolean ecomp(EnumerationValue e, Object v) {
		if(e.getDataType().isString()) {
			String ev = e.hasValue() ? e.getDataValue().toString() : e.getName();	// value if defined, else name of the enumvalue
			return ev.equals(v);
		} else if(e.getDataType().isInteger()) {
			// right value must be an integer type
			DataValue ev = e.getDataValue();
			return ev.getValue().equals(v);
		}
		return null;
	}

	@Override
	DataType getType() {
		DataType dt = super.getType();
		return DataType.VOID.equals(dt) ? DataType.BOOLEAN : dt;
	}
	
	
}
