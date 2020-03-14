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

import java.util.List;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.DateTime;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage Addition, Subtraction, Multiplication, and Division
 * @author Sharad Singhal
 */
class Arithmetic extends Node {

	/**
	 * Create an Arithmetic Node. Handles addition, subtraction, multiplication, and division
	 */
	Arithmetic(Operator arith) {
		super(arith,null, null);
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		List<Node> children = getChildren();
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
				if(getOperator() != Operator.MULTIPLY)
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
}
