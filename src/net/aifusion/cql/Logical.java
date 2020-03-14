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

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage Logical operators AND, OR, ANY, and EVERY
 * @author Sharad Singhal
 */
class Logical extends Node {

	/**
	 * Create a logical (AND | OR | ANY | EVERY) Node. Takes Boolean[Array]-valued children, and has Boolean value
	 */
	Logical(Operator logical) {
		super(logical,null, null);
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		Operator op = getOperator();
		Boolean currentValue = (op == Operator.AND || op == Operator.EVERY) ? Boolean.TRUE : Boolean.FALSE;
		for(Node c : getChildren()){
			if(c.getType().isArray()){
				Boolean [] childValues = c.getBooleanArrayValue();
				if(childValues != null){
					for(int i = 0; i < childValues.length; i++){
						currentValue = getValue(op, currentValue, childValues[i]);
					}
				} else {
					currentValue = getValue(op, currentValue,null);
				}
			} else {
				currentValue = getValue(op, currentValue, c.getBooleanValue());
			}
		}
		setValue(new DataValue(DataType.BOOLEAN,currentValue));
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
		return;
	}

	/**
	 * Get the logical operation value based on current value and new child value
	 * @param op - logical operator (AND/OR/ANY/EVERY)
	 * @param currentValue - current expression value
	 * @param childValue - new value received from child
	 * @return - new current value
	 */
	private Boolean getValue(Operator op, Boolean currentValue, Boolean childValue){
		switch(op){
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
	
	@Override
	DataType getType() {
		DataType dt = super.getType();
		return DataType.VOID.equals(dt) ? DataType.BOOLEAN : dt;
	}


}
