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

import java.util.List;

/**
 * Class to manage a Sign node. Takes one child, and applies the given sign to it.
 * @author Sharad Singhal
 */
class FQLSign extends FQLNode {

	/**
	 * Create a Sign node. accepts a single numerical child and has numerical (Real64 or Sint64) value
	 * @param name 
	 */
	FQLSign(String name) {
		super(FQLOperator.SIGN,name, null);
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		List<FQLNode> children = getChildren();
		if(children.size() != 1)throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only 1 child expected, found "+children.size());
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
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
		return;
	}
}
