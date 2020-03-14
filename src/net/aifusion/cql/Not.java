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

/**
 * Class to manage NOT
 * @author Sharad Singhal
 */
class Not extends Node {
	/**
	 * Create NOT node
	 */
	Not() {
		super(Operator.NOT, null,null);
		return;
	}
	
	@Override
	void evaluate(){
		if(debug) System.out.println(toString()+"() - Enter");
		// NOT has one Boolean-valued child, and the value of this node is the negation of that node
		Boolean bv = getChildren().get(0).getBooleanValue();
		setValue( (bv == null) ? new DataValue(DataType.BOOLEAN,null) : new DataValue(DataType.BOOLEAN,!bv));
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
		return;
	}
	
	@Override
	DataType getType() {
		DataType dt = super.getType();
		return DataType.VOID.equals(dt) ? DataType.BOOLEAN : dt;
	}
}
