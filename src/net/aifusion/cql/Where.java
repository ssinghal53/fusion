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

import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to handle WHERE Node
 * @author Sharad Singhal
 *
 */
class Where extends Node {
	/**
	 * Create a WHERE node
	 */
	Where() {
		super(Operator.WHERE, null, null);
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		boolean result = true;
		for(Node child : getChildren()){
			switch(child.getType()){
			case BOOLEAN:	// check child value
				if(child.hasNullValue() || !child.getBooleanValue()) result = false;
				break;
			case BOOLEAN_ARRAY:	// do an implicit EVERY ...
				if(child.hasNullValue()){
					result = false;
					break;
				}
				Boolean [] values = child.getBooleanArrayValue();
				for(Boolean v : values){
					if(!v){ result = false; break;}
				}
				break;
			default:
				throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" does not handle type "+child.getType());
			}
			
		}
		setValue(new DataValue(result));
		if(debug) System.out.println(toString()+"() - Exit = "+getValue());
	}
	
}
