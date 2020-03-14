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
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage a RANGE node
 * @author Sharad Singhal
 */
class Range extends Node {

	/**
	 * Create a RANGE Node. A range returns an integer array with two values representing a lower and upper bound. 
	 * The upper bound may be null indicating the max possible value.
	 */
	Range() {
		super(Operator.RANGE,null, null);
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		List<Node> children = getChildren();
		// RANGE has two children containing {lower, upper} or the string "*". Note that upper may be a "*" indicating max
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
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
		return;
	}
}
