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
import java.util.Vector;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage Index
 * @author Sharad Singhal
 */
class Index extends Node {

	/**
	 * Create an Index node. An index node returns an integer array with index values in it.
	 */
	Index() {
		super(Operator.INDEX,null, null);
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		List<Node> children = getChildren();
		Vector<Integer> values = new Vector<Integer>();
		for(Node c : children){
			switch(c.getOperator()){
			case RANGE:	// Expect an array of length 2, the upper bound may be null indicating max value
				Integer [] bounds = (Integer []) c.getValue().getValue();
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
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
	}
}
