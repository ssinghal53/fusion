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

import net.aifusion.metamodel.StructureValue;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage ISA
 * @author Sharad Singhal
 */
class IsA extends Node {

	/**
	 * Create an ISA Node. Returns true if the candidate instance is an instance of the named node
	 * @param name - desired class name
	 */
	IsA(String name) {
		super(Operator.ISA,name, null);
		return;
	}

	@Override
	void evaluate(String header, StructureValue instance) {
		if(debug) System.out.println(toString()+"(instance) - Enter");
		List<Node> children = getChildren();
		if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" expected one child, found "+children.size());
		Node child = children.get(0);
		if(child.getOperator() != Operator.IDENTIFIER) 
			throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" currently does not handle children of type "+child.toString());
		String expected = getName();
		switch(child.getOperator()){
		case IDENTIFIER:
			setValue(header.equals(child.getName()) && instance.isInstanceOf(expected) ? TrueValue : FalseValue);
			break;
		default:
			throw new ModelException(ExceptionReason.NOT_SUPPORTED,toString()+" does not currently handle children of type "+child.toString());
		}
		if(debug) System.out.println(toString()+"(instance) - Exit "+getValue());
		return;
	}

	@Override
	DataType getType() {
		DataType dt = super.getType();
		return DataType.VOID.equals(dt) ? DataType.BOOLEAN : dt;
	}
	
}
