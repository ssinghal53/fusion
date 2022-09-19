/**
 * Copyright 2022 Sharad Singhal. All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
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
 * Created Sep 19, 2022 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.List;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to manage a property assignment
 * @author Sharad Singhal
 */
public class Assign extends Node {
	/**
	 * create an ASSIGNMENT node
	 * @param name - name of the property to use
	 */
	public Assign(String name) {
		super(Operator.ASSIGN,name,null);
		return;
	}

	// assign has a propertyName as its name, and an expr() node as its child
	
	@Override
	void evaluate(String header, StructureValue instance) {
		if(!instance.hasProperty(getName())) return;
		List<Node> children = getChildren();
		if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" expected one child, found "+children.size());
		
		// evaluate the value to be set against this instance
		children.get(0).evaluate(header, instance);
		
		// save the value at this node to be the OLD value of the instance
		setValue(instance.getPropertyValue(getName()));
		
		// update the value of the instance
		instance.setPropertyValue(getName(), children.get(0).getValue());
		
		return;
	}
	
}
