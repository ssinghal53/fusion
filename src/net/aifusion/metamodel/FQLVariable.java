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

/**
 * Class to create a variable (a delayed constant) node
 * @author Sharad Singhal
 */
class FQLVariable extends FQLNode {

	/**
	 * Create a variable node. A Variable node is a delayed constant, whose value can be set prior to query evaluation
	 * @param name - name of the variable
	 */
	FQLVariable(String name) {
		super(FQLOperator.VARIABLE, name, null);
		return;
	}

	@Override
	void setVariable(String name, DataValue value) {
		if(getName().equalsIgnoreCase(name)){
			if(value == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Variable "+getName()+" requires typed data value, and cannot be set to null");
			setValue(value);
		}
		return;
	}

	@Override
	void evaluate() {
		if(getValue() == null) throw new ModelException(ExceptionReason.INVALID_ENUMERATION_CONTEXT,"Variable "+getName()+" not defined before query evaluation");
		return;
	}
}
