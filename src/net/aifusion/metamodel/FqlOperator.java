/**
 * Copyright 2021 Hewlett Packard Enterprise Development LP
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
 * Created Dec 5, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Operators understood in Filter Query Language
 * @author Sharad Singhal
 */
enum FqlOperator {
	ADD, AND, ANY, CLASS_PATH, CONCAT, CONSTANT, DIVIDE, ENUM, EQ, EVERY,
	FUNCTION, GE, GT, IDENTIFIER, INDEX, ISA, ISNOTNULL, ISNULL, LE, LIKE, LT, MULTIPLY,
	NE, NOT, NOTLIKE, OR, RANGE, SATISFIES, SCOPE, SIGN, SUBTRACT, VARIABLE, PERIOD,EOF
	;
	/**
	 * Get an FQL Node for this operator
	 * @return - FQL node
	 */
	FqlNode getFqlNode() {
		return new FqlNode(this);
	}

	/**
	 * Get an FQL node with a given data value
	 * @param value - data value to add to the node
	 * @return FQL node
	 */
	FqlNode getFqlNode(DataValue value) {
		return new FqlNode(this,null,value);
	}

	/**
	 * Get an FQL node with the given name
	 * @param name - name of the node
	 * @return FQL node
	 */
	FqlNode getFqlNode(String name) {
		return new FqlNode(this,name,null);
	}

}
