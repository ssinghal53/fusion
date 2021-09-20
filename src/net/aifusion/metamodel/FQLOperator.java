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
 * Operators known to CQL
 * @author Sharad Singhal
 */
enum FQLOperator {
	/** Select Node identifier (this is the root node of the query) */
	SELECT,
	/** Select List identifier */
	SELECT_LIST,
	/** Node containing class names which are selected */
	CLASS_LIST,
	/** Node containing the where clause */
	WHERE,
	/** Node defining a class path */
	CLASS_PATH,
	/** Node defining a class name */
	CLASS_NAME,
	/** Node defining a property name */
	PROPERTY_NAME,
	/** Node defining a chain */
	CHAIN,
	/** Node containing a value symbol */
	VALUE_SYMBOL,
	/** Node defining a function. Arguments are in child nodes */
	FUNCTION,
	/** Leaf Node defining a property or class name */
	IDENTIFIER,
	/** Leaf Node defining a constant value */
	CONSTANT, 
	/** Node defining a NOT expression */
	NOT,
	/** left child equals right child */
	EQUALS,
	/** left child not equals right child */
	NE,
	/** left child < right child */
	LT,
	/** left child <= right child */
	LE,
	/** left child > right child */
	GT,
	/** left child >= right child */
	GE,
	/** left child AND right child */
	AND,
	/** left child OR right child */
	OR,
	/** node defining a variable node that can be modified in the query */
	VARIABLE,
	/** node defining that child is NULL */
	ISNULL,
	/** node defining that child is NOT NULL */
	ISNOTNULL,
	/** Node defining that child is a subclass */
	ISA,
	/** node defining index for the parent */
	INDEX,
	/** node defining any of values in underlying comparison are true */
	ANY,
	/** node defining every one of values in underlying comparison are true */
	EVERY,
	/** Alias values defined in this query */
	ALIAS,
	/** Enumeration value */
	ENUM,
	/** node defining an index range */
	RANGE,
	/** node defining a regexp match */
	LIKE,
	/** node defining a property scope */
	SCOPE,
	/** Node defining division */
	DIVIDE,
	/** Node defining multiplication */
	MULTIPLY,
	/** Node defining addition */
	ADD,
	/** Node defining subtraction */
	SUBTRACT,
	/** Node defining concat operator */
	CONCAT,
	/** Node defining sign */
	SIGN,
	/** Node defining satisfies */
	SATISFIES,
	/** Node defining result-set order */
	ORDER_BY,
	/** Node defining result-set size */
	FIRST,
	/** Node defining distinct flag */
	DISTINCT,
	/** Node defining a sort Spec */
	SORT_BY;
	
	/**
	 * Get a generic branch node corresponding to this operator
	 * @return - node corresponding to this operator
	 */
	FQLNode getNode(){
		switch(this){
		case EQUALS:
		case NE:
		case LT:
		case LE:
		case GT:
		case GE:
			return new FQLComparison(this);
		case AND:
		case OR:
		case ANY:
		case EVERY:
			return new FQLLogical(this);
		case NOT:
			return new FQLNot();
		case ISNULL:
		case ISNOTNULL:
			return new FQLNullCheck(this);
		case LIKE:
			return new FQLLike();
		default:
			return new FQLNode(this,null,null);
		}
		
	}
	
	/**
	 * Get a generic branch node corresponding to this operator with a given name
	 * @return - node corresponding to this operator. The node has a given name
	 */
	FQLNode getNode(String name){
		switch(this){
		case VARIABLE:
			return new FQLVariable(name);
		case IDENTIFIER:
			return new FQLIdentifier(name);

		case SIGN:
			return new FQLSign(name);
		case PROPERTY_NAME:
			return new FQLPropertyName(name);
		default:
			return new FQLNode(this,name,null);
		}
	}
	
	/**
	 * Create a node corresponding to this operator with a given value
	 * @param value - value corresponding to this node
	 * @return node corresponding to this operator. The node has a null name and the given value
	 */
	FQLNode getNode(DataValue value){
		switch(this){
		case CONSTANT:
			return new FQLConstant(value);
		default:
			return new FQLNode(this,null,value);
		}
	}
	
	/**
	 * Get a generic branch node corresponding to this operator with a given name and value
	 * @return - node corresponding to this operator. The node has the given name and value
	 */
	FQLNode getNode(String name, DataValue value){
		switch(this){
		default:
			return new FQLNode(this,name,value);
		}
	}
}
