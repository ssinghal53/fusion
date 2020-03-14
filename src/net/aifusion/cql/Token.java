/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Dec 18, 2015 by Sharad Singhal
 */
package net.aifusion.cql;

import net.aifusion.metamodel.ModelException;

/**
 * Class to represent a CQL token
 * @author Sharad Singhal
 */
class Token {

	/** Type of this token */
	private TokenType type;
	/** value corresponding to this token */
	private String value;
	/** Parsed object associated with this token */
	private Object object = null;
	
	/**
	 * Create a new token. The value of the token is the value defined in the tokenType
	 * @param type - type of the token
	 */
	Token(TokenType type){
		this.type = type;
		value = type.getValue();
		if(value == null) throw new ModelException("Internal Error-- Illegal constructor for TokenType "+type);
		return;
	}
	
	/**
	 * Create a new Token
	 * @param type - type of the token
	 * @param value - value of the token
	 */
	Token(TokenType type, String value){
		this.type = type;
		this.value = value;
		if(type.getValue() != null && !value.equalsIgnoreCase(type.getValue()))
			throw new ModelException("Internal Error-- Attempt to use value "+value+" on predefined tokenType "+type);
		return;
	}
	
	/**
	 * Check if this token matches the expected tokenType
	 * @param expected - expected token type
	 * @return - true if the type matches, false otherwise
	 */
	boolean is(TokenType expected){
		if(type == expected) return true;
		
		// NOTE that NUMBER matches BINARY, HEX, INTEGER, and REAL
		if(expected == TokenType.NUMBER) return type == TokenType.NUMBER || type == TokenType.INTEGER || 
				type == TokenType.REAL || type == TokenType.HEX || type == TokenType.BINARY;
		
		// Note that COMPARISON matches EQUALS, NE, LT, LE, GT, GE, LIKE
		if(expected == TokenType.COMPARISON) return type == TokenType.EQUALS || type == TokenType.NE ||
				type == TokenType.LT || type == TokenType.LE || type == TokenType.GT || type == TokenType.GE || 
				type == TokenType.LIKE;
		
		// SIGN matches PLUS or MINUS
		if(expected == TokenType.SIGN) return type == TokenType.MINUS || type == TokenType.PLUS;
		
		// BOOLEAN matches TRUE or FALSE
		if(expected == TokenType.BOOLEAN) return type == TokenType.TRUE || type == TokenType.FALSE;
		
		// CLASSNAME matches IDENTIFIER
		if(expected == TokenType.CLASSPATH && type == TokenType.IDENTIFIER){
			return value.matches("^([a-zA-Z0-9])+_([a-zA-Z0-9])+$");
		}
		
		// IDENTIFIER matches on java identifiers
		if(expected == TokenType.IDENTIFIER) return isIdentifier();
		return false;
	}
	/**
	 * Check if this token is an identifier
	 * @return - true if token can be used as an identifier, false otherwise
	 */
	private boolean isIdentifier() {
		return type.isIdentifier();
	}

	/**
	 * Get the type of this token
	 * @return - type of token
	 */
	TokenType type(){
		return type;
	}
	/**
	 * Get the value of this token
	 * @return - value of token
	 */
	String value(){
		return value;
	}
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString(){
		return type.toString()+"["+value+"]:"+(object == null ? null : object.toString());
		
	}

}
