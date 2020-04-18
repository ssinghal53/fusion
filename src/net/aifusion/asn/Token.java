/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
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
 * Created Mar 2, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

/**
 * Class to represent a lexical token
 * @author Sharad Singhal
 */
public class Token {
	/** type of the token */
	private TokenType type;
	/** value of the token */
	private String value;
	/** line number in source */
	private int lineNumber;
	/** cursor in the source */
	private int cursor;
	
	/**
	 * Create a token for a given type
	 * @param type - type of token
	 */
	public Token(TokenType type) {
		this.type = type;
		this.value = "??";
	}
	
	/**
	 * Create a token with given type and value
	 * @param type - token type
	 * @param value - value of the token
	 */
	public Token(TokenType type, String value) {
		this.type = type;
		this.value = value;
	}
	
	/**
	 * Check if this token is of a given type
	 * @param expected - expected type of the token
	 * @return - true if the token type matches the expected type, false otherwise
	 */
	public boolean is(TokenType expected) {
		if(type == expected) return true;
		switch(expected){
		// tokens that can match other tokens
		case NUMBER_STRING:
			return type == TokenType.INTEGER;
			// note that this implies that CLASS_REFERENCE must be checked before TYPE_REFERENCE
			// since a CLASS_REFERENCE can also be a TYPE_REFERENCE, but not vice versa
		case TYPE_REFERENCE:
		return type == TokenType.CLASS_REFERENCE;	
		default:
			break;
		}
		return false;
	}
	
	public boolean isBuiltInType() {
		return 	type == TokenType.BIT_STRING || 
				type == TokenType.BMP_STRING ||
				type == TokenType.BOOLEAN ||
				type == TokenType.CHARACTER_STRING ||
				type == TokenType.CHOICE ||
				type == TokenType.ENUMERATED ||
				type == TokenType.GENERALIZED_TIME ||
				type == TokenType.GENERAL_STRING ||
				type == TokenType.GRAPHIC_STRING ||
				type == TokenType.NUMBER_STRING ||
				type == TokenType.IA5_STRING ||
				type == TokenType.INTEGER ||
				type == TokenType.ISO646_STRING ||
				type == TokenType.NULL ||
				type == TokenType.OBJECT_IDENTIFIER ||
				type == TokenType.OBJECT_DESCRIPTOR ||
				type == TokenType.OCTET_STRING ||
				type == TokenType.EMBEDDED_PDV ||
				type == TokenType.PRINTABLE_STRING ||
				type == TokenType.REAL ||
				type == TokenType.RELATIVE_OID ||
				type == TokenType.SEQUENCE ||
				type == TokenType.CHOICE ||
				type == TokenType.SET ||
				type == TokenType.SEQUENCE_OF ||
				type == TokenType.SET_OF ||
				type == TokenType.T61_STRING ||
				type == TokenType.TELETEX_STRING ||
				type == TokenType.UNIVERSAL_STRING ||
				type == TokenType.UTC_TIME ||
				type == TokenType.UTF8_STRING ||
				type == TokenType.VIDEOTEX_STRING ||
				type == TokenType.VISIBLE_STRING ||
				type == TokenType.NUMERIC_STRING || 
				type == TokenType.INSTANCE_OF || 
				type == TokenType.ANY;
	}
	
	/**
	 * Reset the type of this token to the expected type
	 * @param expected - expected type of the token
	 */
	public void setType(TokenType expected) {
		type = expected;
		return;
	}
	
	/**
	 * Set the input position for this token
	 * @param lineNumber - line number in input
	 * @param cursor - cursor within the line
	 */
	public void setPosition(int lineNumber, int cursor) {
		this.lineNumber = lineNumber;
		this.cursor = cursor;
	}
	
	/**
	 * Get the value of this token
	 * @return - value of the token. '??' returned if the token does not have a value
	 */
	public String value() {
		return value;
	}
	
	/**
	 * Get the type of this token
	 * @return - type of the token
	 */
	
	public TokenType type() {
		return type;
	}
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append(type).append(" [").append(value).append("](").append(lineNumber).append(",").append(cursor).append(")");
		return b.toString();
	}
}
