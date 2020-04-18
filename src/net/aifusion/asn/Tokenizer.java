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
 * Created Mar 31, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.Stack;
import java.util.Vector;

import net.aifusion.metamodel.ModelException;

/**
 * Class to break input into tokens
 * @author Sharad Singhal
 */
public class Tokenizer {
	/** Look-ahead token for the parser */
	private Token lookAheadToken = null;
	/** Current input reader */
	private BufferedReader input = null;
	/** Current line text (if any) */
	private String line = "";
	/** Character position in line */
	private int cursor = 0;
	/** Line number in current file */
	private int lineNumber = 0;
	/** Length of current line */
	private int lineLength = 0;
	private Vector<Token> tokens = new Vector<Token>(1000,1000);
	/** Debugging flag */
	private boolean debug = false;
	
	private int tokenCursor = 0;
	private Stack<Integer> stack = new Stack<Integer>();

	/**
	 * Create a tokenizer and tokenize the input file
	 * @param inputFile input file to use
	 */
	public Tokenizer(File inputFile) {
		try {
			input = new BufferedReader(new FileReader(inputFile));
			do {
				nextToken();
				tokens.add(lookAheadToken);
			} while(!lookAheadToken.is(TokenType.EOF));
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
		}
		reduce();
		if(debug) System.out.println("Found "+tokens.size()+" tokens");
		return;
	}
	
	/**
	 * Create a tokenizer and tokenize the input string
	 * @param inputString - string to use
	 */
	public Tokenizer(String inputString) {
		input = new BufferedReader(new StringReader(inputString));
		do {
			nextToken();
			tokens.add(lookAheadToken);
		} while(!lookAheadToken.is(TokenType.EOF));
		reduce();
		if(debug) System.out.println("Found "+tokens.size()+" tokens");
		return;
	}
	
	/**
	 * Push the current position of the tokenizer onto a stack
	 */
	public void push() {
		stack.push(tokenCursor);
		return;
	}
	
	/**
	 * Pop the stack to reset the tokenizer position
	 */
	public void pop() {
		if(!stack.isEmpty()) tokenCursor = stack.pop();
	}
	
	/**
	 * Get the next token from the tokenizer, and advance its position
	 * @return - next token.
	 */
	public Token next() {
		return tokens.elementAt(tokenCursor++);
	}
	
	/**
	 * Get the lookAhead token
	 * @return - token to be retrieved at the next call to next()
	 */
	public Token lookAhead() {
		return tokens.elementAt(tokenCursor);
	}
	
	/**
	 * Get the token at a given offset (positive or negative) from the current position
	 * @param offset - offset from the current position
	 * @return - token at the location. EOF is returned if the location is past the end or the beginning
	 */
	public Token lookAhead(int offset) {
		if(tokenCursor+offset >= 0 && tokenCursor+offset < tokens.size()) return tokens.elementAt(tokenCursor+offset);
		return new Token(TokenType.EOF);
	}
	
	/**
	 * Check if the lookAhead token is of a given type
	 * @param t - expected token type
	 * @return - true if the lookAheadToken matches the expected type, false otherwise
	 */
	public boolean lookAheadIs(TokenType t) {
		return tokens.elementAt(tokenCursor).is(t);
	}
	
	/**
	 * Check if the lookahead token at a given offset matches a given type
	 * @param t - expected token type
	 * @param offset - offset from the current position
	 * @return true if the token at the given offset matches the expected type, false otherwise
	 */
	public boolean lookAheadIs(TokenType t, int offset) {
		if(tokenCursor+offset >= 0 && tokenCursor+offset < tokens.size()) return tokens.elementAt(tokenCursor+offset).is(t);
		return false;
	}
	
	/**
	 * Reset the tokenizer
	 */
	public void reset() {
		tokenCursor = 0;
		return;
	}
	
	/**
	 * Skip over a given token, and move past it
	 * @param expected - expected token
	 * @return - token value of the expected token
	 */
	public Token skipOver(TokenType expected) {
		Token t = tokens.elementAt(tokenCursor);
		if(!t.is(expected)){
			// the expected token could not be found. Throw an exception
			throw new ModelException("Parse Error - expected "+expected.toString()+" found "+t.toString());
		}
		// reset the type to be expected. This is needed because several keywords can also be used as identifiers
		if(t.type() != expected) t.setType(expected);
		// fetch the next token into p.lookaheadToken unless we are finished
		if(expected != TokenType.EOF) tokenCursor++;
		if(debug) System.out.println("Skip Over: "+t.type()+" ["+t.value()+"] Expected: ("+expected+
		 		") LookAhead "+tokens.elementAt(tokenCursor));
		// return the current token
		return t;
	}
	
	/**
	 * Check if a list of tokens matches the incoming tokens
	 * @param expected - array containing list of tokens
	 * @return - true if the look-ahead matches the list, false otherwise
	 */
	public boolean matches(TokenType [] expected) {
		if(tokenCursor+expected.length >= tokens.size()) return false;
		for(int i = 0; i < expected.length; i++) {
			if(!tokens.elementAt(tokenCursor+i).is(expected[i])) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Scan forward for a given token type (advancing the cursor to the token if found)
	 * @param expected - expected token type
	 * @return - expected token. Null if end is reached (position is not changed)
	 */
	public Token scanFor(TokenType expected) {
		int savedCursor = tokenCursor;
		while(tokenCursor < tokens.size() &&  !tokens.elementAt(tokenCursor).is(expected)) {
			tokenCursor++;
		}
		if(tokenCursor >= tokens.size()) {
			tokenCursor = savedCursor;
			return null;
		}
		return tokens.get(tokenCursor);
	}
	
	/**
	 * Scan forward for a sequence of tokens
	 * @param expected - array containing the expected tokens
	 * @return - first token in the sequence. Null returned if not found (position is not changed)
	 */
	public Token scanFor(TokenType [] expected) {
		int savedCursor = tokenCursor;
		while(tokenCursor < tokens.size()-expected.length) {
			boolean found = true;
			for(int i = 0; i < expected.length; i++) {
				if(!tokens.elementAt(tokenCursor+i).is(expected[i])) {
					found = false;
					break;
				}
			}
			if(found) return tokens.elementAt(tokenCursor);
			tokenCursor++;
		}
		tokenCursor = savedCursor;
		return null;
	}
	
	/**
	 * Reduce two word tokens to single token (needed in case we have extra intervening white spaces
	 */
	private void reduce() {
		for(int i = tokens.size()-1; i > 0; i--) {
			switch(tokens.get(i).type()) {
			case DESCRIPTOR:
				if(tokens.get(i-1).is(TokenType.OBJECT)) {
					tokens.set(i-1, new Token(TokenType.OBJECT_DESCRIPTOR,TokenType.OBJECT_DESCRIPTOR.asn1Value()));
					tokens.remove(i);
				}
				break;
			case IDENTIFIER:
				if(tokens.get(i-1).is(TokenType.OBJECT)) {
					tokens.set(i-1, new Token(TokenType.OBJECT_IDENTIFIER,TokenType.OBJECT_IDENTIFIER.asn1Value()));
					tokens.remove(i);
				}
				break;
			case STRING:
				switch(tokens.get(i-1).type()) {
				case OCTET:
					tokens.set(i-1, new Token(TokenType.OCTET_STRING,TokenType.OCTET_STRING.asn1Value()));
					tokens.remove(i);
					break;
				case BIT:
					tokens.set(i-1, new Token(TokenType.BIT_STRING,TokenType.BIT_STRING.asn1Value()));
					tokens.remove(i);
					break;
				case CHARACTER:
					tokens.set(i-1, new Token(TokenType.CHARACTER_STRING,TokenType.CHARACTER_STRING.asn1Value()));
					tokens.remove(i);
					break;
				default:
					break;
				}
				break;
			case PDV:
				if(tokens.get(i-1).is(TokenType.EMBEDDED)) {
					tokens.set(i-1, new Token(TokenType.EMBEDDED_PDV,TokenType.EMBEDDED_PDV.asn1Value()));
					tokens.remove(i);
				}
				break;
			case BY:
				if(tokens.get(i-1).is(TokenType.DEFINED)) {
					tokens.set(i-1, new Token(TokenType.DEFINED_BY,TokenType.DEFINED_BY.asn1Value()));
					tokens.remove(i);
				}
				break;
			case IMPLIED:
				if(tokens.get(i-1).is(TokenType.EXTENSIBILITY)) {
					tokens.set(i-1, new Token(TokenType.EXTENSIBILITY_IMPLIED,TokenType.EXTENSIBILITY_IMPLIED.asn1Value()));
					tokens.remove(i);
				}
			case OF:
				switch(tokens.get(i-1).type()) {
				case SEQUENCE:
					tokens.set(i-1, new Token(TokenType.SEQUENCE_OF,TokenType.SEQUENCE_OF.asn1Value()));
					tokens.remove(i);
					break;
				case SET:
					tokens.set(i-1, new Token(TokenType.SET_OF,TokenType.SET_OF.asn1Value()));
					tokens.remove(i);
					break;
				case INSTANCE:
					tokens.set(i-1, new Token(TokenType.INSTANCE_OF,TokenType.INSTANCE_OF.asn1Value()));
					tokens.remove(i);
					break;
				default:
					break;
				}
				break;
			case SYNTAX:
				if(tokens.get(i-1).is(TokenType.WITH)) {
					tokens.set(i-1, new Token(TokenType.WITH_SYNTAX,TokenType.WITH_SYNTAX.asn1Value()));
					tokens.remove(i);
				}
			default:
				break;
			}
		}
		return;
	}
	
	/**
	 * Read the next token into lookAheadToken, skipping white space and comments
	 */
	private void nextToken(){
		// read the next Token and set its position, skipping any comments and white space
		int currentLine = lineNumber;
		int tokenCursor = cursor;
		readNextToken();
		lookAheadToken.setPosition(currentLine, tokenCursor);
		while(lookAheadToken.is(TokenType.WHITESPACE) || lookAheadToken.is(TokenType.COMMENT)){
			// if(debug) System.out.println("WhiteSpace/Comment ["+lookAheadToken.value()+"] cursor "+cursor+" remaining ["+line.substring(cursor)+"]");
			currentLine = lineNumber;
			tokenCursor = cursor;
			readNextToken();
			lookAheadToken.setPosition(currentLine, tokenCursor);
		}
		if(debug) System.out.println("LookAhead -> "+lookAheadToken.type()+" ["+lookAheadToken.value()+"]");
		return;
	}

	/**
	 * Read the next token into the lookAheadToken
	 */
	private void readNextToken() {
		// if the current line is null, empty, or we have read the complete line, get next line
		if(line == null || lineLength == 0 || cursor >= lineLength){
			boolean haveLine = nextLine();
			// nothing more is available, return EOF Token
			if(!haveLine){
				lookAheadToken = new Token(TokenType.EOF,"");
				return;
			}
		}
		// Handle white space
		if(Character.isWhitespace(line.charAt(cursor))){
			// check for white space
			int savedLoc = cursor;	// save cursor and scan white space
			while(++cursor < lineLength && Character.isWhitespace(line.charAt(cursor)));
			lookAheadToken = new Token(TokenType.WHITESPACE,line.substring(savedLoc,cursor));
			return;	
		}
		// Handle comments starting with '--'
		// comment terminates at the next '--' or the end of the line, whichever comes first
		if(line.startsWith("--", cursor)) {
			StringBuilder b = new StringBuilder("--");
			cursor += 2;
			while(cursor < lineLength) {
				if(line.startsWith("--", cursor)) {
					b.append("--");
					cursor += 2;
					break;
				}
				b.append(line.charAt(cursor++));
			}
			lookAheadToken = new Token(TokenType.COMMENT,b.toString());
			return;
		}
		// handle multi-line comments starting with '/*'
		// comments terminate with '*/' and may be nested
		if(line.startsWith("/*",cursor)){
			// Block comment. Scan until end of comment (or EOF)
			StringBuilder b = new StringBuilder("/*");
			cursor += 2;	// go past the /*
			int startNumber = lineNumber;	// save current line number
			boolean foundMatch = false;
			int nesting = 0;
			while(!foundMatch){
				while(cursor < lineLength){
					if(line.startsWith("/*",cursor)){
						nesting++;
						b.append("/*");
						cursor += 2;
					} else if(line.startsWith("*/",cursor)){
						// found end of comment
						b.append("*/");
						cursor += 2;
						if(nesting > 0) {
							nesting--;
						} else {
							lookAheadToken = new Token(TokenType.COMMENT,b.toString());
							foundMatch = true;
							break;
						}
					} else {
						b.append(line.charAt(cursor++));
					}
				}
				// if comment did not end on line, advance line
				if(!foundMatch && cursor >= lineLength){
					b.append("\n");
					if(!nextLine()){
						// reached EOF with an unterminated comment
						throw new ModelException("Unterminated comment starting at line "+startNumber);
					}
					continue;
				}
				return;
			}
		}
		// other tokens
		char c = line.charAt(cursor++);
		switch(c){
		case ',':
			lookAheadToken = new Token(TokenType.COMMA,",");
			return;
		case ';':
			lookAheadToken = new Token(TokenType.SEMI_COLON,";");
			return;
		case '(':
			lookAheadToken = new Token(TokenType.LEFT_PAREN,"(");
			return;
		case ')':
			lookAheadToken = new Token(TokenType.RIGHT_PAREN,")");
			return;
		case '{':
			lookAheadToken = new Token(TokenType.LEFT_BRACE,"{");
			return;
		case '}':
			lookAheadToken = new Token(TokenType.RIGHT_BRACE,"}");
			return;
		case '<':
			lookAheadToken = new Token(TokenType.LESS_THAN,"<");
			return;
		case '>':
			lookAheadToken = new Token(TokenType.GREATER_THAN,">");
			return;
		case '|':
			lookAheadToken = new Token(TokenType.VERTICAL_BAR,"|");
			return;
		case '_':
			lookAheadToken = new Token(TokenType.UNDERSCORE,"_");
			return;
		case '-':
			lookAheadToken = new Token(TokenType.MINUS,"-");
			return;
		case '&':
			lookAheadToken = new Token(TokenType.AMPERSAND,"&");
			return;
		case '@':
			lookAheadToken = new Token(TokenType.AT,"@");
			return;
		case '!':
			lookAheadToken = new Token(TokenType.NOT,"!");
			return;
		case '=':
			lookAheadToken = new Token(TokenType.EQUALS,"=");
			return;
		case '^':
			lookAheadToken = new Token(TokenType.CARAT,"^");
			return;
		case '[':
			lookAheadToken = new Token(TokenType.LEFT_BRACKET,"[");
			return;
		case ']':
			lookAheadToken = new Token(TokenType.RIGHT_BRACKET,"]");
			return;
		case '.':
			if(cursor < lineLength && line.charAt(cursor) == '.'){
				cursor++;
				if(cursor < lineLength && line.charAt(cursor) == '.'){
					cursor++;
					lookAheadToken = new Token(TokenType.TRIPLE_DOT,"...");
				} else {
					lookAheadToken = new Token(TokenType.DOUBLE_DOT,"..");
				}
			} else {
				lookAheadToken = new Token(TokenType.DOT,".");
			}
			return;
		case ':':
			if(line.startsWith(":=", cursor)){
				lookAheadToken = new Token(TokenType.ASSIGNMENT,"::=");
				cursor += 2;
			} else {
				lookAheadToken = new Token(TokenType.COLON,":");
			}
			return;
		default:
			cursor--;
			if(quotedString() || bitString() || numberString() || identifierString()) return;
			throw new ModelException("Could not parse input at Line "+lineNumber+" cursor "+cursor+":"+line.substring(cursor));
		}
	}

	/**
	 * Get a quoted string
	 * @return - true if quoted string found, false otherwise
	 */
	private boolean quotedString() {
		char c = line.charAt(cursor);
		if(c == '"') {
			int startLine = lineNumber;
			boolean foundMatch = false;
			StringBuilder b = new StringBuilder();
			cursor++;
			while(!foundMatch){
				while(cursor < lineLength){
					if(line.charAt(cursor) != '"') {
						b.append(line.charAt(cursor++));
					} else if(cursor < lineLength-1 && line.charAt(cursor+1) == '"'){
						b.append('"');
						cursor +=2;
					} else {
						cursor++;
						foundMatch = true;
						break;
					}
				}
				// if string did not end on line, advance line.
				// note that nextLine() already strips leading and trailing white space
				if(!foundMatch && cursor >= lineLength){
					// b.append("\n");
					if(!nextLine()){
						// reached EOF with an unterminated comment
						throw new ModelException("Unterminated string starting starting at line "+startLine);
					}
					continue;
				}
				lookAheadToken = new Token(TokenType.QUOTED_STRING,b.toString());
				return true;
			}
		}
		return false;
	}

	/**
	 * Get a bit string
	 * @return - true if quoted string found, false otherwise
	 */
	private boolean bitString() {
		char c = line.charAt(cursor);
		if(c == '\'') {
			int startLine = lineNumber;
			boolean foundMatch = false;
			boolean isBool = true;
			StringBuilder b = new StringBuilder("'");	// skip past the initial '
			cursor++;
			while(!foundMatch){
				while(cursor < lineLength){
					c = line.charAt(cursor++);
					if(c >= '0' && c <= '9' || c >= 'A' && c <= 'F'){
						b.append(c);
						if( c != '0' && c != '1') isBool = false;
					} else if(Character.isWhitespace(c)){
						continue;
					} else if(c == '\'') {
						b.append("'");
						foundMatch = true;
						break;
					} else {
						throw new ModelException("Expected bitString starting starting at line "+startLine+" found "+b.toString()+c);
					}
				}
				// if string did not end on line, advance line
				if(!foundMatch && cursor >= lineLength){
					if(!nextLine()){
						// reached EOF with an unterminated comment
						throw new ModelException("Unterminated bitString starting starting at line "+startLine);
					}
					continue;
				}
				c = line.charAt(cursor++);
				if(c == 'H' || c == 'B' && isBool) {
					b.append(c);
					lookAheadToken = new Token(TokenType.BINARY_STRING,b.toString());
					return true;
				}
				throw new ModelException("Expected bitString starting starting at line "+startLine+" found "+b.toString()+c);
			}
		}
		return false;
	}

	/**
	 * Get an identifier or type reference and place in lookAhead token if available
	 * @return - true if identifier found, false otherwise
	 */
	private boolean identifierString(){
		int currentCursor = cursor;
		char c = line.charAt(currentCursor);
		boolean isTypeReference = c >= 'A' && c <= 'Z';
		if(!(isTypeReference || c >= 'a' && c <= 'z')) return false;
		currentCursor++;
		while(currentCursor < lineLength) {
			c = line.charAt(currentCursor);
			if(c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'){
				currentCursor++;
			} else if(c == '-') {
				if(line.charAt(currentCursor-1) == '-') {
					currentCursor--;
					break;
				}
				currentCursor++;
			} else {
				break;
			}
		}
		if(line.charAt(currentCursor-1) == '-') {
			currentCursor--;
		}
		boolean isClassReference = true;
		String tokenString = line.substring(cursor, currentCursor);
		for(int i = 0; i < tokenString.length(); i++ ) {
			if(Character.isLowerCase(tokenString.charAt(i))) {
				isClassReference = false;
				break;
			}
		}
		// check if the incoming token matches a known type
		boolean foundMatch = false;
		for(TokenType t : TokenType.values()) {
			if(tokenString.equals(t.asn1Value())) {
				foundMatch = true;
				lookAheadToken = new Token(t,tokenString);
				break;
			}
		}
		if(!foundMatch) {
			if(isClassReference) {
				lookAheadToken = new Token(TokenType.CLASS_REFERENCE,tokenString);
			} else {
				lookAheadToken = new Token((isTypeReference ? TokenType.TYPE_REFERENCE : TokenType.IDENTIFIER_STRING),tokenString);
			}
		}
		cursor = currentCursor;
		return true;
	}

	/**
	 * Get an unsigned real or integer number and place it in the look-ahead token
	 * @return - true if real number found, false otherwise
	 */
	private boolean numberString(){
		// All Asn.1 real and integer numbers MUST start with a number
		if(!Character.isDigit(line.charAt(cursor))) return false;
		boolean seenDecimal = false, needSign = false, needDigit = true;
		StringBuilder b = new StringBuilder();
		while(cursor < lineLength){
			char c = line.charAt(cursor);
			if(Character.isDigit(c)){
				b.append(c);
				needSign = needDigit = false;
			} else if(c == '.'){				// check for decimal rules
				if(seenDecimal || cursor < lineLength-1 && line.charAt(cursor+1) == '.' ) break; // . is part of the next token, we are done.
				b.append(c);
				needSign = false;
				needDigit = seenDecimal = true;
			} else if(needSign && (c == '+' || c == '-')){ // sign can only be after (e | E)
				b.append(c);
				needSign = false;
			} else if(!needDigit && seenDecimal && (c == 'E' || c == 'e')){
				b.append(c);
				needSign = needDigit = true;
			} else {	// reached a non-number character
				break;
			}
			cursor++;
		}
		lookAheadToken = new Token(seenDecimal ? TokenType.REAL_NUMBER_STRING : TokenType.NUMBER_STRING, b.toString());
		return true;
	}
	
	/** 
	 * read in the next non-empty line (if any), discarding any leading or trailing white-space characters
	 * @return true if line successfully read, false if end of stream is reached
	 */
	private boolean nextLine(){
		try {
			while ((line = input.readLine()) != null){
				lineNumber++;
				cursor = 0;
				lineLength = line.length();
				while(lineLength > 0 && Character.isWhitespace(line.charAt(lineLength-1))) lineLength--;
				while(cursor < lineLength && Character.isWhitespace(line.charAt(cursor))) cursor++;
				if(lineLength > 0 && cursor < lineLength) break;
			}
			if(line == null) {
				input.close();
				return false;
			}
		} catch (IOException e){
			return false;
		}
		if(debug) System.out.println("Line "+lineNumber+" -> "+line.substring(cursor, lineLength));
		return true;
	}
}
