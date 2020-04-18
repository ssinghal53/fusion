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
 * Created May 20, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.Vector;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage a symbol table
 * @author Sharad Singhal
 */

public class SymbolTable {
	/** Table of known Symbols <fullName,Symbol> */
	private LinkedHashMap<String,Symbol> symbolTable = new LinkedHashMap<String,Symbol>();
	/** true if the scanner is expecting a new module definition */
	boolean isNewModule = true;
	/** Name of the current module */
	private Symbol currentModule = null;	// current module
	/** Tokens for the current symbol */
	private Vector<Token> defn = new Vector<Token>();	// current definition tokens
	/** Tokenizer to use */
	private Tokenizer tokenizer;
	/** Debugging flag */
	private boolean debug = false;

	/**
	 * Create a symbol table using the tokens in the tokenizer
	 * @param tokenizer - tokenizer containing the input definition
	 */
	public SymbolTable(Tokenizer tokenizer) {
		this.tokenizer = tokenizer;
		int terminator = 0;				// nesting level for parentheses and braces
		Token token = null;				// next token to scan
		Vector<Token> imports = new Vector<Token>();	// intermediate tokens held for imports
		while(!tokenizer.lookAheadIs(TokenType.EOF)) {
			token = tokenizer.next();
			// if(debug) System.out.println("\t"+token);
			if(token.is(TokenType.END)) {
				isNewModule = true;
				if(debug) debug("--- End of Module "+currentModule+" ---\n");
				continue;
			} else if(isNewModule) {
				String module = token.value();
				currentModule = new Symbol(module,AsnProduction.MODULE_IDENTIFIER,null,null);
				symbolTable.put(currentModule.getFullName(), currentModule);
				isNewModule = false;
				if(debug) debug("--- Start of Module "+currentModule+" ---");
				while(!tokenizer.next().is(TokenType.BEGIN));
				continue;
			} else if(token.is(TokenType.EXPORTS)) {	// ignore exports
				while(!tokenizer.next().is(TokenType.SEMI_COLON));
				continue;
			} else if(token.is(TokenType.IMPORTS)) {
				if(debug) debug("--- Importing" );
				do {
					token = tokenizer.next();	// next token
					if(token.is(TokenType.SEMI_COLON)) break;
					// System.out.println("\t"+token);
					if(token.is(TokenType.COMMA) || token.is(TokenType.LEFT_BRACE) || token.is(TokenType.RIGHT_BRACE)) continue; // skip commas and braces
					if(!token.is(TokenType.FROM)) {
						// if(debug) System.out.println("\t\t"+token);
						imports.add(token);	// add to our list
					} else {
						String moduleName = tokenizer.next().value();	// module name
						// if(debug) System.out.println("\tFrom "+moduleName);
						for(Token t : imports) {	// add imported tokens to symbol table
							Symbol s = new Symbol(t.value(),AsnProduction.SYMBOLS_IMPORTED,currentModule,moduleName);
							symbolTable.put(s.getFullName(),s);
							if(debug) System.out.println("\t"+s);
						}
						imports.clear();	// clear the imports
						if(tokenizer.lookAheadIs(TokenType.LEFT_BRACE)) {	// ignore module OID
							do {
								token = tokenizer.next();
							} while(!token.is(TokenType.RIGHT_BRACE)); 
						}
					}
				} while(!token.is(TokenType.SEMI_COLON));
				continue;
			} else if(token.is(TokenType.LEFT_BRACKET) || token.is(TokenType.LEFT_BRACE) || token.is(TokenType.LEFT_PAREN)) {
				terminator++;
				continue;
			} else if(token.is(TokenType.RIGHT_BRACKET) || token.is(TokenType.RIGHT_BRACE) || token.is(TokenType.RIGHT_PAREN)) {
				terminator--;
				continue;
			} else if(terminator > 0){
				continue;
			}
			if(token.is(TokenType.WITH_SYNTAX)) {
				continue;
			}
			defn.add(token);
			if(!token.is(TokenType.ASSIGNMENT)) continue;

			// have an assignment
			if(debug) {
				for(Token t : defn) System.out.print(t+" ");
				System.out.println("Lookahead "+tokenizer.lookAhead()+" "+tokenizer.lookAhead(1));
				System.out.flush();
			}
			Symbol s = null;
			Token nameToken = defn.get(0);
			String definingSymbol = null;
			switch(nameToken.type()) {
			case IDENTIFIER_STRING:	// identifier ...
				String name = nameToken.value();
				Token typeToken = defn.get(1);
				if(typeToken.is(TokenType.CLASS_REFERENCE)) {	// Object definition
					s = new Symbol(name,AsnProduction.OBJECT,currentModule,typeToken.value());
					symbolTable.put(s.getFullName(), s);
				} else if(typeToken.is(TokenType.ABSTRACT_SYNTAX) || typeToken.is(TokenType.TYPE_IDENTIFIER)) {
					s = new Symbol(name,AsnProduction.OBJECT,currentModule,typeToken.value());
					symbolTable.put(s.getFullName(), s);
				} else if(typeToken.is(TokenType.TYPE_REFERENCE)) {	// Value definition with Type defined elsewhere
					s = new Symbol(name,AsnProduction.VALUE,currentModule,typeToken.value());
					symbolTable.put(s.getFullName(), s);
				} else if(typeToken.isBuiltInType()) {	// Value definition with built-in type
					AsnProduction p = getAstProduction(typeToken);
					definingSymbol = getDefiningType(p,true);
					s = new Symbol(name,p,currentModule,definingSymbol);
					symbolTable.put(s.getFullName(), s);
				} else {
					error(nameToken+"Type not yet handled "+typeToken);
					return;
				}
				if(tokenizer.lookAheadIs(TokenType.NUMBER_STRING) || tokenizer.lookAheadIs(TokenType.QUOTED_STRING) || 
						tokenizer.lookAheadIs(TokenType.IDENTIFIER_STRING) || tokenizer.lookAheadIs(TokenType.BINARY_STRING) || tokenizer.lookAheadIs(TokenType.NULL) ||
						tokenizer.lookAheadIs(TokenType.REAL_NUMBER_STRING)) tokenizer.next();
				break;
			case CLASS_REFERENCE:
				if(defn.size() == 2) {	// CLASSNAME ::=  
					if(tokenizer.lookAheadIs(TokenType.CLASS)) { // ClassName ::= CLASS ...
						tokenizer.skipOver(TokenType.CLASS);
						s = new Symbol(nameToken.value(),AsnProduction.OBJECT_CLASS,currentModule,null);	// Object class definition
						symbolTable.put(s.getFullName(), s);
						break;
					} else if(tokenizer.lookAheadIs(TokenType.CLASS_REFERENCE)) {	// ClassName ::= ClassReference
						s = new Symbol(nameToken.value(),AsnProduction.OBJECT_CLASS_REFERENCE,currentModule,tokenizer.skipOver(TokenType.CLASS_REFERENCE).value());
						symbolTable.put(s.getFullName(), s);
						break;
					}
				} else if(defn.get(1).is(TokenType.CLASS_REFERENCE)){
					// ClassName CLASSReference ::= ...
					s = new Symbol(nameToken.value(),AsnProduction.OBJECT_SET_REFERENCE,currentModule,defn.get(1).value());
					symbolTable.put(s.getFullName(), s);
					// error("Expected size = 2, found "+defn.size()+" ::= "+tokenizer.lookAhead());
					break;
				}
				// else treat CLASSNAME as a type name
			case TYPE_REFERENCE:
				if(defn.size() == 2) {	// TypeName ::= ...
					name = nameToken.value();
					typeToken = tokenizer.next();
					if(typeToken.is(TokenType.LEFT_BRACKET)) {	// TypeName ::= [tag] type
						terminator++;
						do {
							Token t = tokenizer.next();
							if(t.is(TokenType.LEFT_BRACKET)) terminator++;
							if(t.is(TokenType.RIGHT_BRACKET)) terminator--;
						} while(terminator > 0);
						typeToken = tokenizer.next();
						if(typeToken.is(TokenType.IMPLICIT) || typeToken.is(TokenType.EXPLICIT)) typeToken = tokenizer.next();
					}
					if(typeToken.is(TokenType.ANY)) {
						String definingType = null;
						if(tokenizer.lookAheadIs(TokenType.DEFINED_BY)) {
							tokenizer.skipOver(TokenType.DEFINED_BY);
							definingType = tokenizer.next().value();
						}
						s = new Symbol(name,AsnProduction.ANY_TYPE,currentModule,definingType);
						symbolTable.put(s.getFullName(), s);
					} else if(typeToken.is(TokenType.TYPE_REFERENCE)) {	// TypeName ::= Type
						definingSymbol = typeToken.value();
						if(tokenizer.lookAheadIs(TokenType.DOT)) {
							definingSymbol += tokenizer.skipOver(TokenType.DOT).value();
							if(tokenizer.lookAheadIs(TokenType.AMPERSAND)) definingSymbol += tokenizer.skipOver(TokenType.AMPERSAND);
							definingSymbol += tokenizer.next().value();
						}
						if(debug) debug("--- Type Defn : "+name+" "+typeToken+" "+definingSymbol);
						s = new Symbol(name,AsnProduction.TYPE_REFERENCE,currentModule,typeToken.value());
						symbolTable.put(s.getFullName(), s);
					} else if(typeToken.isBuiltInType()){
						AsnProduction p = getAstProduction(typeToken);
						definingSymbol = getDefiningType(p,false);
						if(definingSymbol != null) {
							if(typeToken.is(TokenType.SEQUENCE)) {
								p = AsnProduction.SEQUENCE_OF_TYPE;
							} else if(typeToken.is(TokenType.SET)) {
								p = AsnProduction.SET_OF_TYPE;
							}
						}
						if(debug) debug("--- BuiltIn Defn : "+name+" "+typeToken+" "+p+" "+definingSymbol);
						s = new Symbol(name,p,currentModule,definingSymbol);
						symbolTable.put(s.getFullName(), s);
					} else if(typeToken.is(TokenType.IDENTIFIER_STRING) && tokenizer.lookAheadIs(TokenType.LESS_THAN)) {
						tokenizer.skipOver(TokenType.LESS_THAN);
						Token choice = tokenizer.next();
						definingSymbol = choice.value()+"."+typeToken.value();
						s = new Symbol(name,AsnProduction.SELECTION_TYPE,currentModule,definingSymbol);
						symbolTable.put(s.getFullName(), s);
					} else {
						switch(typeToken.type()) {
						case EXTERNAL:
							s = new Symbol(name,AsnProduction.EXTERNAL_TYPE,currentModule,definingSymbol);
							symbolTable.put(s.getFullName(), s);
							break;
						case TYPE_IDENTIFIER:
						case ABSTRACT_SYNTAX:
							s = new Symbol(name,AsnProduction.USEFUL_OBJECT_CLASS_REFERENCE,currentModule,definingSymbol);
							symbolTable.put(s.getFullName(),s);
							break;
						default:
							error(typeToken+" not yet handled (2)");
							return;
						}
					}
					break;
				} else if(defn.size() == 3){	// ObjectSet or valueSet // TypeName CLASS ::= or TypeName Type ::= 
					name = nameToken.value();
					typeToken = defn.get(1);
					if(typeToken.is(TokenType.CLASS_REFERENCE)) {
						s = new Symbol(name,AsnProduction.OBJECT_SET,currentModule,null);
						symbolTable.put(s.getFullName(), s);
					} else if(typeToken.isBuiltInType()){
						s = new Symbol(name,AsnProduction.VALUE_SET,currentModule,typeToken.value());
						symbolTable.put(s.getFullName(), s);
					} else if(typeToken.is(TokenType.TYPE_REFERENCE)){
						s = new Symbol(name,AsnProduction.VALUE_SET,currentModule,typeToken.value());
						symbolTable.put(s.getFullName(), s);
					} else {
						error(typeToken+" not yet handled (3)");
					}
				} else {
					error("Expected size 2 or 3, found "+defn.size());
				}
				break;
			default:
				error(nameToken+" not yet handled (3)");
				return;
			}
			defn.clear();
			// if(tokenizer.lookAheadIs(TokenType.NUMBER_STRING) || tokenizer.lookAheadIs(TokenType.QUOTED_STRING)) tokenizer.next();
		}
		tokenizer.reset();
		if(debug) {
			System.out.println("-- Symbol Table --");
			for(Entry<String,Symbol> e : symbolTable.entrySet()) {
				System.out.println(e.getKey()+" : "+e.getValue());
			}
			System.out.println("-- End Symbol Table --");
			System.out.flush();
		}
		return;
	}

	/**
	 * Get the underlying map containing &lt;symbolName,Symbol&gt; from this symbol table
	 * @return map containing &lt;symbolName,Symbol&gt;
	 */
	public LinkedHashMap<String,Symbol> getSymbolTable(){
		return symbolTable;
	}

	private void debug(String message) {
		System.out.println(message);
		System.out.flush();
		return;
	}

	private void error(String error) {
		System.out.print("** E ** ");
		System.out.println(error);
		for(Token t : defn) {
			System.out.print(t+" ");
		}
		System.out.print("\n");
		System.out.flush();
		throw new ModelException(tokenizer.lookAhead()+" "+tokenizer.lookAhead(1)+" "+tokenizer.lookAhead(2)+" "+tokenizer.lookAhead(3));
	}

	private String getDefiningType(AsnProduction p, boolean isValue) {
		Token typeToken = tokenizer.lookAhead();
		if(typeToken.is(TokenType.LEFT_BRACE)) return null;
		switch(p) {
		case SEQUENCE_TYPE:
		case SET_TYPE:
			if(typeToken.is(TokenType.SIZE)) {
				tokenizer.skipOver(TokenType.SIZE);
				int terminator = 1;
				tokenizer.skipOver(TokenType.LEFT_PAREN);
				while(terminator != 0) {
					Token t = tokenizer.next();
					if(t.is(TokenType.LEFT_PAREN) || t.is(TokenType.LEFT_BRACE) || t.is(TokenType.LEFT_BRACKET)) terminator++;
					if(t.is(TokenType.RIGHT_PAREN) || t.is(TokenType.RIGHT_BRACE) || t.is(TokenType.RIGHT_BRACKET)) terminator--;
				}
				tokenizer.skipOver(TokenType.OF);
				// handle SEQUENCE|SET OF namedValue
				if(tokenizer.lookAheadIs(TokenType.IDENTIFIER_STRING)) tokenizer.skipOver(TokenType.IDENTIFIER_STRING);
				return nextValue();
			} else if(typeToken.is(TokenType.LEFT_PAREN)) {
				tokenizer.skipOver(TokenType.LEFT_PAREN);
				int terminator = 1;
				while(terminator != 0) {
					Token t = tokenizer.next();
					if(t.is(TokenType.LEFT_PAREN) || t.is(TokenType.LEFT_BRACE) || t.is(TokenType.LEFT_BRACKET)) terminator++;
					if(t.is(TokenType.RIGHT_PAREN) || t.is(TokenType.RIGHT_BRACE) || t.is(TokenType.RIGHT_BRACKET)) terminator--;
				}
				tokenizer.skipOver(TokenType.OF);
				// handle SEQUENCE|SET OF namedValue
				if(tokenizer.lookAheadIs(TokenType.IDENTIFIER_STRING)) tokenizer.skipOver(TokenType.IDENTIFIER_STRING);
				return nextValue();
			}
			break;
		case ANY_TYPE:
			if(typeToken.is(TokenType.DEFINED_BY)) {
				tokenizer.skipOver(TokenType.DEFINED_BY);
			}
		case SET_OF_TYPE:
		case SEQUENCE_OF_TYPE:
			// handle SEQUENCE|SET OF namedValue
			if(tokenizer.lookAheadIs(TokenType.IDENTIFIER_STRING)) tokenizer.skipOver(TokenType.IDENTIFIER_STRING);
			return nextValue();
		case BOOLEAN_TYPE:
		case INTEGER_TYPE:
		case BIT_STRING_TYPE:
		case OCTET_STRING_TYPE:
		case NULL_TYPE:
		case OBJECT_IDENTIFIER_TYPE:
		case OBJECT_DESCRIPTOR:
		case INSTANCE_OF_TYPE:
		case EXTERNAL_TYPE:
		case REAL_TYPE:
		case ENUMERATED_TYPE:
		case EMBEDDED_PDV_TYPE:
		case UTF8_STRING_TYPE:
		case RELATIVE_OID_TYPE:
		case CHOICE_TYPE:
		case NUMERIC_STRING_TYPE:
		case PRINTABLE_STRING_TYPE:
		case T61_STRING_TYPE:
		case TELETEX_STRING_TYPE:
		case VIDEOTEX_STRING_TYPE:
		case IA5_STRING_TYPE:
		case UTC_TIME_TYPE:
		case GENERALIZED_TIME_TYPE:
		case GRAPHIC_STRING_TYPE:
		case VISIBLE_STRING_TYPE:
		case ISO646_STRING_TYPE:
		case GENERAL_STRING_TYPE:
		case UNIVERSAL_STRING_TYPE:
		case CHARACTER_STRING_TYPE:
		case BMP_STRING_TYPE:
			break;
		default:
			error("getDefiningType - not handled "+typeToken);
		}
		return null;
	}

	private String nextValue() {
		StringBuilder b = new StringBuilder();
		b.append(tokenizer.next().value());
		while(tokenizer.lookAheadIs(TokenType.DOT)) {
			b.append(tokenizer.next().value());	// X "."
			if(tokenizer.lookAheadIs(TokenType.AMPERSAND)) {
				b.append(tokenizer.next().value());	// X "." "&"
			}
			b.append(tokenizer.next().value());	// X ["." [&] Y]*
		}
		return b.toString();
	}

	private AsnProduction getAstProduction(Token typeToken) {
		// System.out.println("---------- Checking type - "+typeToken);
		switch(typeToken.type()) {
		case BOOLEAN:
			return AsnProduction.BOOLEAN_TYPE;
		case INTEGER:
			return AsnProduction.INTEGER_TYPE;
		case BIT_STRING:
			return AsnProduction.BIT_STRING_TYPE;
		case OCTET_STRING:
			return AsnProduction.OCTET_STRING_TYPE;
		case NULL:
			return AsnProduction.NULL_TYPE;
		case OBJECT_IDENTIFIER:
			return AsnProduction.OBJECT_IDENTIFIER_TYPE;
		case OBJECT_DESCRIPTOR:
			return AsnProduction.OBJECT_DESCRIPTOR;
		case INSTANCE_OF:
			return AsnProduction.INSTANCE_OF_TYPE;
		case EXTERNAL:
			return AsnProduction.EXTERNAL_TYPE;
		case REAL:
			return AsnProduction.REAL_TYPE;
		case ENUMERATED:
			return AsnProduction.ENUMERATED_TYPE;
		case EMBEDDED_PDV:
			return AsnProduction.EMBEDDED_PDV_TYPE;
		case UTF8_STRING:
			return AsnProduction.UTF8_STRING_TYPE;
		case RELATIVE_OID:
			return AsnProduction.RELATIVE_OID_TYPE;
		case SEQUENCE:
			return AsnProduction.SEQUENCE_TYPE;
		case SEQUENCE_OF:
			return AsnProduction.SEQUENCE_OF_TYPE;
		case SET:
			return AsnProduction.SET_TYPE;
		case CHOICE:
			return AsnProduction.CHOICE_TYPE;
		case SET_OF:
			return AsnProduction.SET_OF_TYPE;
		case NUMERIC_STRING:
			return AsnProduction.NUMERIC_STRING_TYPE;
		case PRINTABLE_STRING:
			return AsnProduction.PRINTABLE_STRING_TYPE;
		case T61_STRING:
			return AsnProduction.T61_STRING_TYPE;
		case TELETEX_STRING:
			return AsnProduction.TELETEX_STRING_TYPE;
		case VIDEOTEX_STRING:
			return AsnProduction.VIDEOTEX_STRING_TYPE;
		case IA5_STRING:
			return AsnProduction.IA5_STRING_TYPE;
		case UTC_TIME:
			return AsnProduction.UTC_TIME_TYPE;
		case GENERALIZED_TIME:
			return AsnProduction.GENERALIZED_TIME_TYPE;
		case GRAPHIC_STRING:
			return AsnProduction.GRAPHIC_STRING_TYPE;
		case VISIBLE_STRING:
			return AsnProduction.VISIBLE_STRING_TYPE;
		case ISO646_STRING:
			return AsnProduction.ISO646_STRING_TYPE;
		case GENERAL_STRING:
			return AsnProduction.GENERAL_STRING_TYPE;
		case UNIVERSAL_STRING:
			return AsnProduction.UNIVERSAL_STRING_TYPE;
		case CHARACTER_STRING:
			return AsnProduction.CHARACTER_STRING_TYPE;
		case BMP_STRING:
			return AsnProduction.BMP_STRING_TYPE;
		case ANY:
			return AsnProduction.ANY_TYPE;
		default:
			error("getAstProduction - not handled "+typeToken);
		}
		return null;
	}

	/**
	 * Get the definition of a symbol
	 * @param name - fully qualified name of the symbol
	 * @return - definition of the symbol, if any. Null if the symbol is not defined, or does not yet have a definition
	 */
	public AsnNode getDefinition(String name) {
		if(symbolTable.containsKey(name)) {
			return symbolTable.get(name).getDefinition();
		}
		return null;
	}

	/**
	 * Set the definition of a symbol
	 * @param name - fully qualified name of the symbol
	 * @param definition - corresponding definition
	 */
	public void setDefinition(String name, AsnNode definition) {
		if(symbolTable.containsKey(name)) {
			symbolTable.get(name).setDefinition(definition);
		} else {
			throw new ModelException(ExceptionReason.NOT_FOUND,"No such name in the symbol table: "+name);
		}
		return;
	}

	/**
	 * Get the symbol associated with a given symbol
	 * @param name - fully qualified name of the symbol
	 * @return - associated symbol. Null if none defined
	 */
	public Symbol get(String name) {
		return symbolTable.get(name);
	}

	/**
	 * Get the production associated with a given symbol
	 * @param name - fully qualified name of the symbol
	 * @return - production associated with the symbol, null if none defined
	 */
	public AsnProduction getProduction(String name) {
		if(!symbolTable.containsKey(name)) return null;
		Symbol s = symbolTable.get(name);
		if(debug) {
			System.out.println("SymbolTable: Locating - "+name+" ["+s+"]");
			System.out.flush();
		}
		return s.getProduction();
	}

	/**
	 * Get the primitive type associated with a symbol
	 * @param name - fully qualified name of the symbol
	 * @return - primitive (universal) type of the symbol. Null if the symbol is not defined, or does not reduce to a universal type
	 */
	public AsnNode getType(String name) {
		if(!symbolTable.containsKey(name)) return null;
		Symbol s = symbolTable.get(name);
		if(debug) {
			System.out.println("SymbolTable: Locating - "+name+" ["+s+"]");
			System.out.flush();
		}
		AsnProduction p = s.getProduction();
		if(p.isBuiltIn()) return new AsnNode(p);
		switch(p) {
		case MODULE_IDENTIFIER:
			return new AsnNode(p);
		case SYMBOLS_IMPORTED:
			String def = "@"+s.getDefiningType()+"."+s.getName();
			return getType(def);
		case VALUE:
		case TYPE_REFERENCE:
			def = s.getParent().getFullName()+"."+s.getDefiningType();
			return getType(def);
		case OBJECT_CLASS_REFERENCE:
		case OBJECT_SET_REFERENCE:
		case OBJECT_CLASS:
		case SELECTION_TYPE:
		case USEFUL_OBJECT_CLASS_REFERENCE:
		case OBJECT:
		case OBJECT_SET:
		case VALUE_SET:
			return new AsnNode(p);
		default:
			System.out.println("** WARNING ** SymbolTable.getType() - does not handle "+p);
			System.out.flush();
			return new AsnNode(p);
		}
	}

	public static void main(String [] args) {
		HashMap<String,Symbol> symbols = new HashMap<String,Symbol>();
		// for(String f : new String[] {}) {
		//	Tokenizer tokenizer = new Tokenizer(new File("resources/rfc/"+f));
		for(String f : new String[] {"UsefulDefinitions","informationFramework","authenticationFramework","x501","pkix2009","rfc2459","rfc2985","rfc2986","rfc3039","rfc3280","rfc4211","rfc5280","rfc5652",
				"rfc5911","rfc5912","rfc6268","rfc8017","ASN1-Object-Identifier-Module"}) {
			// for(String f : new String[] {"pkix2009","rfc2459","rfc2986","rfc3280","rfc5280","rfc5911","rfc5912","rfc6268","ASN1-Object-Identifier-Module","ASN1-CHARACTER-MODULE"}) {
			System.out.println(f);
			Tokenizer tokenizer = new Tokenizer(new File("resources/rfc/"+f+".asn"));
			try {
				SymbolTable table = new SymbolTable(tokenizer);
				// System.out.println("-----------");
				for(String key : table.symbolTable.keySet()) {
					if(!symbols.containsKey(key)) {
						// System.out.println(key+" : "+table.symbolTable.get(key).toString());
						symbols.put(key, table.symbolTable.get(key));
					} else {
						Symbol prev = symbols.get(key);
						Symbol newSymbol = table.symbolTable.get(key);
						if(!prev.getProduction().equals(newSymbol.getProduction())){
							System.out.println("Mismatched "+key+"\n\t"+prev+"\n\t"+newSymbol);
						}
					}
				}
			} catch(Exception e) {
				System.out.println(f);
				System.out.flush();
				e.printStackTrace();
				break;
			}
		}
		try {
			File output = new File("resources/rfc/symbols.txt");
			PrintStream outStream = new PrintStream(output);
			for(String s : symbols.keySet()) {
				outStream.println(s+" "+symbols.get(s));
			}
			outStream.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
}
