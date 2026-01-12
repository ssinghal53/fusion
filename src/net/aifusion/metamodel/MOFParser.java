/**
 * Copyright 2013, Sharad Singhal, All Rights Reserved
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
 * Created Dec 26, 2013 by Sharad Singhal
 * Last Modified Jan 11, 2018 by Sharad Singhal
 * Last Modified May 22, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Stack;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Recursive descent parser implementing CIM Version 3 Metamodel as described in DSP0004 and DSP0221
 * @author Sharad Singhal
 */
public class MOFParser implements Parser {

	/* 
	 * ********************************************************************
	 * Internal Class Declarations and static variables used in the parser
	 * ********************************************************************
	 */
	/**
	 * Class to define the current state of the parser
	 */
	private class ParserState {
		/** Look-ahead token for the parser */
		Token lookAheadToken = null;
		/** Current input reader */
		BufferedReader input = null;
		/** current directory (if any) */
		String directory = null;
		/** Current file (if any) */
		String file = null;
		/** Current line text (if any) */
		String line = "";
		/** Character position in line */
		int cursor = 0;
		/** Line number in current file */
		int lineNumber = 0;
		/** Length of current line */
		int lineLength = 0;
		/** Current namespace path */
		NameSpacePath path = null;
		/** Current Log Level */
		Level logLevel = Level.INFO;
		/** Current OID */
		public String oid = null;
	}

	/**
	 * Class to declare lexer tokens.
	 */
	private class Token {
		private TokenType type;
		private String value = null;
		/**
		 * Lexer Token
		 * @param t - Type of the token
		 * @param value - dataValue of the token
		 * @see TokenType
		 */
		public Token(TokenType t,String value){
			this.value = value;
			value = value.toLowerCase();
			this.type = (t == TokenType.IDENTIFIER && mofKeywords.containsKey(value)) ? mofKeywords.get(value) : t;			
			return;
		}
		/**
		 * Check if this token is of a given type
		 * @param t - expected token
		 * @return - true if this token is of the same type as the given token, false otherwise
		 */
		public boolean is(TokenType t){
			// tokens that require special handling
			if(t == TokenType.ALIAS_IDENTIFIER && value.startsWith("$")){
				// ALIAS_IDENTIFIERS start with '$'
				return true;
			} else if(t == TokenType.POLICY){
				// POLICY will also match FLAVOR
				return type == TokenType.POLICY || type == TokenType.FLAVOR;
			} else if(t == TokenType.IDENTIFIER){
				return isIdentifier();
			}
			// return true if this token matches the expected token
			return t == type;
		}
		/**
		 * (Re)set the type of this token
		 * @param t - new token type
		 */
		public void setType(TokenType t){
			type = t;
			return;
		}

		/**
		 * Get a string representation of this token
		 * @return string containing TokenType:[TokenValue]
		 */
		public String toString(){
			return type + " : ["+value+"]";
		}
		/**
		 * Check if this token is a primitive data type
		 * @return - true if this token is a primitive data type, false otherwise
		 */
		public boolean isPrimitive(){
			// primitiveType = DT_Integer / DT_Real / DT_STRING / DT_DATETIME / DT_BOOLEAN / DT_OCTETSTRING / DT_CHAR16
			return isInteger() || isReal() || isString() || type == TokenType.DATETIME || type == TokenType.BOOLEAN || type == TokenType.OCTET_STRING ||
					type == TokenType.CHAR16;
		}
		/**
		 * Check if this token is an integer type
		 * @return - true if this token is a signed or unsigned integer, false otherwise
		 */
		public boolean isInteger(){
			return isUnsignedInteger() || isSignedInteger();
		}
		/**
		 * Check if this token is an unsigned integer type
		 * @return - true if this token is an unsigned integer, false otherwise
		 */
		public boolean isUnsignedInteger(){
			return type == TokenType.UINT8 || type == TokenType.UINT16 || type == TokenType.UINT32 || type == TokenType.UINT64;
		}
		/**
		 * Check if this token is a signed integer type
		 * @return - true if this token is a signed integer, false otherwise
		 */
		public boolean isSignedInteger(){
			return type == TokenType.SINT8 || type == TokenType.SINT16 || type == TokenType.SINT32 || type == TokenType.SINT64;
		}
		/**
		 * Check if this token type is a real number (REAL32 or REAL64)
		 * @return - true if this token is a real number, false otherwise
		 */
		public boolean isReal(){
			return type == TokenType.REAL32 || type == TokenType.REAL64;
		}
		/**
		 * Check if this token is TokenType.STRING
		 * @return - true if this token is a string, false otherwise
		 */
		public boolean isString(){
			return type == TokenType.STRING;
		}
		/**
		 * Check if this Token can be used as an IDENTIFIER<br>
		 * Note that MOF keywords may be usable as identifiers-- this checks if the token can be used as an identifier
		 * @return - true if this token can be used as an identifier, false otherwise
		 */
		public boolean isIdentifier(){
			if(!Character.isJavaIdentifierStart(value.charAt(0))) return false;
			for(int i = 1; i < value.length(); i++){
				if(!Character.isJavaIdentifierPart(value.charAt(i))) return false;
			}
			return true;
		}
	}

	/**
	 * Class to declare known token types
	 */
	private enum TokenType {
		ANY,AS,ASCII,ASSOCIATION,ALIAS_IDENTIFIER,BOOLEAN,CHAR16,CLASS,COLON,COMMA,COMMENT,DATETIME,ENUMERATION,ENUMERATION_VALUE,EQUALS,ERROR,EOF,FALSE,
		FLAVOR,IDENTIFIER,INCLUDE,INSTANCE,INTERFACE,LBRACE,LBRACKET,LPAREN,MARK,METHOD,NULL,NUMBER,OCTET_STRING,OF,PARAMETER,POLICY,PRAGMA,PROPERTY, 
		QUALIFIER,QUOTED_CHARACTER,RBRACE,RBRACKET,REAL32,REAL64,REF,RESTRICTED,RPAREN,SCOPE,SEMICOLON,SINT8,SINT16,SINT32,SINT64,
		START,STRING,STRING_VALUE,STRUCTURE,TRUE,UINT8,UINT16,UINT32,UINT64,VALUE,VOID,WHITE_SPACE
		;
	}

	/** Logger for the parser */
	private static final Logger logger = Logger.getLogger(MOFParser.class.getName());

	/** Map containing known mof keywords and corresponding token types*/
	private static HashMap<String,TokenType> mofKeywords = new HashMap<String,TokenType>();

	static {
		mofKeywords.put("any",TokenType.ANY);
		mofKeywords.put("as",TokenType.AS);
		mofKeywords.put("boolean", TokenType.BOOLEAN);
		mofKeywords.put("association",TokenType.ASSOCIATION);
		mofKeywords.put("char16",TokenType.CHAR16);
		mofKeywords.put("class",TokenType.CLASS);
		mofKeywords.put("datetime",TokenType.DATETIME);
		mofKeywords.put("enumeration",TokenType.ENUMERATION);
		mofKeywords.put("enumerationvalue",TokenType.ENUMERATION_VALUE);
		mofKeywords.put("false",TokenType.FALSE);
		mofKeywords.put("flavor",TokenType.FLAVOR);
		mofKeywords.put("include",TokenType.INCLUDE);
		mofKeywords.put("instance",TokenType.INSTANCE);
		mofKeywords.put("interface",TokenType.INTERFACE);
		mofKeywords.put("method",TokenType.METHOD);
		mofKeywords.put("null",TokenType.NULL);
		mofKeywords.put("octetstring",TokenType.OCTET_STRING);
		mofKeywords.put("of",TokenType.OF);
		mofKeywords.put("parameter",TokenType.PARAMETER);
		mofKeywords.put("policy",TokenType.POLICY);
		mofKeywords.put("property",TokenType.PROPERTY);
		mofKeywords.put("qualifier",TokenType.QUALIFIER);
		mofKeywords.put("real32",TokenType.REAL32);
		mofKeywords.put("real64",TokenType.REAL64);
		mofKeywords.put("ref", TokenType.REF);
		mofKeywords.put("restricted",TokenType.RESTRICTED);
		mofKeywords.put("scope",TokenType.SCOPE);
		mofKeywords.put("sint8",TokenType.SINT8);
		mofKeywords.put("sint16",TokenType.SINT16);
		mofKeywords.put("sint32",TokenType.SINT32);
		mofKeywords.put("sint64",TokenType.SINT64);
		mofKeywords.put("string",TokenType.STRING);
		mofKeywords.put("structure",TokenType.STRUCTURE);
		mofKeywords.put("true",TokenType.TRUE);
		mofKeywords.put("uint8",TokenType.UINT8);
		mofKeywords.put("uint16",TokenType.UINT16);
		mofKeywords.put("uint32",TokenType.UINT32);
		mofKeywords.put("uint64",TokenType.UINT64);
		mofKeywords.put("value",TokenType.VALUE);
		mofKeywords.put("void",TokenType.VOID);
	};

	/*
	 * *******************************
	 * Parser variables
	 * *******************************
	 */

	/** Current parser stack */
	private Stack<ParserState> inputStack = new Stack<ParserState> ();
	/** Parser state */
	private ParserState p;
	/** debugging flag */
	private static boolean debug = false;
	/** number of productions parsed */
	private int productionNumber = 0;
	/** Repository being used for the parser */
	private Repository repository;
	/** Repository used to look up known definitions */
	private Repository definitions;
	/** Globally aliased instance values defined in the parser */
	private HashMap<String, CimInstance> aliasedInstanceValues = new HashMap<String,CimInstance>();
	/** Globally aliased Structure Values defined during parsing */
	private HashMap<String, StructureValue> aliasedStructureValues = new HashMap<String,StructureValue>();
	/** Instances that contain aliased references that need resolution */
	private HashSet<CimInstance> instancesToResolve = new HashSet<CimInstance>();
	/** Structure values that contain aliased references that need resolution */
	private HashSet<StructureValue> structureValuesToResolve = new HashSet<StructureValue>();

	/**
	 * Create a MOF Parser with a default in-memory repository
	 */
	public MOFParser(){
		return;
	}

	/**
	 * Create a MOF Parser with the given repository
	 * @param repository - CIM Repository to use for the parser
	 */
	public MOFParser(Repository repository){
		this.repository = repository;
		return;
	}
	
	/**
	 * Create a MOF parser with a definition repository
	 * @param repository - repository to parse new input
	 * @param definitions - repository containing pre-existing definitions (not modified)
	 */
	public MOFParser(Repository repository, Repository definitions) {
		this.repository = repository;
		this.definitions = definitions;
		return;
	}

	/**
	 * Parse a file
	 * @param fileName - name of the file to parse
	 * @param path - default namespace path to use during parsing
	 */
	public void parse(String fileName, NameSpacePath path){
		try {
			p = new ParserState();
			int index = fileName.lastIndexOf('/');
			if(index >= 0) {
				p.directory = fileName.substring(0,index+1);
				p.file = fileName.substring(index+1);
			} else {
				p.directory = "";
				p.file = fileName;
			}
			if(path != null) p.path = path;
			if(debug) System.out.println("Parsing File: ["+p.directory+"]"+p.file);
			p.input = new BufferedReader(new FileReader(p.directory+p.file));
			parse(p.input, path);
		} catch (FileNotFoundException e) {
			// If we don't find the file, try reading it as a resource
			InputStream resourceAsStream = getClass().getClassLoader().getResourceAsStream(fileName);
			if (resourceAsStream != null) {
				p.input = new BufferedReader(new InputStreamReader(resourceAsStream));
				parse(p.input, path);
			} else {
				logger.warning("MofParser - Could not read file "+fileName);
				throw new ModelException(ExceptionReason.NOT_FOUND,"MofParser: File "+fileName+" not found or not readable");
			}
		}
		return;
	}

	/*
	 * (non-Javadoc)
	 * @see com.hp.sspcim.metamodel.parsers.CimParser#parse(java.io.InputStream)
	 */
	public void parse(InputStream in, NameSpacePath path){
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));
		p = null;
		parse(reader, path);
		return;
	}

	/**
	 * Parse model constructs from a buffered reader
	 * @param in - reader to parse
	 */
	public synchronized void parse(BufferedReader in, NameSpacePath path){
		long startTime = System.currentTimeMillis();
		productionNumber = 0;
		try {
			// create a parser state if not set
			if(p == null){
				p = new ParserState();
			}
			// initialize the current parser state
			p.input = in;
			p.line = "";
			p.lineLength = p.lineNumber = p.cursor = 0;
			if(p.directory == null) p.directory = "";
			if(p.file == null) p.file = "";
			if(repository == null) repository = path != null ? new InMemoryRepository(path) : new InMemoryRepository();
			if(p.path == null) p.path = path != null ? path : Constants.defaultNameSpacePath;
			if(!aliasedInstanceValues.isEmpty()) aliasedInstanceValues.clear();
			if(!aliasedStructureValues.isEmpty()) aliasedStructureValues.clear();

			nextToken();			// get first token for look-ahead
			mofSpecification();		// parse the specification
			// resolveReferences();
			// TODO: update and write instances dependent on aliased instance values and aliased structure values
		} catch(Exception e){
			StringBuilder b = new StringBuilder("*******************\n");
			b.append("* Error: ");
			b.append(e.toString());
			b.append("\n");
			if(p.file != null) b.append("* File: "+p.file+"\n");
			if(p.directory != null) b.append("* Directory: "+p.directory+"\n");
			if(p.path != null) b.append("* Name Space Path: "+p.path+"\n");
			b.append("* Line "+p.lineNumber+" Column "+p.cursor+"\n");
			b.append("* "+p.line+"\n");
			b.append("* LookAhead: "+p.lookAheadToken.type.toString()+" ["+p.lookAheadToken.value+"]\n");
			b.append("*******************\n");
			// log the error
			// logger.log(Level.SEVERE, b.toString(), e);
			// re-throw all exceptions as ModelExceptions
			if(e instanceof ModelException)
				throw new ModelException(((ModelException) e).getReason(),b.toString(),e);
			else 
				throw new ModelException(b.toString(),e);
		}
		if(debug) System.out.println("Parsed "+productionNumber+" productions in "+(System.currentTimeMillis()-startTime)+" ms");
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Parser#parseValues(java.lang.String)
	 */
	@Override
	public StructureValue parseValue(String indication, NameSpacePath path){
		try {
			return parseValue(new ByteArrayInputStream(indication.getBytes("UTF-8")),path);
		} catch (UnsupportedEncodingException e) {
			throw new ModelException("Internal error -- converting string to input stream");
		}
	}
	
	/**
	 * Parse a property value from the input stream
	 * @param path - path of the element to use for the property
	 * @param propertyName - name of the property to parse
	 * @param inputStream - input stream containing property value
	 * @return - data value for the property
	 */
	public DataValue parsePropertyValue(ObjectPath path, String propertyName, InputStream inputStream) {
		try {
			// create a parser state if not set
			if(p == null){
				p = new ParserState();
			}
			// initialize the current parser state
			p.input = new BufferedReader(new InputStreamReader(inputStream));
			p.line = "";
			p.lineLength = p.lineNumber = p.cursor = 0;
			p.directory = "";
			p.file = "";
			if(repository == null) repository = path != null ? new InMemoryRepository(path.getNameSpacePath()) : new InMemoryRepository();
			if(p.path == null) p.path = path != null ? path.getNameSpacePath() : Constants.defaultNameSpacePath;
			if(!aliasedInstanceValues.isEmpty()) aliasedInstanceValues.clear();
			if(!aliasedStructureValues.isEmpty()) aliasedStructureValues.clear();
			nextToken();			// get first token for look-ahead
			NamedElement target = get(path);
			if(target == null) throw new ModelException(ExceptionReason.NOT_FOUND,"Path "+path+" not found");
			DataValue value = null;
			CimStructure cls = null;
			switch(target.getElementType()){
			case CLASS:
			case STRUCTURE:
				cls = (CimStructure) target;
				break;
			case STRUCTUREVALUE:
				cls = ((StructureValue) target).getCreationStruct();
				break;
			case INSTANCE:
				cls = ((CimInstance)target).getCreationClass();
				break;
			case ENUMERATION:
				CimEnumeration enumeration = (CimEnumeration) target;
				value = enumerationTypeValue(enumeration.getDataType(),enumeration);
				return value;
			default:
				throw new ModelException("MOFParser#parsePropertyValue() does not handle elements of type "+target.getElementType());
			}
			CimProperty p = cls.getProperty(propertyName);
			DataType dt = p.getDataType();
			if(dt.isPrimitive()){
				value = primitiveTypeValue(dt);
			} else if(dt.isEnumerationValue()){
				value = enumerationTypeValue(dt, p.getEnum());
			} else if(dt.isStructureValue()){
				value = complexTypeValue(dt, p.getStruct());
			} else if(dt.isReference()){
				value = referenceTypeValue(dt, p.getRefClassName());
			} else {
				throw new ModelException("Data type "+dt+" not handled in MOFParser#parseParameters");
			}
			return value;
		} catch(Exception e){
			StringBuilder b = new StringBuilder("*******************\n");
			b.append("* Error: ");
			b.append(e.toString());
			b.append("\n");
			if(p.file != null) b.append("* File: "+p.file+"\n");
			if(p.directory != null) b.append("* Directory: "+p.directory+"\n");
			if(p.path != null) b.append("* Name Space Path: "+p.path+"\n");
			b.append("* Line "+p.lineNumber+" Column "+p.cursor+"\n");
			b.append("* "+p.line+"\n");
			b.append("* LookAhead: "+p.lookAheadToken.type.toString()+" ["+p.lookAheadToken.value+"]\n");
			b.append("*******************\n");
			// log the error
			// logger.log(Level.SEVERE, b.toString(), e);
			// re-throw all exceptions as ModelExceptions
			if(e instanceof ModelException)
				throw new ModelException(((ModelException) e).getReason(),b.toString());
			else 
				throw new ModelException(b.toString(),e);
		}		
	}
	
	/**
	 * Parse a list of comma separated parameter values from the input stream
	 * @param params - expected parameters. At return values are set in the parameters
	 * @param in - input stream to parse
	 */
	public void parseParameters(List<CimParameter> params, InputStream in){
		try {
			// create a parser state if not set
			if(p == null){
				p = new ParserState();
			}
			// initialize the current parser state
			p.input = new BufferedReader(new InputStreamReader(in));
			p.line = "";
			p.lineLength = p.lineNumber = p.cursor = 0;
			p.directory = "";
			p.file = "";
			if(repository == null) repository = p.path == null ? new InMemoryRepository() : new InMemoryRepository(p.path);
			if(p.path == null) p.path = Constants.defaultNameSpacePath;
			if(!aliasedInstanceValues.isEmpty()) aliasedInstanceValues.clear();
			if(!aliasedStructureValues.isEmpty()) aliasedStructureValues.clear();

			nextToken();			// get first token for look-ahead
			// TODO: -- for each parameter in the parameter list, parse the corresponding data value
			for(int i = 0; i < params.size(); i++){
				CimParameter p = params.get(i);
				DataType dt = p.getDataType();
				DataValue value = null;
				if(dt.isPrimitive()){
					value = primitiveTypeValue(dt);
				} else if(dt.isEnumerationValue()){
					value = enumerationTypeValue(dt, p.getEnum());
				} else if(dt.isStructureValue()){
					value = complexTypeValue(dt, p.getStruct());
				} else if(dt.isReference()){
					value = referenceTypeValue(dt, p.getRefClassName());
				} else {
					throw new ModelException("Data type "+dt+" not handled in MOFParser#parseParameters");
				}
				p.setValue(value);
				if(i < params.size()-1) skipOver(TokenType.COMMA);
			}
		} catch(Exception e){
			StringBuilder b = new StringBuilder("*******************\n");
			b.append("* Error: ");
			b.append(e.toString());
			b.append("\n");
			if(p.file != null) b.append("* File: "+p.file+"\n");
			if(p.directory != null) b.append("* Directory: "+p.directory+"\n");
			if(p.path != null) b.append("* Name Space Path: "+p.path+"\n");
			b.append("* Line "+p.lineNumber+" Column "+p.cursor+"\n");
			b.append("* "+p.line+"\n");
			b.append("* LookAhead: "+p.lookAheadToken.type.toString()+" ["+p.lookAheadToken.value+"]\n");
			b.append("*******************\n");
			// log the error
			// logger.log(Level.SEVERE, b.toString(), e);
			// re-throw all exceptions as ModelExceptions
			if(e instanceof ModelException)
				throw new ModelException(((ModelException) e).getReason(),b.toString());
			else 
				throw new ModelException(b.toString(),e);
		}
		return;
	}

	/**
	 * Parse values from an input stream
	 * @param in - input stream to parse values from
	 * @param path - namespace path to be used as initial path for the incoming values
	 * @return - list of structure values
	 */
	public StructureValue parseValue(InputStream in, NameSpacePath path) {
		try {
			// create a parser state if not set
			if(p == null){
				p = new ParserState();
			}
			// initialize the current parser state
			p.input = new BufferedReader(new InputStreamReader(in));
			p.line = "";
			p.lineLength = p.lineNumber = p.cursor = 0;
			p.directory = "";
			p.file = "";
			if(repository == null) repository = path != null ? new InMemoryRepository(path) : new InMemoryRepository();
			if(p.path == null) p.path = path != null ? path : Constants.defaultNameSpacePath;
			if(!aliasedInstanceValues.isEmpty()) aliasedInstanceValues.clear();
			if(!aliasedStructureValues.isEmpty()) aliasedStructureValues.clear();

			nextToken();			// get first token for look-ahead
			return valueSpecification();		// parse the specification
		} catch(Exception e){
			StringBuilder b = new StringBuilder("*******************\n");
			b.append("* Error: ");
			b.append(e.toString());
			b.append("\n");
			if(p.file != null) b.append("* File: "+p.file+"\n");
			if(p.directory != null) b.append("* Directory: "+p.directory+"\n");
			if(p.path != null) b.append("* Name Space Path: "+p.path+"\n");
			b.append("* Line "+p.lineNumber+" Column "+p.cursor+"\n");
			b.append("* "+p.line+"\n");
			b.append("* LookAhead: "+p.lookAheadToken.type.toString()+" ["+p.lookAheadToken.value+"]\n");
			b.append("*******************\n");
			// log the error
			// logger.log(Level.SEVERE, b.toString(), e);
			// re-throw all exceptions as ModelExceptions
			if(e instanceof ModelException)
				throw new ModelException(((ModelException) e).getReason(),b.toString());
			else 
				throw new ModelException(b.toString(),e);
		}
	}

	/**
	 * Generate an error with a FAILED reason and a string explanation
	 * @param information - string to explain the error
	 */
	private void error(String information){
		throw new ModelException(information);
	}

	/**
	 * Generate an error with a given reason and explanation
	 * @param reason - reason for the exception
	 * @param information - additional information for explanation
	 */
	private void error(ExceptionReason reason, String information){
		throw new ModelException(reason,information);
	}

	/**
	 * Generate a warning in the log
	 * @param warning - string containing warning
	 */
	private void warn(String warning){
		logger.warning(warning);
		return;
	}

	/**
	 * Skip over an expected token (from lookahead token) and read the next token from the input into the lookahead token
	 * @param expected - Expected token type to skip over
	 * @return - Expected token, if successful
	 * @throws Exception - if the lookahead token does not match the expected token
	 */
	private Token skipOver(TokenType expected) {
		Token t = p.lookAheadToken;
		if(!t.is(expected)){
			// the expected token could not be found. Throw an exception
			error("Parse Error - expected "+expected.toString()+" found "+t.toString());
		}
		if(debug) System.out.println("Skip Over: "+t.type+" ["+t.value+"] Expected: ("+expected+")");
		// reset the type to be expected. This is needed because several keywords can also be used as identifiers
		if(t.type != expected) t.setType(expected);
		// fetch the next token into p.lookaheadToken unless we are finished
		if(expected != TokenType.EOF) nextToken();
		// return the current token
		return t;
	}
	
	private NamedElement get(ObjectPath path) {
		NamedElement target = repository.get(path);
		if(target != null) return target;
		if(definitions != null) {
			target = definitions.get(path);
			if(target != null) return target;
		}
		return null;
	}
	
	private boolean contains(ObjectPath path) {
		if(repository.contains(path)) return true;
		return definitions != null ? definitions.contains(path) : false;
	}
	
	private List<NameSpacePath> getNameSpaces(){
		Vector<NameSpacePath> nameSpaces = new Vector<NameSpacePath>();
		nameSpaces.addAll(repository.getNameSpaces());
		for(NameSpacePath p : definitions.getNameSpaces()) {
			if(!nameSpaces.contains(p)) {
				nameSpaces.add(p);
			}
		}
		return nameSpaces;
	}

	/*
	 * ********************************
	 * MOF grammar rules start here
	 * ********************************
	 */

	/**
	 * obtain a stand-alone value specification for a structure value
	 */
	private StructureValue valueSpecification() {
		// StructureValue = VALUE OF structureName [ alias ] propertyValueList ";"
		// propertyValueList = "{" *propertySlot "}"
		// propertySlot = featureName "=" propertyValue ";"
		// propertyValue = primitiveTypeValue / complexTypeValue / referenceTypeValue / enumTypeValue
		// alias = AS aliasIdentifier

		// we will accept #pragma namespace to position the indication
		if(p.lookAheadToken.is(TokenType.PRAGMA)){
			skipOver(TokenType.PRAGMA);
			String pragmaName = skipOver(TokenType.IDENTIFIER).value;	// pragma name
			skipOver(TokenType.LPAREN);
			String pragmaParameter = stringValue();		// pragma parameter
			skipOver(TokenType.RPAREN);
			if("namespace".equalsIgnoreCase(pragmaName)){	// #pragma namespace ()
				// switch the name space. Lookahead token stays the same
				if(debug) System.out.println("Switching name space to "+pragmaParameter);
				p.path = new NameSpacePath(pragmaParameter);
			} else {
				warn("Pragma "+pragmaName+" "+pragmaParameter+" ignored");
			}
		}
		// [VALUE OF] is optional
		if(p.lookAheadToken.is(TokenType.VALUE)) {
			skipOver(TokenType.VALUE);
			skipOver(TokenType.OF);
		}

		// locate the name of the structure, class, or association
		String structName = skipOver(TokenType.IDENTIFIER).value;
		CimStructure expected = (CimStructure) get(new ObjectPath(ElementType.STRUCTURE,structName,p.path, null, null));
		if(expected == null) error(ExceptionReason.NOT_FOUND,"CimStructure "+structName+" is not defined");
		String alias = null;
		if(p.lookAheadToken.is(TokenType.AS)){
			skipOver(TokenType.AS);
			alias = skipOver(TokenType.ALIAS_IDENTIFIER).value;
		}
		// get the definition
		return getStructureValue(alias, expected);
	}

	/**
	 * MOF Specification
	 */
	private void mofSpecification() {
		// mofSpecification = *mofProduction
		while(!p.lookAheadToken.is(TokenType.EOF)) mofProduction();
		return;
	}

	/**
	 * MOF Production
	 */
	private void mofProduction(){
		// mofProduction = compilerDirective / structureDeclaration / classDeclaration / associationDeclaration / 
		//		enumerationDeclaration / instanceDeclaration / qualifierDeclaration
		//		/ interfaceDeclaration ; Interface declarations are extensions to DSP0004

		// compiler directives do not take qualifier lists
		if(p.lookAheadToken.is(TokenType.PRAGMA)){
			// all compiler directives start with #pragma
			compilerDirective();
			return;
		}
		// Get qualifier list, if any
		List<Qualifier> quals = qualifierList();

		if(p.lookAheadToken.is(TokenType.QUALIFIER)){			// qualifier Type declaration
			qualifierTypeDeclaration(quals);	
		} else if(p.lookAheadToken.is(TokenType.ENUMERATION)){	// enumeration declaration
			enumerationTypeDeclaration(quals);
		} else if(p.lookAheadToken.is(TokenType.STRUCTURE)){	// structure declaration
			structureDeclaration(quals);
		} else if(p.lookAheadToken.is(TokenType.CLASS) || p.lookAheadToken.is(TokenType.INTERFACE) || p.lookAheadToken.is(TokenType.ASSOCIATION)){
			classDeclaration(quals);		// class, interface, or association declarations
		} else if(p.lookAheadToken.is(TokenType.INSTANCE)){
			if(!quals.isEmpty()) error("Instances do not accept qualifiers");
			instanceDeclaration(quals);		// instance or structure value declaration
		} else if(p.lookAheadToken.is(TokenType.VALUE)){
			if(!quals.isEmpty()) error("Structure Values do not accept qualifiers");
			structureValueDeclaration();
		} else if(p.lookAheadToken.is(TokenType.IDENTIFIER)){
			// Have an instance or structure value declared as ClassOrStructName [Alias] { ... };
			// Note that ClassOrStructName must exist in the repository here. This is an extension to DSP0004
			String classOrStructName = skipOver(TokenType.IDENTIFIER).value;
			String alias = getAlias();
			// check if a structure is defined with this name-- if so we must get a StructureValue
			ObjectPath path = new ObjectPath(ElementType.STRUCTURE,classOrStructName,p.path,null, null);
			if(contains(path)){
				CimStructure expected = (CimStructure) get(path);
				StructureValue value = getStructureValue(alias, expected);
				if(alias != null) aliasedStructureValues.put(alias, value);
			} else {
				// check if a class is defined with this name, if so, we must get an Instance
				path = new ObjectPath(ElementType.CLASS,classOrStructName,p.path,null, null);
				if(contains(path)){
					CimClass expected = (CimClass) get(path);
					CimInstance instance = getInstanceValue(alias,expected);
					if(alias != null) aliasedInstanceValues.put(alias, instance);
					repository.put(instance);
				} else {
					// no class or structure definition found, have an error
					error(ExceptionReason.NOT_FOUND,"Class or Structure not in repository: "+classOrStructName);
				}
				skipOver(TokenType.SEMICOLON);
			}
		} else if(!p.lookAheadToken.is(TokenType.EOF)){
			error("Parse failed");
		}
		productionNumber++;
		return;
	}

	/**
	 * Rules to manage a compiler directive. Currently only PRAGMA include(file) is handled
	 * @throws Exception - in case of error
	 */
	private void compilerDirective() {
		// compilerDirective = PRAGMA ( pragmaName / standardPragmaName ) "(" pragmaParameter ")"
		// pragmaName = IDENTIFIER
		// standardPragmaName = INCLUDE
		// pragmaParameter = stringValue ; if the pragma is INCLUDE, the parameter value shall represent a relative or full file path
		// PRAGMA = "#pragma" ; keyword: case insensitive
		// INCLUDE = "include" ; keyword: case insensitive
		skipOver(TokenType.PRAGMA);
		String pragmaName = skipOver(TokenType.IDENTIFIER).value;	// pragma name
		skipOver(TokenType.LPAREN);
		String pragmaParameter = stringValue();		// pragma parameter
		skipOver(TokenType.RPAREN);
		if("include".equalsIgnoreCase(pragmaName)){			// #pragma include ()
			// include a new file. Push current parser state (including the lookahead token) onto the input stack,
			// open new input file, and get the next token
			openNewInput(pragmaParameter);
			nextToken();
		} else if("namespace".equalsIgnoreCase(pragmaName)){	// #pragma namespace ()
			// switch the name space. Lookahead token stays the same
			if(debug) System.out.println("Switching name space to "+pragmaParameter);
			p.path = new NameSpacePath(pragmaParameter);
		} else if("debug".equalsIgnoreCase(pragmaName)){		// #pragma debug ("on" | "off")
			// turn compiler debugging on or off
			debug = "on".equalsIgnoreCase(pragmaParameter);
		} else if("oid".equalsIgnoreCase(pragmaName)) {
			if("null".equalsIgnoreCase(pragmaParameter)) {
				p.oid = null;
			} else {
				p.oid = pragmaParameter;
				if(!p.oid.matches("^\\d+(\\.\\d+)*$")) error(ExceptionReason.INVALID_PARAMETER,
					"Expected OID of the form nn[.nn]*, found "+pragmaParameter);
			}
		} else {
			warn("Pragma "+pragmaName+" "+pragmaParameter+" ignored");
		}
		return;
	}

	/**
	 * Global qualifier type declaration. The QualifierType is added to the repository
	 * @param quals - qualifiers associated with this qualifier type
	 */
	private void qualifierTypeDeclaration(List<Qualifier> quals){
		// CIM V2 Qualifiers
		// qualifierDeclaration = QUALIFIER qualifierName qualifierType scope [ defaultFlavor ] ";"
		// qualifierName = IDENTIFIER
		// qualifierType = ":" dataType [ array ] [ defaultValue ]
		// scope = "," SCOPE "(" metaElement *( "," metaElement ) ")"
		// metaElement = CLASS / ASSOCIATION / INDICATION / QUALIFIER // PROPERTY / REFERENCE / METHOD / PARAMETER / ANY
		// defaultFlavor = "," FLAVOR "(" flavor *( "," flavor ) ")"
		
		// Cim V3 Qualifiers
		// qualifierTypeDeclaration = [ qualifierList ] QUALIFIER qualifierName ":" qualifierType qualifierScope [ qualifierPolicy ] ";"		
		skipOver(TokenType.QUALIFIER);
		String qualifierName = skipOver(TokenType.IDENTIFIER).value;	// qualifierName = elementName
		if("UMLPackagePath".equalsIgnoreCase(qualifierName)) qualifierName = StandardQualifierType.PACKAGEPATH.getMofName();
		skipOver(TokenType.COLON);
		// default data type and value of the qualifier type
		DataType qType = null;
		DataValue v = null;
		// qualifierType = primitiveQualifierType / enumQualiferType
		if(p.lookAheadToken.isPrimitive()){
			// primitiveQualifierType = primitiveType [ array ] [ "=" primitiveTypeValue ]
			qType = DataType.valueOf(p.lookAheadToken.value.toUpperCase());
			nextToken();
			if(array()){
				qType = qType.getArrayType();
			}
			if(p.lookAheadToken.is(TokenType.EQUALS)){
				skipOver(TokenType.EQUALS);
				v = primitiveTypeValue(qType);
			}
		} else {
			// enumQualiferType = enumName [ array ] "=" enumTypeValue
			String enumName = skipOver(TokenType.IDENTIFIER).value;
			CimEnumeration en = (CimEnumeration) get(new ObjectPath(ElementType.ENUMERATION,enumName,p.path, null, null));
			if(en == null) error(ExceptionReason.NOT_FOUND,"QualifierType "+qualifierName+": Enumeration "+enumName+" not defined");
			qType = DataType.ENUMERATIONVALUE;
			if(array()){
				qType = qType.getArrayType();
			}
			skipOver(TokenType.EQUALS);
			v = enumerationTypeValue(qType,en);
		}
		
		boolean isV2Qualifier = false;
		// V2 qualifiers have a comma before SCOPE
		if(p.lookAheadToken.is(TokenType.COMMA)) {
			isV2Qualifier = true;
			skipOver(TokenType.COMMA);
			// map conflicting qualifiers between v2 and v3 MOFs
			if(qualifierName.equalsIgnoreCase("Override")) {
				qType = StandardQualifierType.OVERRIDE.getDataType();
				v = StandardQualifierType.OVERRIDE.getDefaultValue();
			}
		}
		
		// qualifierScope = SCOPE "(" ANY / scopeKindList ")"
		skipOver(TokenType.SCOPE);
		skipOver(TokenType.LPAREN);
		Vector<Scope> scopes = new Vector<Scope>();
		// scopeKindList = scopeKind *( "," scopeKind )
		// scopeKind = STRUCTURE / CLASS / ASSOCIATION / ENUMERATION / ENUMERATIONVALUE /
		// PROPERTY / REFPROPERTY / METHOD / PARAMETER / QUALIFIERTYPE 
		scopes.add(Scope.valueOf(skipOver(TokenType.IDENTIFIER).value.toUpperCase()));
		if(!scopes.contains(Scope.ANY)){
			while(p.lookAheadToken.is(TokenType.COMMA)){
				skipOver(TokenType.COMMA);
				scopes.add(Scope.valueOf(skipOver(TokenType.IDENTIFIER).value.toUpperCase()));
			}
		}
		skipOver(TokenType.RPAREN);
		Policy policy = null;	// null => Policy = ENABLEOVERRIDE
		if(isV2Qualifier && p.lookAheadToken.is(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			skipOver(TokenType.FLAVOR);
			skipOver(TokenType.LPAREN);
			// flavor = ENABLEOVERRIDE / DISABLEOVERRIDE / RESTRICTED / TOSUBCLASS / TRANSLATABLE
			Vector<String> flavors = new Vector<String>();
			while(!p.lookAheadToken.is(TokenType.RPAREN)) {
				flavors.add(skipOver(TokenType.IDENTIFIER).value.toUpperCase());
				if(p.lookAheadToken.is(TokenType.COMMA)) skipOver(TokenType.COMMA);
			}
			skipOver(TokenType.RPAREN);
			if(flavors.contains("RESTRICTED")) {
				policy = Policy.RESTRICTED;
			} else if(flavors.contains("DISABLEOVERRIDE")){
				policy = Policy.DISABLEOVERRIDE;
			} else {
				policy = Policy.ENABLEOVERRIDE;
			}
		} else {
			// qualifierPolicy = POLICY "(" policyKind ")" 
			if(p.lookAheadToken.is(TokenType.POLICY)){
				// policyKind = DISABLEOVERRIDE / ENABLEOVERRIDE / RESTRICTED 
				skipOver(TokenType.POLICY);
				skipOver(TokenType.LPAREN);
				policy = Policy.valueOf(skipOver(TokenType.IDENTIFIER).value.toUpperCase());
				skipOver(TokenType.RPAREN);
			}
		}
		skipOver(TokenType.SEMICOLON);
		QualifierType qt = new QualifierType(qualifierName, qType, v, scopes, policy, quals, p.path);
		repository.put(qt);
		return;
	}

	/**
	 * Global Enumeration declaration. The enumeration is added to the repository
	 * @param quals - qualifiers on this Enumeration
	 */
	private void enumerationTypeDeclaration(List<Qualifier> quals) {
		repository.put(getEnumeration(quals,null, null));
		return;
	}

	/**
	 * Get an Enumeration declaration<br/>
	 * Note that Enumerations can be defined with global scope, or within a class or structure
	 * @param quals - qualifiers - qualifiers, if any, defined for this enumeration
	 * @param localFeatures - locally defined features, if any, within the class being constructed
	 * @param superType - superType, if any, of the local class or structure being constructed
	 * @return - enumeration value
	 */
	private CimEnumeration getEnumeration(List<Qualifier>quals,List<QualifiedElement>localFeatures, CimStructure superType){
		// enumDeclaration = enumTypeHeader enumName ":" enumTypeDeclaration ";"
		// enumTypeHeader = [ qualifierList ] ENUMERATION 
		// enumName = elementName
		// enumTypeDeclaration = ( DT_Integer / enumName ) integerEnumDeclaration / ( DT_STRING / enumName ) stringEnumDeclaration
		// integerEnumDeclaration = "{" [ integerEnumElement *( "," integerEnumElement ) ] "}"
		// stringEnumDeclaration = "{" [ stringEnumElement *( "," stringEnumElement ) ] "}"
		// integerEnumElement = [ qualifierList ] enumLiteral "=" integerValue
		// stringEnumElement = [ qualifierList ] enumLiteral [ "=" stringValue ]
		// enumLiteral = IDENTIFIER
		// ENUMERATION = "enumeration" ; keyword: case insensitive

		skipOver(TokenType.ENUMERATION);
		String enumName = skipOver(TokenType.IDENTIFIER).value;	// enumeration name
		skipOver(TokenType.COLON);
		String fullEnumName = enumName;	// full name for enumeration (used for values)
		DataType dataType = null;		// data type for this enumeration
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();	// values, if any
		CimEnumeration superEnum = null;	// supertype of enumeration, if any

		// get the data type of the enum
		if(p.lookAheadToken.isInteger() || p.lookAheadToken.isString()){	// have an integer or String typed enum
			dataType = DataType.valueOf(skipOver(p.lookAheadToken.type).value.toUpperCase());
		} else {	// have an superEnumType
			String superEnumName = skipOver(TokenType.IDENTIFIER).value;	// supertype name
			// check the locally defined features for the superEnumType
			if(localFeatures != null && !localFeatures.isEmpty()){
				for(QualifiedElement e : localFeatures){
					if(!(e instanceof CimEnumeration)) continue;
					if(superEnumName.equalsIgnoreCase(e.getName())){
						superEnum = (CimEnumeration) e;
						break;
					}
				}
			}
			// if not in local features, check the superType(s) of the current class for the superEnumType
			if(superEnum == null && superType != null) superEnum = superType.getEnumeration(superEnumName);
			// if not in local features and superType, check repository for superEnumType
			if(superEnum == null) superEnum = (CimEnumeration) get(new ObjectPath(ElementType.ENUMERATION,superEnumName,p.path, null, null));
			if(superEnum == null){	// error if no superEnumType found
				error(ExceptionReason.NOT_FOUND, "Enumeration "+superEnumName+" not found");
			}
			dataType = superEnum.getDataType();
			fullEnumName = superEnum.getFullName()+"."+enumName;	// update the full name
		}
		// note that values must contain fully qualified enumName
		skipOver(TokenType.LBRACE);
		if(!p.lookAheadToken.is(TokenType.RBRACE)){
			// have at least one value in this enumeration
			values.add(getEnumerationValue(fullEnumName,dataType));
			while (p.lookAheadToken.is(TokenType.COMMA)){
				skipOver(TokenType.COMMA);
				values.add(getEnumerationValue(fullEnumName,dataType));
			}
		}
		skipOver(TokenType.RBRACE);
		skipOver(TokenType.SEMICOLON);
		if(p.oid != null) updateOidInQualifierList(quals);
		return new CimEnumeration(enumName, superEnum, quals, p.path, dataType, values);
	}

	/**
	 * Get an EnumerationValue
	 * @param fullEnumName - fully qualified name of the enumeration within which this data type resides
	 * @param dataType - expected data type for this EnumerationValue
	 * @return - EnumerationValue
	 */
	private EnumerationValue getEnumerationValue(String fullEnumName, DataType dataType){
		// integerEnumElement = [ qualifierList ] enumLiteral "=" integerValue
		// stringEnumElement = [ qualifierList ] enumLiteral [ "=" stringValue ]

		Vector<Qualifier> qualifiers = qualifierList();
		String valueName = skipOver(TokenType.IDENTIFIER).value;	// enumLiteral
		DataValue dataValue = null;
		// note that integer enums are required to have values; they are optional for string enums
		if(dataType.isInteger() || p.lookAheadToken.is(TokenType.EQUALS)){
			skipOver(TokenType.EQUALS);
			dataValue = new DataValue(dataType,literalValue(dataType));
		}
		return new EnumerationValue(valueName, fullEnumName, dataValue, qualifiers);
	}

	/**
	 * Global structure declaration. The structure definition is added to the repository
	 * @param quals - qualifiers on this structure
	 */
	private void structureDeclaration(List<Qualifier> quals){
		// structureDeclaration = [ qualifierList ] STRUCTURE structureName [ superstructure ] "{" *structureFeature "}" ";" 
		// structureName = elementName
		// superStructure = ":" structureName
		// structureFeature = structureDeclaration / ; local structure
		// 				enumDeclaration / ; local enumeration
		//				propertyDeclaration
		// STRUCTURE = "structure" ; keyword: case insensitive
		repository.put(getStructure(quals, null, null, null));
		return;
	}


	/**
	 * Global structure value declaration
	 */
	private void structureValueDeclaration(){
		// StructureValue = VALUE OF structureName [ alias ] propertyValueList ";"
		// propertyValueList = "{" *propertySlot "}"
		// propertySlot = featureName "=" propertyValue ";"
		// propertyValue = primitiveTypeValue / complexTypeValue / referenceTypeValue / enumTypeValue
		// alias = AS aliasIdentifier
		skipOver(TokenType.VALUE);
		// OF is optional
		if(p.lookAheadToken.is(TokenType.OF)) skipOver(p.lookAheadToken.type);

		// locate the name of the structure, class, or association
		String structName = skipOver(TokenType.IDENTIFIER).value;
		CimStructure expected = (CimStructure) get(new ObjectPath(ElementType.STRUCTURE,structName,p.path, null, null));
		if(expected == null) error(ExceptionReason.NOT_FOUND,"CimStructure "+structName+" is not defined");

		String alias = null;
		if(p.lookAheadToken.is(TokenType.AS)){
			skipOver(TokenType.AS);
			alias = skipOver(TokenType.ALIAS_IDENTIFIER).value;
		}
		
		// get the definition
		StructureValue v = getStructureValue(alias, expected);
		skipOver(TokenType.SEMICOLON);
		// save the alias'd value for later use
		aliasedStructureValues.put(alias, v);
		repository.put(v);
		return;
	}


	/**
	 * Global Instance declaration
	 * @param quals - qualifiers for this instance
	 */
	private void instanceDeclaration(List<Qualifier> quals){
		// InstanceValue = INSTANCE OF ( className / associationName ) [ alias ] propertyValueList 
		// propertyValueList = "{" *propertySlot "}"
		// propertySlot = featureName "=" propertyValue ";"
		// propertyValue = primitiveTypeValue / complexTypeValue / referenceTypeValue / enumTypeValue
		// alias = AS aliasIdentifier
		skipOver(TokenType.INSTANCE);
		// OF is optional
		if(p.lookAheadToken.is(TokenType.OF)) skipOver(p.lookAheadToken.type);

		// locate the name of the class, or association
		String className = skipOver(TokenType.IDENTIFIER).value;
		CimClass expected = (CimClass) get(new ObjectPath(ElementType.CLASS,className,p.path, null, null));
		if(expected == null) error(ExceptionReason.NOT_FOUND,"Class "+className+" is not defined");

		// alias, if any
		String alias = getAlias();

		// get the definition
		CimInstance v = (CimInstance) getInstanceValue(alias, expected);
		skipOver(TokenType.SEMICOLON);

		// save the alias'd value for later resolution
		if(alias != null) aliasedInstanceValues.put(alias, v);
		repository.put(v);
		return;
	}

	/**
	 * Get an alias identifier, if any
	 * @return - alias identifier. Null is returned if no alias is defined
	 */
	private String getAlias(){
		String alias = null;
		if(p.lookAheadToken.is(TokenType.AS)){
			skipOver(TokenType.AS);
			alias = skipOver(TokenType.ALIAS_IDENTIFIER).value;
		}
		return alias;
	}


	/**
	 * Add the declaration for a class, association, or interface to the repository
	 * @param quals - qualifiers for this class, association, or interface
	 */
	private void classDeclaration(List<Qualifier> quals){
		// classDeclaration = [ qualifierList ] (CLASS | ASSOCIATION | INTERFACE) className [ superClass ] "{" *classFeature "}" ";"
		// className = elementName
		// superClass = ":" className
		// classFeature = structureFeature / methodDeclaration

		ElementType elementType = null;
		switch(p.lookAheadToken.type){
		case CLASS:
			elementType = ElementType.CLASS;
			break;
		case INTERFACE:
			elementType = ElementType.INTERFACE;
			break;
		default:
			error("Internal error-- should not happen");
		}
		skipOver(p.lookAheadToken.type);
		String elementName = skipOver(TokenType.IDENTIFIER).value;	// className
		CimStructure superType = null;
		Vector<QualifiedElement> elementFeatures = new Vector<QualifiedElement>();
		if(p.lookAheadToken.is(TokenType.COLON)){
			skipOver(TokenType.COLON);
			String superTypeName = skipOver(TokenType.IDENTIFIER).value;
			// classes can inherit from other classes or structures
			ObjectPath op = new ObjectPath(elementType,superTypeName,p.path, null, null);
			superType = (CimStructure) (contains(op) ? get(op) : get(new ObjectPath(ElementType.STRUCTURE,superTypeName,p.path, null, null)));
			if(superType == null) error(ExceptionReason.NOT_FOUND,"Class "+elementName+" SuperType "+superTypeName+" not defined");
		}
		skipOver(TokenType.LBRACE);
		addInterfaces(quals, elementFeatures);
		while(!p.lookAheadToken.is(TokenType.RBRACE)){
			List<Qualifier> featureQualifiers = qualifierList();
			elementFeatures.add(getClassFeature(featureQualifiers,elementName,superType,elementFeatures));
			if(debug) System.out.println("Found classFeature "+elementFeatures.elementAt(elementFeatures.size()-1).toMOF());
		}
		skipOver(TokenType.RBRACE);
		skipOver(TokenType.SEMICOLON);
		NamedElement c = null;
		if(p.oid != null) updateOidInQualifierList(quals);
		switch(elementType){
		case CLASS:
			c = new CimClass(elementType,elementName,superType,quals,p.path,elementFeatures);
			break;
		case INTERFACE:
			c = new CimInterface(elementName,superType,quals,p.path,elementFeatures);
			break;
		default:
			error("Internal error-- should not happen");
		}
		repository.put(c);
		return;
	}

	/**
	 * Add any interfaces declared in the qualifiers to the element features
	 * @param quals - qualifiers on the element being constructed
	 * @param path - name space path
	 * @param elementFeatures - element features for the element
	 */
	private void addInterfaces(List<Qualifier> quals, List<QualifiedElement> elementFeatures){
		if(quals != null){
			for(Qualifier q : quals){
				if(!"Implements".equalsIgnoreCase(q.getName())) continue;
				String [] interfaces = (String []) q.getValue().getValue();
				for(String intf : interfaces){
					// TODO: the logic below implies that interfaces can only reside in the same namespace 
					ObjectPath path = new ObjectPath(ElementType.INTERFACE,intf,p.path,null,null);
					if(contains(path)){
						CimClass e = (CimClass) get(path);	// interfaces are always instantiated as CimClass
						elementFeatures.add(e);
						continue;
					}
					error("Could not find definition for interface "+intf);
				}
			}
		}
		return;
	}
	
	/**
	 * Get a class feature. A class feature is a structure, enumeration, property, or method declaration within a class
	 * @param featureQualifiers - qualifiers (if any) for this feature
	 * @param className - name of the class being constructed
	 * @param superType - superType, if any for this class
	 * @param classFeatures - features defined so far in the class, if any
	 * @return - class feature
	 */
	private QualifiedElement getClassFeature(List<Qualifier> featureQualifiers, String className, CimStructure superType, List<QualifiedElement> classFeatures) {
		//  classFeature = structureFeature / methodDeclaration
		//  structureFeature = structureDeclaration / enumDeclaration / propertyDeclaration

		// propertyDeclaration = [ qualifierList ] ( primitivePropertyDeclaration / complexPropertyDeclaration / enumPropertyDeclaration / referencePropertyDeclaration ) ";"
		// primitivePropertyDeclaration = primitiveType [ array ] featureName  [ "=" primitiveTypeValue ]
		// complexPropertyDeclaration = structureOrClassName [ array ] featureName  [ "=" ( complexTypeValue / aliasIdentifier ) ] 
		// enumPropertyDeclaration = enumName [ array ] featureName  [ "=" enumTypeValue ]
		// referencePropertyDeclaration = classReference [ array ] featureName  [ "=" referenceTypeValue ]
		// array = "[" "]" 
		// featureName = IDENTIFIER 
		// structureOrClassName = IDENTIFIER
		// REF = "ref" ; keyword: case insensitive

		// methodDeclaration = [ qualifierList ] ( ( returnDataType [ array ] ) / VOID ) methodName "(" [ parameterList ] ")" ";" 
		// returnDataType = primitiveType / structureOrClassName / enumName / classReference
		// methodName = IDENTIFIER
		// classReference = DT_REFERENCE
		// VOID = "void" ; keyword: case insensitive
		// parameterList = parameterDeclaration *( "," parameterDeclaration )

		if(p.lookAheadToken.is(TokenType.ENUMERATION)){
			// have an enumeration
			return getEnumeration(featureQualifiers,classFeatures, superType);
		} else if(p.lookAheadToken.is(TokenType.STRUCTURE)){
			// have a structure
			return getStructure(featureQualifiers, className, superType, classFeatures);
		} else {
			// have a property or method
			DataType dt = null;			// data type for this feature
			boolean isArray = false;	// true for array values
			String featureName = null;	// name of the feature
			if(p.lookAheadToken.is(TokenType.VOID)){	// void data type. Feature is a VOID method
				skipOver(TokenType.VOID);
				featureName = skipOver(TokenType.IDENTIFIER).value; // method name
				List<CimParameter> params = parameterList(className, featureName, superType, classFeatures);
				skipOver(TokenType.SEMICOLON);
				return new CimMethod(className,featureName,DataType.VOID,featureQualifiers, params);
			} else if(p.lookAheadToken.isPrimitive()){	// primitive property or method
				dt = DataType.valueOf(skipOver(p.lookAheadToken.type).value.toUpperCase());
				isArray = array();
				featureName = skipOver(TokenType.IDENTIFIER).value;
				if(!isArray) isArray = array();
				if(isArray) dt = dt.getArrayType();
				if(p.lookAheadToken.is(TokenType.LPAREN)){
					List<CimParameter> params = parameterList(className, featureName, superType, classFeatures);
					skipOver(TokenType.SEMICOLON);
					return new CimMethod(className,featureName,dt,featureQualifiers, params);
				} else {
					return primitivePropertyDeclaration(className, featureQualifiers, featureName, dt);
				}					
			} else {
				// (className [ref] | enumName | structName) property or method
				String refName = skipOver(TokenType.IDENTIFIER).value;	// named of referenced class, structure, or enum
				if(p.lookAheadToken.is(TokenType.REF)){
					// TODO: Note that since we do not search the local features and superType here, the logic below
					// implies that reference properties or methods can ONLY refer to globally defined classes.
					// Check in the grammar what is permissible
					// have a className ref property or method
					if(!refName.equalsIgnoreCase(className)){	// check that the referenced class is defined in the repository
						NamedElement e = locateStructure(refName, superType, classFeatures);
						if(e == null) error(ExceptionReason.NOT_FOUND, className+": could not find class definition for "+refName);
						// ObjectPath path = new ObjectPath(ElementType.CLASS,refName,p.path, null);
						// if(!repository.contains(path)) error(ExceptionReason.NOT_FOUND, className+": could not find class definition for "+refName);
					}
					skipOver(TokenType.REF);
					isArray = array();
					featureName = skipOver(TokenType.IDENTIFIER).value;
					if(!isArray) isArray = array();
					if(p.lookAheadToken.is(TokenType.LPAREN)){
						List<CimParameter> params = parameterList(className, featureName, superType, classFeatures);
						skipOver(TokenType.SEMICOLON);
						return new CimMethod(className,featureName,refName,isArray,featureQualifiers, params);
					} else {
						return referencePropertyDeclaration(className, featureQualifiers, featureName, refName, isArray);
					}
				}
				// have a (enumName | structName) property or method
				isArray = array();
				featureName = skipOver(TokenType.IDENTIFIER).value;
				if(!isArray) array();
				NamedElement refStructOrEnum = locateStructureOrEnum(refName, superType, classFeatures);
				if(refStructOrEnum == null) error(ExceptionReason.NOT_FOUND, className+": could not find definition for enum or structure "+refName);

				switch(p.lookAheadToken.type){
				case LPAREN: // have a method declaration
					List<CimParameter> params = parameterList(className, featureName, superType, classFeatures);
					skipOver(TokenType.SEMICOLON);
					if(refStructOrEnum instanceof CimEnumeration){
						return new CimMethod(className,featureName,(CimEnumeration) refStructOrEnum,isArray,featureQualifiers, params);
					} else {
						return new CimMethod(className,featureName,(CimStructure)refStructOrEnum,isArray,featureQualifiers, params);
					}
				case EQUALS:
				case SEMICOLON:
					return (refStructOrEnum instanceof CimEnumeration) ? enumPropertyDeclaration(null, featureQualifiers, featureName,(CimEnumeration) refStructOrEnum, isArray) :
						complexPropertyDeclaration(null, featureQualifiers, featureName,(CimStructure) refStructOrEnum, isArray);
				default:
					error("Expected semicolon or equals, found "+p.lookAheadToken);
				}
			}
		}
		// note that since error() throws an exception, we will not come here
		return null;
	}

	/**
	 * Get a (possibly nested) structure definition
	 * @param quals - qualifiers on this structure, if any
	 * @param enclosingStructure - name of the enclosing structure, if any
	 * @param superType - superType of the enclosing structure, if any
	 * @param enclosingStructureFeatures - features currently defined in the enclosing structure, if any
	 * @return - structure definition
	 */
	private CimStructure getStructure(List<Qualifier> quals, String enclosingStructure, CimStructure superType, List<QualifiedElement> enclosingStructureFeatures){
		// structureDeclaration = [ qualifierList ] STRUCTURE structureName [ superstructure ] "{" *structureFeature "}" ";" 
		// structureName = elementName
		// superStructure = ":" structureName
		// structureFeature = structureDeclaration / ; local structure
		// 				enumDeclaration / ; local enumeration
		//				propertyDeclaration
		// STRUCTURE = "structure" ; keyword: case insensitive

		skipOver(TokenType.STRUCTURE);
		String structureName = skipOver(TokenType.IDENTIFIER).value;
		CimStructure superStruct = null;

		if(p.lookAheadToken.is(TokenType.COLON)){
			// have the name of a superStructure
			skipOver(TokenType.COLON);
			String superTypeName = skipOver(TokenType.IDENTIFIER).value;
			// note that the superType can be a global structure, or a locally defined structure name in the enclosing structure, or the parents of the enclosing structure
			if(enclosingStructureFeatures != null && !enclosingStructureFeatures.isEmpty()){
				// scan for matching superType in the enclosing structure
				for(QualifiedElement s : enclosingStructureFeatures){
					if(!(s instanceof CimStructure) || !s.getName().equalsIgnoreCase(superTypeName)) continue;
					superStruct = (CimStructure)s;
					break;
				}	
			}
			// scan in the superType, if any
			if(superStruct == null && superType != null) superStruct = superType.getStructure(structureName);
			// scan the repository
			if(superStruct == null) superStruct = (CimStructure) get(new ObjectPath(ElementType.STRUCTURE,superTypeName,p.path, null, null));
			// error if we cannot locate the superStruct
			if(superStruct == null) error(ExceptionReason.NOT_FOUND,structureName+": Supertype not defined - "+superTypeName);
		}		
		skipOver(TokenType.LBRACE);
		List<QualifiedElement> structureFeatures = new Vector<QualifiedElement>();
		addInterfaces(quals, structureFeatures);
		while(!p.lookAheadToken.is(TokenType.RBRACE)){
			List<Qualifier> featureQualifiers = qualifierList();
			structureFeatures.add(getStructureFeature(featureQualifiers,structureName,superStruct, structureFeatures));		
		}
		skipOver(TokenType.RBRACE);
		skipOver(TokenType.SEMICOLON);
		if(p.oid != null) updateOidInQualifierList(quals);
		CimStructure struct = new CimStructure(ElementType.STRUCTURE,structureName,superStruct,quals, p.path, structureFeatures);
		
		if(debug) System.out.println(struct.toMOF());
		return struct;
	}

	/**
	 * Get a structure feature (a structure, enumeration, or property)
	 * @param featureQualifiers - qualifiers for the feature
	 * @param structureName - name of the structure
	 * @param superType - supertype of the structure, if any
	 * @param enclosingStructureFeatures - features declared so far within the structure, if any
	 * @return - parsed structure feature
	 */
	private QualifiedElement getStructureFeature(List<Qualifier> featureQualifiers, String structureName, CimStructure superType, List<QualifiedElement> enclosingStructureFeatures) {
		//  structureFeature = structureDeclaration / 	; local structure 
		// 					enumDeclaration / 			; local enumeration
		// 					propertyDeclaration
		if(p.lookAheadToken.is(TokenType.ENUMERATION)){
			// have an enumeration
			return getEnumeration(featureQualifiers,enclosingStructureFeatures, superType);
		} else if(p.lookAheadToken.is(TokenType.STRUCTURE)){
			// have a structure
			return getStructure(featureQualifiers, structureName, null, enclosingStructureFeatures);
		} else if(p.lookAheadToken.isPrimitive()){
			// have a primitive property
			return primitivePropertyDeclaration(structureName,featureQualifiers);
		} else {
			// have a enum or reference valued property
			String enumOrRefClassName = skipOver(TokenType.IDENTIFIER).value;
			HashSet<ElementType> requested = new HashSet<ElementType>();
			requested.add(ElementType.STRUCTURE);
			requested.add(ElementType.CLASS);
			if(p.lookAheadToken.is(TokenType.REF)){
				// have a reference property
				// ReferenceTypeProperty = className "ref" featureName [ array ] [ "=" referenceTypeValue ]
				if(enumOrRefClassName.equalsIgnoreCase(structureName)){
					// have a self-reference. Note self-references cannot have initial values
					return referencePropertyDeclaration(structureName, enumOrRefClassName, featureQualifiers);
				} else {
					// TODO: this creates an infinite loop if a persistent repository is used, the expected
					// elements are in the backing store on media, and the referenced element is a subclass of
					// the current element, e.g.,
					//
					// Structure A { B ref x; }; Structure B : A { ...};
					//
					// When A is parsed, it looks for B. B is in the backing store, so needs A for its
					// superclass, which is then again fetched (and parsed) to get its definition.
					// for the moment, we just disallow initial values on reference properties.
					
					// attempt to locate the referenced class or structure
					// CimStructure expected = (CimStructure) locateStructureOrEnum(enumOrRefClassName, superType, enclosingStructureFeatures);
					// if(expected != null){
						// add the property
					// 	return referencePropertyDeclaration(structureName, featureQualifiers, expected);
					// } else {
						// could not find the referenced class or structure
						// assume forward reference, and disallow initial values
						return referencePropertyDeclaration(structureName,enumOrRefClassName, featureQualifiers);
						// error("Undefined class or structure "+enumOrRefClassName);
					// }
				}	
			} else {
				requested.add(ElementType.ENUMERATION);
				// must be an enum or structureValue property
				// complexPropertyDeclaration = structureOrClassName [ array ] featureName  [ "=" ( complexTypeValue / aliasIdentifier ) ] 
				// enumProperty = enumName featureName [ array ] [ "=" enumTypeValue ]
				NamedElement expected = locateStructureOrEnum(enumOrRefClassName, superType, enclosingStructureFeatures);
				if(expected != null){
					switch(expected.getElementType()){
					case CLASS:
					case STRUCTURE:
						return complexPropertyDeclaration(structureName, featureQualifiers, (CimStructure) expected);
					case ENUMERATION:
						return enumPropertyDeclaration(structureName, featureQualifiers, (CimEnumeration) expected);
					default:
						error("Internal error in locating property");
					}					
				} else {
					// could not find the referenced class or structure
					error("Undefined Enumeration "+enumOrRefClassName);
				}
			}
		}
		return null;
	}
	
	/**
	 * Locate a class or structure
	 * @param elementName - name of the class or structure
	 * @param superType - supertype of the class being constructed, if any
	 * @param localFeatures - local features, if any
	 * @return - class or structure. Null if none found
	 */
	private NamedElement locateStructure(String elementName, CimStructure superType, List<QualifiedElement> localFeatures){
		if(localFeatures != null && !localFeatures.isEmpty()){
			for(QualifiedElement element : localFeatures){
				if(!(element instanceof CimStructure)) continue;
				if(!elementName.equalsIgnoreCase(element.getName())) continue;
				return (NamedElement) element;
			}
		}
		// scan the superType next
		if(superType != null){
			CimStructure found = superType.getStructure(elementName);
			if(found != null) return found;
		}
		// scan the repository with the given path
		for(ElementType t : new ElementType[]{ElementType.STRUCTURE,ElementType.CLASS}){
			ObjectPath path = new ObjectPath(t,elementName,p.path, null, null);
			if(contains(path)) return get(path);
		}
		// finally scan the repository using all name spaces
		for(NameSpacePath ns : getNameSpaces()){
			if(ns.equals(p.path)) continue;
			for(ElementType t : new ElementType[]{ElementType.STRUCTURE,ElementType.CLASS}){
				ObjectPath path = new ObjectPath(t,elementName,ns, null, null);
				if(contains(path)) return get(path);
			}
		}
		// nothing found
		return null;
	}

	/**
	 * Locate a structure, class, or enumeration by its name
	 * @param elementName - name of the element
	 * @param superType - superType (if any) of the element within which elementName is being searched
	 * @param localFeatures - local features defined in the element within which elementName is being searched
	 * @return - the corresponding structure, class, or enumeration. Null if none found
	 */
	private NamedElement locateStructureOrEnum(String elementName,CimStructure superType, List<QualifiedElement>localFeatures){
		// TODO: At the moment, this means that all classes, enums, and structures must have unique names. May need to be
		// more selective if duplicate names are desirable
		// TODO: local features also includes implemented interfaces. Also search them for the struct/enum defintion
		// scan the local features first
		if(localFeatures != null && !localFeatures.isEmpty()){
			for(QualifiedElement element : localFeatures){
				if(!(element instanceof NamedElement)) continue;
				if(!elementName.equalsIgnoreCase(element.getName())) continue;
				return (NamedElement) element;
			}
		}
		// scan the superType next
		if(superType != null){
			NamedElement found = superType.getEnumeration(elementName);
			if(found != null) return found;
			found = superType.getStructure(elementName);
			if(found != null) return found;
		}
		// scan the repository with the given path
		for(ElementType t : new ElementType[]{ElementType.ENUMERATION,ElementType.STRUCTURE,ElementType.CLASS}){
			ObjectPath path = new ObjectPath(t,elementName,p.path, null, null);
			if(contains(path)) return get(path);
		}
		// finally scan the repository using all name spaces
		for(NameSpacePath ns : getNameSpaces()){
			if(ns.equals(p.path)) continue;
			for(ElementType t : new ElementType[]{ElementType.ENUMERATION,ElementType.STRUCTURE,ElementType.CLASS}){
				ObjectPath path = new ObjectPath(t,elementName,ns, null, null);
				if(contains(path)) return get(path);
			}
		}
		// nothing found
		return null;
	}

	/**
	 * Get an optional qualifier list. Note qualifier lists are always optional
	 * @return - vector containing qualifiers. Empty if none present
	 */
	private Vector<Qualifier> qualifierList(){
		// qualifierList = "[" qualifierValue *( "," qualifierValue ) "]"
		Vector<Qualifier> quals = new Vector<Qualifier>();
		if(p.lookAheadToken.is(TokenType.LBRACKET)){
			skipOver(TokenType.LBRACKET);
			// if a '[' is present, then at least one qualifierValue must be present
			quals.add(qualifierValue());
			while(p.lookAheadToken.is(TokenType.COMMA)){
				skipOver(TokenType.COMMA);
				quals.add(qualifierValue());
			}	
			skipOver(TokenType.RBRACKET);
		}
		return quals;
	}	

	/**
	 * Get the value of a qualifier<br>
	 * @return - Qualifier value
	 */
	private Qualifier qualifierValue() {
		// qualifierValue = qualifierName [ qualifierValueInitializer / qualiferValueArrayInitializer ]
		// qualifierValueInitializer = "(" literalValue ")" 
		// qualiferValueArrayInitializer = "{" literalValue *( "," literalValue ) "}"

		/* 
		 * TODO: The grammar seems to be incomplete here ---
		 * at the moment, this means that although EnumerationValueTyped qualifierTypes can be created, the corresponding
		 * qualifiers cannot be created.
		 */
		
		// Translate v2 Qualifiers UMLPACKAGEPATH and OVERRIDE to their v3 equivalents
		
		String qName = skipOver(TokenType.IDENTIFIER).value;
		if("UMLPACKAGEPATH".equalsIgnoreCase(qName)) qName = StandardQualifierType.PACKAGEPATH.getMofName();
		QualifierType t = (QualifierType) get(new ObjectPath(ElementType.QUALIFIERTYPE,qName,p.path, null, null));
		if(t == null) error(ExceptionReason.NOT_FOUND,"QualifierType "+qName+" is not defined in namespace "+p.path);
		DataType dt = t.getDataType();
		
		
		DataValue dataValue = null;
		if(dt.isArray()){
			dataValue = new DataValue(dt, literalArrayValue(dt));
		} else if(p.lookAheadToken.is(TokenType.LPAREN)){
			skipOver(TokenType.LPAREN);
			// translate conflicting Qualifier definition between v2 and v3 MOF for OVERRIDE
			if(qName.equalsIgnoreCase("Override") && p.lookAheadToken.is(TokenType.STRING_VALUE)) {
				t = StandardQualifierType.OVERRIDE.getQualifierType(p.path);
				dataValue = new DataValue(true);
				skipOver(TokenType.STRING_VALUE); 
			} else {
				dataValue = new DataValue(dt,literalValue(dt));
			}
			skipOver(TokenType.RPAREN);
		}
		Qualifier q = dataValue == null ? new Qualifier(t) : new Qualifier(t,dataValue);
		return q;
	}
	
	/**
	 * Add an OID qualifier to qualifier list if not present and #pragma oid() is present
	 * @param quals - qualifier list to check
	 */
	private void updateOidInQualifierList(List<Qualifier> quals) {
		if(p.oid != null) {
			for(Qualifier q : quals) {
				if(q.getName().equalsIgnoreCase("oid")) return;
			}
			QualifierType qt = StandardQualifierType.OID.getQualifierType(p.path);
			quals.add(new Qualifier(qt,new DataValue(DataType.STRING,p.oid)));
		}
		return;
	}

	/**
	 * Get a method parameter list
	 * @param className - name of the class within which the method resides
	 * @param methodName - name of the method within which the parameter resides
	 * @param superType - superType of the class, if any
	 * @param classFeatures - declared features in the class, if any
	 * @return - list of parameters for the method
	 */
	private List<CimParameter> parameterList(String className, String methodName, CimStructure superType, List<QualifiedElement> classFeatures){
		// parameterList = "(" parameterDeclaration *( "," parameterDeclaration ) ")"
		Vector<CimParameter> params = new Vector<CimParameter>();
		skipOver(TokenType.LPAREN);
		while(!p.lookAheadToken.is(TokenType.RPAREN)){
			Vector<Qualifier> paramQuals = qualifierList();
			params.add(getParameter(paramQuals, className, methodName, superType, classFeatures));
			while(p.lookAheadToken.is(TokenType.COMMA)){
				skipOver(TokenType.COMMA);
				paramQuals = qualifierList();
				params.add(getParameter(paramQuals, className, methodName, superType, classFeatures));
			}
		}
		skipOver(TokenType.RPAREN);
		return params;
	}

	/**
	 * Get a method parameter
	 * @param quals - qualifiers for this parameter
	 * @param className - name of the class within which the method resides
	 * @param methodName - full name of the method within which this parameter resides
	 * @param superType - supertype of the class, if any
	 * @param classFeatures - features declared in the class, if any
	 * @return - method parameter
	 */
	private CimParameter getParameter(List<Qualifier> quals, String className, String methodName, CimStructure superType, List<QualifiedElement>classFeatures){
		// parameterDeclaration = [ qualifierList ] ( primitiveParamDeclaration / complexParamDeclaration / enumParamDeclaration / referenceParamDeclaration )
		// primitiveParamDeclaration = primitiveType parameterName [ array ] [ "=" primitiveTypeValue ]
		// complexParamDeclaration = structureOrClassName parameterName [ array ] [ "=" ( complexTypeValue / aliasIdentifier ) ]
		// enumParamDeclaration = enumName parameterName [ array ] [ "=" enumValue ] 
		// referenceParamDeclaration = classReference parameterName [ array ] [ "=" referenceTypeValue ] 
		// parameterName = IDENTIFIER

		// NOTE: The grammar for parameters is identical to that for properties, with the exception that the terminating ";" is not present.
		// TODO: Should re-factor code to make the code more compact
		if(p.lookAheadToken.isPrimitive()){
			// have a primitive property or parameter
			DataType dt = DataType.valueOf(skipOver(p.lookAheadToken.type).value.toUpperCase());
			boolean isArray = array();
			// name of the property
			String pName = skipOver(TokenType.IDENTIFIER).value;
			if(!isArray) isArray = array();	// allow both "int64 [] pName" and "int pName []"
			if(isArray){
				dt = dt.getArrayType();
			};
			DataValue value = null;
			if(p.lookAheadToken.is(TokenType.EQUALS)){
				skipOver(TokenType.EQUALS);
				value = primitiveTypeValue(dt);
			}
			// create the parameter, and return it
			return new CimParameter(methodName,pName,dt,value, quals);
		} else {
			// (className [ref] | enumName | structName) parameter
			String refName = skipOver(TokenType.IDENTIFIER).value;	// named of referenced class, structure, or enum
			if(p.lookAheadToken.is(TokenType.REF)){
				// have a className ref parameter
				CimStructure struct = (CimStructure) locateStructure(refName, superType, null);
				
				// ObjectPath path = new ObjectPath(ElementType.CLASS,refName,p.path, null, null);
				// CimStructure struct = (CimStructure) repository.get(path);
				if(struct == null && !refName.equalsIgnoreCase(className)) error(ExceptionReason.NOT_FOUND, className+": could not find class definition for "+refName);

				skipOver(TokenType.REF);
				boolean isArray = array();
				String pName = skipOver(TokenType.IDENTIFIER).value;
				if(!isArray) isArray = array();
				DataValue defaultvalue = null;

				if(p.lookAheadToken.is(TokenType.EQUALS)){
					skipOver(TokenType.EQUALS);
					DataType dt = isArray ? DataType.OBJECTPATH_ARRAY : DataType.OBJECTPATH;
					// note that reference type values are always object paths or aliases -- we handle them the same way as primitive types
					defaultvalue = referenceTypeValue(dt, struct.getName());
				}
				return new CimParameter(methodName,pName,refName,isArray,defaultvalue, quals);
			}
			// have a (enumName | structName) property or method
			boolean isArray = array();
			String featureName = skipOver(TokenType.IDENTIFIER).value;
			if(!isArray) array();
			NamedElement refStructOrEnum = locateStructureOrEnum(refName, superType, classFeatures);
			if(refStructOrEnum == null) error(ExceptionReason.NOT_FOUND, className+": could not find definition for enum or structure "+refName);
			if(refStructOrEnum instanceof CimEnumeration){
				DataType dt = isArray ? DataType.ENUMERATIONVALUE_ARRAY : DataType.ENUMERATIONVALUE;
				DataValue value = null;
				if(p.lookAheadToken.is(TokenType.EQUALS)){
					skipOver(TokenType.EQUALS);
					value = enumerationTypeValue(dt,(CimEnumeration) refStructOrEnum);
				}
				return new CimParameter(methodName,featureName,(CimEnumeration) refStructOrEnum,isArray,value, quals);
			} else {
				DataType dt = refStructOrEnum instanceof CimClass ? DataType.INSTANCEVALUE : DataType.STRUCTUREVALUE;
				if(isArray) dt = dt.getArrayType();
				DataValue v = null;
				if(p.lookAheadToken.is(TokenType.EQUALS)) {
					skipOver(TokenType.EQUALS);
					v = complexTypeValue(dt, (CimStructure) refStructOrEnum);
				}
				return new CimParameter(methodName,featureName,(CimStructure) refStructOrEnum,isArray,v, quals);
			}
		}
	}

	/**
	 * Get a primitive property declaration
	 * @param originClass - structure within which this property is located
	 * @param quals - qualifiers for this property
	 * @return - Property value
	 */
	private CimProperty primitivePropertyDeclaration(String originClass, List<Qualifier> quals){
		// PrimitivePropertyDeclaration = primitiveDataType [ array ] featureName [ array ] [ "=" primitiveTypeValue ] ";"
		// cursor is at primitiveDataType
		if(!p.lookAheadToken.isPrimitive()){
			error("Expected Primitive type, found "+p.lookAheadToken);
		}
		// get the data type
		DataType dt = DataType.valueOf(skipOver(p.lookAheadToken.type).value.toUpperCase());
		boolean isArray = array();
		// name of the property
		String propertyName = skipOver(TokenType.IDENTIFIER).value;
		if(!isArray) isArray = array();	// allow both "int64 [] propName" and "int propName []"
		if(isArray){
			dt = dt.getArrayType();
		};
		return primitivePropertyDeclaration(originClass,quals,propertyName, dt);
	}

	/**
	 * Get a primitive property declaration
	 * @param originClass - class within this property is declared
	 * @param quals - qualifiers on this property
	 * @param dt - data type for the property
	 * @param featureName name of the property
	 * @return - CimProperty found
	 */
	private CimProperty primitivePropertyDeclaration(String originClass, List<Qualifier> quals, String propertyName, DataType dt) {
		// cursor is at equals (or semicolon) after the property name
		// check if an initial value is given
		DataValue value = null;
		if(p.lookAheadToken.is(TokenType.EQUALS)){
			skipOver(TokenType.EQUALS);
			value = primitiveTypeValue(dt);
		}
		skipOver(TokenType.SEMICOLON);
		// create the property, and return it
		return new CimProperty(originClass,propertyName,dt,value, quals);
	}

	/**
	 * Get a reference property declaration (forward reference-- no default value allowed)
	 * @param originClass - class within this property is declared
	 * @param quals - qualifiers for this property
	 * @return - reference property
	 */
	private CimProperty referencePropertyDeclaration(String originClass, String refClassName, List<Qualifier> quals){
		// ReferenceTypeProperty = className "ref" [ array ] featureName [ array ] [ "=" referenceTypeValue ] ";"
		// at entry, we expect the lookahead token to be TokenType.REF
		skipOver(TokenType.REF);
		boolean isArray = array();
		String propertyName = skipOver(TokenType.IDENTIFIER).value;
		if(!isArray) isArray = array();
		return referencePropertyDeclaration(null, quals, propertyName, refClassName, isArray);
	}

	/**
	 * Create a reference property declaration
	 * @param originClass - class within this property is declared
	 * @param quals - qualifiers for this property
	 * @param refClassName - name of the referenced class
	 * @param isArray - true, if this property is an array property, false otherwise
	 * @param featureName - name of the property
	 * @return - reference property 
	 */
	private CimProperty referencePropertyDeclaration(String originClass, List<Qualifier> quals,String propertyName, String refClassName, boolean isArray) {
		// at entry, we expect cursor at "=" or ";"
		DataValue defaultValue = null;
		if(p.lookAheadToken.is(TokenType.EQUALS)){
			error("Class or CimStructure "+refClassName+" is not yet defined. No default value is allowed");
		}
		skipOver(TokenType.SEMICOLON);
		return new CimProperty(originClass,propertyName,refClassName,isArray,defaultValue, quals);
	}

	/**
	 * Get a reference property declaration
	 * @param originClass - class within this property is declared
	 * @param quals - list of qualifiers
	 * @param struct - expected structure or class
	 * @return - reference property
	 */
	private CimProperty referencePropertyDeclaration(String originClass, List<Qualifier> quals, CimStructure struct){
		// ReferenceTypeProperty = classname "ref" [ array ] featureName [ array ] [ "=" referenceTypeValue ] ";"
		// at entry, we expect the lookahead token to be TokenType.REF
		skipOver(TokenType.REF);
		boolean isArray = array();
		String propertyName = skipOver(TokenType.IDENTIFIER).value;
		if(!isArray) isArray = array();
		DataValue defaultvalue = null;
		if(p.lookAheadToken.is(TokenType.EQUALS)){
			skipOver(TokenType.EQUALS);
			DataType dt = isArray ? DataType.OBJECTPATH_ARRAY : DataType.OBJECTPATH;
			// note that reference type values are always object paths or aliases -- we handle them the same way as primitive types
			defaultvalue = referenceTypeValue(dt, struct.getName());
		}
		skipOver(TokenType.SEMICOLON);
		return new CimProperty(originClass,propertyName,struct.getName(),isArray,defaultvalue, quals);
	}

	/**
	 * get an enumeration property declaration
	 * @param originClass - class within this property is declared
	 * @param quals - qualifiers on this property
	 * @param enumeration - enumeration to be used for the property
	 * @return - enumeration property
	 */
	private CimProperty enumPropertyDeclaration(String originClass, List<Qualifier> quals, CimEnumeration enumeration){
		// enumProperty = enumName [array] featureName [ array ] [ "=" enumTypeValue ] ";"
		// cursor is located at the featureName at entry
		boolean isArray = array();
		String propertyName = skipOver(TokenType.IDENTIFIER).value;
		if(!isArray) isArray = array();
		return enumPropertyDeclaration(originClass,quals,propertyName,enumeration, isArray);
	}

	/**
	 * get an enumeration property declaration
	 * @param originClass - class within this property is declared
	 * @param quals - qualifiers for this property
	 * @param enumeration - corresponding enumeration
	 * @param isArray - true if property is array property, false otherwise
	 * @param featureName - name of the property
	 * @return - enumeration property
	 */
	private CimProperty enumPropertyDeclaration(String originClass, List<Qualifier> quals, String propertyName, CimEnumeration enumeration, boolean isArray) {
		DataType dt = isArray ? DataType.ENUMERATIONVALUE_ARRAY : DataType.ENUMERATIONVALUE;
		DataValue value = null;
		if(p.lookAheadToken.is(TokenType.EQUALS)){
			skipOver(TokenType.EQUALS);
			value = enumerationTypeValue(dt,enumeration);
		}
		skipOver(TokenType.SEMICOLON);
		return new CimProperty(originClass,propertyName,enumeration,isArray,value, quals);
	}

	/**
	 * Get a complex (structure or instance valued) property declaration
	 * @param originClass TODO
	 * @param quals - qualifiers for this property
	 * @param expected - structure or class definition for the expected property
	 * @return - complex valued property
	 */
	private CimProperty complexPropertyDeclaration(String originClass, List<Qualifier> quals, CimStructure expected){
		// complexPropertyDeclaration = structureOrClassName [ array ] featureName [ array ] [ "=" ( complexTypeValue / aliasIdentifier ) ] 
		// cursor is after the structureOrClassName
		boolean isArray = array();
		String propertyName = skipOver(TokenType.IDENTIFIER).value;
		if(!isArray) isArray = array();		// This enables acceptance of both className ref [] x and className ref x [] as valid
		return complexPropertyDeclaration(originClass,quals,propertyName,expected, isArray);
	}

	/**
	 * Get a complex (structure or instance valued) property declaration
	 * @param originClass TODO
	 * @param quals - qualifiers on this property
	 * @param expected - expected class or structure definition
	 * @param isArray - true if this property is array property
	 * @param featureName - name of this property
	 * @return - complex valued property
	 */
	private CimProperty complexPropertyDeclaration(String originClass, List<Qualifier> quals, String propertyName, CimStructure expected, boolean isArray){
		// complexPropertyDeclaration = structureOrClassName [ array ] featureName [ array ] [ "=" ( complexTypeValue / aliasIdentifier ) ] 
		// cursor is at the "=" or ";" sign
		DataType dt = expected instanceof CimClass ? DataType.INSTANCEVALUE : DataType.STRUCTUREVALUE;
		if(isArray) dt = dt.getArrayType();
		DataValue v = null;
		if(p.lookAheadToken.is(TokenType.EQUALS)) {
			skipOver(TokenType.EQUALS);
			v = complexTypeValue(dt, expected);
		}
		skipOver(TokenType.SEMICOLON);
		return new CimProperty(originClass,propertyName,expected,isArray,v, quals);
	}

	/**
	 * Check if the next two tokens represent an array. Array markings are always optional, so we can
	 * just return true/false. Cursor is positioned beyond the array mark
	 * @return - true if an array mark follows, false otherwise.
	 */
	private boolean array(){
		//  array = "[" "]" 
		if(!p.lookAheadToken.is(TokenType.LBRACKET)) return false;
		skipOver(TokenType.LBRACKET);
		skipOver(TokenType.RBRACKET);
		return true;
	}

	/**
	 * Get a property list within a structure or class
	 * @param expected - expected structure or class
	 * @return - Map containing <featureName,propertyValue> pairs
	 */
	private HashMap<String,DataValue> getPropertyList(CimStructure expected){
		// propertyValueList = "{" *propertySlot "}"
		LinkedHashMap<String,DataValue> propertyValues = new LinkedHashMap<String,DataValue>();
		skipOver(TokenType.LBRACE);
		while(!p.lookAheadToken.is(TokenType.RBRACE)){
			// propertySlot = featureName "=" propertyValue ";"
			String propertyName = skipOver(TokenType.IDENTIFIER).value;
			skipOver(TokenType.EQUALS);
			if(expected.hasProperty(propertyName)){
				CimProperty p = expected.getProperty(propertyName);
				DataType dt = p.getDataType();
				// propertyValue = primitiveTypeValue / complexTypeValue / referenceTypeValue / enumTypeValue
				if(dt.isPrimitive()){
					// primitive property type
					DataValue pv = primitiveTypeValue(dt);
					propertyValues.put(propertyName, pv);
				} else if(dt.isReference()){
					// reference property type
					String refClassName = p.getRefClassName();
					DataValue pv = referenceTypeValue(dt, refClassName);
					propertyValues.put(propertyName, pv);
				} else if(dt.isEnumerationValue()){
					CimEnumeration en = p.getEnum();
					DataValue pv = enumerationTypeValue(dt,en);
					propertyValues.put(propertyName, pv);
				} else if(dt.isStructureValue()){
					CimStructure s = p.getStruct();
					DataValue pv = complexTypeValue(dt,s);
					propertyValues.put(propertyName, pv);
				} else {
					error("Internal Error in parser when parsing structure/Instance value "+expected.toMOF());
				}
			} else {
				error(ExceptionReason.NO_SUCH_PROPERTY, expected.getName()+" Property "+propertyName+" not found");
			}
			skipOver(TokenType.SEMICOLON);
		}
		skipOver(TokenType.RBRACE);
		return propertyValues;
	}

	/*
	 * *****************************************
	 * Methods to handle initializer values
	 * *****************************************
	 */

	/**
	 * Return an enumeration typed value<br/>
	 * @param dt - dataType desired
	 * @param en - enumeration to be used for the value
	 * @return - data value containing the corresponding enumeration values
	 */
	private DataValue enumerationTypeValue(DataType dt, CimEnumeration en){
		// enumTypeValue = enumValue / enumValueArray
		return dt.isArray() ? new DataValue(dt,enumerationArrayValue(en)) : new DataValue(dt,enumerationValue(en));
	}

	/**
	 * Get an array of enum values<br/>
	 * @param en - enumeration to be used to locate the values
	 * @return - Array of EnumerationValue. Null if empty list
	 */
	private EnumerationValue[] enumerationArrayValue(CimEnumeration en) {
		// enumValueArray = "{" [ enumValue *( "," enumValue ) ] "}"
		skipOver(TokenType.LBRACE);
		if(p.lookAheadToken.is(TokenType.RBRACE)){
			// array dataValue with nothing defined in it
			return null;
		}
		// get the first dataValue
		Vector<EnumerationValue> v = new Vector<EnumerationValue>();
		v.add(enumerationValue(en));
		// while other values are defined, get them
		while(p.lookAheadToken.is(TokenType.COMMA)){
			skipOver(TokenType.COMMA);
			v.add(enumerationValue(en));
		}
		// end of the array
		skipOver(TokenType.RBRACE);
		// construct the array data dataValue from the corresponding java values
		EnumerationValue [] value = new EnumerationValue[v.size()];
		for(int i = 0; i < v.size(); i++) value[i] = v.get(i);
		return value;
	}

	/**
	 * Get an enumeration value for qualifierTypes<br/>
	 * @param en - enum to use
	 * @return - Data dataValue containing the singleton dataValue
	 */
	private EnumerationValue enumerationValue(CimEnumeration en){
		// enumValue = [ enumName "." ] enumLiteral
		// enumLiteral = IDENTIFIER
		String enumValue = skipOver(TokenType.IDENTIFIER).value;
		while(p.lookAheadToken.value.equals(".")){
			skipOver(TokenType.ASCII);
			enumValue = enumValue + "." + skipOver(TokenType.IDENTIFIER).value;
		}
		// TODO: This block may need fixing depending on how many "." values are present in the name
		if(enumValue.contains(".")){
			String enumName = enumValue.substring(0, enumValue.lastIndexOf('.'));
			if(!en.getName().equalsIgnoreCase(enumName)) error(ExceptionReason.TYPE_MISMATCH,"Value "+enumValue+" does not match Enumeration "+en.getName());
			enumValue = enumValue.substring(enumValue.lastIndexOf('.')+1);
		}
		// ensure that the value given is a valid key in the enumeration
		if(!en.getKeys().contains(enumValue)) error(ExceptionReason.NOT_FOUND,en.getName()+" does not define the value "+enumValue);
		return en.getValue(enumValue);
	}

	/**
	 * Return a complex value initializer
	 * @param dataType - expected data type
	 * @param expected - structure or class name for the expected class
	 * @return - Data value containing the CimInstance or StructureValue
	 */
	private DataValue complexTypeValue(DataType dt, CimStructure expected){
		// complexTypeValue = complexValue / complexValueArray
		return dt.isArray() ? new DataValue(dt,complexArrayValue(expected)) : new DataValue(dt,complexValue(expected));
	}

	/**
	 * Get the values for a structure array
	 * @return - complex valued array. Null is returned if an empty array "{" "}" is seen
	 */
	private Object[] complexArrayValue(CimStructure expected){
		// complexValueArray = "{" [ complexValue *( "," complexValue ) ] "}"
		skipOver(TokenType.LBRACE);
		if(p.lookAheadToken.is(TokenType.RBRACE)){
			// return value with nothing in it
			return null;
		}
		Vector<Object> values = new Vector<Object>();
		values.add(complexValue(expected));
		while(p.lookAheadToken.is(TokenType.COMMA)){
			skipOver(TokenType.COMMA);
			values.add(complexValue(expected));
		}
		skipOver(TokenType.RBRACE);
		if(expected.getElementType() == ElementType.CLASS){
			CimInstance [] v = new CimInstance[values.size()];
			for(int i = 0; i < values.size(); i++) v[i] = (CimInstance) values.get(i);
			return v;
		} else {
			StructureValue[] v = new StructureValue[values.size()];
			for(int i = 0; i < values.size(); i++) v[i] = (StructureValue) values.get(i);
			return v;
		}
	}

	/**
	 * Get the value of a structure
	 * @param expected - structure for which the value is expected
	 * @return - value of the structure
	 */
	private Object complexValue(CimStructure expected){
		// complexValue = ([( INSTANCE / VALUE ) OF ( structureName / className / associationName ) [ alias ] ] propertyValueList) / AliasIdentifier
		// propertyValueList = "{" *propertySlot "}"
		// propertySlot = featureName "=" propertyValue ";"

		// NOTE: that we assume that we are looking for a StructureValue if we expect a structure, while we want an instance if we expect a class
		ElementType expectedType = (expected instanceof CimClass) ? ElementType.CLASS : ElementType.STRUCTURE;	
		// Since we know the expected structure, we can make (Instance | Value) of (className | structName) optional
		// this is a variation on the Grammar defined in DSP0004
		if(p.lookAheadToken.is(TokenType.ALIAS_IDENTIFIER)){
			String propAlias = skipOver(TokenType.ALIAS_IDENTIFIER).value;
			switch(expectedType){
			case CLASS:
				CimInstance instance = aliasedInstanceValues.get(propAlias);
				return instance;
			case STRUCTURE:
				StructureValue value = aliasedStructureValues.get(propAlias);
				return value;
			default:
				error("Internal parse error in complexValue");	
			}
		} else if(p.lookAheadToken.is(TokenType.INSTANCE) || p.lookAheadToken.is(TokenType.VALUE)){
			skipOver(expectedType == ElementType.CLASS ? TokenType.INSTANCE : TokenType.VALUE);
			// OF is optional
			if(p.lookAheadToken.is(TokenType.OF)) skipOver(p.lookAheadToken.type);
			// locate the name of the structure, class, or association
			String classOrStructName = skipOver(TokenType.IDENTIFIER).value;
			// structure/className must match the expected type
			if(!expected.isSubTypeOf(classOrStructName)) error(ExceptionReason.TYPE_MISMATCH,"Expected instance/value of "+expected.getName()+" found "+classOrStructName);

			// alias, if any
			String alias = null;
			if(p.lookAheadToken.is(TokenType.AS)){
				skipOver(TokenType.AS);
				alias = skipOver(TokenType.ALIAS_IDENTIFIER).value;
			}
			// TODO: This logic puts all aliased values in global scope. This is probably a bug
			switch(expectedType){
			case CLASS:
				CimInstance instance = getInstanceValue(alias,(CimClass) expected);

				if(alias != null) aliasedInstanceValues.put(alias, instance);
				return instance;
			case STRUCTURE:
				StructureValue value =  getStructureValue(alias,expected);
				// if(alias != null) aliasedStructureValues.put(alias, value);
				return value;
			default:
				error("Internal parser error in complexValue");
			}
		} else {
			// error
		}
		// Should not happen-- error() throws an exception
		return null;
	}

	/**
	 * Get the value for a structure
	 * @param expected - expected structure
	 * @return - structure value.
	 */
	private StructureValue getStructureValue(String alias, CimStructure expected){
		// At entry, cursor is at LBRACE
		// propertyValueList = "{" *propertySlot "}"
		if(p.lookAheadToken.is(TokenType.LBRACE)){
			return StructureValue.createStructureValue(expected, getPropertyList(expected), alias);
		}
		// should not happen-- error() throws an exception
		return null;
	}

	/**
	 * Get the instance value for a  class
	 * @param expected - expected class
	 * @return - instance value.
	 */
	private CimInstance getInstanceValue(String alias, CimClass expected){
		// At entry, cursor is at LBRACE
		// propertyValueList = "{" *propertySlot "}" | aliasIdentifier
		if(p.lookAheadToken.is(TokenType.LBRACE)){
			return CimInstance.createInstance(expected, getPropertyList(expected),alias);
		} else if(p.lookAheadToken.is(TokenType.ALIAS_IDENTIFIER)){
			String alias1 = skipOver(TokenType.ALIAS_IDENTIFIER).value;
			if(aliasedInstanceValues.containsKey(alias)) return aliasedInstanceValues.get(alias1);
			error("Alias "+alias1+" is not defined");
		}
		// should not happen-- error() throws an exception
		return null;
	}

	/**
	 * Return a DataValue containing references to a structure (or array of references to the structure)
	 * @param dt - expected data type
	 * @param refClassName - class or structure to which the references are desired
	 * @return - data value containing the reference (or array of references)
	 */
	private DataValue referenceTypeValue(DataType dt, String refClassName){
		// referenceTypeValue = referenceValue / referenceValueArray
		return dt.isArray() ? new DataValue(dt,referenceArrayValue(refClassName)) : new DataValue(dt,referenceValue(refClassName));
	}

	/**
	 * Return an array of references to an expected class or structure
	 * @param refClassName - expected class or structure
	 * @return - ObjectPath[] containing the reference values
	 */
	private ObjectPath [] referenceArrayValue(String refClassName){
		// referenceValueArray = "{" [objectPathValue *( "," objectPathValue) ] "}"
		skipOver(TokenType.LBRACE);
		if(p.lookAheadToken.is(TokenType.RBRACE)){
			skipOver(TokenType.RBRACE);
			return null;
		}
		Vector<ObjectPath> v = new Vector<ObjectPath>();
		v.add(referenceValue(refClassName));
		while(p.lookAheadToken.is(TokenType.COMMA)){
			skipOver(TokenType.COMMA);
			v.add(referenceValue(refClassName));
		}
		skipOver(TokenType.RBRACE);
		ObjectPath [] value = new ObjectPath[v.size()];
		for(int i = 0; i < v.size(); i++) value[i] = v.elementAt(i);
		return value;
	}

	/**
	 * Return a reference to a class or structure
	 * @param refClassName - class or structure expected
	 * @return ObjectPath containing the reference
	 */
	private ObjectPath referenceValue(String refClassName){
		if(p.lookAheadToken.is(TokenType.IDENTIFIER) && p.lookAheadToken.value.startsWith("$")){
			// TODO: Need to know whether the refClass is to an instance or a structure.
			// for now assume reference
			// have an alias reference
			return new ObjectPath(ElementType.INSTANCE,refClassName, skipOver(TokenType.ALIAS_IDENTIFIER).value);
		} else if(p.lookAheadToken.is(TokenType.STRING_VALUE)){
			// have an object path
			ObjectPath v = new ObjectPath(stringValue());
			// TODO: This check fails if the ObjectPath refers to a subclass of the expected path. Need to fix
			// Note that this check should be performed correctly when a property value is set
			// if(!refClassName.equalsIgnoreCase(v.getName())){
			// 	error("Expected reference to class "+refClassName+" found "+v.toString());
			// }
			return v;
		} else if (p.lookAheadToken.is(TokenType.NULL)){
			skipOver(TokenType.NULL);
			// NULL is a valid reference value
			return null;
		} else {
			error(ExceptionReason.TYPE_MISMATCH,"expected AliasIdentifier or objectPath, found "+p.lookAheadToken);
		}
		// note that since error() throws an exception, we will never return null
		return null;
	}

	/**
	 * Get the data dataValue corresponding to the primitive data type dt
	 * @param dt - expected data type
	 * @return - corresponding dataValue
	 */
	private DataValue primitiveTypeValue(DataType dt) {
		//  primitiveTypeValue = literalValue / literalValueArray
		return dt.isArray() ? new DataValue(dt, literalArrayValue(dt)) : new DataValue(dt,literalValue(dt));
	}

	/**
	 * Return a java array object containing the desired literal values
	 * @param dt - desired data type for the initializer (array or component)
	 * @return - An array of the proper type containing the given values. A null is returned if the initializer
	 * does not contain any values, i.e., consists of "{}"
	 */
	private Object literalArrayValue(DataType dt) {
		// literalValueArray = "{" [ literalValue *( "," literalValue ) ] "}"
		DataType componentType = dt.getComponentType();
		Object [] o = null;
		Class <?> c = componentType.getClassForType();
		skipOver(TokenType.LBRACE);
		if(!p.lookAheadToken.is(TokenType.RBRACE)){
			Vector<Object> objects = new Vector<Object>();
			objects.add(literalValue(componentType));
			while(p.lookAheadToken.is(TokenType.COMMA)){
				skipOver(TokenType.COMMA);
				objects.add(literalValue(dt.getComponentType()));
			}
			o = (Object[]) Array.newInstance(c, objects.size());
			for(int i = 0; i < objects.size(); i++) o[i] = objects.elementAt(i);	
		}
		skipOver(TokenType.RBRACE);
		return o;
	}

	/**
	 * Return a java object containing a literal value
	 * @param dt - desired data type
	 * @return - a java object containing the proper data type. Note that a null value is returned for the MOF "null" data value.
	 */
	private Object literalValue(DataType dt){
		// literalValue = integerValue / realValue / stringValue / octetStringValue / booleanValue / nullValue / dateTimeValue
		Token t = p.lookAheadToken;
		Object v = null;
		// null value matches all types
		if(t.is(TokenType.NULL)){
			skipOver(t.type);
			return v;
		}
		switch(dt){
		case BOOLEAN:
			if(!(t.is(TokenType.TRUE) || t.is(TokenType.FALSE))) error(ExceptionReason.TYPE_MISMATCH,"Boolean dataValue");
			v = Boolean.parseBoolean(t.value);
			break;
		case STRING:
			// note that STRING already skips over lookAheadToken, so we can return here
			if(!t.is(TokenType.STRING_VALUE)) error(ExceptionReason.TYPE_MISMATCH,"String dataValue");
			StringBuilder b = new StringBuilder(skipOver(TokenType.STRING_VALUE).value);
			while(p.lookAheadToken.is(TokenType.STRING_VALUE)) {
				b.append(skipOver(TokenType.STRING_VALUE).value);
			}
			return b.toString();
		case OCTETSTRING:
			// note that OctetString already skips over lookAheadToken, so we can return here
			return new OctetString(octetStringValue());
		case UINT64:
			if(!t.is(TokenType.NUMBER))error(ExceptionReason.TYPE_MISMATCH,"UINT64 dataValue");
			v = new UInt64(t.value);
			break;
		case SINT64:
			if(!t.is(TokenType.NUMBER))error(ExceptionReason.TYPE_MISMATCH,"SINT64 dataValue");
			v = Long.valueOf(t.value);
			break;
		case UINT8:
			if(!t.is(TokenType.NUMBER))error(ExceptionReason.TYPE_MISMATCH,"UINT8 dataValue");
			v = new UInt8(t.value);
			break;
		case SINT8:
			if(!t.is(TokenType.NUMBER))error(ExceptionReason.TYPE_MISMATCH,"SINT8 dataValue");
			v = Byte.valueOf(t.value);
			break;
		case UINT16:
			if(!t.is(TokenType.NUMBER))error(ExceptionReason.TYPE_MISMATCH,"UINT16 dataValue");
			v = new UInt16(t.value);
			break;
		case SINT16:
			if(!t.is(TokenType.NUMBER))error(ExceptionReason.TYPE_MISMATCH,"SINT16 dataValue");
			v = Short.valueOf(t.value);
			break;
		case UINT32:
			if(!t.is(TokenType.NUMBER)) error(ExceptionReason.TYPE_MISMATCH,"UINT32 dataValue");
			v = new UInt32(t.value);
			break;
		case SINT32:
			if(!t.is(TokenType.NUMBER)) error(ExceptionReason.TYPE_MISMATCH,"SINT32 dataValue");
			v = Integer.valueOf(t.value);
			break;
		case REAL32:
			if(!t.is(TokenType.NUMBER)) error(ExceptionReason.TYPE_MISMATCH,"REAL32 dataValue");
			v = Float.valueOf(t.value);
			break;
		case REAL64:
			if(!t.is(TokenType.NUMBER)) error(ExceptionReason.TYPE_MISMATCH,"REAL64 dataValue");
			v = Double.valueOf(t.value);
			break;
		case CHAR16:
			if(!t.is(TokenType.QUOTED_CHARACTER)) error(ExceptionReason.TYPE_MISMATCH,"Quoted character");;
			v = Character.valueOf(t.value.charAt(0));
			break;
		case DATETIME:
			if(!t.is(TokenType.STRING_VALUE)) error(ExceptionReason.TYPE_MISMATCH,"String dataValue");;
			v = new DateTime(t.value);
			break;
		default:
			error(ExceptionReason.TYPE_MISMATCH,"Expected primitive literalValue: Found unknown type "+dt.toString());
			break;
		}
		skipOver(t.type);
		return v;
	}

	/**
	 * Obtain an octetString value, and update lookaheadToken.<br/>
	 * An octetString is a byte [] represented as a hex string
	 * @return - octetString value
	 */
	private String octetStringValue(){
		// octetStringValue = doubleQuote "0x" *(octetStringElementValue) doubleQuote
		//						* ( *WS doubleQuote *( octetStringElementValue ) doubleQuote )
		// octetStringElementValue = 2 (hexDigit)
		if(!p.lookAheadToken.is(TokenType.STRING_VALUE)){
			error("Expected OCTET_STRING_VALUE, found "+p.lookAheadToken);
		}
		StringBuilder b = new StringBuilder("0x");
		while(p.lookAheadToken.is(TokenType.STRING_VALUE)){
			String nextValue = skipOver(TokenType.STRING_VALUE).value;
			if(!nextValue.startsWith("0x")) error("Expected Octet String to start with 0x, found "+nextValue);
			b.append(nextValue.substring(2).toLowerCase());
		}
		return b.toString();
	}

	/*
	 * **********************************
	 * Tokenizer methods start here
	 * **********************************
	 */

	/**
	 * Get the next token into the look-ahead token (skipping any white space)
	 */
	private void nextToken(){
		// read the next Token, and skip any comments and white spaces
		readNextToken();
		while(p.lookAheadToken.is(TokenType.WHITE_SPACE) || p.lookAheadToken.is(TokenType.COMMENT)){
			// if(debug) System.out.println("// ---> "+p.lookAheadToken.value());
			readNextToken();
		}
		if(debug) System.out.println("// (nextToken) -> "+p.lookAheadToken.type+" ["+p.lookAheadToken.value+"]");
		return;

	}

	/**
	 * Read the next Token. If the current file is finished, pop the input stack and continue
	 * reading tokens from the previous file
	 */
	private void readNextToken(){
		// if the current line is null, empty, or we have read the complete line, get next line
		if(p.cursor >= p.lineLength || p.lineLength == 0 || p.line == null){
			boolean haveLine = nextLine();
			// if no more lines in the current file, but input stack is not empty, pop stack and read from there
			while(!haveLine && !inputStack.isEmpty()){
				p = inputStack.pop();
				if(debug) System.out.println("Revert to file: "+p.file);
				// if the popped state has an unfinished line, we continue there (lookahead token is already in parser state)
				if(p.cursor < p.lineLength && p.lineLength != 0 && p.line != null) {
					return;
				}
				// otherwise, we try and read the next line from the popped input file
				haveLine = nextLine();
			}
			// nothing more is available, return EOF Token
			if(!haveLine){
				p.lookAheadToken = new Token(TokenType.EOF,"");
				return;
			}
		}
		// scan for next token
		boolean foundMatch = false;
		// First handle comments and white space
		if(Character.isWhitespace(p.line.charAt(p.cursor))){
			// check for white space
			int savedLoc = p.cursor;	// save cursor and scan white space
			while(++p.cursor < p.lineLength && Character.isWhitespace(p.line.charAt(p.cursor)));
			p.lookAheadToken = new Token(TokenType.WHITE_SPACE,p.line.substring(savedLoc,p.cursor));
			foundMatch = true;	
		} else if(p.line.startsWith("//", p.cursor)){
			// comment to end of line
			p.lookAheadToken = new Token(TokenType.COMMENT,p.line.substring(p.cursor));
			p.cursor = p.lineLength;
			foundMatch = true;
		} else if(p.line.startsWith("/*",p.cursor)){
			// Block comment. Scan until end of comment (or EOF)
			StringBuilder b = new StringBuilder("/*");
			p.cursor += 2;	// go past the /*
			int startNumber = p.lineNumber;	// save current line number
			while(!foundMatch){
				while(p.cursor < p.lineLength){
					if(p.line.startsWith("*/",p.cursor)){
						// found end of comment
						b.append("*/");
						p.cursor += 2;
						p.lookAheadToken = new Token(TokenType.COMMENT,b.toString());
						foundMatch = true;
						break;
					} else {
						b.append(p.line.charAt(p.cursor++));
					}
				}
				// if comment did not end on line, advance line
				if(!foundMatch && p.cursor >= p.lineLength){
					b.append("\n");
					if(!nextLine()){
						// reached EOF with an unterminated comment
						p.lookAheadToken = new Token(TokenType.ERROR, "Unterminated comment starting at line "+startNumber);
						foundMatch = true;
					}
				}
			}
		}
		if(!foundMatch){
			char c = p.line.charAt(p.cursor++);
			switch(c){
			case '(':	// left paranthesis
				p.lookAheadToken = new Token(TokenType.LPAREN,"(");
				foundMatch = true;
				break;
			case ')':	// right paranthesis
				p.lookAheadToken = new Token(TokenType.RPAREN,")");
				foundMatch = true;
				break;
			case '{':
				p.lookAheadToken = new Token(TokenType.LBRACE,String.valueOf(c));
				foundMatch = true;
				break;
			case '}':
				p.lookAheadToken = new Token(TokenType.RBRACE,String.valueOf(c));
				foundMatch = true;
				break;
			case '[':
				p.lookAheadToken = new Token(TokenType.LBRACKET,String.valueOf(c));
				foundMatch = true;
				break;
			case ']':
				p.lookAheadToken = new Token(TokenType.RBRACKET,String.valueOf(c));
				foundMatch = true;
				break;
			case ';':
				p.lookAheadToken = new Token(TokenType.SEMICOLON,String.valueOf(c));
				foundMatch = true;
				break;
			case ':':
				p.lookAheadToken = new Token(TokenType.COLON,String.valueOf(c));
				foundMatch = true;
				break;
			case ',':
				p.lookAheadToken = new Token(TokenType.COMMA,String.valueOf(c));
				foundMatch = true;
				break;
			case '=':
				p.lookAheadToken = new Token(TokenType.EQUALS,String.valueOf(c));
				foundMatch = true;
				break;
			case '\'':		// have a quoted character
				if(p.cursor+2 < p.lineLength){	// must have at least 2 more characters, i.e., x'
					char t = getQuotedCharacter();
					p.lookAheadToken = new Token(TokenType.QUOTED_CHARACTER,String.valueOf(t));
				} else {
					// have an unterminated character on a line -- syntax error
					p.lookAheadToken = new Token(TokenType.ERROR,
							"Unterminated character at Line "+p.lineNumber+" location " + p.cursor);
				}
				if(p.cursor < p.lineLength && p.line.charAt(p.cursor) == '\''){
					p.cursor++;	// advance past the terminating '
				} else {
					// have an unterminated character
					p.lookAheadToken = new Token(TokenType.ERROR,
							"Unterminated character at line "+p.lineNumber+" column "+p.cursor);
				}
				foundMatch = true;
				break;
			case '"':	// string dataValue
				int savedCursor = p.cursor;
				StringBuilder b = new StringBuilder();
				while(!foundMatch && p.cursor < p.lineLength){
					switch(p.line.charAt(p.cursor)){
					case '"':
						// reached end of string
						p.lookAheadToken = new Token(TokenType.STRING_VALUE,b.toString());
						p.cursor++;
						foundMatch = true;
						break;
					case '\\':
						// linePosition--;
						b.append(getQuotedCharacter());
						break;
					default:
						b.append(p.line.charAt(p.cursor++));		
					}
				}
				if(!foundMatch){
					// have an unterminated String
					p.lookAheadToken = new Token(TokenType.ERROR,
							"Unterminated string at line "+p.lineNumber+" column "+savedCursor);
					foundMatch = true;
				}

				break;
			case '#':	// #pragma
				String pragma = p.line.substring(p.cursor,p.cursor+6);	// move past 'pragma'
				if(pragma.equalsIgnoreCase("pragma")){
					p.lookAheadToken = new Token(TokenType.PRAGMA,"#pragma");
					p.cursor += 6;	
				} else {
					p.lookAheadToken = new Token(TokenType.ERROR,"Expected PRAGMA found "+pragma);
				}
				foundMatch = true;
				break;
			default:
				// check for numbers and identifiers
				p.cursor--;
				if(c == '+' || c == '-' || Character.isDigit(c)){	
					foundMatch = numericValue();;
				} else {
					foundMatch = getIdentifier();
				}
				break;
			}				
		}
		if(!foundMatch){
			p.lookAheadToken = new Token(TokenType.ASCII,
					String.valueOf(p.line.charAt(p.cursor++)));
		}
		return;
	}

	/**
	 * Get a (possibly backslash-quoted) character. The sequences recognized are:<br>
	 * \b - \x0008: backspace BS<br>
	 * \t - \x0009: horizontal tab HT<br>
	 * \n - \x000A: linefeed LF<br>
	 * \f - \x000C: form feed FF<br>
	 * \r - \x000D: carriage return CR<br>
	 * \" - \x0022: double quote "<br>
	 * \' - \x0027: single quote '<br>
	 * \\ - \x005C: backslash \<br>
	 * \x<hex> // where <hex> is one to four hex digits<br>
	 * \X<hex> // where <hex> is one to four hex digits<br>
	 * In all other cases, the \ is ignored, and the subsequent character is returned.
	 * If the \ is the last character on the line, then a \n (LF) is returned
	 * @return character dataValue. The linePosition is just past the character (at the terminating ')
	 */
	private char getQuotedCharacter(){
		char c = p.line.charAt(p.cursor++);		// get character dataValue and move
		if(c != '\\') return c;					// if no backslash, we are done	
		if(p.cursor >= p.lineLength ) return '\n';	// have a \ terminated line; return \n	
		c = p.line.charAt(p.cursor++);			// get character after backslash and advance
		switch(c){
		case 'b':
			return '\b';
		case 't':
			return '\t';
		case 'n':
			return '\n';
		case 'f':
			return '\f';
		case 'r':
			return '\r';
		case '\\':
			return '\\';
		case '\'':
			return '\'';
		case '"':
			return '"';
		case 'x':
		case 'X':
			if(p.cursor >= p.lineLength) return 0;	// have a \x terminated line, return \x0000
			int startPosition = p.cursor;			// startPosition is first character after \x
			for(int i=0; i<4; i++){						// pick up to four hex characters
				c = p.line.charAt(p.cursor++);	// next character
				// if we reached a non-Hex character, break
				if(!(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F')) break;
				if(p.cursor >= p.lineLength) break;	// went past the end of line
			}
			return (char) Integer.parseInt(p.line.substring(startPosition,p.cursor), 16);
		default:
			return c;		// for all others, a '\c' is simply a 'c'
		}
	}

	/**
	 * Obtain an identifier. An identifier currently follows the same rules as a Java identifier. Returns
	 * true if an identifier was found (in lookAheadToken), false otherwise. LinePosition is advanced to just
	 * beyond the identifier
	 */
	private boolean getIdentifier(){
		char c = p.line.charAt(p.cursor);
		StringBuilder b = new StringBuilder();
		if(Character.isJavaIdentifierStart(c)){
			b.append(c);
			p.cursor++;
			while(p.cursor < p.lineLength && Character.isJavaIdentifierPart(c=p.line.charAt(p.cursor))){
				b.append(c);
				p.cursor++;
			}
			p.lookAheadToken = new Token(TokenType.IDENTIFIER,b.toString());
			return true;	
		}
		return false;
	}

	/**
	 * Obtain a string dataValue. A string dataValue is one or more strings in sequence.
	 */
	private String stringValue() {
		// stringValue = 1*( """ *stringChar """ )
		if(!p.lookAheadToken.is(TokenType.STRING_VALUE)){
			error("Expected STRING_VALUE, found "+p.lookAheadToken);
		}
		StringBuilder b = new StringBuilder();
		while(p.lookAheadToken.is(TokenType.STRING_VALUE)) {
			b.append(skipOver(TokenType.STRING_VALUE).value);
		}
		return b.toString();
	}

	/**
	 * Obtain a numeric dataValue, and update lookAheadToken.<br>
	 * Numerical values include BINARY_VALUE, OCTAL_VALUE, DECIMAL_VALUE, HEX_VALUE,FLOAT_VALUE
	 * @return - true if a numeric dataValue was found, false otherwise
	 */
	private boolean numericValue(){
		boolean isHex = false, seenDecimal = false, needSign = true;
		boolean needDigit = true, isBool = true, isInt = true;

		StringBuilder b = new StringBuilder();		
		while(p.cursor < p.lineLength){
			char c = p.line.charAt(p.cursor);
			if(Character.isDigit(c)){
				b.append(c);
				needSign = needDigit = false;		// we have seen a digit, so the inferred sign is +
				if(isBool && (c != '0' && c != '1')) {
					isBool = false;					// seen a non-0/1 digit. Boolean no longer possible
				}
			} else if(isHex && (c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f')){
				b.append(c);
				needDigit = needSign = isBool = false;
			} else if((c == 'x' || c == 'X')){
				if((b.length() == 1 && b.charAt(0) == '0') ||	// 0x..
						(b.length() == 2 && b.charAt(1) == '0' && 	// (+|-)0x..
						(b.charAt(0) == '+' || b.charAt(0) == '-'))){
					b.append(c);
					needDigit = isHex = true;
					needSign = isBool = false;
				} else {
					break;	// x is part of the next token. We are done.
				}
			} else if(c == '.'){
				// check for decimal rules
				if(isHex || seenDecimal) break;	// . is part of the next token, we are done.
				b.append(c);
				needSign = isBool = isHex = isInt = false;
				needDigit = seenDecimal = true;
			} else if(needSign && (c == '+' || c == '-')){
				// sign can only be at the beginning or after (e | E)
				b.append(c);
				needSign = false;
			} else if(!needDigit && seenDecimal && (c == 'E' || c == 'e')){
				b.append(c);
				needSign = needDigit = true;
			} else if(isBool && (c == 'B' || c == 'b')){
				// boolean termination reached
				isInt = false;
				b.append(c);
				p.cursor++;
				break;
			} else {
				// reached a non-number character
				break;
			}
			p.cursor++;
		}
		p.lookAheadToken = new Token(TokenType.NUMBER, b.toString());
		// System.out.println(currentLine);
		// System.out.println("Number Token ["+b.toString()+"] ends at "+lineNumber+":"+linePosition);
		return true;
	}

	/** 
	 * read in the next non-empty line (if any).
	 * @return true if line successfully read, false if end of stream is reached
	 */
	private boolean nextLine(){
		// read next non-empty line, set the cursor to the first non-whitespace character, and the length
		// to be just past the last non-whitespace character
		try {
			while ((p.line = p.input.readLine()) != null){
				// System.out.println(p.line);
				p.lineNumber++;
				p.cursor = 0;
				p.lineLength = p.line.length();
				// trim trailing and leading spaces
				while(p.lineLength > 0 && Character.isWhitespace(p.line.charAt(p.lineLength-1))) p.lineLength--;
				while(p.cursor < p.lineLength && Character.isWhitespace(p.line.charAt(p.cursor))) p.cursor++;
				if(p.lineLength > 0 && p.cursor < p.lineLength) break; // have at least 1 non-whitespace character on line
			}
			// null line implies EOF reached on the Reader on the current file
			// close the input file and return false
			if(p.line == null) {
				p.input.close();
				return false;
			}
		} catch (IOException e){
			logger.finest(e.getMessage());
			return false;
		}
		if(debug) System.out.println("// (nextline) -> "+p.line.substring(p.cursor, p.lineLength));
		return true;
	}

	/**
	 * Push current parser state on stack, and open a new input file for reading
	 * @param fileName - name of file to read (relative to current directory)
	 */
	private void openNewInput(String fileName) {
		// filePath = [absoluteFilePrefix] relativeFilePath
		// relativeFilePath = IDENTIFIER *( pathDelimiter IDENTIFIER)
		// pathDelimiter = "/" / "\" absoluteFilePrefix = rootDirectory / driveLetter 
		// rootDirectory = pathDelimiter
		// driveLetter = UPPERALPHA ":" [pathDelimiter]

		if(debug) {
			System.out.println("Switching to file: "+fileName);
			if(p != null){
				System.out.println("Previous file: "+p.directory+p.file);
			}
		}
		// TODO: Currently we only handle relative paths. Also add checks when fileName contains full path information
		String lastDirectory = "";
		NameSpacePath lastPath = null;
		String lastOid = null;
		if(p != null){
			lastDirectory = p.directory;
			lastPath = p.path;
			lastOid = p.oid;
			inputStack.push(p);
		}
		p = new ParserState();
		p.directory = lastDirectory;
		p.file = fileName;
		p.path = lastPath;
		p.oid = lastOid;
		if(debug) System.out.println("OpenNewInput: Read File: ["+p.directory+"]"+p.file);
		String filePath = p.directory + p.file;
		try {
			p.input = new BufferedReader(new FileReader(filePath));
		} catch (FileNotFoundException e) {
			// If we don't find the file, try reading it as a resource
			InputStream resourceAsStream = getClass().getClassLoader().getResourceAsStream(filePath);
			if (resourceAsStream != null) {
				p.input = new BufferedReader(new InputStreamReader(resourceAsStream));
			}
		}
		if (p.input == null) {
			throw new ModelException(ExceptionReason.NOT_FOUND, "Could not find file or resource "+ filePath);
		}		
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Parser#getRepository()
	 */
	@Override
	public Repository getRepository() {
		return repository;
	}
	
	/**
	 * Parse one or more files into a repository
	 * @param args - program arguments in the form [options] fileName [fileName ...]
	 * The parser accepts the following options (defaults in [ ]):
	 * <ul>
	 * <li>-r repository : name of a directory in the file system where the parsed files will be added [repository]</li>
	 * <li>-n nameSpace : default namespace for the classes [/root/local]</li>
	 * <li>-l logLevel : logging level for parser messages [finest]</li>
	 * <li>-h : (must be first argument) print a usage message (also printed if no arguments are given)</li>
	 * </ul>
	 */
	public static void main(String[] args) {
		if (args.length == 0 || args.length >= 1 && args[0].startsWith("-h")) {
			System.out.println("Use: java MOFParser [-r repository] [-n namespace] [-l logLevel] [-i true|false] fileName [fileName ...]");
			System.out.println("Where\n\t-r repository : name of a directory in the file system where the parsed files will be added [null]");
			System.out.println("\t-n nameSpace : default namespace for the classes ["+Constants.defaultNameSpacePath+"]");
			System.out.println("\t-l logLevel : logging level for parser messages [finest]");
			System.out.println("\t-h : (must be first argument) print a usage message (also printed if no arguments are given)");
			System.out.println("\t-s true|false : true: show parsed output on stdout, false: do not output parsed output [false]");
			System.out.println("\t-d true|false : true: turn on debugging [false]");
			return;
		}
		String repoName = null;
		String nameSpace = null;
		String logLevel = "finest";
		Vector<String> files = new Vector<String>();
		for(int i=0; i<args.length; i++){
			if(args[i].equalsIgnoreCase("-r") && i < args.length-1){
				repoName = args[++i];
				continue;
			}
			if(args[i].equalsIgnoreCase("-n") && i < args.length-1){
				nameSpace = args[++i];
				continue;
			}
			if(args[i].equalsIgnoreCase("-l") && i < args.length-1){
				logLevel = args[++i];
				continue;
			}
			if(args[i].equalsIgnoreCase("-d") && i < args.length-1){
				debug = Boolean.parseBoolean(args[++i]);
				continue;
			}
			files.add(args[i]);
		}

		long startTime = System.currentTimeMillis();
		Repository repository;
		NameSpacePath path = nameSpace != null ? new NameSpacePath(nameSpace) : Constants.defaultNameSpacePath;
		repository = repoName != null ? new PersistentCache(repoName) : new InMemoryCache();
		MOFParser parser = new MOFParser(repository);
		Level level = Level.parse(logLevel.toUpperCase());
		Logger l = logger;
		l.setLevel(level);
		while(l.getParent() != null){
			l = l.getParent();
			l.setLevel(level);
		}
		for (String file : files) {
			try {
				parser.parse(file,path);
			} catch (Exception ex) {
				System.out.println(ex.toString());
				ex.printStackTrace();
				break;
			}
		}
		long endTime = System.currentTimeMillis();
		System.out.println("Parse Time: " + (endTime - startTime));
		startTime = System.currentTimeMillis();
		repository.shutdown();
		long shutTime = System.currentTimeMillis();
		System.out.println("Shutdown Time: " + (shutTime - startTime));
		return;

	}
}
