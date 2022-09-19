/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
 * Created Dec 18, 2015 by Sharad Singhal
 */
package net.aifusion.cql;

/**
 * @author Sharad Singhal
 *
 */
enum TokenType {
	// value is the ascii input value corresponding to the token type
	AND("and",false),
	ANY("any",false),
	AS("as",false),
	ASC("asc",false),
	BOOLEAN(null,false),
	BY("by",false),
	CLASSQUALIFIER("classqualifier",false),
	COMMA(",",false),
	DESC("desc",false),
	DELETE("delete",false),
	DISTINCT("distinct",false),
	EOF(null,false),
	EQUALS("=",false),
	EVERY("every",false),
	FALSE("false",false),
	FIRST("first",false),
	FROM("from",false),
	GE(">=",false),
	GT(">",false),
	IDENTIFIER(null,true),
	IN("in",false),
	IS("is",false),
	ISNOT("isnot",false),
	ISA("isa",false),
	LE("<=",false),
	LBRACE("{",false),
	LIKE("like",false),
	LPAREN("(",false),
	LT("<",false),
	NE("<>",false),
	NOT("not",false),
	NULL("null",false),
	OR("or",false),
	ORDER("order",false),
	PERIOD(".",false),
	PROPERTYQUALIFIER("propertyQualifier",false),
	QUALIFIER("Qualifier",false),
	RBRACE("}",false),
	RPAREN(")",false),
	SATISFIES("satisfies",false),
	SELECT("select",false),
	STAR("*",false),
	TRUE("true",false),
	WHERE("where",false),
	COMPARISON(null,false),
	WHITE_SPACE(null,false),
	
	COMMENT(null,false),
	SEMICOLON(";",false),
	LBRACKET("[",false),
	RBRACKET("]",false),
	COLON(":",false),
	QUOTED_CHARACTER(null,false),
	ERROR(null,false),
	STRING_VALUE(null,false),
	HASH("#",false),
	CHARACTER(null,false),
	NUMBER(null,false),
	INTEGER(null,false),
	REAL(null,false),
	BINARY(null,false),
	HEX(null,false),
	DATETIMETOMICROSECONDS("dateTimeToMicroseconds",true),
	STRINGTOUINT("stringToUint",true),
	STRINGTOSINT("stringToSint",true),
	STRINGTOREAL("stringToReal",true),
	UPPERCASE("uppercase",true),
	NUMERICTOSTRING("numerictostring",true),
	REFERENCETOSTRING("referencetostring",true),
	INSTANCEOF("instanceof",true),
	CLASSPATH("classpath",true),
	OBJECTPATH("objectpath",true),
	CURRENTDATETIME("currentdatetime",true),
	DATETIME("datetime",true),
	MICROSECONDTOTIMESTAMP("microsecondtotimestamp",true),
	MICROSECONDTOINTERVAL("microsecondtointerval",true),
	SCOPE("::",false),
	SLASH("/",false),
	PLUS("+",false),
	MINUS("-",false),
	RANGE("..",false),
	CONCAT("||",false),
	SIGN (null,false),
	UPDATE("update",true),
	SET("set",true);
	
	private final String value;
	private boolean isIdentifier;
	private TokenType(String value,boolean isIdentifier){
		this.value = value;
		this.isIdentifier = isIdentifier;
	}
	
	/**
	 * Get all predefined Token Types
	 * @return - array containing all pre-defined token types
	 */
	static TokenType[] getTokenTypes(){
		return TokenType.class.getEnumConstants();
	}
	
	/**
	 * Get the string value for this token type
	 * @return - string value for this token type. Null if none defined
	 */
	String getValue(){
		return value;
	}

	/**
	 * Check if this tokenType can be used as an identifier
	 * @return - true if this tokenType can be used as an identifier, false otherwise
	 */
	boolean isIdentifier() {
		return isIdentifier;
	}

}
