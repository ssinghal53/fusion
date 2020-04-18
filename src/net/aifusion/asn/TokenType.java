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
 * Reserved keywords known in ASN.1 Rec. X.680 (07/2002) page 22
 * @author Sharad Singhal
 */
public enum TokenType {
	// Keywords must be ordered such that if an asn1Value is a prefix of a second asn1Value,
	// the prefix must be defined later to allow the longer string to be matched first
	
	// ASN.1 Reserved words
	ABSENT("ABSENT"),
	ABSTRACT_SYNTAX("ABSTRACT-SYNTAX"),
	ALL("ALL"),
	ANY("ANY"),
	APPLICATION("APPLICATION"),
	AUTOMATIC("AUTOMATIC"),
	BEGIN("BEGIN"),
	BIT_STRING("BIT STRING"),
	BIT("BIT"),
	BMP_STRING("BMPString"), 
	BOOLEAN("BOOLEAN"),
	BY("BY"),
	CHARACTER_STRING("CHARACTER STRING"),
	CHARACTER("CHARACTER"),
	CHOICE("CHOICE"),
	CLASS("CLASS"),
	COMPONENTS("COMPONENTS"),
	COMPONENT("COMPONENT"),
	CONSTRAINED("CONSTRAINED"), 
	CONTAINING("CONTAINING"),
	CONTEXT_SPECIFIC("CONTEXT-SPECIFIC"),
	DEFAULT("DEFAULT"),
	DEFINED_BY("DEFINED BY"),
	DEFINED("DEFINED"),
	DEFINITIONS("DEFINITIONS"),
	DESCRIPTOR("DESCRIPTOR"),
	EMBEDDED_PDV("EMBEDDED PDV"),
	EMBEDDED("EMBEDDED"),
	ENCODED("ENCODED"),
	END("END"),
	ENUMERATED("ENUMERATED"),
	EXCEPT("EXCEPT"), 
	EXPLICIT("EXPLICIT"),
	EXPORTS("EXPORTS"),
	EXTENSIBILITY_IMPLIED("EXTENSIBILITY IMPLIED"),
	EXTENSIBILITY("EXTENSIBILITY"),
	EXTERNAL("EXTERNAL"),
	FALSE("FALSE"),
	FROM("FROM"),
	GENERALIZED_TIME("GeneralizedTime"), 
	GENERAL_STRING("GeneralString"),
	GRAPHIC_STRING("GraphicString"),
	IA5_STRING("IA5String"), 
	IDENTIFIER("IDENTIFIER"),
	IMPLICIT("IMPLICIT"),
	IMPLIED("IMPLIED"),
	IMPORTS("IMPORTS"),
	INCLUDES("INCLUDES"),
	INSTANCE_OF("INSTANCE OF"),
	INSTANCE("INSTANCE"),
	INTEGER("INTEGER"), 
	INTERSECTION("INTERSECTION"),
	ISO646_STRING("ISO646String"), 
	MAX("MAX"),
	MINUS_INFINITY("MINUS-INFINITY"),
	MIN("MIN"),
	NULL("NULL"),
	NUMERIC_STRING("NumericString"),
	OBJECT_DESCRIPTOR("ObjectDescriptor"),
	OBJECT_IDENTIFIER("OBJECT IDENTIFIER"),
	OBJECT("OBJECT"),
	OCTET_STRING("OCTET STRING"),
	OCTET("OCTET"),
	OF("OF"),
	OPTIONAL("OPTIONAL"),
	PATTERN("PATTERN"),
	PDV("PDV"),
	PLUS_INFINITY("PLUS-INFINITY"),
	PRESENT("PRESENT"),
	PRINTABLE_STRING("PrintableString"),
	PRIVATE("PRIVATE"),
	REAL("REAL"),
	RELATIVE_OID("RELATIVE-OID"),
	SEQUENCE_OF("SEQUENCE OF"),
	SEQUENCE("SEQUENCE"),
	SET_OF("SET OF"),
	SET("SET"),
	SIZE("SIZE"),
	STRING("STRING"),
	SYNTAX("SYNTAX"),
	T61_STRING("T61String"), 
	TAGS("TAGS"),
	TELETEX_STRING("TeletexString"), 
	TRUE("TRUE"),
	TYPE_IDENTIFIER("TYPE-IDENTIFIER"),
	UNION("UNION"),
	UNIQUE("UNIQUE"),
	UNIVERSAL("UNIVERSAL"),
	UNIVERSAL_STRING("UniversalString"),
	UTC_TIME("UTCTime"), 
	UTF8_STRING("UTF8String"), 
	VIDEOTEX_STRING("VideotexString"),
	VISIBLE_STRING("VisibleString"),
	WITH_SYNTAX("WITH SYNTAX"),
	WITH("WITH"),
	
	// non-word tokens
	TRIPLE_DOT("..."),
	DOUBLE_DOT(".."),
	DOT("."),
	COMMA(","),
	ASSIGNMENT("::="),
	COLON(":"),
	EQUALS("="),
	SEMI_COLON(";"),
	LEFT_PAREN("("),
	RIGHT_PAREN(")"),
	LEFT_BRACE("{"),
	RIGHT_BRACE("}"),
	LEFT_BRACKET("["),
	RIGHT_BRACKET("]"),
	MINUS("-"),
	LESS_THAN("<"),
	GREATER_THAN(">"),
	UNDERSCORE("_"),
	AMPERSAND("&"),
	VERTICAL_BAR("|"),
	CARAT("^"),
	AT("@"),
	STAR("*"),
	NOT("!"),
	
	// Other interim tokens used
	EOF(""),
	BINARY_STRING(""),
	QUOTED_STRING(""),
	IDENTIFIER_STRING(""),
	TYPE_REFERENCE(""),
	CLASS_REFERENCE(""),
	NUMBER_STRING(""),
	REAL_NUMBER_STRING(""),
	WHITESPACE(""),
	COMMENT("")
	;
	
	/** Matching text in ASN.1 */
	private final String asn1Value;
	/**
	 * Create an ASN.1 Lexical Token
	 * @param value - text matching the token
	 */
	private TokenType(String value){
		this.asn1Value = value;
		return;
	}
	
	/**
	 * Get the ASN.1 text matching this token
	 * @return - asn.1 text. May be empty for non-text tokens used in the parser
	 */
	public String asn1Value() {
		return asn1Value;
	}
}