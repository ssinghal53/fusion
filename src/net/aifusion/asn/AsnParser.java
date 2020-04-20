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
 * Created Apr 21, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.io.File;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import net.aifusion.metamodel.ModelException;

/**
 * Class to construct AST for ASN.1 Specifications
 * @author Sharad Singhal
 */
public class AsnParser {
	private Tokenizer tokenizer;
	private SymbolTable symbolTable = null;
	private AsnNode root = AsnProduction.ROOT.getNode();
	private AsnModule currentModule = null;
	private AsnNode currentAssignments = null;
	private boolean debug = false;
	
	/** pre-defined ASN.1 definitions used as needed */
	private static HashMap<String,String> knownClassDefinitions = new HashMap<String,String>();
	static {
		knownClassDefinitions.put("TYPE-IDENTIFIER",
				"TYPE-IDENTIFIER ::= CLASS {\r\n"
				+ "\t&id OBJECT IDENTIFIER UNIQUE,\r\n"
				+ "\t&Type\r\n"
				+ "} WITH SYNTAX {&Type IDENTIFIED BY &id}\r\n");
		knownClassDefinitions.put("ABSTRACT-SYNTAX", "ABSTRACT-SYNTAX ::= CLASS {\r\n"
				+ "\t&id OBJECT IDENTIFIER UNIQUE,\r\n"
				+ "\t&Type,\r\n"
				+ "\t&property BIT STRING {handles-invalid-encodings(0)} DEFAULT {}\r\n"
				+ "} WITH SYNTAX {\r\n"
				+ "\t&Type IDENTIFIED BY &id [HAS PROPERTY &property]\r\n"
				+ "}\r\n");
	}

	/**
	 * Class to build an ASN.1 Syntax tree
	 */
	public AsnParser() {
		return;
	}
	/*
	 * **********************************
	 * Helper methods to access tokenizer
	 * **********************************
	 */
	/**
	 * Skip over a token, and return it
	 * @param expected - token type expected
	 * @return - corresponding token
	 */
	private Token skipOver(TokenType expected) {
		return tokenizer.skipOver(expected);
	}
	/**
	 * Check if the lookahead token is a given token type
	 * @param expected - expected token type
	 * @return - true if the lookahead token is the expected type, false otherwise
	 */
	private boolean lookAheadIs(TokenType expected) {
		return tokenizer.lookAheadIs(expected);
	}
	/**
	 * Check if the token at a given offset from the current cursor is of a given type
	 * @param expected - expected token type
	 * @param offset - offset from the current cursor
	 * @return - true if the selected token is of the given type, false otherwise
	 */
	private boolean lookAheadIs(TokenType expected, int offset) {
		return tokenizer.lookAheadIs(expected, offset);
	}
	/**
	 * Get the lookahead token without advancing the cursor
	 * @return - lookahead token
	 */
	private Token lookAhead() {
		return tokenizer.lookAhead();
	}
	/**
	 * Get the lookahead token at a given offset without advancing the cursor
	 * @param offset - offset for the lookahead token
	 * @return - lookahead token
	 */
	private Token lookAhead(int offset) {
		return tokenizer.lookAhead(offset);
	}
	/**
	 * throw an exception with a given message
	 * @param message - exception message
	 */
	private void error(String message) {
		System.out.flush();
		throw new ModelException(message+" LookAhead "+lookAhead()+" "+lookAhead(1)+" "+lookAhead(2)+" "+lookAhead(3)+" "+lookAhead(4));
	}
	/**
	 * write a warning message on standard out
	 * @param message - warning message
	 */
	private void warn(String message) {
		if(debug) {
			System.out.println("\n** W ** "+message);
			System.out.flush();
		}
		return;
	}
	/*
	 * ***********************************
	 * External interface to parser
	 * ***********************************
	 */
	/**
	 * Get the symbol table being used by the parser
	 * @return - symbolTable for the parser
	 */
	public SymbolTable getSymbolTable() {
		return symbolTable;
	}
	/**
	 * Parse a file for ASN.1 Modules
	 * @param s - String containing definitions
	 * @return root node of the AST
	 */
	public AsnNode parse(String s) {
		tokenizer = new Tokenizer(s);
		symbolTable = new SymbolTable(tokenizer);
		root.addChild(moduleDefinition());
		while(!lookAheadIs(TokenType.EOF)){
			root.addChild(moduleDefinition());
		}
		return root;
	}
	/**
	 * Parse a file for ASN.1 Modules
	 * @param file - file containing definitions
	 * @return - root node of the AST
	 */
	public AsnNode parse(File file){
		tokenizer = new Tokenizer(file);
		symbolTable = new SymbolTable(tokenizer);
		root.addChild(moduleDefinition());
		while(!lookAheadIs(TokenType.EOF)){
			root.addChild(moduleDefinition());
		}
		return root;
	}
	/*
	 * *******************************
	 * ASN.1 Grammar Methods
	 * *******************************
	 */
	private AsnNode actualParameter(){
		//Used By:  ActualParameterList
		// Type | Value | ValueSet | DefinedObjectClass | Object | ObjectSet
		trace("actualParameter");
		AsnNode ap = null;
		if(lookAhead().isBuiltInType() || lookAheadIs(TokenType.TYPE_REFERENCE)) {
			ap = type(null);
		} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)){
			ap = new AsnNode(AsnProduction.VALUE_REFERENCE,skipOver(TokenType.IDENTIFIER_STRING));
		} else {
			error("not implemented");
		}
		return ap;
	}
	private AsnNode actualParameterList(AsnNode parent){
		//Used By:  ParameterizedType ParameterizedObjectClass ParameterizedObjectSet ParameterizedValue ParameterizedObject ParameterizedValueSetType
		// "{", ActualParameter, ","+, "}"
		trace("actualParameterList");
		AsnNode pList = new AsnNode(AsnProduction.ACTUAL_PARAMETER_LIST);
		skipOver(TokenType.LEFT_BRACE);
		pList.addChild(actualParameter());
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			pList.addChild(actualParameter());
		}
		skipOver(TokenType.RIGHT_BRACE);
		return pList;
	}
	private AsnNode parameterizedObject(AsnNode parent){
		//Used By:  ReferencedObjects Object
		// DefinedObject, ActualParameterList
		trace("parametrizedObject");
		AsnNode po = null;
		AsnNode defObj = definedObject(parent);
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			po = new AsnNode(AsnProduction.PARAMETERIZED_OBJECT);
			po.addChild(defObj);
			po.addChild(actualParameterList(parent));
			return po;
		}
		return defObj;
	}
	private AsnNode parameterizedObjectSet(AsnNode parent){
		//Used By:  ReferencedObjects ObjectSetElements
		// DefinedObjectSet, ActualParameterList
		trace("parametrizedObjectSet");
		AsnNode defObjSet = definedObjectSet(parent);
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			AsnNode pos = new AsnNode(AsnProduction.PARAMETERIZED_OBJECT_SET);
			pos.addChild(defObjSet);
			pos.addChild(actualParameterList(parent));
			return pos;
		}
		return defObjSet;
	}
	private AsnNode parameterizedObjectClass(AsnNode parent){
		//Used By:  ObjectClass
		// DefinedObjectClass, ActualParameterList
		trace("parameterizedObjectClass");
		AsnNode oc = definedObjectClass();
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			AsnNode poc = new AsnNode(AsnProduction.PARAMETERIZED_OBJECT_CLASS);
			poc.addChild(oc);
			poc.addChild(actualParameterList(parent));
			return poc;
		}
		return oc;
	}
	private AsnNode parameterizedValueSetType(AsnNode parent){
		//Used By:  DefinedType
		// SimpleDefinedType, ActualParameterList
		trace("parametrizedValueSetType");
		AsnNode sdt = simpleDefinedType();
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			AsnNode pvst = new AsnNode(AsnProduction.PARAMETERIZED_VALUE_SET_TYPE);
			pvst.addChild(sdt);
			pvst.addChild(actualParameterList(parent));
			return pvst;
		}
		return sdt;
	}
	private AsnNode parameterizedValue(AsnNode expectedType){
		//Used By:  DefinedValue
		// SimpleDefinedValue, ActualParameterList
		trace("parametrizedValue");
		AsnNode sdv = simpleDefinedValue(expectedType);
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			AsnNode pv = new AsnNode(AsnProduction.PARAMETERIZED_VALUE);
			pv.addChild(sdv);
			pv.addChild(actualParameterList(expectedType));
			return pv;
		}
		return sdv;
	}
	private AsnNode parameterizedType(AsnNode parameterList){
		//Used By:  DefinedType
		// SimpleDefinedType, ActualParameterList
		trace("parametrizedType");
		if(parameterList != null) showDebug(parameterList,"ParameterList> ",System.out);
		AsnNode sdt = simpleDefinedType();
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			AsnNode pt = new AsnNode(AsnProduction.PARAMETERIZED_TYPE);
			AsnNode type = pt.addChild(sdt);
			pt.addChild(actualParameterList(type));
			return pt;
		}
		return sdt;
	}
	private AsnNode simpleDefinedValue(AsnNode expectedType){
		//Used By:  ParameterizedValue
		// ExternalValueReference | valuereference
		trace("simpleDefinedValue");
		AsnNode sdv = null;
		if(lookAheadIs(TokenType.TYPE_REFERENCE) && lookAheadIs(TokenType.DOT,1) && lookAheadIs(TokenType.IDENTIFIER_STRING,2)) {
			sdv = externalValueReference(expectedType);
		} else {
			sdv = new AsnNode(AsnProduction.VALUE_REFERENCE,skipOver(TokenType.IDENTIFIER_STRING));
		}
		return sdv;
	}
	private AsnNode simpleDefinedType(){
		//Used By:  ParameterizedType ParameterizedValueSetType
		// ExternalTypeReference | typereference
		trace("SimpleDefinedType");
		AsnNode sdt = null;
		if(lookAheadIs(TokenType.DOT,1)) {
			sdt = externalTypeReference();
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE)){
			sdt = new AsnNode(AsnProduction.TYPE_REFERENCE,skipOver(TokenType.TYPE_REFERENCE));
		} else if(lookAhead().isBuiltInType()){
			sdt = type(null);
		} else {
			error("Not yet implemented");
		}
		return sdt;
	}
	private AsnNode dummyReference(AsnNode governor){
		//Used By:  Parameter DummyGovernor
		// Reference
		/*
		 * If the governor is	| and if the parameter name			| then the parameter is
		 * --------------------------------------------------------------------------------
		 * absent				| begins with an upper-case letter	| a type
		 * a type 				| begins with an lower-case letter	| a value
		 * a type 				| begins with an upper-case letter	| a value set
		 * absent 				| is entirely in upper-case letters | a class (or a type)
		 * a class name 		| begins with an lower-case letter	| an object
		 * a class name			| begins with an upper-case letter	| an object set
		 * --------------------------------------------------------------------------------
		 */
		
		trace("dummyReference");
		if(governor != null) {	// dummy reference is governed by the governor definition
			switch(governor.getNameToken().type()) {
			case TYPE_REFERENCE:
			case CLASS_REFERENCE:
				return AsnProduction.DUMMY_REFERENCE.getNode(skipOver(lookAhead().type()));
			default:
				error(governor+" not yet implememented");
				break;
			}
		} else {
			error("not yet implememented");
		}
		// dummy reference is other reference
		return reference(null);
	}
	private AsnNode dummyGovernor(){
		//Used By:  ParamGovernor
		// DummyReference
		return dummyReference(null);
	}
	private AsnNode governor(){
		//Used By:  UserDefinedConstraintParameter ParamGovernor
		// Type | DefinedObjectClass
		trace("governer");
		if(lookAhead().isBuiltInType()){
			return new AsnNode(AsnProduction.GOVERNOR,skipOver(lookAhead().type()));
		}
		AsnNode t = getTypeFor(lookAhead().value());
		// TODO: Check that t is either a TYPE_REFERENCE or an OBJECT_CLASS_REFERENCE
		if(t != null) {
			return new AsnNode(AsnProduction.GOVERNOR,skipOver(lookAhead().type()));
		}
		error("not implemented");
		return null;
	}
	private AsnNode paramGovernor(){
		//Used By:  Parameter
		// Governor | DummyGovernor
		trace("paramGoverner");
		if(lookAhead().isBuiltInType() || getTypeFor(lookAhead().value()) != null) {
			return governor();
		} else {
			return dummyGovernor();
		}
	}
	private AsnNode parameter(){
		//Used By:  ParameterList
		// ParamGovernor, ":", DummyReference | DummyReference
		/*
		 * If the governor is	| and if the parameter name			| then the parameter is
		 * --------------------------------------------------------------------------------
		 * absent				| begins with an upper-case letter	| a type
		 * a type 				| begins with an lower-case letter	| a value
		 * a type 				| begins with an upper-case letter	| a value set
		 * absent 				| is entirely in upper-case letters | a class (or a type)
		 * a class name 		| begins with an lower-case letter	| an object
		 * a class name			| begins with an upper-case letter	| an object set
		 * --------------------------------------------------------------------------------
		 */
		trace("parameter");
		AsnNode parameter = null;
		if(lookAheadIs(TokenType.COLON,1)) { // have a governor
			AsnNode governer = paramGovernor();
			skipOver(TokenType.COLON);
			AsnNode ref = dummyReference(governer);
			ref.addChild(governer);
			return ref;
		} else {	// governor is absent
			if(lookAheadIs(TokenType.CLASS_REFERENCE)) {
				parameter = new AsnNode(AsnProduction.PARAMETER,skipOver(TokenType.CLASS_REFERENCE));
				parameter.addChild(new AsnNode(AsnProduction.OBJECT_CLASS));
			} else if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
				parameter = new AsnNode(AsnProduction.PARAMETER,skipOver(TokenType.TYPE_REFERENCE));
				parameter.addChild(new AsnNode(AsnProduction.TYPE));
			} else if(lookAhead().isBuiltInType()) {
				parameter = new AsnNode(AsnProduction.PARAMETER,skipOver(lookAhead().type()));
				parameter.addChild(new AsnNode(AsnProduction.BUILTIN_TYPE));
			}
		}
		return parameter;
	}
	private AsnNode parameterList(){
		//Used By:  ParameterizedTypeAssignment ParameterizedValueAssignment ParameterizedObjectSetAssignment ParameterizedValueSetTypeAssignment ParameterizedObjectAssignment ParameterizedObjectClassAssignment
		// "{", Parameter, ","+, "}"
		trace("parameterList");
		AsnNode pList = new AsnNode(AsnProduction.PARAMETER_LIST);
		skipOver(TokenType.LEFT_BRACE);
		pList.addChild(parameter());
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			pList.addChild(parameter());
		}
		skipOver(TokenType.RIGHT_BRACE);
		return pList;
	}
	private AsnNode parameterizedObjectSetAssignment(){
		//Used By:  ParameterizedAssignment
		// objectsetreference, ParameterList, DefinedObjectClass, "::=", ObjectSet
		trace("parametrizedObjectSetAssignment");
		AsnNode posa = new AsnNode(AsnProduction.PARAMETERIZED_OBJECT_SET_ASSIGNMENT,skipOver(TokenType.TYPE_REFERENCE));
		posa.addChild(parameterList());
		AsnNode defn = posa.addChild(definedObjectClass());
		skipOver(TokenType.ASSIGNMENT);
		posa.addChild(objectSet(defn));
		return posa;
	}
	private AsnNode parameterizedObjectAssignment(){
		//Used By:  ParameterizedAssignment
		// objectreference, ParameterList, DefinedObjectClass, "::=", Object
		trace("parametrizedObjectAssignment");
		AsnNode poa = new AsnNode(AsnProduction.PARAMETERIZED_OBJECT_ASSIGNMENT,skipOver(TokenType.TYPE_REFERENCE));
		poa.addChild(parameterList());
		AsnNode defn = poa.addChild(definedObjectClass());
		skipOver(TokenType.ASSIGNMENT);
		poa.addChild(object(defn));
		return poa;
	}
	private AsnNode parameterizedObjectClassAssignment(){
		//Used By:  ParameterizedAssignment
		// objectclassreference, ParameterList, "::=", ObjectClass
		trace("parametrizedObjectClassAssignment");
		AsnNode poca = new AsnNode(AsnProduction.PARAMETERIZED_OBJECT_CLASS_ASSIGNMENT,skipOver(TokenType.CLASS_REFERENCE));
		poca.addChild(parameterList());
		skipOver(TokenType.ASSIGNMENT);
		AsnNode oc = objectClass();
		poca.addChild(oc);
		return poca;
	}
	private AsnNode parameterizedValueSetTypeAssignment(){
		//Used By:  ParameterizedAssignment
		// typereference, ParameterList, Type, "::=", ValueSet
		trace("parametrizedValueSetTypeAssignment");
		AsnNode pvsta = new AsnNode(AsnProduction.PARAMETERIZED_VALUE_SET_TYPE_ASSIGNMENT,skipOver(TokenType.TYPE_REFERENCE));
		pvsta.addChild(parameterList());
		AsnNode type = pvsta.addChild(type(null));
		skipOver(TokenType.ASSIGNMENT);
		pvsta.addChild(valueSet(type));
		return pvsta;
	}
	private AsnNode parameterizedValueAssignment(){
		//Used By:  ParameterizedAssignment
		// valuereference, ParameterList, Type, "::=", Value
		trace("parametrizedValueAssignment");
		AsnNode pva = new AsnNode(AsnProduction.PARAMETERIZED_VALUE_ASSIGNMENT,skipOver(TokenType.IDENTIFIER_STRING));
		pva.addChild(parameterList());
		AsnNode type = pva.addChild(type(null));
		skipOver(TokenType.ASSIGNMENT);
		pva.addChild(value(type));
		return pva;
	}
	private AsnNode parameterizedTypeAssignment(){
		//Used By:  ParameterizedAssignment
		// typereference, ParameterList, "::=", Type
		trace("parametrizedTypeAssignment");
		AsnNode pta = AsnProduction.PARAMETERIZED_TYPE_ASSIGNMENT.getNode(skipOver(TokenType.TYPE_REFERENCE));
		AsnNode parameterList = pta.addChild(parameterList());
		skipOver(TokenType.ASSIGNMENT);
		pta.addChild(type(parameterList));
		return pta;
	}
	private AsnNode parameterizedAssignment(){
		//Used By:  Assignment
		// ParameterizedTypeAssignment | ParameterizedValueAssignment | ParameterizedValueSetTypeAssignment |
		// ParameterizedObjectClassAssignment | ParameterizedObjectAssignment | ParameterizedObjectSetAssignment
		/*
		 * See Dubuisson p 108
		 * left assignment values
		 * 1st character of 1st the first lexeme | first character of the second lexeme ::= Assignment category
		 * Upper-case-letter  | none	::= type or information object class
		 * lower-case-letter  | Upper-case-letter ::= value or information object
		 * Upper-case-letter  | Upper-case-letter ::= value set or information object set
		 */
		trace("parametrizedAssignment");
		AsnNode a = null;
		Token first = lookAhead();	// first lexeme
		// skip all parameters
		int nesting = 0;
		int i = 1;
		do {
			if(lookAheadIs(TokenType.LEFT_BRACE,i)) nesting++;
			if(lookAheadIs(TokenType.RIGHT_BRACE,i)) nesting--;
			i++;
		} while(nesting > 0);
		Token second = lookAhead(i);	// second lexeme
		if(debug) {
			System.out.println("\tFirst : "+first+" Second : "+second+"\n");
			System.out.flush();
		}

		if(first.is(TokenType.CLASS_REFERENCE)){
			if(second.is(TokenType.CLASS_REFERENCE)) {
				// objectsetreference, ParameterList, DefinedObjectClass, "::=", ObjectSet
				a = parameterizedObjectSetAssignment();
			} else if(second.is(TokenType.ASSIGNMENT)){
				// objectclassreference, ParameterList, "::=", ObjectClass
				a = parameterizedObjectClassAssignment();
			} else {
				error("not implemented");
			}
		} else if(first.is(TokenType.TYPE_REFERENCE)) {
			if(second.is(TokenType.TYPE_REFERENCE) || second.isBuiltInType()) {
				// typereference, ParameterList, Type, "::=", ValueSet
				a = parameterizedValueSetTypeAssignment();
			} else if(second.is(TokenType.ASSIGNMENT)){
				// typeReference parameterList "::=" type
				a = parameterizedTypeAssignment();
			} else {
				error("not implemented");
			}
		} else if(first.is(TokenType.IDENTIFIER_STRING)) {
			if(second.is(TokenType.CLASS_REFERENCE)) {
				// objectreference, ParameterList, DefinedObjectClass, "::=", Object
				a = parameterizedObjectAssignment();
			} else if(second.is(TokenType.TYPE_REFERENCE) || second.isBuiltInType()) {
				// valueReference parameterlist Type "::=" value
				a = parameterizedValueAssignment();
			} else {
				error("not implemented");
			}
		} else {
			error("not implemented");
		}
		return a;
	}
	private AsnNode contentsConstraint(AsnNode parent){
		//Used By:  GeneralConstraint
		// CONTAINING, Type
		// ENCODED, BY, Value
		// CONTAINING, Type, ENCODED, BY, Value
		trace("contentsConstraint");
		AsnNode gc = null;
		if(lookAheadIs(TokenType.CONTAINING) || lookAheadIs(TokenType.ENCODED)) {
			gc = AsnProduction.GENERAL_CONSTRAINT.getNode();
			if(lookAheadIs(TokenType.CONTAINING)) {
				skipOver(TokenType.CONTAINING);
				AsnNode containing = gc.addChild(AsnProduction.CONTAINED_SUBTYPE.getNode());
				containing.addChild(type(null));
			}
			if(lookAheadIs(TokenType.ENCODED)) {
				skipOver(TokenType.ENCODED);
				skipOver(TokenType.BY);
				AsnNode enc = gc.addChild(AsnProduction.CONTENTS_CONSTRAINT.getNode());
				enc.addChild(value(parent));
			}
		} else {
			error("not handled");
		}
		return gc;
	}
	private AsnNode componentIdList(AsnNode parent){
		//Used By:  AtNotation
		// identifier, "."+
		AsnNode idList = new AsnNode(AsnProduction.COMPONENT_ID_LIST);
		idList.addChild(new AsnNode(AsnProduction.COMPONENT_ID,skipOver(TokenType.IDENTIFIER_STRING)));
		while(lookAheadIs(TokenType.DOT)) {
			skipOver(TokenType.DOT);
			idList.addChild(new AsnNode(AsnProduction.COMPONENT_ID,skipOver(TokenType.IDENTIFIER_STRING)));
		}
		return idList;
	}
	private AsnNode level(AsnNode parent){
		//Used By:  AtNotation Level
		// ".", Level | empty
		AsnNode level = null;
		if(lookAheadIs(TokenType.DOT)) {
			level = AsnProduction.LEVEL.getNode(skipOver(TokenType.DOT));
			level.addChild(level(parent));
		}
		return level;
	}
	private AsnNode atNotation(AsnNode parent){
		//Used By:  ComponentRelationConstraint
		// "@", ComponentIdList
		// "@.", Level, ComponentIdList
		AsnNode atN = AsnProduction.AT_NOTATION.getNode(skipOver(TokenType.AT));
		if(lookAheadIs(TokenType.DOT)) {
			atN.addChild(AsnProduction.LEVEL.getNode(skipOver(TokenType.DOT)));
			atN.addChild(level(parent));
		}
		atN.addChild(componentIdList(parent));
		return atN;
	}
	private AsnNode componentRelationConstraint(AsnNode parent){
		//Used By:  TableConstraint
		// "{", DefinedObjectSet, "}", "{", AtNotation, ","+, "}"
		AsnNode n = null;
		skipOver(TokenType.LEFT_BRACE);
		AsnNode dos = definedObjectSet(parent);
		skipOver(TokenType.RIGHT_BRACE);
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			n = AsnProduction.COMPONENT_RELATION_CONSTRAINT.getNode();
			n.addChild(dos);
			skipOver(TokenType.LEFT_BRACE);
			n.addChild(atNotation(parent));
			while(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				n.addChild(atNotation(parent));
			}
			skipOver(TokenType.RIGHT_BRACE);
			return n;
		}
		n = AsnProduction.SIMPLE_TABLE_CONSTRAINT.getNode();
		n.addChild(dos);
		return n;
	}
	private AsnNode simpleTableConstraint(AsnNode parent){
		//Used By:  TableConstraint
		// ObjectSet
		error("not implemented");
		return null;
	}
	private AsnNode tableConstraint(AsnNode parent){
		//Used By:  GeneralConstraint
		// SimpleTableConstraint | ComponentRelationConstraint
		// Note: SimpleTableConstraint is included in ComponentRelationConstraint
		return componentRelationConstraint(parent);
	}
	private AsnNode userDefinedConstraintParameter(AsnNode parent){
		//Used By:  UserDefinedConstraint
		// Governor, ":", Value
		// Governor, ":", ValueSet
		// Governor, ":", Object
		// Governor, ":", ObjectSet
		// Type
		// DefinedObjectClass
		error("not implemented");
		return null;
	}
	private AsnNode userDefinedConstraint(AsnNode parent){
		//Used By:  GeneralConstraint
		// CONSTRAINED, BY, "{", UserDefinedConstraintParameter, ",", *, "}"
		AsnNode uc = new AsnNode(AsnProduction.USER_DEFINED_CONSTRAINT);
		skipOver(TokenType.CONSTRAINED);
		skipOver(TokenType.BY);
		skipOver(TokenType.LEFT_BRACE);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			uc.addChild(userDefinedConstraintParameter(parent));
			while(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				uc.addChild(userDefinedConstraintParameter(parent));
			}
		}
		skipOver(TokenType.RIGHT_BRACE);
		return uc;
	}
	private AsnNode generalConstraint(AsnNode parent){
		//Used By:  ConstraintSpec
		// UserDefinedConstraint
		// TableConstraint
		// ContentsConstraint
		trace("GeneralConstraint");
		AsnNode gc = AsnProduction.GENERAL_CONSTRAINT.getNode();
		switch(lookAhead().type()) {
		case CONSTRAINED:
			gc.addChild(userDefinedConstraint(parent));
			break;
		case LEFT_BRACE:
			gc.addChild(tableConstraint(parent));
			break;
		case CONTAINING:
		case ENCODED:
			gc.addChild(contentsConstraint(parent));
		default:
			error("not implemented");
			break;
		}		
		return gc;
	}
	private AsnNode instanceOfValue(AsnNode parent){
		//Used By:  BuiltinValue
		// Value
		error("not implemented");
		return null;
	}
	private AsnNode instanceOfType(){
		//Used By:  BuiltinType
		// INSTANCE, OF, DefinedObjectClass
		// INSTANCE_OF definedObjectClass
		/*
		 * See Dubuisson Page 357
		 * InstanceofType only extracts information from TYPE-IDENTIFIER class, and is implicitly defined as
		 * SEQUENCE { 
		 * 		type-id TYPE-IDENTIFIER.&id,
		 * 		value [0] EXPLICIT TYPE-IDENTIFIER.&Type
		 * }
		 * Thus for a definedObjectClass, INSTANCE OF means
		 * SEQUENCE {
		 * 		type-id DefinedObjectClass.&id,
		 * 		value [0] EXPLICIT DefinedObjectClass.&Type
		 * }
		 * and INSTANCE OF DefinedObjectClass({ObjectSet}) means
		 * SEQUENCE {
		 * 		type-id DefinedObjectClass.&id ({ObjectSet}),
		 * 		value [0] DefinedObjectClass.&Type ({ObjectSet}{@.type-id}) 
		 * }
		 * 
		 * INSTANCE OF type has the tag [UNIVERSAL 8] (same as EXTERNAL)
		 */
		AsnNode iot = AsnProduction.INSTANCE_OF_TYPE.getNode(skipOver(TokenType.INSTANCE_OF));
		iot.addChild(definedObjectClass());
		return iot;
	}
	private AsnNode objectSetFromObjects(AsnNode parent){
		//Used By:  InformationFromObjects ObjectSetElements
		// ReferencedObjects, ".", FieldName
		error("not implemented");
		return null;
	}
	private AsnNode objectFromObject(AsnNode parent){
		//Used By:  Object InformationFromObjects
		// ReferencedObjects, ".", FieldName
		error("not implemented");
		return null;
	}
	private AsnNode typeFromObject(){
		//Used By:  InformationFromObjects ReferencedType
		// ReferencedObjects, ".", FieldName
		/*
	case CLASS_REFERENCE:
		mref = skipOver(TokenType.CLASS_REFERENCE);
		if(lookAheadIs(TokenType.DOT)) {
			skipOver(TokenType.DOT);
			if(lookAheadIs(TokenType.AMPERSAND)) {
				skipOver(TokenType.AMPERSAND);
				n = new AstNode()

			} else {
				n =  new AstNode(AstProduction.EXTERNAL_OBJECT_CLASS_REFERENCE,mref,skipOver(TokenType.TYPE_REFERENCE));
			}

		} else {
			n = new AstNode(AstProduction.EXTERNAL_OBJECT_CLASS_REFERENCE,mref);
		}
		break;
		 */
		error("not implemented");
		return null;
	}
	private AsnNode valueSetFromObjects(){
		//Used By:  InformationFromObjects ReferencedType
		// ReferencedObjects, ".", FieldName
		error("not implemented");
		return null;
	}
	private AsnNode valueFromObject(AsnNode parent){
		//Used By:  InformationFromObjects ReferencedValue
		// ReferencedObjects, ".", FieldName
		AsnNode vfo = AsnProduction.VALUE_FROM_OBJECT.getNode();
		vfo.addChild(referencedObjects(parent));
		skipOver(TokenType.DOT);
		vfo.addChild(fieldName(parent));
		return vfo;
	}
	private AsnNode referencedObjects(AsnNode parent){
		//Used By:  ObjectFromObject ValueFromObject TypeFromObject ObjectSetFromObjects ValueSetFromObjects
		// DefinedObject | ParameterizedObject | DefinedObjectSet | ParameterizedObjectSet
		// Note-- ParametrizedObject|ObjectSet will return DefinedObject | DefinedObjectSet if no parameters are given
		if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			return parameterizedObjectSet(parent);
		} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			return definedObject(parent);
		}
		error("not implemented");
		return null;
	}
	private AsnNode informationFromObjects(AsnNode targetObject){
		//Used By: Root
		// ValueFromObject | ValueSetFromObjects | TypeFromObject | ObjectFromObject | ObjectSetFromObjects
		/*
		 * Dubuisson page 338
		 * First part (a)	| Last field name (a) 	| Production (b)
		 * -------------------------------------------------------------
		 * 					| Fixed-type value 		| ValueFromObject
		 * 					| Variable-type value 	| ValueFromObject
		 * 		  			| Fixed-type value set 	| ValueSetFromObjects
		 * Object 			| Variable-type value set | ValueSetFromObjects
		 * 		 			| Type  				| TypeFromObject
		 * 		  			| Object 				|  ObjectFromObject
		 * 		  			| Object set 			| ObjectSetFromObjects
		 * --------------------------------------------------------
		 * 					| Fixed-type value 		| ValueSetFromObjects
		 * 					| Variable-type value 	| Impossible
		 * 					| Fixed-type value set 	| ValueSetFromObjects
		 * Object set 		| Variable-type value set | Impossible
		 * 					| Type 					| Impossible
		 * 					| Object 				| ObjectSetFromObjects
		 * 					| Object set 			| ObjectSetFromObjects
		 * ---------------------------------------------------------------
		 * (a) Let a dotted notation of the form "obj.&a.&b.&c.&d", the part
		 * "obj.&a.&b.&c" is called the first part and "&d" the last field name or
		 * "second part" (it is the object field or object set field pointed at by
		 * "obj.&a.&b.&c").
		 * (b) See Dubuisson Section 15.6.2 Page 339
		 */
		// all elements are of the form ReferencedObjects "." FieldName, where ReferencedObjects defines the First part and 
		// FieldName defines the second part
		StringBuilder b = new StringBuilder();
		if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			b.append(skipOver(TokenType.IDENTIFIER_STRING).value());
			// starting point is an object
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			// starting point is an object set
			b.append(skipOver(TokenType.TYPE_REFERENCE).value());
		} else {
			return null;
		}
		while(lookAheadIs(TokenType.DOT)) {
			b.append(skipOver(TokenType.DOT).value());
			b.append(skipOver(TokenType.AMPERSAND).value());
			b.append(skipOver(lookAhead().type()).value());
		}
		return null;
	}
	private AsnNode fixedTypeFieldVal(AsnNode expectedType){
		//Used By:  ObjectClassFieldValue
		// BuiltinValue | ReferencedValue
		error("not implemented");
		return null;
	}
	private AsnNode openTypeFieldVal(AsnNode expectedType){
		//Used By:  ObjectClassFieldValue
		// Type, ":", Value
		error("not implemented");
		return null;
	}
	private AsnNode objectClassFieldValue(AsnNode expectedType){
		//Used By:  Value
		// OpenTypeFieldVal | FixedTypeFieldVal
		trace("objectClassFieldValue"+expectedType);
		
		
		
		error("not implemented");
		return null;
	}
	private AsnNode objectClassFieldType(){
		// Should return null if no match found...
		// Should return the type corresponding to an object class field, if any. Should return null if
		// lookAhead is not an object class field
		// TODO: This may need back-tracking if we cannot find definedObjectClass
		//Used By:  BuiltinType
		// DefinedObjectClass, ".", FieldName
		trace("objectClassFieldType");
		AsnNode definedObjectClass = definedObjectClass();
		if(definedObjectClass == null) return null;
		// System.out.println(definedObjectClass);
		// System.out.flush();
		AsnNode ocft = new AsnNode(AsnProduction.OBJECT_CLASS_FIELD_TYPE);
		ocft.addChild(definedObjectClass);
		skipOver(TokenType.DOT);
		ocft.addChild(fieldName(ocft));
		return ocft;
	}
	private AsnNode objectSetElements(AsnNode expectedClassDefn){
		//Used By:  Elements
		// Object | DefinedObjectSet | ObjectSetFromObjects | ParameterizedObjectSet
		error("not implemented");
		return null;
	}
	private AsnNode objectSetSpec(AsnNode parent){
		//Used By:  ObjectSet
		// RootElementSetSpec | RootElementSetSpec, ",", "..." | "..." | "...", ",", AdditionalElementSetSpec | RootElementSetSpec, ",", "...", ",", AdditionalElementSetSpec
		trace("objectSetSpec");
		AsnNode oss = new AsnNode(AsnProduction.OBJECT_SET_SPEC);
		if(lookAheadIs(TokenType.TRIPLE_DOT)) {
			oss.addChild(new AsnNode(AsnProduction.EXTENSION,skipOver(TokenType.TRIPLE_DOT)));
			if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
				skipOver(TokenType.COMMA);
				oss.addChild(additionalElementSetSpec(parent));
			}
		} else {
			oss.addChild(rootElementSetSpec(parent));
			if(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				oss.addChild(new AsnNode(AsnProduction.EXTENSION,skipOver(TokenType.TRIPLE_DOT)));
				if(lookAheadIs(TokenType.COMMA)) {
					skipOver(TokenType.COMMA);
					oss.addChild(additionalElementSetSpec(parent));
				}
			}
		}
		return oss;
	}
	private AsnNode objectSet(AsnNode parent){
		//Used By:  ParameterizedObjectSetAssignment ObjectSetOptionalitySpec UserDefinedConstraintParameter ActualParameter Setting ObjectSetAssignment SimpleTableConstraint
		// "{", ObjectSetSpec, "}"
		trace("objectSet");
		AsnNode os = new AsnNode(AsnProduction.OBJECT_SET);
		skipOver(TokenType.LEFT_BRACE);
		os.addChild(objectSetSpec(parent));
		skipOver(TokenType.RIGHT_BRACE);
		return os;
	}
	private AsnNode objectSetAssignment(){
		//Used By:  Assignment
		// objectsetreference, DefinedObjectClass, "::=", ObjectSet
		AsnNode osa = new AsnNode(AsnProduction.OBJECT_SET_ASSIGNMENT,skipOver(TokenType.TYPE_REFERENCE));
		AsnNode dfc = definedObjectClass();
		osa.addChild(dfc);
		skipOver(TokenType.ASSIGNMENT);
		osa.addChild(objectSet(dfc));
		return osa;
	}
	private AsnNode externalObjectSetReference(){
		//Used By:  DefinedObjectSet
		// modulereference, ".", objectsetreference
		Token mref = skipOver(TokenType.TYPE_REFERENCE);
		skipOver(TokenType.DOT);
		return AsnProduction.EXTERNAL_OBJECT_SET_REFERENCE.getNode(mref, skipOver(TokenType.TYPE_REFERENCE));
	}
	private AsnNode definedObjectSet(AsnNode parent){
		//Used By:  ParameterizedObjectSet ReferencedObjects ComponentRelationConstraint ObjectSetElements
		// ExternalObjectSetReference | objectsetreference
		AsnNode dos = null;
		if(lookAheadIs(TokenType.TYPE_REFERENCE) && lookAheadIs(TokenType.DOT,1) && lookAheadIs(TokenType.TYPE_REFERENCE,2)) {
			dos = externalObjectSetReference();
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			dos = AsnProduction.OBJECT_SET_REFERENCE.getNode(skipOver(TokenType.TYPE_REFERENCE));
		} else {
			error("DefinedObjectSet reached null");
		}
		return dos;
	}
	private AsnNode setting(String fieldName, AsnNode definingClass, AsnNode currentObject){
		//Used By:  FieldSetting DefinedSyntaxToken
		// Type | Value | ValueSet | Object | ObjectSet
		// Note: Here the parent contains the defining class, while current contains the object being constructed
		// for variable-types, the type fields must precede the value fields, so the type can be located
		// see Dubuission Page 322 for details
		trace("setting");
		AsnNode fieldType = definingClass.locate(null,fieldName);
		if(fieldType == null) {
			error("Field "+fieldName+" not defined in CLASS "+definingClass);
		}
		// showTree(definingClass,"Defining Class> ",System.out);
		// System.out.print("\n");
		showDebug(currentObject,"Current Object> ",System.out);
		// System.out.print("\n");
		// System.out.println("\nField Name: "+fieldName);
		// showTree(fieldType,"Field type> ",System.out);
		// System.out.print("\n");
		// System.out.flush();
		AsnNode setting = null;
		switch(fieldType.getProduction()) {
		case FIXED_TYPE_VALUE_FIELD_SPEC:	// the field contains a value of a known type
			setting = new AsnNode(AsnProduction.FIXED_TYPE_VALUE_FIELD_SPEC);
			AsnNode type = fieldType.getChild(0);
			setting.addChild(new AsnNode(type.getProduction()));
			setting.addChild(value(type));
			break;
		case TYPE_FIELD_SPEC:	// the field contains a type name
			setting = new AsnNode(AsnProduction.TYPE_FIELD_SPEC);
			setting.addChild(type(null));
			break;
		case VARIABLE_TYPE_VALUE_SET_FIELD_SPEC:	// the field contains a value set whose type is defined in another field
			setting = new AsnNode(AsnProduction.VARIABLE_TYPE_VALUE_SET_FIELD_SPEC);
			String typeName = fieldType.getChild(0).getChild(0).getName();
			// System.out.println("TypeName :"+typeName+"\n");
			AsnNode typeNode = currentObject.locate(null,typeName);
			// showTree(typeNode,"TypeNode >",System.out);
			AsnNode currentType = typeNode.getChild(0).getChild(0);
			// System.out.println("CurrentType :"+currentType);
			// System.out.print("\n");
			// System.out.flush();
			// need a value set here conforming to the type of its argument type
			setting.addChild(valueSet(currentType));
			break;
		case VARIABLE_TYPE_VALUE_FIELD_SPEC:	// the field contains a value, whose type is defined in another field
			setting = new AsnNode(AsnProduction.VARIABLE_TYPE_VALUE_FIELD_SPEC);
			typeName = fieldType.getChild(0).getChild(0).getName();	// fieldtype.fieldName.primitiveFieldName
			typeNode = currentObject.locate(null,typeName);
			currentType = typeNode.getChild(0).getChild(0);
			setting.addChild(new AsnNode(currentType.getProduction(),currentType.getNameToken(),currentType.getValueToken()));
			// System.out.println("TypeName :"+typeName+"\n");
			// System.out.println("CurrentType :"+currentType);
			// showTree(typeNode,"TypeNode >",System.out);
			// System.out.print("\n");
			// System.out.flush();
			setting.addChild(value(currentType));
			break;
		case OBJECT_FIELD_SPEC:					// the field contains a reference to an object of a known type
			setting = new AsnNode(AsnProduction.OBJECT_FIELD_SPEC);
			showDebug(fieldType,"FieldType > ",System.out);
			type = fieldType.getChild(0);
			setting.addChild(new AsnNode(type.getProduction()));
			setting.addChild(value(type));
			break;
		case OBJECT_SET_FIELD_SPEC:				// the field contains set of objects that are instances of the given class
		case FIXED_TYPE_VALUE_SET_FIELD_SPEC:	// the field contains a value set of known type
			// TBD -- 
		default:
			System.out.println("\nField Name: "+fieldName);
			showTree(fieldType,"Field type> ",System.out);
			showTree(definingClass,"Defining Class> ",System.out);
			showTree(currentObject,"Current Object> ",System.out);
			System.out.print("\n");
			System.out.flush();
			error(fieldType+" not yet implemented");
			break;
		}
		// showTree(setting,"Setting > ",System.out);
		// System.out.print("\n");
		// System.out.flush();
		return setting;
	}
	private AsnNode definedSyntaxToken(AsnNode definingClass, AsnNode currentObject, AsnNode expectedSyntax){
		//Used By:  DefinedSyntax
		// Literal | Setting
		AsnNode dst = null;
		switch(expectedSyntax.getProduction()) {
		case LITERAL:
			Token incoming = lookAhead();
			Token expected = expectedSyntax.getNameToken();
			// System.out.println("Expected : "+expected+" Incoming "+incoming);
			if(incoming.is(expected.type()) || expected.value().equals(incoming.value())) {
				skipOver(incoming.type());
			} else {
				error("Expected : "+expected+" Incoming "+incoming);
			}
			break;
		case PRIMITIVE_FIELD_NAME:
			// System.out.println("Expected PrimitiveFieldName "+expectedSyntax+" LookAhead "+lookAhead());
			dst = new AsnNode(AsnProduction.FIELD_SETTING,expectedSyntax.getNameToken());
			// AstNode fieldName = primitiveFieldName();
			// fieldSetting.addChild(fieldName);
			dst.addChild(setting(expectedSyntax.getName(),definingClass,currentObject));
			break;
		case OPTIONAL_GROUP:
			expected = expectedSyntax.getChild(0).getNameToken();
			incoming = lookAhead();
			if(!incoming.is(expected.type()) || !expected.value().equals(incoming.value())) break;
			for(AsnNode g : expectedSyntax.getChildren()) {
				currentObject.addChild(definedSyntaxToken(definingClass,currentObject,g));
			}
			break;
		default:
			error(expectedSyntax+": not implemented");
		}
		return dst;
	}
	private AsnNode definedSyntax(AsnNode objectClassDefn){
		//Used By:  ObjectDefn
		// "{", DefinedSyntaxToken, empty, *, "}"
		trace("definedSyntax");
		showDebug(objectClassDefn,"DefinedSyntax > ",System.out);
		AsnNode objDefn = AsnProduction.OBJECT.getNode();
		skipOver(TokenType.LEFT_BRACE);
		int cursor = 0;
		AsnNode syntaxList = getSyntax(objectClassDefn).getChild(0);
		showDebug(syntaxList, "SyntaxList >", System.out);
		while(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			// ds.addChild(definedSyntaxToken(parent,ds,syntaxList,cursor++));
			// Literal | setting
			for(AsnNode expect : syntaxList.getChildren()) {
				objDefn.addChild(definedSyntaxToken(objectClassDefn,objDefn,expect));
			}
		}
		skipOver(TokenType.RIGHT_BRACE);
		return objDefn;
	}
	private AsnNode fieldSetting(AsnNode objectClassDefn, AsnNode currentObject){
		//Used By:  DefaultSyntax
		// PrimitiveFieldName, Setting
		trace("fieldSetting");
		skipOver(TokenType.AMPERSAND);
		// new AstNode(AstProduction.PRIMITIVE_FIELD_NAME,skipOver(lookAhead().type()),true);
		AsnNode fieldSetting = new AsnNode(AsnProduction.FIELD_SETTING,skipOver(lookAhead().type()),true);
		// AstNode fieldName = primitiveFieldName();
		// fieldSetting.addChild(fieldName);
		fieldSetting.addChild(setting(fieldSetting.getName(),objectClassDefn,currentObject));
		return fieldSetting;
	}
	private AsnNode defaultSyntax(AsnNode objectClassDefn){
		//Used By:  ObjectDefn
		// "{", FieldSetting, ",", *, "}"
		trace("defaultSyntax");
		AsnNode objectDefn = AsnProduction.OBJECT.getNode();
		skipOver(TokenType.LEFT_BRACE);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			objectDefn.addChild(fieldSetting(objectClassDefn,objectDefn));
			while(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				objectDefn.addChild(fieldSetting(objectClassDefn,objectDefn));
			}
		}
		skipOver(TokenType.RIGHT_BRACE);
		return objectDefn;
	}
	private AsnNode objectDefn(AsnNode objectClassDefn){
		//Used By:  Object
		// DefaultSyntax | DefinedSyntax
		// use definedSyntax if the parent as "WITH SYNTAX" clause in it, else use default Syntax
		trace("objectDefn");
		if(debug) System.out.println("Defining Class > "+objectClassDefn);
		showDebug(objectClassDefn,"ObjectDefn >>",System.out);
		if(hasSyntax(objectClassDefn)) {
			return definedSyntax(objectClassDefn);
		} else {
			return defaultSyntax(objectClassDefn);
		}
	}
	private boolean hasSyntax(AsnNode parent) {
		AsnNode n = parent;
		if(n.getProduction().equals(AsnProduction.WITH_SYNTAX_SPEC)) return true;
		for(AsnNode c : n.getChildren()) {
			if(hasSyntax(c)) return true;
		}
		return false;
	}
	private AsnNode getSyntax(AsnNode parent) {
		AsnNode n = parent;
		if(n.getProduction().equals(AsnProduction.WITH_SYNTAX_SPEC)) return n;
		for(AsnNode c : n.getChildren()){
			AsnNode n1 = getSyntax(c);
			if(n1 != null) return n1;
		}
		return null;
	}
	private AsnNode object(AsnNode objectClassDefn){
		//Used By:  ObjectOptionalitySpec UserDefinedConstraintParameter ActualParameter ObjectAssignment Setting ObjectSetElements ParameterizedObjectAssignment
		// DefinedObject | ObjectDefn | ObjectFromObject | ParameterizedObject
		trace("object");
		showDebug(objectClassDefn,"DefiningClass >>",System.out);
		AsnNode parentDef = objectClassDefn;
		switch(parentDef.getProduction()) {
		case OBJECT_CLASS:
			break;
		case USEFUL_OBJECT_CLASS_REFERENCE:
			parentDef = currentAssignments.locate(AsnProduction.OBJECT_CLASS_ASSIGNMENT, parentDef.getName());
			break;
		case OBJECT_CLASS_REFERENCE:
			String name = parentDef.getName();
			do {
				parentDef = locate(AsnProduction.OBJECT_CLASS_ASSIGNMENT, name);
				if(parentDef == null) {
					error("Object Class Definition Not found for "+objectClassDefn);
				}
				showDebug(parentDef, "Parent Def >" , System.out);
				name = parentDef.getChild(0).getName();
			} while(!AsnProduction.OBJECT_CLASS.equals(parentDef.getChild(0).getProduction()));
			parentDef = parentDef.getChild(0);
			break;
		default:
			error("Not yet implemented "+parentDef);
		}
		AsnNode objectDef = null;
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			objectDef = objectDefn(parentDef);
		} else {
			error("Object is null");
		}
		return objectDef;
	}
	private AsnNode objectAssignment(){
		//Used By:  Assignment
		// objectreference, DefinedObjectClass, "::=", Object
		trace("objectAssignment");
		AsnNode oa = new AsnNode(AsnProduction.OBJECT_ASSIGNMENT,skipOver(TokenType.IDENTIFIER_STRING));
		AsnNode doc = definedObjectClass();
		oa.addChild(doc);
		skipOver(TokenType.ASSIGNMENT);
		oa.addChild(object(doc));
		return oa;
	}
	private AsnNode externalObjectReference() {
		// modulereference, ".", objectreference
		trace("externalObjectReference");
		Token moduleRef = skipOver(TokenType.TYPE_REFERENCE);
		skipOver(TokenType.DOT);
		return AsnProduction.EXTERNAL_OBJECT_REFERENCE.getNode(moduleRef,skipOver(TokenType.IDENTIFIER_STRING));
	}
	private AsnNode definedObject(AsnNode parent){
		//Used By:  ReferencedObjects ParameterizedObject Object
		// ExternalObjectReference | objectreference
		trace("definedObject");
		if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			return AsnProduction.OBJECT_REFERENCE.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE) && lookAheadIs(TokenType.DOT,1)) {
			return externalObjectReference();
		}
		error("Defined Object - reached Null ");
		return null;		
	}
	private AsnNode literal(){
		//Used By:  DefinedSyntaxToken RequiredToken
		// word
		// ","
		if(lookAheadIs(TokenType.COMMA)) {
			return new AsnNode(AsnProduction.LITERAL,skipOver(TokenType.COMMA));
		} else {
			return new AsnNode(AsnProduction.LITERAL,skipOver(lookAhead().type()));
		}
	}
	private AsnNode requiredToken(AsnNode parent){
		//Used By:  TokenOrGroupSpec
		// Literal | PrimitiveFieldName
		if(lookAheadIs(TokenType.AMPERSAND)) return primitiveFieldName();
		return literal();
	}
	private AsnNode optionalGroup(AsnNode parent){
		//Used By:  TokenOrGroupSpec
		// "[", TokenOrGroupSpec, empty+, "]"
		AsnNode og = new AsnNode(AsnProduction.OPTIONAL_GROUP);
		skipOver(TokenType.LEFT_BRACKET);
		while(!lookAheadIs(TokenType.RIGHT_BRACKET)) {
			og.addChild(tokenOrGroupSpec(parent));
		}
		skipOver(TokenType.RIGHT_BRACKET);
		return og;
	}
	private AsnNode tokenOrGroupSpec(AsnNode parent){
		//Used By:  SyntaxList OptionalGroup
		// RequiredToken | OptionalGroup
		if(lookAheadIs(TokenType.LEFT_BRACKET)) {
			return optionalGroup(parent);
		}
		return requiredToken(parent);
	}
	private AsnNode syntaxList(AsnNode parent){
		//Used By:  WithSyntaxSpec
		// "{", TokenOrGroupSpec, empty+, "}"
		AsnNode sl = new AsnNode(AsnProduction.SYNTAX_LIST);
		skipOver(TokenType.LEFT_BRACE);
		while(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			sl.addChild(tokenOrGroupSpec(parent));
		}
		skipOver(TokenType.RIGHT_BRACE);
		return sl;
	}
	private AsnNode withSyntaxSpec(AsnNode parent){
		//Used By: Root
		// WITH, SYNTAX, SyntaxList
		trace("withSyntaxSpec");
		AsnNode sl = new AsnNode(AsnProduction.WITH_SYNTAX_SPEC);
		skipOver(TokenType.WITH_SYNTAX);
		sl.addChild(syntaxList(parent));
		return sl;
	}
	private AsnNode objectSetOptionalitySpec(AsnNode parent){
		//Used By: Root
		// OPTIONAL | DEFAULT, ObjectSet
		trace("objectSetOptionalitySpec");
		AsnNode osos = null;
		switch(lookAhead().type()) {
		case OPTIONAL:
			osos = new AsnNode(AsnProduction.OBJECT_SET_OPTIONALITY_SPEC,skipOver(TokenType.OPTIONAL));
			break;
		case DEFAULT:
			osos = new AsnNode(AsnProduction.OBJECT_SET_OPTIONALITY_SPEC,skipOver(TokenType.DEFAULT));
			osos.addChild(objectSet(parent));
			break;
		default:
			break;
		}
		return osos;
	}
	private AsnNode objectSetFieldSpec(AsnNode parent){
		//Used By:  FieldSpec
		// objectsetfieldreference, DefinedObjectClass, ObjectSetOptionalitySpec?
		trace("objectSetFieldSpec");
		skipOver(TokenType.AMPERSAND);
		AsnNode osfs = new AsnNode(AsnProduction.OBJECT_SET_FIELD_SPEC,skipOver(TokenType.TYPE_REFERENCE),true);
		osfs.addChild(new AsnNode(AsnProduction.OBJECT_SET_FIELD_REFERENCE,skipOver(TokenType.CLASS_REFERENCE)));
		osfs.addChild(objectSetOptionalitySpec(parent));
		return osfs;
	}
	private AsnNode objectOptionalitySpec(AsnNode parent){
		//Used By: Root
		// OPTIONAL | DEFAULT, Object
		trace("objectOptionalitySpec");
		AsnNode oos = null;
		switch(lookAhead().type()) {
		case OPTIONAL:
			oos = new AsnNode(AsnProduction.OBJECT_OPTIONALITY_SPEC,skipOver(TokenType.OPTIONAL));
			break;
		case DEFAULT:
			oos = new AsnNode(AsnProduction.OBJECT_OPTIONALITY_SPEC,skipOver(TokenType.DEFAULT));
			oos.addChild(object(parent));
			break;
		default:
			break;
		}
		return oos;
	}
	private AsnNode objectFieldSpec(AsnNode parent){
		//Used By:  FieldSpec
		// objectfieldreference, DefinedObjectClass, ObjectOptionalitySpec?
		trace("objectFieldSpec");
		skipOver(TokenType.AMPERSAND);
		AsnNode ofs = new AsnNode(AsnProduction.OBJECT_FIELD_SPEC,skipOver(TokenType.IDENTIFIER_STRING),true);
		AsnNode definedClass = ofs.addChild(definedObjectClass());
		ofs.addChild(objectOptionalitySpec(definedClass));
		return ofs;
	}
	private AsnNode variableTypeValueSetFieldSpec(AsnNode parent){
		//Used By:  FieldSpec
		// valuesetfieldreference, FieldName, ValueSetOptionalitySpec?
		trace("variableTypeValueSetFieldSpec");
		skipOver(TokenType.AMPERSAND);
		AsnNode vtvs = new AsnNode(AsnProduction.VARIABLE_TYPE_VALUE_SET_FIELD_SPEC,skipOver(TokenType.TYPE_REFERENCE),true);
		vtvs.addChild(fieldName(parent));
		vtvs.addChild(valueSetOptionalitySpec(parent));
		return vtvs;
	}
	private AsnNode valueSetOptionalitySpec(AsnNode expectedType){
		//Used By: Root
		// OPTIONAL | DEFAULT, ValueSet
		trace("valueSetOptionalitySpec");
		AsnNode vsos = null;
		switch(lookAhead().type()) {
		case OPTIONAL:
			vsos = new AsnNode(AsnProduction.VALUE_SET_OPTIONALITY_SPEC,skipOver(TokenType.OPTIONAL));
			break;
		case DEFAULT:
			vsos = new AsnNode(AsnProduction.VALUE_SET_OPTIONALITY_SPEC,skipOver(TokenType.DEFAULT));
			vsos.addChild(valueSet(expectedType));
			break;
		default:
			break;
		}
		return vsos;
	}
	private AsnNode fixedTypeValueSetFieldSpec(AsnNode parent){
		//Used By:  FieldSpec
		// valuesetfieldreference, Type, ValueSetOptionalitySpec?
		trace("fixedTypeValueSetFieldSpec");
		skipOver(TokenType.AMPERSAND);
		AsnNode ftvs = new AsnNode(AsnProduction.FIXED_TYPE_VALUE_SET_FIELD_SPEC,skipOver(TokenType.TYPE_REFERENCE),true);
		AsnNode type = type(null);
		ftvs.addChild(type);
		ftvs.addChild(valueSetOptionalitySpec(type));
		return ftvs;
	}
	private AsnNode variableTypeValueFieldSpec(AsnNode parent){
		//Used By:  FieldSpec
		// valuefieldreference, FieldName, ValueOptionalitySpec?
		trace("variableTypeValueFieldSpec");
		skipOver(TokenType.AMPERSAND);
		AsnNode vtv = new AsnNode(AsnProduction.VARIABLE_TYPE_VALUE_FIELD_SPEC,skipOver(TokenType.IDENTIFIER_STRING),true);
		vtv.addChild(fieldName(parent));
		vtv.addChild(valueOptionalitySpec(parent));
		return vtv;
	}
	private AsnNode valueOptionalitySpec(AsnNode parent){
		//Used By: Root
		// OPTIONAL | DEFAULT, Value
		trace("valueOptionalitySpec");
		AsnNode vos = null;
		switch(lookAhead().type()) {
		case OPTIONAL:
			vos = new AsnNode(AsnProduction.VALUE_OPTIONALITY_SPEC,skipOver(TokenType.OPTIONAL));
			break;
		case DEFAULT:
			vos = new AsnNode(AsnProduction.VALUE_OPTIONALITY_SPEC,skipOver(TokenType.DEFAULT));
			vos.addChild(value(parent));
			break;
		default:
			break;
		}
		return vos;
	}
	private AsnNode fixedTypeValueFieldSpec(AsnNode parent){
		//Used By:  FieldSpec
		// valuefieldreference, Type, UNIQUE?, ValueOptionalitySpec?
		trace("fixedTypeValueFieldSpec");
		skipOver(TokenType.AMPERSAND);
		AsnNode ftvfs = new AsnNode(AsnProduction.FIXED_TYPE_VALUE_FIELD_SPEC,skipOver(TokenType.IDENTIFIER_STRING),true);
		AsnNode type = type(null);
		ftvfs.addChild(type);
		if(lookAheadIs(TokenType.UNIQUE)) {
			ftvfs.addChild(new AsnNode(AsnProduction.CONSTRAINT,skipOver(TokenType.UNIQUE)));
		}
		ftvfs.addChild(valueOptionalitySpec(type));
		return ftvfs;
	}
	private AsnNode typeOptionalitySpec(AsnNode parent){
		//Used By: componentType
		// OPTIONAL
		// DEFAULT, Type
		trace("typeOptionalitySpec");
		AsnNode tos = null;
		switch(lookAhead().type()) {
		case OPTIONAL:
			tos = AsnProduction.TYPE_OPTIONALITY_SPEC.getNode(skipOver(TokenType.OPTIONAL));
			break;
		case DEFAULT:
			tos = AsnProduction.TYPE_OPTIONALITY_SPEC.getNode(skipOver(TokenType.DEFAULT));
			tos.addChild(type(null));
			break;
		default:
			break;
		}
		return tos;
	}
	private AsnNode typeFieldSpec(AsnNode parent){
		//Used By:  FieldSpec
		// typefieldreference, TypeOptionalitySpec?
		trace("typeFieldSpec");
		skipOver(TokenType.AMPERSAND);
		AsnNode tfs = new AsnNode(AsnProduction.TYPE_FIELD_SPEC,skipOver(TokenType.TYPE_REFERENCE),true);
		tfs.addChild(typeOptionalitySpec(parent));
		return tfs;
	}
	private AsnNode fieldName(AsnNode parent){
		//Used By:  VariableTypeValueSetFieldSpec ObjectFromObject VariableTypeValueFieldSpec ValueFromObject TypeFromObject ObjectClassFieldType ObjectSetFromObjects ValueSetFromObjects
		// PrimitiveFieldName, "."+
		AsnNode fn = new AsnNode(AsnProduction.FIELD_NAME);
		fn.addChild(primitiveFieldName());
		while(lookAheadIs(TokenType.DOT) && lookAheadIs(TokenType.AMPERSAND,1)) {
			skipOver(TokenType.DOT);
			fn.addChild(primitiveFieldName());
		}
		return fn;
	}
	private AsnNode primitiveFieldName(){
		//Used By:  FieldSetting FieldName RequiredToken
		// typefieldreference | valuefieldreference | valuesetfieldreference | objectfieldreference | objectsetfieldreference
		trace("primitiveFieldName");
		skipOver(TokenType.AMPERSAND);
		AsnNode fn = new AsnNode(AsnProduction.PRIMITIVE_FIELD_NAME,skipOver(lookAhead().type()),true);
		return fn;
	}
	private AsnNode fieldSpec(AsnNode parent){
		//Used By:  ObjectClassDefn
		// TypeFieldSpec | FixedTypeValueFieldSpec | VariableTypeValueFieldSpec |
		// FixedTypeValueSetFieldSpec | VariableTypeValueSetFieldSpec | ObjectFieldSpec |
		// ObjectSetFieldSpec
		/*
		 * See Dubuisson Page 314
		 * If the field name starts with | and if it is followed by | then the field of the object contains
		 * &Upper-case  | nothing | a type field spec
		 * &lower-case  | a type or a type reference (Upper-case) | a fixed-type value field spec
		 * &lower-case  | a type field (&Upper-case)  | a variable-type value field spec
		 * &Upper-case  | a type or a type reference (Upper-case) | a fixed-type value set field spec
		 * &Upper-case  | a type field (&Upper-case) | a variable-type value set field spec
		 * &lower-case  | a class name (UPPER-CASES) | an information object field spec
		 * &Upper-case  | a class name (UPPER-CASES) | an information object set field spec
		 */
		trace("fieldSpec");
		AsnNode fs = null;
		if(lookAheadIs(TokenType.AMPERSAND)) {
			if(lookAheadIs(TokenType.TYPE_REFERENCE,1)) { // "&" Upper-case
				if(lookAheadIs(TokenType.CLASS_REFERENCE,2)) { // "&" Upper-case All-upper-case
					//informationObjectSetFieldSpec
					fs = objectSetFieldSpec(parent);
				} else if(lookAheadIs(TokenType.TYPE_REFERENCE,2) || lookAhead(2).isBuiltInType()) { // "&" Upper-case Type/TypeReference
					// fixdTypeValueSetFieldSpec
					fs = fixedTypeValueSetFieldSpec(parent);
				} else if(lookAheadIs(TokenType.AMPERSAND,2)) { // "&" Upper-case "&" ...
					if(lookAheadIs(TokenType.TYPE_REFERENCE,3)) {	// "&" Upper-case "&" Upper-case
						fs = variableTypeValueSetFieldSpec(parent);
					} else {
						error("Expected &TypeReference ");
					}
				} else {	// "&" Upper-case ,
					// typeFieldSpec
					fs = typeFieldSpec(parent);
				}
			} else if(lookAheadIs(TokenType.IDENTIFIER_STRING,1)) { // "&" lower-case
				if(lookAheadIs(TokenType.CLASS_REFERENCE,2)){	// "&" lower-case ALL-UPPER-CASE
					fs = objectFieldSpec(parent);
				} else if(lookAheadIs(TokenType.TYPE_REFERENCE,2) || lookAhead(2).isBuiltInType()) {	// "&" lower-case Type/TypeReference
					fs = fixedTypeValueFieldSpec(parent);
				} else if(lookAheadIs(TokenType.AMPERSAND,2)) {		// "&" lower-case "&" ...
					if(lookAheadIs(TokenType.TYPE_REFERENCE,3)) {
						fs = variableTypeValueFieldSpec(parent);	// "&" lower-case "&" Upper-case
					} else {
						error("Expected &TypeReference ");
					}
				}
			}			
		} else {
			error("fieldSpec Expected & ");
		}
		return fs;
	}
	private AsnNode objectClassDefn(){
		//Used By:  ObjectClass
		// CLASS, "{", FieldSpec, ","+, "}", WithSyntaxSpec?
		trace("objectClassDefn");
		AsnNode cls = AsnProduction.OBJECT_CLASS.getNode();
		skipOver(TokenType.CLASS);
		skipOver(TokenType.LEFT_BRACE);
		cls.addChild(fieldSpec(cls));
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			cls.addChild(fieldSpec(cls));
		}
		skipOver(TokenType.RIGHT_BRACE);
		if(lookAheadIs(TokenType.WITH_SYNTAX)) {
			cls.addChild(withSyntaxSpec(cls));
		}
		// showDebug(cls,"ObjectClassDen >",System.out);
		return cls;
	}
	private AsnNode objectClass(){
		//Used By:  ObjectClassAssignment ParameterizedObjectClassAssignment
		// DefinedObjectClass | ObjectClassDefn | ParameterizedObjectClass
		trace("objectClass");
		AsnNode oc = null;
		if(lookAheadIs(TokenType.CLASS)) {
			oc = objectClassDefn();
		} else if(lookAheadIs(TokenType.ABSTRACT_SYNTAX) || lookAheadIs(TokenType.TYPE_IDENTIFIER)){
			oc = usefulObjectClassReference();
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE) || lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			oc = definedObjectClass();
		} else {
			error("not implemented");
		}
		return oc;
	}
	private AsnNode objectClassAssignment(){
		//Used By:  Assignment
		// objectclassreference, "::=", ObjectClass
		trace("objectClassAssignment");
		if(lookAheadIs(TokenType.LEFT_BRACE,1)) {
			return parameterizedObjectClassAssignment();
		}
		AsnNode oca = new AsnNode(AsnProduction.OBJECT_CLASS_ASSIGNMENT,skipOver(TokenType.CLASS_REFERENCE));
		skipOver(TokenType.ASSIGNMENT);
		oca.addChild(objectClass());
		// showDebug(oca,"ObjectClassAssignment > ",System.out);
		return oca;
	}
	private AsnNode usefulObjectClassReference(){
		//Used By:  DefinedObjectClass
		// TYPE-IDENTIFIER | ABSTRACT-SYNTAX
		trace("usefulObjectClassReference");
		AsnNode n = null;
		switch(lookAhead().type()) {
		case TYPE_IDENTIFIER:
			 // TYPE-IDENTIFIER ::= CLASS {	-- Dubuisson p 356
			 // 	&id OBJECT IDENTIFIER UNIQUE,
			 //     &Type
			 // } WITH SYNTAX {&Type IDENTIFIED BY &id}
			n = new AsnNode(AsnProduction.USEFUL_OBJECT_CLASS_REFERENCE,skipOver(lookAhead().type()));
			AsnNode ti = locate(AsnProduction.OBJECT_CLASS_ASSIGNMENT,"TYPE-IDENTIFIER");
			if(ti == null) {
				AsnNode oca = new AsnNode(AsnProduction.OBJECT_CLASS_ASSIGNMENT,new Token(TokenType.CLASS_REFERENCE,"TYPE-IDENTIFIER"));
				AsnNode ocd = oca.addChild(new AsnNode(AsnProduction.OBJECT_CLASS));
				AsnNode id = ocd.addChild(new AsnNode(AsnProduction.FIXED_TYPE_VALUE_FIELD_SPEC,new Token(TokenType.IDENTIFIER_STRING,"id"),true));
				id.addChild(new AsnNode(AsnProduction.OBJECT_IDENTIFIER_TYPE));
				id.addChild(new AsnNode(AsnProduction.CONSTRAINT,new Token(TokenType.UNIQUE,"UNIQUE")));
				ocd.addChild(new AsnNode(AsnProduction.TYPE_FIELD_SPEC,new Token(TokenType.TYPE_REFERENCE,"Type"),true));
				AsnNode syntax = ocd.addChild(new AsnNode(AsnProduction.WITH_SYNTAX_SPEC));
				AsnNode sl = syntax.addChild(new AsnNode(AsnProduction.SYNTAX_LIST));
				sl.addChild(new AsnNode(AsnProduction.PRIMITIVE_FIELD_NAME,new Token(TokenType.TYPE_REFERENCE,"Type"),true));
				sl.addChild(new AsnNode(AsnProduction.LITERAL,new Token(TokenType.CLASS_REFERENCE,"IDENTIFIED")));
				sl.addChild(new AsnNode(AsnProduction.LITERAL,new Token(TokenType.CLASS_REFERENCE,"BY")));
				sl.addChild(new AsnNode(AsnProduction.PRIMITIVE_FIELD_NAME,new Token(TokenType.IDENTIFIER_STRING,"id"),true));
				currentAssignments.addChild(oca);
			}
			break;
		case ABSTRACT_SYNTAX:
			 // ABSTRACT-SYNTAX ::= CLASS { -- Dubuisson p 362
			 //  &id OBJECT IDENTIFIER,
			 //  &Type,
			 //  &property BIT STRING { handles-invalid-encodings(0)}
			 //   DEFAULT {}
			 // } WITH SYNTAX { &Type IDENTIFIED BY &id [HAS PROPERTY &property] }
			n = new AsnNode(AsnProduction.USEFUL_OBJECT_CLASS_REFERENCE,skipOver(lookAhead().type()));
			AsnNode as = locate(AsnProduction.OBJECT_CLASS_ASSIGNMENT,"ABSTRACT-SYNTAX");
			if(as == null) {
				AsnNode oca = new AsnNode(AsnProduction.OBJECT_CLASS_ASSIGNMENT,new Token(TokenType.CLASS_REFERENCE,"ABSTRACT-SYNTAX"));
				AsnNode ocd = oca.addChild(new AsnNode(AsnProduction.OBJECT_CLASS));
				AsnNode id = ocd.addChild(new AsnNode(AsnProduction.FIXED_TYPE_VALUE_FIELD_SPEC,new Token(TokenType.IDENTIFIER_STRING,"id"),true));
				id.addChild(new AsnNode(AsnProduction.OBJECT_IDENTIFIER_TYPE));
				ocd.addChild(new AsnNode(AsnProduction.TYPE_FIELD_SPEC,new Token(TokenType.TYPE_REFERENCE,"Type"),true));
				
				AsnNode p = ocd.addChild(new AsnNode(AsnProduction.FIXED_TYPE_VALUE_FIELD_SPEC,new Token(TokenType.IDENTIFIER_STRING,"property"),true));
				p.addChild(new AsnNode(AsnProduction.BIT_STRING_TYPE))
				 .addChild(new AsnNode(AsnProduction.NAMED_BIT_LIST))
				 .addChild(new AsnNode(AsnProduction.NAMED_NUMBER,new Token(TokenType.IDENTIFIER_STRING,"handles-invalid-encodings")))
				 .addChild(new AsnNode(AsnProduction.NUMBER_FORM,new Token(TokenType.NUMBER_STRING,"0")));
				p.addChild(new AsnNode(AsnProduction.VALUE_OPTIONALITY_SPEC,new Token(TokenType.DEFAULT,"DEFAULT")))
				 .addChild(new AsnNode(AsnProduction.BIT_STRING_VALUE))
				 .addChild(new AsnNode(AsnProduction.IDENTIFIER_LIST));
				
				AsnNode syntax = ocd.addChild(new AsnNode(AsnProduction.WITH_SYNTAX_SPEC));
				AsnNode sl = syntax.addChild(new AsnNode(AsnProduction.SYNTAX_LIST));
				sl.addChild(new AsnNode(AsnProduction.PRIMITIVE_FIELD_NAME,new Token(TokenType.TYPE_REFERENCE,"Type"),true));
				sl.addChild(new AsnNode(AsnProduction.LITERAL,new Token(TokenType.CLASS_REFERENCE,"IDENTIFIED")));
				sl.addChild(new AsnNode(AsnProduction.LITERAL,new Token(TokenType.CLASS_REFERENCE,"BY")));
				sl.addChild(new AsnNode(AsnProduction.PRIMITIVE_FIELD_NAME,new Token(TokenType.IDENTIFIER_STRING,"id"),true));
				AsnNode og = sl.addChild(new AsnNode(AsnProduction.OPTIONAL_GROUP));
				og.addChild(new AsnNode(AsnProduction.LITERAL,new Token(TokenType.CLASS_REFERENCE,"HAS")));
				og.addChild(new AsnNode(AsnProduction.LITERAL,new Token(TokenType.CLASS_REFERENCE,"PROPERTY")));
				og.addChild(new AsnNode(AsnProduction.PRIMITIVE_FIELD_NAME,new Token(TokenType.IDENTIFIER_STRING,"property"),true));
				currentAssignments.addChild(oca);				
			}
			break;
		default:
			error("UsefulClassReference not implemented -");
			break;
		}
		return n;
	}
	private AsnNode externalObjectClassReference(){
		//Used By:  DefinedObjectClass
		// modulereference, ".", objectclassreference
		trace("externalObjectClassReference");
		Token moduleRef = skipOver(TokenType.TYPE_REFERENCE);
		skipOver(TokenType.DOT);
		return AsnProduction.EXTERNAL_OBJECT_CLASS_REFERENCE.getNode(moduleRef,skipOver(TokenType.CLASS_REFERENCE));
	}
	private AsnNode definedObjectClass(){
		//Used By:  ParameterizedObjectClass ObjectClass ActualParameter ObjectSetAssignment ObjectSetFieldSpec ObjectFieldSpec Governor ParameterizedObjectSetAssignment InstanceOfType UserDefinedConstraintParameter ObjectAssignment ObjectClassFieldType ParameterizedObjectAssignment
		// ExternalObjectClassReference | objectclassreference | UsefulObjectClassReference
		trace("definedObjectClass");
		AsnNode doc = null;
		if(lookAheadIs(TokenType.TYPE_IDENTIFIER) || lookAheadIs(TokenType.ABSTRACT_SYNTAX)) {
			doc = usefulObjectClassReference();
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE) && lookAheadIs(TokenType.DOT,1) && lookAheadIs(TokenType.CLASS_REFERENCE,2)) {
			doc = externalObjectClassReference();
		} else if(lookAheadIs(TokenType.CLASS_REFERENCE)) {
			doc = AsnProduction.OBJECT_CLASS_REFERENCE.getNode(skipOver(TokenType.CLASS_REFERENCE));			
		} else {
			warn("DefinedObjectClass was null");
		}
		return doc;
	}
	private AsnNode patternConstraint(AsnNode parent){
		//Used By:  SubtypeElements
		// PATTERN, Value
		error("not implemented");
		return null;
	}
	private AsnNode presenceConstraint(AsnNode parent){
		//Used By:  ComponentConstraint
		// PRESENT | ABSENT | OPTIONAL | empty
		return lookAheadIs(TokenType.PRESENT) || lookAheadIs(TokenType.ABSENT) || lookAheadIs(TokenType.OPTIONAL) ? 
				AsnProduction.PRESENCE_CONSTRAINT.getNode(skipOver(lookAhead().type())) : null;
	}
	private AsnNode valueConstraint(AsnNode parent){
		//Used By:  ComponentConstraint
		// Constraint | empty
		return lookAheadIs(TokenType.LEFT_PAREN) ? constraint(parent) : null;
	}
	private AsnNode componentConstraint(AsnNode parent){
		//Used By:  NamedConstraint
		// ValueConstraint, PresenceConstraint
		AsnNode n = AsnProduction.COMPONENT_CONSTRAINT.getNode();
		n.addChild(valueConstraint(parent));
		n.addChild(presenceConstraint(parent));
		return n;
	}
	private AsnNode namedConstraint(AsnNode parent){
		//Used By:  TypeConstraints
		// identifier, ComponentConstraint
		AsnNode n = AsnProduction.NAMED_CONSTRAINT.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		n.addChild(componentConstraint(parent));
		return n;
	}
	private AsnNode typeConstraints(AsnNode parent){
		//Used By:  PartialSpecification FullSpecification TypeConstraints
		// NamedConstraint
		// NamedConstraint, ",", TypeConstraints
		AsnNode n = AsnProduction.TYPE_CONSTRAINTS.getNode();
		n.addChild(namedConstraint(parent));
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			n.addChild(namedConstraint(parent));
		}
		return n;
	}
	private AsnNode partialSpecification(AsnNode parent){
		//Used By:  MultipleTypeConstraints
		// "{", "...", ",", TypeConstraints, "}"
		skipOver(TokenType.LEFT_BRACE);
		AsnNode ps = AsnProduction.PARTIAL_SPECIFICATION.getNode(skipOver(TokenType.TRIPLE_DOT));
		skipOver(TokenType.COMMA);
		ps.addChild(typeConstraints(parent));
		skipOver(TokenType.RIGHT_BRACE);
		return ps;
	}
	private AsnNode fullSpecification(AsnNode parent){
		//Used By:  MultipleTypeConstraints
		// "{", TypeConstraints, "}"
		skipOver(TokenType.LEFT_BRACE);
		AsnNode fs = AsnProduction.FULL_SPECIFICATION.getNode();
		fs.addChild(typeConstraints(parent));
		skipOver(TokenType.RIGHT_BRACE);
		return fs;
	}
	private AsnNode multipleTypeConstraints(AsnNode parent){
		//Used By:  InnerTypeConstraints
		// FullSpecification
		// PartialSpecification
		return lookAheadIs(TokenType.TRIPLE_DOT,1) ? partialSpecification(parent) : fullSpecification(parent);
	}
	private AsnNode singleTypeConstraint(AsnNode parent){
		//Used By:  InnerTypeConstraints
		// Constraint
		return constraint(parent);
	}
	private AsnNode innerTypeConstraints(AsnNode parent){
		//Used By:  SubtypeElements
		// WITH, COMPONENT, SingleTypeConstraint
		// WITH, COMPONENTS, MultipleTypeConstraints
		skipOver(TokenType.WITH);
		AsnNode ic = null;
		if(lookAheadIs(TokenType.COMPONENT)) {
			ic = AsnProduction.INNER_TYPE_CONSTRAINTS.getNode(skipOver(TokenType.COMPONENT));
			ic.addChild(singleTypeConstraint(parent));
		} else if(lookAheadIs(TokenType.COMPONENTS)) {
			ic = AsnProduction.INNER_TYPE_CONSTRAINTS.getNode(skipOver(TokenType.COMPONENTS));
			ic.addChild(multipleTypeConstraints(parent));
		} else {
			error("not implemented");
		}
		return ic;
	}
	private AsnNode typeConstraint(AsnNode parent){
		//Used By:  SubtypeElements
		// Type
		error("not implemented");
		return null;
	}
	private AsnNode permittedAlphabet(AsnNode parent){
		//Used By:  SubtypeElements
		// FROM, Constraint
		error("not implemented");
		return null;
	}
	private AsnNode sizeConstraint(AsnNode parent){
		//Used By:  TypeWithConstraint SubtypeElements
		// SIZE, Constraint
		if(lookAheadIs(TokenType.SIZE)) {
			skipOver(TokenType.SIZE);
			AsnNode sizeConstraint = new AsnNode(AsnProduction.SIZE_CONSTRAINT);
			sizeConstraint.addChild(constraint(sizeConstraint));
			return sizeConstraint;
		}
		return null;
	}
	private AsnNode upperEndValue(AsnNode parent){
		//Used By:  UpperEndpoint
		// Value | MAX
		AsnNode l = null;
		switch(lookAhead().type()) {
		case MAX:
			l = new AsnNode(AsnProduction.UPPER_END_VALUE,skipOver(TokenType.MAX));
			break;
		case MINUS:
			l = new AsnNode(AsnProduction.UPPER_END_VALUE,skipOver(TokenType.MINUS),skipOver(TokenType.NUMBER_STRING));
			break;
		case NUMBER_STRING:
			l = new AsnNode(AsnProduction.UPPER_END_VALUE,skipOver(TokenType.NUMBER_STRING));
			break;
		case QUOTED_STRING:
			l = new AsnNode(AsnProduction.UPPER_END_VALUE,skipOver(TokenType.QUOTED_STRING));
			break;
		default:
			l = value(parent);
			break;
		}
		return l;
	}
	private AsnNode lowerEndValue(AsnNode parent){
		//Used By:  LowerEndpoint
		// Value | MIN
		AsnNode l = null;
		AsnProduction p = lookAhead(1).type().equals(TokenType.DOUBLE_DOT) || 
				lookAhead(1).type().equals(TokenType.LESS_THAN) && lookAhead(2).type().equals(TokenType.DOUBLE_DOT)
				?  AsnProduction.LOWER_END_VALUE : AsnProduction.SINGLE_VALUE;
		switch(lookAhead().type()) {
		case MIN:
			l = p.getNode(skipOver(TokenType.MIN));
			break;
		case MINUS:
			l = p.getNode(skipOver(TokenType.MINUS),skipOver(TokenType.NUMBER_STRING));
			break;
		case NUMBER_STRING:
			l = p.getNode(skipOver(TokenType.NUMBER_STRING));
			break;
		case QUOTED_STRING:
			l = p.getNode(skipOver(TokenType.QUOTED_STRING));
			break;
		default:
			l = value(parent);
			break;
		}
		return l;
	}
	private AsnNode upperEndpoint(AsnNode parent){
		//Used By:  ValueRange
		// UpperEndValue |  "<", UpperEndValue
		AsnNode upper = null;
		if(lookAheadIs(TokenType.LESS_THAN)) {
			upper = new AsnNode(AsnProduction.UPPER_ENDPOINT,skipOver(TokenType.LESS_THAN));
			upper.addChild(upperEndValue(parent));
		} else {
			upper = upperEndValue(parent);
		}
		return upper;
	}
	private AsnNode lowerEndpoint(AsnNode parent){
		//Used By:  ValueRange
		// LowerEndValue | LowerEndValue, "<"
		AsnNode lower = null;
		if(lookAheadIs(TokenType.LESS_THAN,1)) {
			AsnNode lowerValue = lowerEndValue(parent);
			lower = new AsnNode(AsnProduction.LOWER_ENDPOINT,skipOver(TokenType.LESS_THAN));
			lower.addChild(lowerValue);
		} else {
			lower = lowerEndValue(parent);
		}
		return lower;
	}
	private AsnNode valueRange(AsnNode parent){
		//Used By:  SubtypeElements
		// LowerEndpoint, "..", UpperEndpoint
		AsnNode lower = lowerEndpoint(parent);
		if(lookAheadIs(TokenType.DOUBLE_DOT)) {
			AsnNode valueRange = AsnProduction.VALUE_RANGE.getNode(skipOver(TokenType.DOUBLE_DOT));
			valueRange.addChild(lower);
			valueRange.addChild(upperEndpoint(parent));
			return valueRange;
		}
		return lower;
	}
	private AsnNode includes(AsnNode parent){
		//Used By:  ContainedSubtype
		// INCLUDES
		// empty
		error("not implemented");
		return null;
	}
	private AsnNode containedSubtype(AsnNode parent){
		//Used By:  SubtypeElements
		// Includes, Type
		error("not implemented");
		return null;
	}
	private AsnNode singleValue(AsnNode parent){
		//Used By:  SubtypeElements
		// Value
		return new AsnNode(AsnProduction.SINGLE_VALUE,skipOver(lookAhead().type()));
	}
	private AsnNode subtypeElements(AsnNode parent){
		//Used By:  Elements
		// SingleValue| ContainedSubtype | ValueRange | PermittedAlphabet
		// SizeConstraint | TypeConstraint | InnerTypeConstraints | PatternConstraint
		trace("subTypeElements");
		// showDebug(parent,"subTypeElements > ",System.out);
		Token t = lookAhead();
		AsnNode element = null;
		switch(t.type()) {
		case SIZE:
			element = sizeConstraint(parent);
			break;
		case MIN:
		case MINUS:
		case QUOTED_STRING:
		case NUMBER_STRING:	// maybe a single value or value range
		case IDENTIFIER_STRING:
			if(lookAheadIs(TokenType.RIGHT_PAREN,1)) {
				element = singleValue(parent);
			} else {
				element = valueRange(parent);
			}
			break;
		case INCLUDES: // INCLUDES | empty
			skipOver(TokenType.INCLUDES);
		case TYPE_REFERENCE: // Includes, Type | Type
			element = AsnProduction.CONTAINED_SUBTYPE.getNode();
			element.addChild(type(null));
			// case IDENTIFIER_STRING:
			//	element = definedValue();
			//	break;
			break;
		case FROM:	// FROM, Constraint
			skipOver(TokenType.FROM);
			element = new AsnNode(AsnProduction.PERMITTED_ALPHABET);
			element.addChild(constraint(parent));
			break;
		case PRESENT:	// PRESENT | ABSENT | OPTIONAL | empty
		case ABSENT:
		case OPTIONAL:
			element = new AsnNode(AsnProduction.PRESENCE_CONSTRAINT,skipOver(t.type()));
			break;
		case PATTERN: // PATTERN, Value
			skipOver(TokenType.PATTERN);
			element = new AsnNode(AsnProduction.PATTERN_CONSTRAINT,skipOver(TokenType.QUOTED_STRING));
			break;
		case WITH:// WITH ( COMPONENT SingleTypeConstraint | COMPONENTS MultipleTypeConstraints )
			element = innerTypeConstraints(parent);
			break;
		default:
			// Constraint | Empty
			element = new AsnNode(AsnProduction.VALUE_CONSTRAINT);
			element.addChild(value(parent));
			break;
		}
		return element;
	}
	private AsnNode elements(AsnNode parent){
		//Used By:  IntersectionElements Exclusions Elems
		// SubtypeElements
		// ObjectSetElements
		// "(", ElementSetSpec, ")"
		trace("elements:"+parent+" isBuiltIn "+parent.isBuiltInType());
		if(parent.isBuiltInType()) {
			switch(lookAhead().type()) {
			case LEFT_PAREN:
				skipOver(TokenType.LEFT_PAREN);
				AsnNode n = elementSetSpec(parent);
				skipOver(TokenType.RIGHT_PAREN);
				return n;
			default:
				return subtypeElements(parent);
			}
		} else {
			switch(lookAhead().type()) {
			case IDENTIFIER_STRING:
			case MIN:
			case MINUS:
			case QUOTED_STRING:
			case NUMBER_STRING:
			case INCLUDES:
			case TYPE_REFERENCE:
			case FROM:
			case PRESENT:
			case ABSENT:
			case OPTIONAL:
			case PATTERN:
			case WITH: 
			case SIZE:
				return subtypeElements(parent);
			default:
				showDebug(parent, "Parent> ", System.out);
				error("Not yet implemented "+parent);
			}
			return objectSetElements(parent);
		}
	}
	private AsnNode intersectionMark(AsnNode parent){
		//Used By:  Intersections
		// "^"
		// INTERSECTION
		error("not implemented");
		return null;
	}
	private AsnNode unionMark(AsnNode parent){
		//Used By:  Unions
		// "|"
		// UNION
		error("not implemented");
		return null;
	}
	private AsnNode exclusions(AsnNode parent){
		//Used By:  IntersectionElements ElementSetSpec
		// EXCEPT, Elements
		AsnNode except = AsnProduction.EXCLUSIONS.getNode(skipOver(TokenType.EXCEPT));
		except.addChild(elements(parent));
		return except;
	}
	private AsnNode elems(AsnNode parent){
		//Used By:  IntersectionElements
		// Elements
		error("not implemented");
		return null;
	}
	private AsnNode intersectionElements(AsnNode parent){
		//Used By:  Intersections
		// Elements | Elems, Exclusions
		AsnNode ie = elements(parent);
		if(lookAheadIs(TokenType.EXCEPT)) {
			ie.addChild(exclusions(parent));
		}
		return ie;
	}
	private AsnNode iElems(AsnNode parent){
		//Used By:  Intersections
		// Intersections
		error("not implemented");
		return null;
	}
	private AsnNode intersections(AsnNode parent){
		//Used By:  IElems Unions
		// IntersectionElements
		// IElems, IntersectionMark, IntersectionElements
		AsnNode n = intersectionElements(parent);
		while(lookAheadIs(TokenType.CARAT) || lookAheadIs(TokenType.INTERSECTION)) {
			AsnNode i = AsnProduction.INTERSECTIONS.getNode(skipOver(tokenizer.lookAhead().type()));
			i.addChild(n);
			AsnNode elem = intersectionElements(parent);
			i.addChild(elem);
			n = i;
		}
		return n;
	}
	private AsnNode uElems(AsnNode parent){
		//Used By:  Unions
		// Unions
		error("not implemented");
		return null;
	}
	private AsnNode unions(AsnNode parent){
		//Used By:  UElems ElementSetSpec
		// Intersections
		// UElems, UnionMark, Intersections
		AsnNode n = intersections(parent);
		while(lookAheadIs(TokenType.VERTICAL_BAR) || lookAheadIs(TokenType.UNION)) {
			AsnNode u = new AsnNode(AsnProduction.UNIONS,skipOver(tokenizer.lookAhead().type()));
			u.addChild(n);
			u.addChild(intersections(parent));
			n = u;
		}
		return n;
	}
	private AsnNode elementSetSpec(AsnNode parent){
		//Used By:  RootElementSetSpec AdditionalElementSetSpec Elements
		// Unions | ALL, Exclusions
		if(lookAheadIs(TokenType.ALL)) {
			AsnNode all = AsnProduction.EXCLUSIONS.getNode(skipOver(TokenType.ALL));
			all.addChild(exclusions(parent));
			return all;
		}
		return unions(parent);
	}
	private AsnNode additionalElementSetSpec(AsnNode expectedType){
		//Used By:  ElementSetSpecs ObjectSetSpec
		// ElementSetSpec
		return elementSetSpec(expectedType);
	}
	private AsnNode rootElementSetSpec(AsnNode expectedType){
		//Used By:  ElementSetSpecs ObjectSetSpec
		// ElementSetSpec
		return elementSetSpec(expectedType);
	}
	private AsnNode elementSetSpecs(AsnNode expectedType){
		//Used By:  SubtypeConstraint ValueSet
		// RootElementSetSpec
		// RootElementSetSpec, ",", "..."
		// RootElementSetSpec, ",", "...", ",", AdditionalElementSetSpec
		trace("elementSetSpecs");
		AsnNode n = AsnProduction.ELEMENT_SET_SPECS.getNode();
		n.addChild(rootElementSetSpec(expectedType));
		if(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			n.addChild(AsnProduction.EXTENSION.getNode(skipOver(TokenType.TRIPLE_DOT)));
			if(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				n.addChild(additionalElementSetSpec(expectedType));
			}
		}
		return n;
	}
	private AsnNode subtypeConstraint(AsnNode expectedType){
		//Used By:  ConstraintSpec
		// ElementSetSpecs
		return elementSetSpecs(expectedType);
	}
	private AsnNode exceptionIdentification(){
		//Used By:  ExceptionSpec
		// SignedNumber | DefinedValue | Type, ":", Value
		AsnNode id = null;
		if(lookAheadIs(TokenType.NUMBER_STRING) || (lookAheadIs(TokenType.MINUS) && lookAheadIs(TokenType.NUMBER_STRING,1))) {
			// signed number
			id = signedNumber();
		} else if(lookAhead().isBuiltInType()) {
			// built in type
			AsnNode type = type(null);
			skipOver(TokenType.COLON);
			id = value(type);
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			// referenced type
			Token typeName = skipOver(lookAhead().type());
			AsnNode type = locate(AsnProduction.TYPE_ASSIGNMENT,typeName.value());
			if(type == null) {
				type = AsnProduction.DEFINED_TYPE.getNode(typeName);
				skipOver(TokenType.COLON);
				AsnNode v = value(type);
				id = AsnProduction.DEFINED_VALUE.getNode(type.getNameToken());
				id.addChild(v);
			} else {
				error("Type "+typeName+" is undefined");
			}
		} else {
			// defined value
			id = definedValue();
		}
		return id;
	}
	private AsnNode exceptionSpec(){
		//Used By:  ExtensionAndException Constraint Enumerations
		// "!", ExceptionIdentification
		// empty
		if(!lookAheadIs(TokenType.NOT)) return null;
		AsnNode n = AsnProduction.EXCEPTION_SPEC.getNode(skipOver(TokenType.NOT));
		n.addChild(exceptionIdentification());
		return n;
	}
	private AsnNode constraintSpec(AsnNode constrainedType){
		//Used By:  Constraint
		// SubtypeConstraint | GeneralConstraint
		trace("ConstraintSpec: "+constrainedType+" "+constrainedType.isBuiltInType());
		// TODO: Check which lookAhead types are possible with a LEFT_BRACE here...
		// StringType values can be SubType Constraints, as well as General Constraints for Object Types
		switch(lookAhead().type()) {
		case CONSTRAINED:	// UserDefinedConstraint::= CONSTRAINED BY ...
		case LEFT_BRACE:	// TableConstraint
		case CONTAINING:	// CONTAINING Type
		case ENCODED:		// ENCODED BY Value
			return generalConstraint(constrainedType);
		default:
			return subtypeConstraint(constrainedType);
		}
	}
	private AsnNode constraint(AsnNode constrainedType){
		//Used By:  ConstrainedType PermittedAlphabet SingleTypeConstraint ValueConstraint TypeWithConstraint SizeConstraint
		// "(", ConstraintSpec, ExceptionSpec, ")"
		// System.out.println("Constraint >>> "+lookAhead()+" "+lookAhead(1)+" "+lookAhead(2)+" "+lookAhead(3)+" "+lookAhead(4)+" "+lookAhead(5));
		trace("constraint");
		if(!lookAheadIs(TokenType.LEFT_PAREN)) return null;
		skipOver(TokenType.LEFT_PAREN);
		AsnNode n = AsnProduction.CONSTRAINT.getNode();
		n.addChild(constraintSpec(constrainedType));
		n.addChild(exceptionSpec());
		skipOver(TokenType.RIGHT_PAREN);
		// showDebug(n,"constraint> ",System.out);
		return n;
	}
	private AsnNode typeWithConstraint(AsnNode parent){
		//Used By:  ConstrainedType
		// SET, Constraint, OF, Type
		// SET, SizeConstraint, OF, Type
		// SEQUENCE, Constraint, OF, Type
		// SEQUENCE, SizeConstraint, OF, Type
		// SET, Constraint, OF, NamedType
		// SET, SizeConstraint, OF, NamedType
		// SEQUENCE, Constraint, OF, NamedType
		// SEQUENCE, SizeConstraint, OF, NamedType
		// NOTE: These are handled by sequenceOfType() and setOfType() respectively
		error("not implemented");
		return null;
	}
	private AsnNode constrainedType(AsnNode parent){
		//Used By:  Type
		// Type, Constraint
		// TypeWithConstraint
		// NOTE: These are handled by type()
		error("not implemented");
		return null;
	}
	private AsnNode usefulType(AsnNode parent){
		//Used By:  ReferencedType
		// typereference
		// Here typeReference refers to one of GeneralizedTime, UTCTime, or ObjectDescriptor (X.680, section 41)
		error("not implemented");
		return null;
	}
	private AsnNode tableRow(AsnNode parent){
		//Used By:  Tuple
		// number
		error("not implemented");
		return null;
	}
	private AsnNode tableColumn(AsnNode parent){
		//Used By:  Tuple
		// number
		error("not implemented");
		return null;
	}
	private AsnNode tuple(AsnNode parent){
		//Used By:  RestrictedCharacterStringValue CharsDefn
		// "{", TableColumn, ",", TableRow, "}"
		skipOver(TokenType.LEFT_BRACE);
		AsnNode t = AsnProduction.TUPLE.getNode();
		for(AsnProduction p : new AsnProduction[] {AsnProduction.TABLE_COLUMN,AsnProduction.TABLE_ROW}) {
			if(lookAheadIs(TokenType.NUMBER_STRING)) {
				t.addChild(p.getNode(skipOver(TokenType.NUMBER_STRING)));
			} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
				t.addChild(p.getNode(skipOver(TokenType.IDENTIFIER_STRING)));
			}
			if(lookAheadIs(TokenType.COMMA)) skipOver(TokenType.COMMA);
		}
		skipOver(TokenType.RIGHT_BRACE);
		return t;
	}
	private AsnNode cell(AsnNode parent){
		//Used By:  Quadruple
		// number
		error("not implemented");
		return null;
	}
	private AsnNode row(AsnNode parent){
		//Used By:  Quadruple
		// number
		error("not implemented");
		return null;
	}
	private AsnNode plane(AsnNode parent){
		//Used By:  Quadruple
		// number
		error("not implemented");
		return null;
	}
	private AsnNode group(AsnNode parent){
		//Used By:  Quadruple
		// number
		error("not implemented");
		return null;
	}
	private AsnNode quadruple(AsnNode parent){
		//Used By:  RestrictedCharacterStringValue CharsDefn
		// "{", Group, ",", Plane, ",", Row, ",", Cell, "}"
		skipOver(TokenType.LEFT_BRACE);
		AsnNode q = AsnProduction.QUADRUPLE.getNode();
		for(AsnProduction p : new AsnProduction[] {AsnProduction.GROUP,AsnProduction.PLANE,AsnProduction.ROW,AsnProduction.CELL}) {
			if(lookAheadIs(TokenType.NUMBER_STRING)) {
				q.addChild(p.getNode(skipOver(TokenType.NUMBER_STRING)));
			} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
				q.addChild(p.getNode(skipOver(TokenType.IDENTIFIER_STRING)));
			}
			if(lookAheadIs(TokenType.COMMA)) skipOver(TokenType.COMMA);
		}
		skipOver(TokenType.RIGHT_BRACE);
		return q;
	}
	private AsnNode charsDefn(AsnNode parent){
		//Used By:  CharSyms
		// cstring | Quadruple | Tuple | DefinedValue
		trace("charsDefn");
		AsnNode cd = null;
		switch(lookAhead().type()) {
		case NUMBER_STRING:
			cd = AsnProduction.NUMBER_FORM.getNode(skipOver(TokenType.NUMBER_STRING));
			break;
		case QUOTED_STRING:
			cd = parent.getProduction().getNode(skipOver(TokenType.QUOTED_STRING));
			break;
		case IDENTIFIER_STRING:
			cd = definedValue(AsnProduction.INTEGER_TYPE);
			break;
		default:
			error("not implemented");
			break;
		}
		return cd;
	}
	private AsnNode charSyms(AsnNode parent){
		//Used By:  CharacterStringList CharSyms
		// CharsDefn
		// CharSyms, ",", CharsDefn
		error("not implemented");
		return null;
	}
	private AsnNode characterStringList(AsnNode parent){
		//Used By:  RestrictedCharacterStringValue
		// "{", CharSyms, "}"
		trace("characterStringList");
		AsnNode n = AsnProduction.CHARACTER_STRING_LIST.getNode();
		skipOver(TokenType.LEFT_BRACE);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			n.addChild(charsDefn(parent));
			while(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				n.addChild(charsDefn(parent));
			}
		}
		skipOver(TokenType.RIGHT_BRACE);
		return n;
	}
	private AsnNode restrictedCharacterStringValue(AsnNode expected){
		//Used By:  CharacterStringValue
		// cstring | CharacterStringList | Quadruple | Tuple
		// NOTE: Quadruple and Tuple are currently handled as CharacterStringList
		trace("restrictedCharacterStringValue");
		switch(lookAhead().type()) {
		case QUOTED_STRING:	// cstring
			return expected.getProduction().getNode(skipOver(TokenType.QUOTED_STRING));
		case LEFT_BRACE:	// characterStringList
			return characterStringList(expected);
		case IDENTIFIER_STRING:
			return definedValue(expected.getProduction());
		default:
			error(expected+" : not implemented");
			break;
		}
		return null;
	}
	private AsnNode unrestrictedCharacterStringValue(AsnNode expected){
		//Used By:  CharacterStringValue
		// SequenceValue
		/*
		 * -- See Rec. X.680 p 63. Assumes AUTOMATIC tagging
		 * SEQUENCE {
		 * 	identification	CHOICE {
		 * 		-- Abstract and transfer syntax object identifiers
		 * 		syntaxes	SEQUENCE {
		 * 			abstract	OBJECT IDENTIFIER,
		 * 			transfer	OBJECT IDENTIFIER
		 * 		},
		 * 		-- A single object identifier for identification of
		 * 		-- Abstract and transfer syntaxes
		 *		syntax	OBJECT IDENTIFIER,
		 *		-- OSI only. Negotiated OSI presentation context id
		 *		presentation-context-id	INTEGER,
		 *		-- OSI only. Context negotiation in progress
		 *		context-negotiation	SEQUENCE {
		 *			presentation-context-id	INTEGER,
		 *			transfer-syntax	OBJECT IDENTIFIER
		 *		},
		 *		-- The type of value is fixed by the application designer
		 *		-- Provided to support selective field encyption
		 *		transfer-syntax	OBJECT IDENTIFIER,
		 *		-- Data value is of a fixed ASN.1 type, and known to both the 
		 *		-- sender and the receiver 
		 *		fixed	NULL,
		 *		-- not for transmission. Provides a human readable description
		 *		data-value-descriptor	ObjectDescriptor OPTIONAL,
		 *		-- actual value
		 *		string-value	OCTET STRING
		 * 	} (WITH COMPONENTS { ... , data-value-descriptor ABSENT })
		 * }
		 */

		error("not implemented");
		return null;
	}
	private AsnNode characterStringValue(AsnNode expected){
		//Used By:  BuiltinValue
		// RestrictedCharacterStringValue
		// UnrestrictedCharacterStringValue
		trace("characterStringValue");
		AsnNode value = null;
		switch(expected.getProduction()) {
		case BMP_STRING_TYPE:
		case GENERAL_STRING_TYPE:
		case GRAPHIC_STRING_TYPE:
		case IA5_STRING_TYPE:
		case ISO646_STRING_TYPE:
		case NUMERIC_STRING_TYPE:
		case PRINTABLE_STRING_TYPE:
		case TELETEX_STRING_TYPE:
		case T61_STRING_TYPE:
		case UNIVERSAL_STRING_TYPE:
		case UTF8_STRING_TYPE:
		case VIDEOTEX_STRING_TYPE:
		case VISIBLE_STRING_TYPE:
		case GENERALIZED_TIME_TYPE:
		case UTC_TIME_TYPE:
			value = restrictedCharacterStringValue(expected);
			break;
		case CHARACTER_STRING_TYPE:
			value = unrestrictedCharacterStringValue(expected);
			break;
		default:
			error("not implemented");
			break;
		}
		return value;
	}
	private AsnNode relativeOIDComponents(AsnNode parent){
		//Used By:  RelativeOIDComponentsList
		// NumberForm
		// NameAndNumberForm
		// DefinedValue
		error("not implemented");
		return null;
	}
	private AsnNode relativeOIDComponentsList(AsnNode parent){
		//Used By:  RelativeOIDValue RelativeOIDComponentsList
		// RelativeOIDComponents
		// RelativeOIDComponents, RelativeOIDComponentsList
		error("not implemented");
		return null;
	}
	private AsnNode relativeOIDValue(AsnNode parent){
		//Used By:  BuiltinValue
		// "{", RelativeOIDComponentsList, "}"
		error("not implemented");
		return null;
	}
	/*
	private AstNode relativeOIDType(AstNode parent){
		//Used By:  BuiltinType
		// RELATIVE-OID
		error("not implemented");
		return null;
	}
	 */
	private AsnNode nameAndNumberForm(AsnNode parent){
		//Used By:  RelativeOIDComponents ObjIdComponents
		// identifier, "(", NumberForm, ")"
		error("not implemented");
		return null;
	}
	private AsnNode numberForm(AsnNode parent){
		//Used By:  NameAndNumberForm RelativeOIDComponents ObjIdComponents
		// number
		// DefinedValue
		error("not implemented");
		return null;
	}
	private AsnNode nameForm(AsnNode parent){
		//Used By:  ObjIdComponents DefinitiveObjIdComponent
		// identifier
		error("not implemented");
		return null;
	}
	private AsnNode objIdComponents(){
		//Used By:  ObjIdComponentsList
		// NameForm | NumberForm | NameAndNumberForm | DefinedValue
		if(lookAheadIs(TokenType.NUMBER_STRING)) {
			return AsnProduction.NUMBER_FORM.getNode(skipOver(TokenType.NUMBER_STRING));
		} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			Token identifier = skipOver(TokenType.IDENTIFIER_STRING);
			if(lookAheadIs(TokenType.LEFT_PAREN)) {
				skipOver(TokenType.LEFT_PAREN);
				Token num = skipOver(TokenType.NUMBER_STRING);
				skipOver(TokenType.RIGHT_PAREN);
				return AsnProduction.NAME_AND_NUMBER_FORM.getNode(identifier,num);
			} else {
				return AsnProduction.NAME_FORM.getNode(identifier);
			}
		} else {
			return definedValue(AsnProduction.OBJECT_IDENTIFIER_TYPE);
		}
	}
	private AsnNode objectIdentifierValue(){
		//Used By:  AssignedIdentifier BuiltinValue
		// "{", ObjIdComponentsList, "}"
		// "{", DefinedValue, ObjIdComponentsList, "}"
		if(!lookAheadIs(TokenType.LEFT_BRACE)) return null;
		AsnNode oid = AsnProduction.OBJECT_IDENTIFIER_VALUE.getNode();
		skipOver(TokenType.LEFT_BRACE);
		while(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			oid.addChild(objIdComponents());
		}
		skipOver(TokenType.RIGHT_BRACE);
		return oid;
	}
	private AsnNode externalValue(AsnNode parent){
		//Used By:  BuiltinValue
		// SequenceValue
		/*
		 * -- See Rec. X.680 p53. Assumes AUTOMATIC tagging
		 * EXTERNAL ::= [UNIVERSAL 8] SEQUENCE {
		 * 	identification	CHOICE {
		 * 		-- Abstract and transfer syntax object identifiers
		 * 		syntaxes	SEQUENCE {
		 * 			abstract	OBJECT IDENTIFIER,
		 * 			transfer	OBJECT IDENTIFIER
		 * 		},
		 * 		-- A single object identifier for identification of
		 * 		-- Abstract and transfer syntaxes
		 *		syntax	OBJECT IDENTIFIER,
		 *		-- OSI only. Negotiated OSI presentation context id
		 *		presentation-context-id	INTEGER,
		 *		-- OSI only. Context negotiation in progress
		 *		context-negotiation	SEQUENCE {
		 *			presentation-context-id	INTEGER,
		 *			transfer-syntax	OBJECT IDENTIFIER
		 *		},
		 *		-- The type of value is fixed by the application designer
		 *		-- Provided to support selective field encyption
		 *		transfer-syntax	OBJECT IDENTIFIER,
		 *		-- Data value is of a fixed ASN.1 type, and known to both the 
		 *		-- sender and the receiver 
		 *		fixed	NULL,
		 *		-- not for transmission. Provides a human readable description
		 *		data-value-descriptor	ObjectDescriptor OPTIONAL,
		 *		-- actual value
		 *		data-value	OCTET STRING
		 * 	} (WITH COMPONENTS { ... , 
		 * 		identification (WITH COMPONENTS { ... ,
		 * 			syntaxes	ABSENT,
		 * 			transfer-syntax	ABSENT,
		 * 			fixed	ABSENT} ) })
		 * }			
		 */
		error("not implemented");
		return null;
	}
	/*
	private AstNode externalType(AstNode parent){
		//Used By:  BuiltinType
		// EXTERNAL

	 * See Dubuisson p 301
	 * EXTERNAL ::= [UNIVERSAL 8] IMPLICIT SEQUENCE {
	 * 	identification CHOICE {
	 * 	syntax OBJECT IDENTIFIER,
	 *  presentation-context-id INTEGER,
	 *  context-negotiation SEQUENCE {
	 *  	presentation-context-id INTEGER,
	 *  	transfer-syntax OBJECT IDENTIFIER }
	 *  },
	 *  data-value-descriptor ObjectDescriptor OPTIONAL,
	 *  data-value OCTET STRING
	 * }

		error("not implemented");
		return null;
	}
	 */
	private AsnNode embeddedPDVValue(AsnNode parent){
		//Used By:  BuiltinValue
		// SequenceValue
		/*
		 * -- See Rec. X.680 p52. Assumes AUTOMATIC tagging
		 * EMBEDDED PDV ::= [UNIVERSAL 11] SEQUENCE {
		 * 	identification	CHOICE {
		 * 		-- Abstract and transfer syntax object identifiers
		 * 		syntaxes	SEQUENCE {
		 * 			abstract	OBJECT IDENTIFIER,
		 * 			transfer	OBJECT IDENTIFIER
		 * 		},
		 * 		-- A single object identifier for identification of
		 * 		-- Abstract and transfer syntaxes
		 *		syntax	OBJECT IDENTIFIER,
		 *		-- OSI only. Negotiated OSI presentation context id
		 *		presentation-context-id	INTEGER,
		 *		-- OSI only. Context negotiation in progress
		 *		context-negotiation	SEQUENCE {
		 *			presentation-context-id	INTEGER,
		 *			transfer-syntax	OBJECT IDENTIFIER
		 *		},
		 *		-- The type of value is fixed by the application designer
		 *		-- Provided to support selective field encyption
		 *		transfer-syntax	OBJECT IDENTIFIER,
		 *		-- Data value is of a fixed ASN.1 type, and known to both the 
		 *		-- sender and the receiver 
		 *		fixed	NULL,
		 *		-- not for transmission. Provides a human readable description
		 *		data-value-descriptor	ObjectDescriptor OPTIONAL,
		 *		-- actual value
		 *		data-value	OCTET STRING
		 * 	} (WITH COMPONENTS { ... , data-value-descriptor ABSENT })
		 * }
		 *
		 *					
		 */
		error("not implemented");
		return null;
	}
	/*
	private AstNode embeddedPDVType(AstNode parent){
		//Used By:  BuiltinType
		// EMBEDDED, PDV
		error("not implemented");
		return null;
	}
	 */
	private AsnNode taggedValue(AsnNode parent){
		//Used By:  BuiltinValue
		// Value
		error("not implemented");
		return null;
	}
	private AsnNode tagClass(AsnNode parent){
		//Used By:  Tag
		// UNIVERSAL
		// APPLICATION
		// PRIVATE
		// empty
		error("not implemented");
		return null;
	}
	private AsnNode classNumber(AsnNode parent){
		//Used By:  Tag
		// number
		// DefinedValue
		error("not implemented");
		return null;
	}
	private AsnNode tag(){
		//Used By:  TaggedType
		// "[", TagClass, ClassNumber, "]"
		skipOver(TokenType.LEFT_BRACKET);
		Token t = tokenizer.lookAhead();
		AsnNode tag = null;
		switch(t.type()) {
		case UNIVERSAL:
			tag = AsnProduction.TAG.getNode(skipOver(TokenType.UNIVERSAL));
			break;
		case PRIVATE:
			tag = AsnProduction.TAG.getNode(skipOver(TokenType.PRIVATE));
			break;
		case APPLICATION:
			tag = AsnProduction.TAG.getNode(skipOver(TokenType.APPLICATION));
			break;
		default:
			tag = AsnProduction.TAG.getNode(new Token(TokenType.CONTEXT_SPECIFIC));
			break;
		}
		if(lookAheadIs(TokenType.NUMBER_STRING)) {
			tag.addChild(AsnProduction.NUMBER_FORM.getNode(skipOver(TokenType.NUMBER_STRING)));
		} else {
			tag.addChild(definedValue(AsnProduction.INTEGER_TYPE));
		}
		skipOver(TokenType.RIGHT_BRACKET);
		return tag;
	}
	private AsnNode taggedType(){
		//Used By:  BuiltinType
		// Tag, Type
		// Tag, IMPLICIT, Type
		// Tag, EXPLICIT, Type
		if(!lookAheadIs(TokenType.LEFT_BRACKET)) return null;
		trace("taggedType");
		AsnNode taggedType = null;
		AsnNode tag = tag();
		Token t = lookAhead();
		switch(t.type()) {
		case EXPLICIT:
			taggedType = AsnProduction.TAGGED_TYPE.getNode(skipOver(TokenType.EXPLICIT));
			break;
		case IMPLICIT:
			taggedType = AsnProduction.TAGGED_TYPE.getNode(skipOver(TokenType.IMPLICIT));
			break;
		default:
			taggedType = AsnProduction.TAGGED_TYPE.getNode();
			break;
		}
		taggedType.addChild(tag);
		AsnNode type = type(null);
		type.addChild(taggedType);
		return type;
	}
	private AsnNode selectionType(){
		//Used By:  ReferencedType
		// identifier, "<", Type
		if(lookAheadIs(TokenType.IDENTIFIER_STRING) && lookAheadIs(TokenType.LESS_THAN,1)) {
			Token identifier = skipOver(TokenType.IDENTIFIER_STRING);
			skipOver(TokenType.LESS_THAN);
			return AsnProduction.SELECTION_TYPE.getNode(skipOver(TokenType.TYPE_REFERENCE),identifier);
		}
		return null;
	}
	private AsnNode choiceValue(AsnNode expected){
		//Used By:  BuiltinValue
		// identifier, ":", Value
		Token id = skipOver(TokenType.IDENTIFIER_STRING);
		AsnNode type = expected.locateType(id.value());
		skipOver(TokenType.COLON);
		AsnNode cv = new AsnNode(AsnProduction.CHOICE_VALUE,id);
		// showTree(expected,">>> ",System.out);
		// System.out.println("\n"+id+" : "+type);
		// System.out.flush();
		cv.addChild(value(expected.locateType(id.value())));
		return cv;
	}
	private AsnNode alternativeTypeList(){
		//Used By:  AlternativeTypeList ExtensionAdditionAlternativesGroup RootAlternativeTypeList
		// NamedType
		// AlternativeTypeList, ",", NamedType
		AsnNode atl = AsnProduction.ALTERNATIVE_TYPE_LIST.getNode();
		atl.addChild(componentType(null));
		while(lookAheadIs(TokenType.COMMA)) {
			if(lookAheadIs(TokenType.TRIPLE_DOT,1)) break;
			skipOver(TokenType.COMMA);
			atl.addChild(componentType(null));
		}
		return atl;
	}
	
	private AsnNode extensionAdditionAlternativesGroup(){
		//Used By:  ExtensionAdditionAlternative
		// "[[", VersionNumber, AlternativeTypeList, "]]"
		AsnNode eaag = null;
		skipOver(TokenType.LEFT_BRACKET);
		skipOver(TokenType.LEFT_BRACKET);
		if(lookAheadIs(TokenType.NUMBER_STRING)) {
			eaag = AsnProduction.EXTENSION_ADDITION_ALTERNATIVES_GROUP.getNode(skipOver(TokenType.NUMBER_STRING));
			skipOver(TokenType.COLON);
		} else {
			eaag = AsnProduction.EXTENSION_ADDITION_ALTERNATIVES_GROUP.getNode();
		}
		eaag.addChild(alternativeTypeList());
		skipOver(TokenType.RIGHT_BRACKET);
		skipOver(TokenType.RIGHT_BRACKET);
		return eaag;
	}
	private AsnNode extensionAdditionAlternative(){
		//Used By:  ExtensionAdditionAlternativesList
		// ExtensionAdditionAlternativesGroup
		// NamedType
		if(lookAheadIs(TokenType.LEFT_BRACKET)) {
			return extensionAdditionAlternativesGroup();
		} else {
			return namedType(null);
		}
	}
	private AsnNode extensionAdditionAlternativesList(){
		//Used By:  ExtensionAdditionAlternatives ExtensionAdditionAlternativesList
		// ExtensionAdditionAlternative
		// ExtensionAdditionAlternativesList, ",", ExtensionAdditionAlternative
		AsnNode eaal = AsnProduction.EXTENSION_ADDITION_ALTERNATIVES_LIST.getNode();
		eaal.addChild(extensionAdditionAlternative());
		if(lookAheadIs(TokenType.COMMA) && !lookAheadIs(TokenType.TRIPLE_DOT,1)) {
			skipOver(TokenType.COMMA);
			eaal.addChild(extensionAdditionAlternative());
		}
		return eaal;
	}
	private AsnNode extensionAdditionAlternatives(){
		//Used By:  AlternativeTypeLists
		// ",", ExtensionAdditionAlternativesList
		// empty
		AsnNode eaa = null;
		if(lookAheadIs(TokenType.COMMA) && !lookAheadIs(TokenType.TRIPLE_DOT,1)) {
			skipOver(TokenType.COMMA);
			eaa = extensionAdditionAlternativesList();
		}
		return eaa;
	}
	private AsnNode rootAlternativeTypeList(){
		//Used By:  AlternativeTypeLists
		// AlternativeTypeList
		return alternativeTypeList();
	}
	private AsnNode alternativeTypeLists(){
		//Used By:  ChoiceType
		// RootAlternativeTypeList
		// RootAlternativeTypeList, ",", ExtensionAndException, ExtensionAdditionAlternatives, OptionalExtensionMarker
		AsnNode atl = AsnProduction.ALTERNATIVE_TYPE_LISTS.getNode();
		atl.addChild(rootAlternativeTypeList());
		if(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			atl.addChild(extensionAndException());
			atl.addChild(extensionAdditionAlternatives());
			atl.addChild(optionalExtensionMarker());
		}
		return atl;
	}
	private AsnNode choiceType(){
		//Used By:  BuiltinType
		// CHOICE, "{", AlternativeTypeLists, "}"
		AsnNode choice =new AsnNode(AsnProduction.CHOICE_TYPE);
		skipOver(TokenType.CHOICE);
		skipOver(TokenType.LEFT_BRACE);
		choice.addChild(alternativeTypeLists());
		skipOver(TokenType.RIGHT_BRACE);
		return choice;
	}
	private AsnNode setOfValue(AsnNode expectedType){
		//Used By:  BuiltinValue
		// "{", ValueList, "}"
		// "{", NamedValueList, "}"
		// "{", "}"
		trace("setOfValue");
		AsnNode value = AsnProduction.SET_OF_VALUE.getNode();
		skipOver(TokenType.LEFT_BRACE);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			value.addChild(valueList(expectedType.getChild(0)));
		}
		skipOver(TokenType.RIGHT_BRACE);
		return value;
	}
	private AsnNode setOfType(){
		//Used By:  BuiltinType
		// SET, [SIZE (Constraint)] OF, Type
		// SET, [SIZE (Constraint)] OF, NamedType
		AsnNode setOfType = AsnProduction.SET_OF_TYPE.getNode();
		if(lookAheadIs(TokenType.SET)) {
			skipOver(TokenType.SET);
			switch(lookAhead().type()) {
			case SIZE:
				setOfType.addChild(sizeConstraint(setOfType));
				skipOver(TokenType.OF);
				break;
			case LEFT_PAREN:
				setOfType.addChild(constraint(setOfType));
				skipOver(TokenType.OF);
				break;
			default:
				error("Type not handled in sequenceOfType");
				break;
			}
		} else {
			skipOver(TokenType.SET_OF);
		}
		setOfType.addChild(lookAheadIs(TokenType.IDENTIFIER_STRING) ? namedType(null) : type(null));
		return setOfType;
	}
	private AsnNode setValue(AsnNode expectedType){
		//Used By:  BuiltinValue
		// "{", ComponentValueList, "}"
		// "{", "}"
		AsnNode sv = AsnProduction.SET_VALUE.getNode();
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			skipOver(TokenType.LEFT_BRACE);
			if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
				sv.addChild(componentValueList(expectedType));
			}
			skipOver(TokenType.RIGHT_BRACE);
		}
		return sv;
	}
	private AsnNode setType(){
		//Used By:  BuiltinType
		// SET, "{", "}"
		// SET, "{", ExtensionAndException, OptionalExtensionMarker, "}"
		// SET, "{", ComponentTypeLists, "}"
		//Used By:  BuiltinType
		// Note that the second form is already included in ComponentTypeLists, and is redundant
		if(!lookAheadIs(TokenType.SET)) return null;
		if(lookAheadIs(TokenType.SIZE,1) || lookAheadIs(TokenType.LEFT_PAREN,1)) {
			return setOfType();
		}
		skipOver(TokenType.SET);
		AsnNode setType =  AsnProduction.SET_TYPE.getNode();
		skipOver(TokenType.LEFT_BRACE);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			setType.addChild(componentTypeLists(null));
		}
		skipOver(TokenType.RIGHT_BRACE);
		return setType;
	}
	private AsnNode valueList(AsnNode expectedType){
		//Used By:  SetOfValue ValueList SequenceOfValue
		// Value | ValueList, ",", Value
		AsnNode vl = AsnProduction.VALUE_LIST.getNode();
		vl.addChild(value(expectedType));
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			vl.addChild(value(expectedType));
		}
		return vl;
	}
	private AsnNode sequenceOfValue(AsnNode expectedType){
		//Used By:  BuiltinValue
		// "{", ValueList, "}"
		// "{", NamedValueList, "}"
		// "{", "}"
		// showTree(expectedType, ">>> ", System.out);
		// System.out.flush();
		trace("sequenceOfValue");
		AsnNode value = AsnProduction.SEQUENCE_OF_VALUE.getNode();
		skipOver(TokenType.LEFT_BRACE);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			value.addChild(valueList(expectedType.getChild(0)));
		}
		skipOver(TokenType.RIGHT_BRACE);
		return value;
	}
	private AsnNode sequenceOfType(AsnNode paramList){
		//Used By:  BuiltinType
		// SEQUENCE, [SIZE (Constraint) ] OF, Type
		// SEQUENCE, [SIZE (Constraint) ] OF, NamedType
		AsnNode sequenceType = AsnProduction.SEQUENCE_OF_TYPE.getNode();
		if(lookAheadIs(TokenType.SEQUENCE)) {
			skipOver(TokenType.SEQUENCE);
			switch(lookAhead().type()) {
			case SIZE:
				sequenceType.addChild(sizeConstraint(sequenceType));
				skipOver(TokenType.OF);
				break;
			case LEFT_PAREN:
				sequenceType.addChild(constraint(sequenceType));
				skipOver(TokenType.OF);
				break;
			default:
				error("Type not handled in sequenceOfType");
				break;
			}
		} else {
			skipOver(TokenType.SEQUENCE_OF);
		}
		sequenceType.addChild(lookAheadIs(TokenType.IDENTIFIER_STRING) ? namedType(paramList) : type(paramList));
		return sequenceType;
	}
	private AsnNode componentValueList(AsnNode expectedType){
		//Used By:  ComponentValueList SequenceValue SetValue
		// NamedValue | ComponentValueList, ",", NamedValue
		AsnNode cvl = AsnProduction.COMPONENT_VALUE_LIST.getNode();
		cvl.addChild(namedValue(expectedType));
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			cvl.addChild(namedValue(expectedType));
		}
		return cvl;
	}
	private AsnNode sequenceValue(AsnNode expectedType){
		//Used By:  UnrestrictedCharacterStringValue BuiltinValue NumericRealValue ExternalValue EmbeddedPDVValue
		// "{", ComponentValueList, "}"
		// "{", "}"
		AsnNode sv = AsnProduction.SEQUENCE_VALUE.getNode();
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			skipOver(TokenType.LEFT_BRACE);
			if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
				sv.addChild(componentValueList(expectedType));
			}
			skipOver(TokenType.RIGHT_BRACE);
		}
		return sv;
	}
	private AsnNode componentType(AsnNode paramList){
		//Used By:  ExtensionAddition ComponentTypeList
		// NamedType | NamedType, OPTIONAL
		// NamedType, DEFAULT, Value
		// COMPONENTS, OF, Type
		trace("componentType");
		switch(lookAhead().type()) {
		case IDENTIFIER_STRING:
			AsnNode n = namedType(paramList);
			// if(lookAheadIs(TokenType.DEFINED_BY)) {	// EXTRA - may need to move this to a constraint
			//	skipOver(TokenType.DEFINED_BY);
			//	n.addChild(Ast.DEFINED_BY.getNode(skipOver(TokenType.IDENTIFIER_STRING)));
			//}
			if(lookAheadIs(TokenType.OPTIONAL)) {
				n.addChild(AsnProduction.TYPE_OPTIONALITY_SPEC.getNode(skipOver(TokenType.OPTIONAL)));
			} else if(lookAheadIs(TokenType.DEFAULT)) {
				AsnNode def = AsnProduction.TYPE_OPTIONALITY_SPEC.getNode(skipOver(TokenType.DEFAULT));
				n.addChild(def);
				def.addChild(value(n.getChild(0)));
			}
			return n;
		case COMPONENTS:
			skipOver(TokenType.COMPONENTS);
			skipOver(TokenType.OF);
			n = AsnProduction.COMPONENT_TYPE.getNode();
			n.addChild(type(paramList));
			return n;
		default:
			error("Not yet handled in componentType()");
			return null;
		}
	}
	private AsnNode componentTypeList(AsnNode paramList){
		//Used By:  ExtensionAdditionGroup RootComponentTypeList ComponentTypeList
		// ComponentType | ComponentTypeList, ",", ComponentType
		AsnNode cl = AsnProduction.COMPONENT_TYPE_LIST.getNode();
		cl.addChild(componentType(paramList));
		while(lookAheadIs(TokenType.COMMA) && !lookAheadIs(TokenType.TRIPLE_DOT,1)) {
			skipOver(TokenType.COMMA);
			cl.addChild(componentType(paramList));
		}
		return cl;
	}
	private AsnNode versionNumber(){
		//Used By:  ExtensionAdditionGroup ExtensionAdditionAlternativesGroup
		// empty | number, ":"
		error("not implemented");
		return null;
	}
	private AsnNode extensionAdditionGroup(){
		//Used By:  ExtensionAddition
		// "[[", VersionNumber, ComponentTypeList, "]]"
		AsnNode eag = null;
		skipOver(TokenType.LEFT_BRACKET);
		skipOver(TokenType.LEFT_BRACKET);
		if(lookAheadIs(TokenType.NUMBER_STRING)) {
			eag = AsnProduction.EXTENSION_ADDITION_GROUP.getNode(skipOver(TokenType.NUMBER_STRING));
			skipOver(TokenType.COLON);
		} else {
			eag = AsnProduction.EXTENSION_ADDITION_GROUP.getNode();
		}
		eag.addChild(componentTypeList(null));
		skipOver(TokenType.RIGHT_BRACKET);
		skipOver(TokenType.RIGHT_BRACKET);
		return eag;
	}
	private AsnNode extensionAddition(){
		//Used By:  ExtensionAdditionList
		// ComponentType | ExtensionAdditionGroup
		return lookAheadIs(TokenType.LEFT_BRACKET) ? extensionAdditionGroup() : componentType(null) ;
	}
	private AsnNode extensionAdditionList(){
		//Used By:  ExtensionAdditionList ExtensionAdditions
		// ExtensionAddition
		// ExtensionAdditionList, ",", ExtensionAddition
		AsnNode eal = AsnProduction.EXTENSION_ADDITION_LIST.getNode();
		eal.addChild(extensionAddition());
		while(lookAheadIs(TokenType.COMMA) && !lookAheadIs(TokenType.TRIPLE_DOT,1)) {
			skipOver(TokenType.COMMA);
			eal.addChild(extensionAddition());
		}
		return eal;
	}
	private AsnNode extensionAdditions(){
		//Used By:  ComponentTypeLists
		// ",", ExtensionAdditionList
		// empty
		AsnNode ea = null;
		if(lookAheadIs(TokenType.COMMA) && !lookAheadIs(TokenType.TRIPLE_DOT,1)) {
			skipOver(TokenType.COMMA);
			ea = AsnProduction.EXTENSION_ADDITIONS.getNode();
			ea.addChild(extensionAdditionList());
		}
		return ea;
	}
	private AsnNode extensionEndMarker(){
		//Used By:  ComponentTypeLists
		// ",", "..."
		if(lookAheadIs(TokenType.COMMA) && lookAheadIs(TokenType.TRIPLE_DOT,1)) {
			skipOver(TokenType.COMMA);
			skipOver(TokenType.TRIPLE_DOT);
			return lookAheadIs(TokenType.RIGHT_BRACE) ? 
					AsnProduction.OPTIONAL_EXTENSION_MARKER.getNode() : AsnProduction.EXTENSION_END_MARKER.getNode();
		}
		return null;
	}
	private AsnNode rootComponentTypeList(AsnNode paramList){
		//Used By:  ComponentTypeLists
		// ComponentTypeList
		return componentTypeList(paramList);
	}
	private AsnNode extensionAndException(){
		//Used By:  ComponentTypeLists SetType SequenceType AlternativeTypeLists
		// "..." | "...", ExceptionSpec
		skipOver(TokenType.TRIPLE_DOT);
		AsnNode n = AsnProduction.EXTENSION_AND_EXCEPTION.getNode();
		n.addChild(exceptionSpec());
		return n;
	}
	private AsnNode componentTypeLists(AsnNode paramList){
		//Used By:  SetType SequenceType
		// RootComponentTypeList
		// RootComponentTypeList, ",", ExtensionAndException, ExtensionAdditions, OptionalExtensionMarker
		// RootComponentTypeList, ",", ExtensionAndException, ExtensionAdditions, ExtensionEndMarker, ",", RootComponentTypeList
		// ExtensionAndException, ExtensionAdditions, OptionalExtensionMarker
		// ExtensionAndException, ExtensionAdditions, ExensionEndMarker, ",", RootComponentTypeList
		AsnNode ctl = AsnProduction.COMPONENT_TYPE_LISTS.getNode();
		if(lookAheadIs(TokenType.TRIPLE_DOT)) {
			ctl.addChild(extensionAndException());
			ctl.addChild(extensionAdditions());
			ctl.addChild(extensionEndMarker());
			if(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				ctl.addChild(rootComponentTypeList(paramList));
			}
		} else {
			ctl.addChild(rootComponentTypeList(paramList));
			if(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				ctl.addChild(extensionAndException());
				ctl.addChild(extensionAdditions());
				ctl.addChild(extensionEndMarker());
				if(lookAheadIs(TokenType.COMMA)) {
					skipOver(TokenType.COMMA);
					ctl.addChild(rootComponentTypeList(paramList));
				}
			}
		}
		return ctl;
	}
	private AsnNode optionalExtensionMarker(){
		//Used By:  ComponentTypeLists SetType SequenceType AlternativeTypeLists
		// ",", "..." | empty
		// Note: Included in ExtensionEndMarker
		if(!lookAheadIs(TokenType.COMMA)) return null;
		skipOver(TokenType.COMMA);
		skipOver(TokenType.TRIPLE_DOT);
		return AsnProduction.OPTIONAL_EXTENSION_MARKER.getNode();
	}
	private AsnNode sequenceType(AsnNode paramList){
		//Used By:  BuiltinType
		// SEQUENCE, "{", "}"
		// SEQUENCE, "{", ExtensionAndException, OptionalExtensionMarker, "}"
		// SEQUENCE, "{", ComponentTypeLists, "}"
		// Note that the second form is already included in ComponentTypeLists, and is redundant
		if(!lookAheadIs(TokenType.SEQUENCE)) return null;
		if(lookAheadIs(TokenType.SIZE,1) || lookAheadIs(TokenType.LEFT_PAREN,1)) {
			return sequenceOfType(paramList);
		}
		skipOver(TokenType.SEQUENCE);
		AsnNode sequenceType =  AsnProduction.SEQUENCE_TYPE.getNode();
		skipOver(TokenType.LEFT_BRACE);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			sequenceType.addChild(componentTypeLists(paramList));
		}
		skipOver(TokenType.RIGHT_BRACE);
		return sequenceType;
	}
	private AsnNode octetStringValue(AsnNode parent){
		//Used By:  BuiltinValue
		// bstring | hstring | CONTAINING, Value
		AsnNode value = null;
		switch(lookAhead().type()) {
		case BINARY_STRING:
			value =  AsnProduction.OCTET_STRING_VALUE.getNode(skipOver(TokenType.BINARY_STRING));
			break;
		case CONTAINING:
			skipOver(TokenType.CONTAINING);
			value = AsnProduction.OCTET_STRING_VALUE.getNode();
			value.addChild(value(AsnProduction.INTEGER_TYPE.getNode()));
			break;
		default:
			error("Not yet implemented");
			break;
		}
		return value;
	}
	/*
	private AstNode octetStringType(AstNode parent){
		//Used By:  BuiltinType
		// OCTET, STRING
		error("not implemented");
		return null;
	}
	 */
	private AsnNode identifierList(AsnNode parent){
		//Used By:  BitStringValue IdentifierList
		// identifier | IdentifierList, ",", identifier
		AsnNode list = new AsnNode(AsnProduction.IDENTIFIER_LIST);
		if(!lookAheadIs(TokenType.RIGHT_BRACE)) {
			list.addChild(new AsnNode(AsnProduction.NAME_FORM,skipOver(TokenType.IDENTIFIER_STRING)));
			while(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				list.addChild(new AsnNode(AsnProduction.NAME_FORM,skipOver(TokenType.IDENTIFIER_STRING)));
			}
		}
		return list;
	}
	private AsnNode bitStringValue(AsnNode parent){
		//Used By:  BuiltinValue
		// bstring | hstring | "{", IdentifierList, "}" | "{", "}" | CONTAINING, Value
		AsnNode value = null;
		switch(lookAhead().type()) {
		case BINARY_STRING:
			value =  AsnProduction.BIT_STRING_VALUE.getNode(skipOver(TokenType.BINARY_STRING));
			break;
		case LEFT_BRACE:
			skipOver(TokenType.LEFT_BRACE);
			value = AsnProduction.BIT_STRING_VALUE.getNode();
			value.addChild(identifierList(parent));
			skipOver(TokenType.RIGHT_BRACE);
			break;
		case CONTAINING:
			value = AsnProduction.BIT_STRING_VALUE.getNode();
			skipOver(TokenType.CONTAINING);
			value.addChild(value(AsnProduction.INTEGER_TYPE.getNode()));
			break;
		default:
			error("Not yet implemented");
			break;
		}
		return value;
	}
	private AsnNode namedBit(){
		//Used By:  NamedBitList
		// identifier, "(", number, ")"
		// identifier, "(", DefinedValue, ")"
		AsnNode n = AsnProduction.NAMED_NUMBER.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		skipOver(TokenType.LEFT_PAREN);
		Token t = tokenizer.lookAhead();
		switch(t.type()) {
		case NUMBER_STRING:
			n.addChild(AsnProduction.NUMBER_FORM.getNode(skipOver(TokenType.NUMBER_STRING)));
			break;
		case IDENTIFIER_STRING:
			n.addChild(definedValue(AsnProduction.INTEGER_TYPE));
			break;
		default:
			error("Type not yet handled in NamedBit");
			break;
		}
		skipOver(TokenType.RIGHT_PAREN);
		return n;

	}
	private AsnNode namedBitList(){
		//Used By:  NamedBitList BitStringType
		// NamedBit | NamedBitList, ",", NamedBit
		AsnNode list = AsnProduction.NAMED_BIT_LIST.getNode();
		list.addChild(namedBit());
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			list.addChild(namedBit());
		}
		return list;
	}
	private AsnNode bitStringType(){
		//Used By:  BuiltinType
		// BIT, STRING
		// BIT, STRING, "{", NamedBitList, "}"
		skipOver(TokenType.BIT_STRING);
		AsnNode bitString = AsnProduction.BIT_STRING_TYPE.getNode();
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			skipOver(TokenType.LEFT_BRACE);
			bitString.addChild(namedBitList());
			skipOver(TokenType.RIGHT_BRACE);
		}
		return bitString;
	}
	private AsnNode realValue(AsnNode parent){
		//Used By:  BuiltinValue
		// NumericRealValue ::= realnumber | "-" realnumber | sequenceValue
		// SpecialRealValue ::= PLUS-INFINITY | MINUS-INFINITY
		// sequencevalue ::=  [UNIVERSAL 9] IMPLICIT SEQUENCE { mantissa INTEGER (ALL EXCEPT 0), base INTEGER (2|10), exponent INTEGER }
		trace("realValue");
		if(lookAheadIs(TokenType.PLUS_INFINITY)) {
			return AsnProduction.SPECIAL_REAL_VALUE.getNode(skipOver(TokenType.PLUS_INFINITY));
		} else if(lookAheadIs(TokenType.MINUS_INFINITY)) {
			return AsnProduction.SPECIAL_REAL_VALUE.getNode(skipOver(TokenType.MINUS_INFINITY));
		} else if(lookAheadIs(TokenType.LEFT_BRACE)){
			AsnNode value = AsnProduction.REAL_VALUE.getNode();
			skipOver(TokenType.LEFT_BRACE);
			for(String n : new String[] {"mantissa","base","exponent"}) {
				Token nm = new Token(TokenType.IDENTIFIER_STRING,n);
				if(lookAheadIs(TokenType.IDENTIFIER_STRING) && !(lookAheadIs(TokenType.COMMA,1) || lookAheadIs(TokenType.RIGHT_BRACE,1))) {
					Token name = skipOver(TokenType.IDENTIFIER_STRING);
					if(!n.equals(name.value())) error("RealValue: Expected "+n+" found "+name.value());
				}
				switch(lookAhead().type()) {
				case MINUS:
					skipOver(TokenType.MINUS);
					value.addChild(AsnProduction.SIGNED_NUMBER.getNode(nm,skipOver(TokenType.NUMBER_STRING)));
					break;
				case NUMBER_STRING:
					value.addChild(AsnProduction.NUMBER_FORM.getNode(nm,skipOver(TokenType.NUMBER_STRING)));
					break;
				case IDENTIFIER_STRING:
					value.addChild(AsnProduction.NAME_FORM.getNode(nm,skipOver(TokenType.IDENTIFIER_STRING)));
					break;
				default:
					error("Expected number");
					break;
				}
				if(lookAheadIs(TokenType.COMMA)) skipOver(TokenType.COMMA);
			}
			skipOver(TokenType.RIGHT_BRACE);
			return value;
		} else {
			boolean isMinus = lookAheadIs(TokenType.MINUS);
			if(isMinus) {
				Token minus = skipOver(TokenType.MINUS);
				AsnNode v =  AsnProduction.REAL_VALUE.getNode(lookAheadIs(TokenType.NUMBER_STRING) ? skipOver(TokenType.NUMBER_STRING) : skipOver(TokenType.REAL_NUMBER_STRING));
				v.addChild(AsnProduction.SIGNED_NUMBER.getNode(minus));
				return v;
			} else {
				return AsnProduction.REAL_VALUE.getNode(lookAheadIs(TokenType.NUMBER_STRING) ? skipOver(TokenType.NUMBER_STRING) : skipOver(TokenType.REAL_NUMBER_STRING));
			}
		}
	}
	/*
	private AstNode realType(AstNode parent){
		//Used By:  BuiltinType
		// REAL
		error("not implemented");
		return null;
	}
	 */
	private AsnNode enumeratedValue(AsnNode parent){
		//Used By:  BuiltinValue
		// identifier
		AsnNode enumeratedValue = AsnProduction.ENUMERATED_VALUE.getNode(parent.getNameToken(),skipOver(TokenType.IDENTIFIER_STRING));
		return enumeratedValue;
	}
	private AsnNode enumerationItem(){
		//Used By:  Enumeration
		// identifier | NamedNumber
		AsnNode enumerationItem = null;
		if(lookAheadIs(TokenType.LEFT_PAREN,1)) { // named number
			enumerationItem =  namedNumber();
		} else {	// identifier
			enumerationItem =  AsnProduction.ENUMERATION_ITEM.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		}
		return enumerationItem;
	}
	private AsnNode enumeration(){
		//Used By:  RootEnumeration Enumeration AdditionalEnumeration
		// EnumerationItem
		// EnumerationItem, ",", Enumeration
		AsnNode enumeration = AsnProduction.ENUMERATION.getNode();
		enumeration.addChild(enumerationItem());
		while(lookAheadIs(TokenType.COMMA) && !lookAheadIs(TokenType.TRIPLE_DOT,1)) {
			skipOver(TokenType.COMMA);
			enumeration.addChild(enumerationItem());
		}
		return enumeration;
	}
	private AsnNode additionalEnumeration(){
		//Used By:  Enumerations
		// Enumeration
		return enumeration();
	}
	private AsnNode rootEnumeration(){
		//Used By:  Enumerations
		// Enumeration
		return enumeration();
	}
	private AsnNode enumerations(){
		//Used By:  EnumeratedType
		// RootEnumeration
		// RootEnumeration, ",", "...", ExceptionSpec
		// RootEnumeration, ",", "...", ExceptionSpec, ",", AdditionalEnumeration
		AsnNode enumerations = AsnProduction.ENUMERATIONS.getNode();
		enumerations.addChild(rootEnumeration());
		if(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			enumerations.addChild(AsnProduction.EXTENSION.getNode(skipOver(TokenType.TRIPLE_DOT)));
			enumerations.addChild(exceptionSpec());
			if(lookAheadIs(TokenType.COMMA)) {
				skipOver(TokenType.COMMA);
				enumerations.addChild(additionalEnumeration());
			}
		}
		return enumerations;
	}
	private AsnNode enumeratedType(){
		//Used By:  BuiltinType
		// ENUMERATED, "{", Enumerations, "}"
		skipOver(TokenType.ENUMERATED);
		AsnNode enumerated = AsnProduction.ENUMERATED_TYPE.getNode();
		skipOver(TokenType.LEFT_BRACE);
		enumerated.addChild(enumerations());
		skipOver(TokenType.RIGHT_BRACE);
		return enumerated;
	}
	private AsnNode integerValue(){
		//Used By:  BuiltinValue
		// SignedNumber | identifier
		if(lookAheadIs(TokenType.IDENTIFIER_STRING) && !lookAheadIs(TokenType.DOT,1)) {
			return AsnProduction.INTEGER_VALUE.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		} else if(lookAheadIs(TokenType.MINUS) || lookAheadIs(TokenType.NUMBER_STRING)) {
			return signedNumber();
		}
		return null;
	}
	private AsnNode signedNumber(){
		//Used By:  IntegerValue ExceptionIdentification NamedNumber
		// number | "-", number
		AsnNode signedNumber = null;
		if(lookAheadIs(TokenType.MINUS)) {
			skipOver(TokenType.MINUS);
			signedNumber = AsnProduction.SIGNED_NUMBER.getNode(skipOver(TokenType.NUMBER_STRING));
		} else if(lookAheadIs(TokenType.NUMBER_STRING)){
			signedNumber = AsnProduction.NUMBER_FORM.getNode(skipOver(TokenType.NUMBER_STRING));
		} else {
			error("not yet implemented");
		}
		return signedNumber;
	}
	private AsnNode namedNumber(){
		//Used By:  NamedNumberList EnumerationItem
		// identifier, "(", SignedNumber, ")"
		// identifier, "(", DefinedValue, ")"
		AsnNode n = AsnProduction.NAMED_NUMBER.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		skipOver(TokenType.LEFT_PAREN);
		Token t = tokenizer.lookAhead();
		switch(t.type()) {
		case MINUS:
		case NUMBER_STRING:
			n.addChild(signedNumber());
			break;
		case IDENTIFIER_STRING:
			n.addChild(definedValue(AsnProduction.INTEGER_TYPE));
			break;
		default:
			error("Type not yet handled in NamedNumber");
			break;
		}
		skipOver(TokenType.RIGHT_PAREN);
		return n;
	}
	private AsnNode namedNumberList(){
		//Used By:  IntegerType NamedNumberList
		// NamedNumber | NamedNumberList, ",", NamedNumber
		AsnNode list = AsnProduction.NAMED_NUMBER_LIST.getNode();
		list.addChild(namedNumber());
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			list.addChild(namedNumber());
		}
		return list;
	}
	private AsnNode namedValue(AsnNode expectedType){
		//Used By:  ComponentValueList
		// identifier, Value
		Token identifier = skipOver(TokenType.IDENTIFIER_STRING);
		AsnNode nv = AsnProduction.NAMED_VALUE.getNode(identifier);
		AsnNode valueType = expectedType.locateType(identifier.value());
		if(valueType == null) error("ValueType is null for "+expectedType);
		nv.addChild(value(valueType));
		return nv;
	}
	private AsnNode referencedValue(AsnNode expectedType){
		//Used By:  Value FixedTypeFieldVal
		// DefinedValue | ValueFromObject
		trace("ReferencedValue"+expectedType);
		switch(expectedType.getProduction()) {
		case SELECTION_TYPE:
			AsnNode ref = currentModule.getType(expectedType.getName());
			AsnNode type = ref.locateType(expectedType.getValue());
			// System.out.println("Ref "+ref+" Type "+type);
			return value(type);
		default:
			if(lookAheadIs(TokenType.DOT,1)) {
				return valueFromObject(expectedType);
			} else if(currentModule.hasType(expectedType.getName())) {
				return value(currentModule.getType(expectedType.getName()));
			}
			return definedValue();
		}
	}
	private AsnNode builtinValue(AsnNode expected){
		//Used By:  Value 
		// FixedTypeFieldVal | BitStringValue | BooleanValue | CharacterStringValue |
		// ChoiceValue | EmbeddedPDVValue | EnumeratedValue | ExternalValue | InstanceOfValue |
		// IntegerValue | NullValue | ObjectIdentifierValue | OctetStringValue | RealValue | RelativeOIDValue |
		// SequenceValue | SequenceOfValue | SetValue | SetOfValue | TaggedValue
		// if(debug) System.out.println("\n\n<BuiltInValue> "+expected.getAstProduction()+" "+lookAhead());
		trace("builtInValue");
		switch(expected.getProduction()) {
		case ANY_TYPE:
			return anyValue(expected);
		case NULL_TYPE:
			return AsnProduction.NULL_VALUE.getNode(skipOver(TokenType.NULL));
		case BIT_STRING_TYPE:
			return bitStringValue(expected);
		case BOOLEAN_TYPE:
			if(lookAheadIs(TokenType.TRUE) || lookAheadIs(TokenType.FALSE)) {
				return AsnProduction.BOOLEAN_VALUE.getNode(skipOver(tokenizer.lookAhead().type()));
			}
			throw new ModelException("Expected boolean value, found "+tokenizer.lookAhead());
		case BMP_STRING_TYPE:
		case GENERAL_STRING_TYPE:
		case GRAPHIC_STRING_TYPE:
		case IA5_STRING_TYPE:
		case ISO646_STRING_TYPE:
		case NUMERIC_STRING_TYPE:
		case PRINTABLE_STRING_TYPE:
		case TELETEX_STRING_TYPE:
		case T61_STRING_TYPE:
		case UNIVERSAL_STRING_TYPE:
		case UTF8_STRING_TYPE:
		case VIDEOTEX_STRING_TYPE:
		case VISIBLE_STRING_TYPE:
		case GENERALIZED_TIME_TYPE:
		case UTC_TIME_TYPE:
		case CHARACTER_STRING_TYPE:
			return characterStringValue(expected);
		case CHOICE_TYPE:
			return choiceValue(expected);
		case EMBEDDED_PDV_TYPE:
			return embeddedPDVValue(expected);
		case ENUMERATED_TYPE:
			return enumeratedValue(expected);
		case EXTERNAL_TYPE:
			return externalValue(expected);
		case INTEGER_TYPE:
			return integerValue();
		case OBJECT_IDENTIFIER_TYPE:
			return objectIdentifierValue();
		case OCTET_STRING_TYPE:
			return octetStringValue(expected);
		case REAL_TYPE:
			return realValue(expected);
		case RELATIVE_OID_TYPE:
			return relativeOIDValue(expected);
		case SEQUENCE_TYPE:
			return sequenceValue(expected);
		case SEQUENCE_OF_TYPE:
			return sequenceOfValue(expected);
		case SET_TYPE:
			return setValue(expected);
		case SET_OF_TYPE:
			return setOfValue(expected);
		default:
			break;
		}
		return null;
	}
	private AsnNode anyValue(AsnNode expected) {
		trace("anyValue");
		AsnNode v = null;
		if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			AsnNode defn = locate(AsnProduction.TYPE_ASSIGNMENT,skipOver(lookAhead().type()).value());
			skipOver(TokenType.COLON);
			v = value(defn.getChild(0));
		} else if(lookAhead().isBuiltInType()) {
			AsnNode type = type(null);
			expected.addChild(type);
			skipOver(TokenType.COLON);
			v = value(type);
		}
		return v;		
	}
	private AsnNode value(AsnNode expectedType){
		//Used By:  ComponentType SingleValue ValueList OpenTypeFieldVal InstanceOfValue ActualParameter TaggedValue Setting ContentsConstraint NamedValue LowerEndValue ValueOptionalitySpec ValueAssignment ChoiceValue ParameterizedValueAssignment BitStringValue UserDefinedConstraintParameter ExceptionIdentification OctetStringValue PatternConstraint UpperEndValue
		// BuiltinValue | ReferencedValue | ObjectClassFieldValue
		trace("value:"+expectedType);
		AsnNode value = builtinValue(expectedType);
		warn("builtInValue returned "+value);
		if(value == null) {
			value = referencedValue(expectedType);
			warn("ReferencedValue returned "+value);
		}
		if(value == null) {
			value = objectClassFieldValue(expectedType);
			warn("ObjectClassFieldValue returned "+value);
		}
		// constant number values
		if(value == null && lookAheadIs(TokenType.NUMBER_STRING)) {
			value = AsnProduction.INTEGER_VALUE.getNode(skipOver(TokenType.NUMBER_STRING));
		}
		if(value != null) return value;
		error("not implemented");
		return null;
	}
	private AsnNode referencedType(AsnNode paramList){
		//Used By:  Type
		// DefinedType | UsefulType | SelectionType | TypeFromObject | ValueSetFromObjects
		AsnNode ref = definedType();
		if(ref != null) return ref;
		ref = selectionType();
		if(ref != null) return ref;
		ref = typeFromObject();
		if(ref != null) return ref;
		ref = valueSetFromObjects();
		error("not implemented");
		return null;
	}
	private AsnNode namedType(AsnNode paramList){
		//Used By:  ComponentType AlternativeTypeList SequenceOfType TypeWithConstraint SetOfType ExtensionAdditionAlternative
		// identifier, Type
		if(!lookAheadIs(TokenType.IDENTIFIER_STRING)) return null;
		AsnNode nt = AsnProduction.NAMED_TYPE.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		nt.addChild(type(paramList));
		return nt;
	}
	private AsnNode builtinType(AsnNode paramList){
		//Used By:  Type
		// BitStringType | BooleanType | CharacterStringType | ChoiceType | EmbeddedPDVType |
		// EnumeratedType | ExternalType | InstanceOfType | IntegerType | NullType | ObjectClassFieldType |
		// ObjectIdentifierType| OctetStringType | RealType | RelativeOIDType | SequenceType |
		// SequenceOfType | SetType | SetOfType | TaggedType
		trace("builtinType");
		AsnNode type = null;
		Token t = tokenizer.lookAhead();
		switch(t.type()) {
		case ANY:
			type = new AsnNode(AsnProduction.ANY_TYPE,skipOver(TokenType.ANY));
			if(lookAheadIs(TokenType.DEFINED_BY)) {	// EXTRA - may need to move this to a constraint
				skipOver(TokenType.DEFINED_BY);
				type.addChild(new AsnNode(AsnProduction.DEFINED_BY,skipOver(lookAhead().type())));
			}
			break;
		case NULL:	// NullType ::= NULL
			type = AsnProduction.NULL_TYPE.getNode(skipOver(TokenType.NULL));
			break;
		case BOOLEAN: // BooleanType ::= BOOLEAN
			type = AsnProduction.BOOLEAN_TYPE.getNode(skipOver(TokenType.BOOLEAN));
			break;
		case REAL:	// RealType = "REAL"
			type = AsnProduction.REAL_TYPE.getNode(skipOver(TokenType.REAL));
			break;
		case INTEGER: // IntegerType ::= INTEGER | INTEGER "{" NamedNumberList "}"
			type = AsnProduction.INTEGER_TYPE.getNode(skipOver(TokenType.INTEGER));
			if(lookAheadIs(TokenType.LEFT_BRACE)) {
				skipOver(TokenType.LEFT_BRACE);
				type.addChild(namedNumberList());
				skipOver(TokenType.RIGHT_BRACE);
			}
			break;
		case OCTET_STRING:	// OctetStringType ::= OCTET STRING
			type = AsnProduction.OCTET_STRING_TYPE.getNode(skipOver(TokenType.OCTET_STRING));
			break;
		case ENUMERATED:	// EnumeratedType ::= ENUMERATED "{" Enumerations "}"
			type = enumeratedType();
			break;
		case BIT_STRING: // bitStringType ::= BIT STRING ['{' nameValueList '}']
			type = bitStringType();
			break;
		case LEFT_BRACKET:	// TagType ::= '[' [TagClass] Number ']' [EXPLICIT | IMPLICIT] AsnType
			type = taggedType();
			break;
		case OBJECT_DESCRIPTOR:
			type = AsnProduction.OBJECT_DESCRIPTOR.getNode(skipOver(TokenType.OBJECT_DESCRIPTOR));
			break;
		case OBJECT_IDENTIFIER:
			type = AsnProduction.OBJECT_IDENTIFIER_TYPE.getNode(skipOver(TokenType.OBJECT_IDENTIFIER));
			break;
		case RELATIVE_OID:
			type = AsnProduction.RELATIVE_OID_TYPE.getNode(skipOver(TokenType.RELATIVE_OID));
			break;
		case EMBEDDED_PDV:
			type = AsnProduction.EMBEDDED_PDV_TYPE.getNode(skipOver(TokenType.EMBEDDED_PDV));
			break;
		case SEQUENCE_OF:
			type = sequenceOfType(paramList);
			break;
		case SEQUENCE:
			type = sequenceType(paramList);
			break;
		case SET_OF:
			type = setOfType();
			break;
		case SET:
			type = setType();
			break;
		case CHOICE:
			type = choiceType();
			break;
		case EXTERNAL:
			skipOver(TokenType.EXTERNAL);
			type = AsnProduction.EXTERNAL_TYPE.getNode();
			break;
		case CHARACTER_STRING:
			type = AsnProduction.CHARACTER_STRING_TYPE.getNode(skipOver(TokenType.CHARACTER_STRING));
			break;
			// RestrictedCharacterStringTypes
		case BMP_STRING:
			type = AsnProduction.BMP_STRING_TYPE.getNode(skipOver(TokenType.BMP_STRING));
			break;
		case GENERAL_STRING:
			type = AsnProduction.GENERAL_STRING_TYPE.getNode(skipOver(TokenType.GENERAL_STRING));
			break;
		case GRAPHIC_STRING:
			type = AsnProduction.GRAPHIC_STRING_TYPE.getNode(skipOver(TokenType.GRAPHIC_STRING));
			break;
		case IA5_STRING:
			type = AsnProduction.IA5_STRING_TYPE.getNode(skipOver(TokenType.IA5_STRING));
			break;
		case ISO646_STRING:
			type = AsnProduction.ISO646_STRING_TYPE.getNode(skipOver(TokenType.ISO646_STRING));
			break;
		case NUMERIC_STRING:
			type = AsnProduction.NUMERIC_STRING_TYPE.getNode(skipOver(TokenType.NUMERIC_STRING));
			break;
		case PRINTABLE_STRING:
			type = AsnProduction.PRINTABLE_STRING_TYPE.getNode(skipOver(TokenType.PRINTABLE_STRING));
			break;
		case TELETEX_STRING:
			type = AsnProduction.TELETEX_STRING_TYPE.getNode(skipOver(TokenType.TELETEX_STRING));
			break;
		case T61_STRING:
			type = AsnProduction.T61_STRING_TYPE.getNode(skipOver(TokenType.T61_STRING));
			break;
		case UNIVERSAL_STRING:
			type = AsnProduction.UNIVERSAL_STRING_TYPE.getNode(skipOver(TokenType.UNIVERSAL_STRING));
			break;
		case UTF8_STRING:
			type = AsnProduction.UTF8_STRING_TYPE.getNode(skipOver(TokenType.UTF8_STRING));
			break;
		case VIDEOTEX_STRING:
			type = AsnProduction.VIDEOTEX_STRING_TYPE.getNode(skipOver(TokenType.VIDEOTEX_STRING));
			break;
		case VISIBLE_STRING:
			type = AsnProduction.VISIBLE_STRING_TYPE.getNode(skipOver(TokenType.VISIBLE_STRING));
			break;
		case GENERALIZED_TIME:
			type = AsnProduction.GENERALIZED_TIME_TYPE.getNode(skipOver(TokenType.GENERALIZED_TIME));
			break;
		case UTC_TIME:
			type = AsnProduction.UTC_TIME_TYPE.getNode(skipOver(TokenType.UTC_TIME));
			break;
		case INSTANCE_OF:
			type = instanceOfType();
			break;
		// case CLASS:
		//	type = objectClassDefn();
		//	break;
		default:
			// Try ObjectClassFieldType()
			type = objectClassFieldType();
			break;
		}
		return type;
	}
	private AsnNode type(AsnNode paramList){
		//Used By:  ParameterizedTypeAssignment ActualParameter NamedType ContentsConstraint ValueAssignment ConstrainedType SelectionType UserDefinedConstraintParameter ExceptionIdentification SequenceOfType ValueSetTypeAssignment ComponentType TaggedType TypeAssignment ContainedSubtype OpenTypeFieldVal TypeOptionalitySpec Setting FixedTypeValueSetFieldSpec Governor FixedTypeValueFieldSpec ParameterizedValueAssignment TypeConstraint TypeWithConstraint ParameterizedValueSetTypeAssignment SetOfType
		// BuiltinType | ReferencedType | ConstrainedType
		trace("type");
		AsnNode type = builtinType(paramList);
		if(type == null) type = referencedType(paramList);
		// ConstrainedType
		if(type != null) {
			while(lookAheadIs(TokenType.LEFT_PAREN)) {
				type.addChild(constraint(type));
			}
		} else {
			error("Not yet handled by type()");
		}
		return type;
	}
	private AsnNode valueSet(AsnNode expectedType){
		//Used By:  UserDefinedConstraintParameter ActualParameter Setting ParameterizedValueSetTypeAssignment ValueSetTypeAssignment ValueSetOptionalitySpec
		// "{", ElementSetSpecs, "}"
		trace("valueSet:"+expectedType);
		AsnNode vs = new AsnNode(AsnProduction.VALUE_SET,expectedType.getNameToken());
		skipOver(TokenType.LEFT_BRACE);
		vs.addChild(elementSetSpecs(expectedType));
		skipOver(TokenType.RIGHT_BRACE);
		return vs;
	}
	private AsnNode valueSetTypeAssignment(){
		//Used By:  Assignment
		// typereference, Type, "::=", ValueSet
		trace("valueSetTypeAssignment");
		AsnNode vsta = new AsnNode(AsnProduction.VALUE_SET_TYPE_ASSIGNMENT,skipOver(TokenType.TYPE_REFERENCE));
		AsnNode type = type(null);
		skipOver(TokenType.ASSIGNMENT);
		vsta.addChild(valueSet(type));
		// TODO: Add this to the known items in the current module
		return vsta;
	}
	private AsnNode valueAssignment(){
		//Used By:  Assignment
		// valuereference, Type, "::=", Value
		trace("valueAssignment");
		AsnNode valueAssignment = AsnProduction.VALUE_ASSIGNMENT.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		AsnNode type = valueAssignment.addChild(type(null));
		skipOver(TokenType.ASSIGNMENT);
		// showDebug(type,"<< ValueAssignment >> ",System.out);
		AsnNode value = value(type);
		valueAssignment.addChild(value);
		currentModule.addValue(valueAssignment.getName(), value);
		return valueAssignment;
	}
	private AsnNode typeAssignment(){
		//Used By:  Assignment
		trace("typeAssignment");
		// typereference, "::=", Type
		AsnNode assignment = AsnProduction.TYPE_ASSIGNMENT.getNode(skipOver(TokenType.TYPE_REFERENCE));
		skipOver(TokenType.ASSIGNMENT);
		AsnNode type = assignment.addChild(type(null));
		currentModule.addType(assignment.getName(), type);
		return assignment;
	}
	private AsnNode absoluteReference(AsnNode parent){
		//Used By: Root
		// "@", ModuleIdentifier, ".", ItemSpec
		// ModuleIdentifier 
		// ItemSpec ::= typereference | ItemSpec, ".", ComponentId
		// componentId ::= identifier | number | "*"
		skipOver(TokenType.AT);
		AsnNode ar = new AsnNode(AsnProduction.ABSOLUTE_REFERENCE);
		ar.addChild(moduleIdentifier());
		skipOver(TokenType.DOT);
		AsnNode itemSpec = new AsnNode(AsnProduction.ITEM_SPEC,skipOver(TokenType.TYPE_REFERENCE));
		while(lookAheadIs(TokenType.DOT)) {
			skipOver(TokenType.DOT);
			if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
				itemSpec.addChild(new AsnNode(AsnProduction.TYPE_REFERENCE,skipOver(TokenType.TYPE_REFERENCE)));
			} else if(lookAheadIs(TokenType.IDENTIFIER_STRING) || lookAheadIs(TokenType.NUMBER_STRING) || lookAheadIs(TokenType.STAR)) {
				itemSpec.addChild(new AsnNode(AsnProduction.COMPONENT_ID,skipOver(lookAhead().type())));
			}
		}
		ar.addChild(itemSpec);
		return ar;
	}
	private AsnNode externalValueReference(AsnNode parent){
		//Used By:  SimpleDefinedValue DefinedValue
		// modulereference, ".", valuereference
		if(lookAheadIs(TokenType.TYPE_REFERENCE) && lookAheadIs(TokenType.DOT,1) && lookAheadIs(TokenType.IDENTIFIER_STRING,2)) {
			Token module = skipOver(TokenType.TYPE_REFERENCE);
			skipOver(TokenType.DOT);
			return AsnProduction.EXTERNAL_VALUE_REFERENCE.getNode(module, skipOver(TokenType.IDENTIFIER_STRING));
		} else {
			error("not implemented");
		}
		return null;
	}
	// Note: this is outside the grammar
	private AsnNode definedValue(AsnProduction expected) {
		// Used By: Imports ObjectIdComponent NamedNumber
		// expected is the expected type of the value
		AsnNode definedValue = null;
		if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			Token t = skipOver( TokenType.TYPE_REFERENCE);
			skipOver(TokenType.DOT);
			definedValue =  AsnProduction.DEFINED_VALUE.getNode(t,skipOver(TokenType.IDENTIFIER_STRING));
		} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			definedValue =  AsnProduction.DEFINED_VALUE.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		}
		AsnNode symbol = symbolTable.getType(currentModule.getFullName()+"."+definedValue.getName());
		if(symbol == null || !symbol.getProduction().equals(expected)) {
			warn("definedValue() expected: "+expected+" found "+definedValue+" defined as "+symbol);
		}
		return definedValue;
	}
	private AsnNode definedValue(){
		//Used By:  NamedBit NumberForm ClassNumber ObjectIdentifierValue ExceptionIdentification RelativeOIDComponents AssignedIdentifier ObjIdComponents NamedNumber CharsDefn ReferencedValue
		// ExternalValueReference | valuereference | ParameterizedValue
		if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			Token t = skipOver( TokenType.TYPE_REFERENCE);
			skipOver(TokenType.DOT);
			return new AsnNode(AsnProduction.DEFINED_VALUE,t,skipOver(TokenType.IDENTIFIER_STRING));
		} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			return new AsnNode(AsnProduction.DEFINED_VALUE,skipOver(TokenType.IDENTIFIER_STRING));
		} else if(lookAhead().isBuiltInType()){
			return new AsnNode(AsnProduction.DEFINED_VALUE,skipOver(lookAhead().type()));
		} else {
			error("Not yet implemened");
		}
		return null;
	}
	private AsnNode nonParameterizedTypeName(AsnNode parent){
		//Used By: Root
		// ExternalTypeReference | typereference
		error("not implemented");
		return null;
	}
	private AsnNode externalTypeReference(){
		//Used By:  SimpleDefinedType NonParameterizedTypeName DefinedType
		// modulereference, ".", typereference
		// TODO: ExternalClassReference, ExternalObjectReference, ExternalObjectSetReference (from X.681)
		AsnNode n = null;
		Token moduleRef = skipOver(TokenType.TYPE_REFERENCE);
		skipOver(TokenType.DOT);
		Token typeRef = skipOver(TokenType.TYPE_REFERENCE);
		n = new AsnNode(AsnProduction.EXTERNAL_TYPE_REFERENCE,moduleRef,typeRef);
		return n;
	}
	private AsnNode definedType(){
		//Used By:  ReferencedType
		// ExternalTypeReference | typereference | ParameterizedType | ParameterizedValueSetType
		trace("DefinedType");
		AsnNode n = null;
		if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			if(lookAheadIs(TokenType.LEFT_BRACE,1)) {	// parameterized type
				n = parameterizedType(null);
			} else if(lookAheadIs(TokenType.DOT,1)){	// external type reference
				n = externalTypeReference();
			} else if(lookAheadIs(TokenType.CLASS_REFERENCE)) {	// Object class reference
				n = new AsnNode(AsnProduction.OBJECT_CLASS_REFERENCE,skipOver(TokenType.CLASS_REFERENCE));
			} else {
				n = new AsnNode(AsnProduction.TYPE_REFERENCE,skipOver(TokenType.TYPE_REFERENCE));	// type reference
			}
		} else if(lookAhead().isBuiltInType()) {
			n = new AsnNode(AsnProduction.BUILTIN_TYPE,skipOver(lookAhead().type()));	// built-in type
		} else {
			warn("defined type returning null");	// TODO: parametrized value set type
		}
		return n;
	}
	private AsnNode assignment(){
		//Used By:  AssignmentList
		// TypeAssignment | ValueAssignment | ValueSetTypeAssignment | ObjectClassAssignment |
		// ObjectAssignment | ObjectSetAssignment | ParameterizedAssignment
		
		// TypeAssignment ::= typeReference "::=" Type
		// ValueAssignment ::= valueReference Type "::=" Value
		// ValueSetTypeAssignment ::= typeReference Type "::=" ValueSet 
		// ValueSet ::= "{" elementSetSpecs "}"
		
		Token ref = lookAhead();
		AsnNode expect = getTypeFor(ref.value());
		trace("assignment:["+ref.value()+" = "+expect+"]");
		if(expect == null) {
			error(ref+": Null symbol type");
		}		
		/*
		 * left assignment values (Dubuisson p 108)
		 * 1st character of 1st the first lexeme | first character of the second lexeme ::= Assignment category
		 * Upper-case-letter  | none	::= type or information object class
		 * lower-case-letter  | Upper-case-letter ::= value or information object
		 * Upper-case-letter  | Upper-case-letter ::= value set or information object set
		 */
		AsnNode assignment = null;
		if(lookAheadIs(TokenType.LEFT_BRACE,1)) {	// xxx "{"
			assignment = parameterizedAssignment();
		} else if(lookAheadIs(TokenType.CLASS_REFERENCE)) { // ClassReference ...
			if(lookAheadIs(TokenType.ASSIGNMENT,1)) {
				// ClassReference ::= ...
				if(lookAheadIs(TokenType.CLASS,2) || lookAheadIs(TokenType.CLASS_REFERENCE,2)) {
					// ClassReference ::= CLASS ...
					assignment = objectClassAssignment();
				} else if(lookAheadIs(TokenType.TYPE_IDENTIFIER,2) || lookAheadIs(TokenType.ABSTRACT_SYNTAX,2)) {
					// ClassReference ::= UsefulObjectClassReference
					assignment = objectClassAssignment();
				} else if(lookAhead(2).isBuiltInType()){
					// TypeReference ::= BuiltInType ...
					assignment = typeAssignment();
				} else if(lookAheadIs(TokenType.LEFT_BRACE,2)) {
					// TypeReference ::= "{" ...
					assignment = valueSetTypeAssignment();
				} else {
					error("not yet implemented");
				}
			} else {
				error("not yet implemented");
			}
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE)) {	// TypeReference ...
			if(lookAheadIs(TokenType.ASSIGNMENT,1)) {	// TypeReference "::=" | TypeReference "::=" "{"...
				assignment = lookAheadIs(TokenType.LEFT_BRACE,2) ? valueSetTypeAssignment() : typeAssignment();
			} else if(lookAheadIs(TokenType.CLASS_REFERENCE,1)) {
				AsnNode definingClass = locate(AsnProduction.OBJECT_CLASS_ASSIGNMENT,lookAhead(1).value());
				assignment = definingClass != null ? objectSetAssignment() : valueSetTypeAssignment();
			} else if(lookAheadIs(TokenType.TYPE_REFERENCE,1) || lookAhead(1).isBuiltInType()) { // TypeReference TypeReference ...
				assignment = valueSetTypeAssignment();
			} else {
				error("not yet implemented");
			}
		} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) { // valueReference ...
			if(lookAheadIs(TokenType.IDENTIFIER_STRING,1) && lookAheadIs(TokenType.LESS_THAN,2)) {
				assignment = valueAssignment();
			} else if(lookAheadIs(TokenType.CLASS_REFERENCE,1)) {	// valueReference ClassReference ...
				assignment = objectAssignment();
			} else if(lookAheadIs(TokenType.TYPE_IDENTIFIER,1) || lookAheadIs(TokenType.ABSTRACT_SYNTAX,1)) {	// valueReference usefulClassReference ...
				assignment = objectAssignment();
			} else if(lookAheadIs(TokenType.TYPE_REFERENCE,1) || lookAhead(1).isBuiltInType()) {	// valueReference TypeReference ...
				assignment = valueAssignment();
			}  else {
				error("not yet implemented");
			}
		} else {
			error("not yet implemented ["+lookAhead()+" , "+lookAhead(1)+"] ");
		}
		if(assignment == null ) {
			error("Null returned for assignment");
		} else {
			if(debug) {
				System.out.print("\n");
				showDebug(assignment,"assignment>",  System.out);
			}
			if(!expect.getProduction().equals(assignment.getChild(0).getProduction())) {
				switch(assignment.getChild(0).getProduction()) {
				case TYPE_REFERENCE:
					AsnNode ind = getTypeFor(assignment.getChild(0).getName());
					// System.out.println(ind);
					if(expect.getProduction().equals(ind.getProduction())) break;
					error(ref+" : assignment expected "+expect+" found "+ind);
				case USEFUL_OBJECT_CLASS_REFERENCE:
				case PARAMETER_LIST:
				case OBJECT_CLASS_REFERENCE:
					ind = assignment.getChild(1);
					if(expect.getProduction().equals(ind.getProduction())) break;
					error(ref+" : assignment expected "+expect+" found "+ind);
				default:
					error(ref+" : assignment expected "+expect+" found "+assignment.getChild(0));
					break;
				}
				
			}
		}
		symbolTable.setDefinition(currentModule.getFullName()+"."+ref.value(), assignment);
		return assignment;	
	}
	private AsnNode assignmentList(){
		//Used By:  ModuleBody AssignmentList
		// Assignment | AssignmentList, Assignment
		if(lookAheadIs(TokenType.END)) return null;
		AsnNode assignmentList = AsnProduction.ASSIGNMENT_LIST.getNode();
		currentAssignments = assignmentList;
		while(!lookAheadIs(TokenType.END)) {
			assignmentList.addChild(assignment());
		}
		return assignmentList;
	}
	private AsnNode reference(AsnNode paramList){
		//Used By:  DummyReference Symbol ParameterizedReference
		// typereference | valuereference | objectclassreference
		// objectreference | objectsetreference

		// Dubuisson page 102/103
		// a moduleReference is the same as a typeReference (17)
		// an objectClassReference is the same as a typeReference, except no lower case letters are allowed (19)
		// and objectReference is the same as a valueReference (21)
		// an objectSetReference is the same as a typeReference (23)
		// an objectFieldreference is '&' followed by an objectReference (20)
		// an objectSetFieldReference is '&' followed by an objectSetReference (22)
		// a typeFieldReference is '&' followed by a typeReference (25)
		// a valueFieldReference is '&' followed by a valueReference (31)
		// a valueSetFieldReference is '&' followed by a type reference (33)
		
		trace("Reference");
		String refName = currentModule.getFullName()+"."+lookAhead().value();
		AsnNode refType = symbolTable.getType(refName);
		AsnNode ref = null;
		if(refType != null) {
				switch(refType.getProduction()) {
				case INTEGER_TYPE:
				case ANY_TYPE:
				case NULL_TYPE:
				case BOOLEAN_TYPE:
				case REAL_TYPE:
				case OCTET_STRING_TYPE:
				case ENUMERATED_TYPE:
				case BIT_STRING_TYPE:
				case OBJECT_DESCRIPTOR:
				case OBJECT_IDENTIFIER_TYPE:
				case RELATIVE_OID_TYPE:
				case EMBEDDED_PDV_TYPE:
				case SEQUENCE_OF_TYPE:
				case SEQUENCE_TYPE:
				case SET_OF_TYPE:
				case SET_TYPE:
				case CHOICE_TYPE:
				case EXTERNAL_TYPE:
				case CHARACTER_STRING_TYPE:
				case BMP_STRING_TYPE:
				case GENERAL_STRING_TYPE:
				case GRAPHIC_STRING_TYPE:
				case IA5_STRING_TYPE:
				case ISO646_STRING_TYPE:
				case NUMERIC_STRING_TYPE:
				case PRINTABLE_STRING_TYPE:
				case TELETEX_STRING_TYPE:
				case T61_STRING_TYPE:
				case UNIVERSAL_STRING_TYPE:
				case UTF8_STRING_TYPE:
				case VIDEOTEX_STRING_TYPE:
				case VISIBLE_STRING_TYPE:
				case GENERALIZED_TIME_TYPE:
				case UTC_TIME_TYPE:
				case INSTANCE_OF_TYPE:
					switch(lookAhead().type()) {
					case TYPE_REFERENCE:
						ref = AsnProduction.TYPE_REFERENCE.getNode(skipOver(TokenType.TYPE_REFERENCE));
						break;
					case IDENTIFIER_STRING:
						ref = AsnProduction.VALUE_REFERENCE.getNode(skipOver(TokenType.IDENTIFIER_STRING));
						break;
					default:
						error(lookAhead().type()+" : Not yet implemented");
						break;
					}
					break;
				case OBJECT_CLASS:
					ref = AsnProduction.OBJECT_CLASS_REFERENCE.getNode(skipOver(TokenType.CLASS_REFERENCE));
					break;
				case OBJECT:
					ref = AsnProduction.OBJECT_REFERENCE.getNode(skipOver(TokenType.IDENTIFIER_STRING));
					break;
				case OBJECT_SET:
					ref = AsnProduction.OBJECT_SET_REFERENCE.getNode(skipOver(TokenType.TYPE_REFERENCE));
					break;
				default:
					error(refType+" : Not yet implemented");
					break;
				}
			if(ref != null) return ref;
		}
		
		
		
		if(lookAheadIs(TokenType.CLASS_REFERENCE)) {
			ref = AsnProduction.OBJECT_CLASS_REFERENCE.getNode(skipOver(TokenType.CLASS_REFERENCE));
		} else if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
			// TODO: can also be an objectSetReference here
			// TODO: can also be a Module reference here
			ref = AsnProduction.TYPE_REFERENCE.getNode(skipOver(TokenType.TYPE_REFERENCE));
		} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
			ref = AsnProduction.VALUE_REFERENCE.getNode(skipOver(TokenType.IDENTIFIER_STRING));
		} else if(lookAheadIs(TokenType.AMPERSAND)) {
			skipOver(TokenType.AMPERSAND);
			if(lookAheadIs(TokenType.TYPE_REFERENCE)) {
				ref = AsnProduction.TYPE_FIELD_REFERENCE.getNode(skipOver(TokenType.TYPE_REFERENCE),true);
			} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
				ref = AsnProduction.VALUE_FIELD_REFERENCE.getNode(skipOver(TokenType.IDENTIFIER_STRING),true);
			}
		} else {
			error("Not yet implemented");
		}
		AsnNode type = symbolTable.getType(currentModule.getFullName()+"."+ref.getName());
		if(type == null || !type.getProduction().equals(ref.getProduction())) {
			warn("reference - Mismatched Productions Expected : "+type + " found : "+ref);
		}
		warn("Symbol Table does not have definition for "+refName+" guessed "+ref+" from context");
		return ref;
	}
	private AsnNode parameterizedReference(){
		//Used By:  Symbol
		// Reference | Reference "{", "}"
		AsnNode ref = reference(null);
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			skipOver(TokenType.LEFT_BRACE);
			skipOver(TokenType.RIGHT_BRACE);
			AsnNode n = AsnProduction.PARAMETERIZED_REFERENCE.getNode();
			n.addChild(ref);
			return n;
		}
		return ref;
	}
	private AsnNode symbolList(){
		//Used By:  SymbolList SymbolsExported SymbolsFromModule imports
		// Symbol | SymbolList "," Symbol
		// Symbol ::= ParametrizedReference | Reference
		AsnNode sl = AsnProduction.SYMBOL_LIST.getNode();
		sl.addChild(parameterizedReference());
		while(lookAheadIs(TokenType.COMMA)) {
			skipOver(TokenType.COMMA);
			sl.addChild(parameterizedReference());
		}
		return sl;
	}
	private AsnNode imports(){
		//Used By:  ModuleBody
		// IMPORTS SymbolsImported ";" | empty
		// SymbolsImported ::= SymbolsFromModuleList | empty
		// SymbolsFromModuleList ::= SymbolsFromModule | SymbolsFromModuleList, SymbolsFromModule
		// SymbolsFromModule ::= SymbolList, FROM, GlobalModuleReference
		// GlobalModuleReference ::= modulereference, AssignedIdentifier
		// AssignedIdentifier ::= ObjectIdentifierValue | DefinedValue | empty
		AsnNode imports = null;
		if(lookAheadIs(TokenType.IMPORTS)) {
			skipOver(TokenType.IMPORTS);
			imports = AsnProduction.IMPORTS.getNode();
			while(!lookAheadIs(TokenType.SEMI_COLON)) {
				AsnNode symbolList = symbolList();
				skipOver(TokenType.FROM);
				AsnNode mref = AsnProduction.GLOBAL_MODULE_REFERENCE.getNode(skipOver(TokenType.TYPE_REFERENCE));
				if(lookAheadIs(TokenType.LEFT_BRACE)) {
					mref.addChild(objectIdentifierValue());
				} else if(lookAheadIs(TokenType.IDENTIFIER_STRING)) {
					mref.addChild(definedValue(AsnProduction.OBJECT_IDENTIFIER_TYPE));
				}
				mref.addChild(symbolList);
				imports.addChild(mref);
			}
			skipOver(TokenType.SEMI_COLON);
		}
		return imports;
	}
	private AsnNode exports(){
		//Used By:  ModuleBody
		// EXPORTS SymbolsExported ";" | EXPORTS ALL ";" | empty
		// SymbolsExported ::= SymbolsList
		// SymbolsList ::= Symbol | Symbol "," SymbolsList
		// Symbol ::= parametrizedReference()
		AsnNode exports =null;
		if(lookAheadIs(TokenType.EXPORTS)) {
			skipOver(TokenType.EXPORTS);
			if(lookAheadIs(TokenType.ALL)) {
				exports = AsnProduction.EXPORTS.getNode(skipOver(TokenType.ALL));
			} else {
				exports = new AsnNode(AsnProduction.EXPORTS);
				if(!lookAheadIs(TokenType.SEMI_COLON)) {
					exports.addChild(parameterizedReference());
					while(lookAheadIs(TokenType.COMMA)) {
						skipOver(TokenType.COMMA);
						exports.addChild(parameterizedReference());
					}
				}
			}
			skipOver(TokenType.SEMI_COLON);
		}
		return exports;
	}
	private AsnNode moduleBody(){
		//Used By:  ModuleDefinition
		// Exports, Imports, AssignmentList | empty
		if(lookAheadIs(TokenType.END)) {
			return null;
		}
		AsnNode body = AsnProduction.MODULE_BODY.getNode();
		body.addChild(exports());
		body.addChild(imports());
		// AssignmentList
		if(!lookAheadIs(TokenType.END)) {
			body.addChild(assignmentList());
		}
		return body;
	}
	private AsnNode extensionDefault(){
		//Used By:  ModuleDefinition
		// EXTENSIBILITY IMPLIED | empty
		AsnNode ed = null;
		if(lookAheadIs(TokenType.EXTENSIBILITY_IMPLIED)) {
			skipOver(TokenType.EXTENSIBILITY_IMPLIED);
			ed = AsnProduction.EXTENSION_DEFAULT.getNode();
		}
		return ed;
	}
	private AsnNode tagDefault(){
		//Used By:  ModuleDefinition
		// EXPLICIT TAGS | IMPLICIT TAGS | AUTOMATIC TAGS | empty
		AsnNode td = null;
		if(lookAheadIs(TokenType.EXPLICIT) || lookAheadIs(TokenType.IMPLICIT) || lookAheadIs(TokenType.AUTOMATIC)) {
			td = AsnProduction.TAG_DEFAULT.getNode(tokenizer.next());
			skipOver(TokenType.TAGS);
		}
		return td;
	}
	private AsnNode definitiveIdentifier(){
		//Used By:  ModuleIdentifier ModuleDefinition
		// "{" DefinitiveObjIdComponentList "}" | empty
		AsnNode oid = null;
		if(lookAheadIs(TokenType.LEFT_BRACE)) {
			skipOver(TokenType.LEFT_BRACE);
			oid = AsnProduction.DEFINITIVE_IDENTIFIER.getNode();
			while(!lookAheadIs(TokenType.RIGHT_BRACE)) {
				// number | identifier | identifier "(" number ")"
				if(lookAheadIs(TokenType.NUMBER_STRING)) {
					oid.addChild(AsnProduction.NUMBER_FORM.getNode(skipOver(TokenType.NUMBER_STRING)));
				} else {
					Token name = skipOver(TokenType.IDENTIFIER_STRING);
					if(!lookAheadIs(TokenType.LEFT_PAREN)) {
						switch(name.value()) {
						case "itu-t":
						case "ccitt":
							oid.addChild(AsnProduction.NAME_AND_NUMBER_FORM.getNode(name, new Token(TokenType.NUMBER_STRING,"0")));
							break;
						case "iso":
							oid.addChild(AsnProduction.NAME_AND_NUMBER_FORM.getNode(name, new Token(TokenType.NUMBER_STRING,"1")));
							break;
						case "joint-iso-itu-t":
						case "joint-iso-ccitt":
							oid.addChild(AsnProduction.NAME_AND_NUMBER_FORM.getNode(name, new Token(TokenType.NUMBER_STRING,"2")));
							break;
						default:
							oid.addChild(AsnProduction.NAME_FORM.getNode(name));
							break;
						}
					} else {	
						skipOver(TokenType.LEFT_PAREN);
						Token number = skipOver(TokenType.NUMBER_STRING);
						skipOver(TokenType.RIGHT_PAREN);
						oid.addChild(AsnProduction.NAME_AND_NUMBER_FORM.getNode(name,number));
					}
				}
			}
			skipOver(TokenType.RIGHT_BRACE);
		}
		return oid;
	}
	private AsnNode moduleIdentifier(){
		//Used By:  ModuleDefinition AbsoluteReference
		// modulereference, DefinitiveIdentifier
		AsnNode moduleIdentifier = AsnProduction.MODULE_IDENTIFIER.getNode(skipOver(TokenType.TYPE_REFERENCE));
		moduleIdentifier.addChild(definitiveIdentifier());
		return moduleIdentifier;
	}
	private AsnNode moduleDefinition(){
		//Used By: Root
		// ModuleIdentifier, DEFINITIONS, TagDefault, ExtensionDefault, "::=", BEGIN, ModuleBody, END
		AsnModule module = (AsnModule) AsnProduction.MODULE_DEFINITION.getNode(skipOver(TokenType.TYPE_REFERENCE));
		currentModule = module;
		module.addChild(definitiveIdentifier());
		// DEFINITIONS
		skipOver(TokenType.DEFINITIONS);
		module.addChild(tagDefault());
		module.addChild(extensionDefault());
		// "::=" "BEGIN" [ModuleBody] "END"
		skipOver(TokenType.ASSIGNMENT);
		skipOver(TokenType.BEGIN);
		module.addChild(moduleBody());
		skipOver(TokenType.END);
		showDebug(module,"",System.out);
		if(debug) System.out.println("\n");
		return module;
	}
	
	/*
	 * **********************************
	 * Helper Methods and debugging
	 * **********************************
	 */

	/**
	 * Create a tree representation of all nodes in the query parse tree
	 * @param n - current node to be represented
	 * @param indent - current level of indent
	 * @param f - printstream to write output
	 */
	public static void showTree(AsnNode n, String indent,PrintStream f){
		f.print(indent);
		if(!indent.isEmpty()) {
			f.print("-- ");
		}
		f.print(n.toString());
		for(AsnNode c : n.getChildren()){
			f.print("\n");
			if(c == null) {
				f.print("|-- Null");
			} else {
				showTree(c,indent+"  |",f);
			}
		}
		f.flush();
		return;
	}

	private AsnNode locate(AsnProduction production,String name) {
		return currentAssignments.locate(production,name);
	}
	
	private AsnNode getTypeFor(String name) {
		return symbolTable.getType(currentModule.getFullName()+"."+name);
	}

	private void trace(String method) {
		if(debug) {
			System.out.println("<"+method+"> "+lookAhead()+" "+lookAhead(1)+" "+lookAhead(2)+" "+lookAhead(3)+" "+lookAhead(4));
			System.out.flush();
		}
		return;
	}

	private void showDebug(AsnNode n, String ident, PrintStream f) {
		if(debug) {
			showTree(n,ident,f);
			f.print("\n\n");
			f.flush();
		}
	}

	/**
	 * @param args input arguments
	 */
	public static void main(String[] args) {
		AsnParser parser = new AsnParser();
		if(testCase != null) {
			String s = testCase.test;
			AsnNode r = parser.parse(s);
			showTree(r,"",System.out);
			System.out.println("\n");
			LinkedHashMap<String,Symbol> t = parser.symbolTable.getSymbolTable();
			for(Entry<String, Symbol> e : t.entrySet()) {
				System.out.println(e.getKey()+" : "+e.getValue());
			}
		} else {
			try {
				File dir = new File("resources/rfc/");
				PrintStream f = new PrintStream(new File("temp.txt"));
				// for(String file : dir.list()) {
				for(String file : new String[] {"rfc5280.asn"}) {
					if(!file.endsWith(".asn")) continue;
					if(file.equalsIgnoreCase("ASN1-CHARACTER-MODULE.asn")) continue;
					// AstNode r = parser.parse(new File("resources/rfc/ASN1-CHARACTER-MODULE.asn"));
					System.out.println("-- File "+file+" --");
					System.out.flush();
					AsnNode r = parser.parse(new File("resources/rfc/"+file));
					// PrintStream f = System.out;
					r.reduce(r,f);
					for(AsnNode n : r.getChildren()) {
						AsnModule m = (AsnModule)n;
						f.println("-------- "+m.getFullName()+" ------------");
						for(String v : m.getTypes().keySet()) {
							f.println(v+" "+m.getType(v));
						}
						f.println("-------- VALUES ------------");
						for(String v : m.getValues().keySet()) {
							AsnNode val = m.getValue(v);
							f.println(v+" "+m.getValue(v));
							if(val.getAsnValue() != null && val.getAsnValue().getTag() == Tag.OBJECT_IDENTIFIER) {
								OidValue value = (OidValue) val.getAsnValue();
								long [] values = value.getValue();
								StringBuilder b = new StringBuilder();
								for(int i = 0; i < v.length(); i++) {
									char c = v.charAt(i);
									if(Character.isUpperCase(c) && !Character.isUpperCase(v.charAt(i-1))) {
										b.append("_").append(c);
									} else if(c == '-') {
										b.append("_");
									} else {
										b.append(Character.toUpperCase(c));
									}
								}
								System.out.print("\t"+b.toString()+"(\""+v+"\",new OidValue(new long[]{");
								// System.out.print("\tknownOids.put(\""+v+"\",new OidValue(new long[]{");
								for(int i = 0; i < values.length; i++) {
									System.out.print(values[i]+(i < values.length-1 ? "," : "}),null),"));
								}
								System.out.print("\n");
							}
						}
						showTree(m,"",f);
						f.print("\n");
						break;
					}
				}
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
	}

	static class TestCase {
		String test;
		int nodes;
		public TestCase(int nodes, String test) {
			this.nodes = nodes;
			this.test = test;
			return;
		}
	}
	
	private static TestCase nullTest = null;
	private static TestCase template = new TestCase(0,"Module DEFINITIONS ::= BEGIN\r\n" +
			"END\r\n");
	
	private static TestCase test01 = new TestCase(0,"Module DEFINITIONS ::= BEGIN\r\n" +
			"EXTENSION ::= CLASS {\r\n" + 
			"	&id OBJECT IDENTIFIER UNIQUE,\r\n" + 
			"	&ExtnType,\r\n" + 
			"	&Critical BOOLEAN DEFAULT {TRUE | FALSE }\r\n" + 
			"} WITH SYNTAX {\r\n" + 
			"	SYNTAX &ExtnType IDENTIFIED BY &id\r\n" + 
			"	[CRITICALITY &Critical]\r\n" + 
			"}\r\n"+
			"Extension{EXTENSION:ExtensionSet} ::= SEQUENCE {\r\n" + 
			"	extnID EXTENSION.&id({ExtensionSet}),\r\n" + 
			"	critical BOOLEAN\r\n" + 
			"-- (EXTENSION.&Critical({ExtensionSet}{@extnID}))\r\n" + 
			"	DEFAULT FALSE,\r\n" + 
			"	extnValue OCTET STRING (CONTAINING\r\n" + 
			"	EXTENSION.&ExtnType({ExtensionSet}{@extnID}))\r\n" + 
			"-- contains the DER encoding of the ASN.1 value\r\n" + 
			"-- corresponding to the extension type identified\r\n" + 
			"-- by extnID\r\n" + 
			"Extensions{EXTENSION:ExtensionSet} ::=\r\n" + 
			"	SEQUENCE SIZE (1..MAX) OF Extension{{ExtensionSet}}\r\n"+
			"}\r\n"+
			"END");
	
	private static TestCase test03 = new TestCase(0,"Module DEFINITIONS ::= BEGIN\r\n" +
			"END\r\n");

	private static TestCase testCase = nullTest;
	

}
