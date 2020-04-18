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
 * Created April 8, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.LinkedHashMap;
import java.util.Map.Entry;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import net.aifusion.metamodel.ModelException;

/**
 * Class to test the AST parser
 * @author Sharad Singhal
 */
public class AsnParserTest {
	static class TestCase {
		int sequence;
		int nodes;
		String test;
		public TestCase(int sequence, int nodes, String test) {
			this.sequence = sequence;
			this.nodes = nodes;
			this.test = test;
			return;
		}
		@Override
		public String toString() {
			StringBuilder b = new StringBuilder("[");
			b.append(sequence).append("] : ").append(nodes).append(" ").append(test);
			return b.toString();
		}
		
	}
	static TestCase [] tests = {
			new TestCase(0,7, "EXPORTS At, B, c;"),	// Exports
			new TestCase(1,16, "IMPORTS x{} FROM Foo {1 2 6 } Xt, y FROM Bar;"),	// Imports
			// built in types
			new TestCase(2,6, "Xt ::= BOOLEAN"),	// Boolean Type Assignment
			new TestCase(3,6, "Xt ::= INTEGER"),	// Integer Type Assignment
			new TestCase(4,13, "Xt ::= INTEGER { a(3), b(-5), c(z) }"),	// Integer ValueSetTypeAssignment
			new TestCase(5,16, "X ::= INTEGER { a(3), b(-5), c(z) } z INTEGER ::= 4"),	// Integer ValueSetTypeAssignment
			new TestCase(6,11, "Xt ::= ENUMERATED { a, b, c }"),		// Enumerations
			new TestCase(7,15, "Xt ::= ENUMERATED { a, b(25), ...,  c(3) }"),	// Enumerations
			new TestCase(8,12, "Xt ::= ENUMERATED { a, b(25), ... }"),	// Enumerations
			new TestCase(9,14, "Xt ::= ENUMERATED { a, b(25), ... !a }"),	// Enumerations
			new TestCase(10,17, "Xt ::= ENUMERATED { a, b(25), ... !a , c(3) }"),	// Enumerations
			new TestCase(11,6, "Xt ::= REAL"),	// REAL Type Assignment
			new TestCase(12,6, "Xt ::= BIT STRING"),	// BitString type
			new TestCase(13,16, "Xt ::= BIT STRING { a(3), b(5), c(z) } z INTEGER ::= 4"),	// named bitString type
			new TestCase(14,6, "Xt ::= OCTET STRING"),	// OctetString Type
			new TestCase(15,6, "Xt ::= NULL"),	// NULL type
			new TestCase(16,6, "Xt ::= SEQUENCE { }"),	// Sequence
			new TestCase(17,8, "Xt ::= SEQUENCE { ... }"),	// ExtensionAndException
			new TestCase(18,11, "Xt ::= SEQUENCE { ... !a , ... }"), // ExtensionAndException OptionalExtensionMarker
			new TestCase(19,12, "Xt ::= SEQUENCE { a INTEGER, b NULL }"),	// RootComponentTypeList
			new TestCase(20,11, "Xt ::= SEQUENCE { a INTEGER OPTIONAL }"),	// NamedType OPTIONAL
			new TestCase(21,12, "Xt ::= SEQUENCE { a INTEGER DEFAULT 4 }"),	// NamedType DEFAULT Value
			new TestCase(22,15, "Xt ::= SEQUENCE { a INTEGER DEFAULT z } z INTEGER ::= 4"),	// NamedType DEFAULT Value
			new TestCase(23,10, "Xt ::= SEQUENCE { COMPONENTS OF Bt }"),	// COMPONENTS OF Type
			new TestCase(24,11, "Xt ::= SEQUENCE { a INTEGER, ... }"), // ComponentTypeList ExtensionAndException 
			new TestCase(25,15, "Xt ::= SEQUENCE { a INTEGER, ... , b NULL }"),	// ComponentTypeList ExtensionAndException ExtensionAdditions 
			new TestCase(26,16, "Xt ::= SEQUENCE { a INTEGER, ... , b NULL, ... }"),	// ComponentTypeList ExtensionAndException ExtensionAdditions OptionalExtensionMarker 
			new TestCase(27,19, "Xt ::= SEQUENCE { a INTEGER, ..., b NULL , ..., c INTEGER}"), // ComponentTypeList ExtensionAndException ExtensionAdditions, ExtensionEndMarker, RootComponentTypeList
			new TestCase(28,16, "Xt ::= SEQUENCE { ..., b NULL , ..., c INTEGER}"), // ExtensionAndException ExtensionAdditions, ExtensionEndMarker, RootComponentTypeList
			new TestCase(29,13, "Xt ::= SEQUENCE { ..., b NULL , ...}"), // ExtensionAndException ExtensionAdditions, OptionalExtensionMarker
			new TestCase(30,17, "Xt ::= SEQUENCE { a INTEGER, ..., [[2: b NULL]] }"),	// ComponentTypeList ExtensionAndException ExtensionAdditionGroup 
			new TestCase(31,7, "Xt ::= SEQUENCE OF INTEGER"),	// Sequence of Type
			new TestCase(32,8, "Xt ::= SEQUENCE OF a INTEGER"),	// Sequence of NamedType
			new TestCase(33,14, "Xt ::= SEQUENCE SIZE (1..4) OF a INTEGER"),	// Sequence of NamedType
			new TestCase(34,16, "Xt ::= SEQUENCE (SIZE (1..4)) OF a INTEGER"),	// Sequence of NamedType
			new TestCase(35,7, "Xt ::= SEQUENCE OF Kv"),	// Sequence of
			new TestCase(36,13, "Xt ::= SEQUENCE SIZE (1..4) OF Kv"),	// Sequence of
			new TestCase(37,6, "Xt ::= SET { }"),	// Set
			new TestCase(38,8, "Xt ::= SET { ... }"),	// ExtensionAndException
			new TestCase(39,11, "Xt ::= SET { ... !a , ... }"), // ExtensionAndException OptionalExtensionMarker
			new TestCase(40,12, "Xt ::= SET { a INTEGER, b NULL }"),	// RootComponentTypeList
			new TestCase(41,11, "Xt ::= SET { a INTEGER OPTIONAL }"),	// NamedType OPTIONAL
			new TestCase(42,12, "Xt ::= SET { a INTEGER DEFAULT 4 }"),	// NamedType DEFAULT Value
			new TestCase(43,15, "Xt ::= SET { a INTEGER DEFAULT z } z INTEGER ::= 4"),	// NamedType DEFAULT Value
			new TestCase(44,10, "Xt ::= SET { COMPONENTS OF Bt }"),	// COMPONENTS OF Type
			new TestCase(45,11, "Xt ::= SET { a INTEGER, ... }"), // ComponentTypeList ExtensionAndException 
			new TestCase(46,15, "Xt ::= SET { a INTEGER, ... , b NULL }"),	// ComponentTypeList ExtensionAndException ExtensionAdditions 
			new TestCase(47,16, "Xt ::= SET { a INTEGER, ... , b NULL, ... }"),	// ComponentTypeList ExtensionAndException ExtensionAdditions OptionalExtensionMarker 
			new TestCase(48,19, "Xt ::= SET { a INTEGER, ..., b NULL , ..., c INTEGER}"), // ComponentTypeList ExtensionAndException ExtensionAdditions, ExtensionEndMarker, RootComponentTypeList
			new TestCase(49,16, "Xt ::= SET { ..., b NULL , ..., c INTEGER}"), // ExtensionAndException ExtensionAdditions, ExtensionEndMarker, RootComponentTypeList
			new TestCase(50,13, "Xt ::= SET { ..., b NULL , ...}"), // ExtensionAndException ExtensionAdditions, OptionalExtensionMarker
			new TestCase(51,17, "Xt ::= SET { a INTEGER, ..., [[2: b NULL]] }"),	// ComponentTypeList ExtensionAndException ExtensionAdditionGroup 
			new TestCase(52,7, "Xt ::= SET OF INTEGER"),	// Set of Type
			new TestCase(53,8, "Xt ::= SET OF a INTEGER"),	// set of NamedType
			new TestCase(54,14, "Xt ::= SET SIZE (1..4) OF a INTEGER"),	// set of NamedType
			new TestCase(55,16, "Xt ::= SET (SIZE (1..4)) OF a INTEGER"),	// set of NamedType
			new TestCase(56,7, "Xt ::= SET OF Kv"),	// Set of
			new TestCase(57,13, "Xt ::= SET SIZE (1..4) OF Kv"),	// set of
			new TestCase(58,12, "Xt ::= CHOICE { a INTEGER, b REAL}"),	// CHOICE RootAlternativeTypeList
			new TestCase(59,13, "Xt ::= CHOICE { a INTEGER, b REAL, ...}"),	// CHOICE RootAlternativeTypeList ExtensionAndException
			new TestCase(60,12, "Xt ::= CHOICE { a INTEGER, ..., ...}"),	// CHOICE RootAlternativeTypeList ExtensionAndException OptionalExtensionMarker
			new TestCase(61,15, "Xt ::= CHOICE { a INTEGER, ..., b REAL, ...}"),	// CHOICE RootAlternativeTypeList ExtensionAndException OptionalExtensionMarker
			new TestCase(62,23, "Xt ::= CHOICE { a INTEGER, ..., [[2: b REAL, d INTEGER]], [[3: c REAL]] ,...}"),	// CHOICE RootAlternativeTypeList ExtensionAndException OptionalExtensionMarker
			new TestCase(63,6, "Xt ::= a < Choice"),	// selection type
			new TestCase(64,9, "Xt ::= [UNIVERSAL 4] INTEGER"),	// Tagged type 
			new TestCase(65,9, "Xt ::= [APPLICATION 4] IMPLICIT INTEGER"),	// Tagged type 
			new TestCase(66,9, "Xt ::= [4] EXPLICIT INTEGER"),	// Tagged type
			new TestCase(67,9, "Xt ::= [4] EXPLICIT INTEGER"),	// Tagged type
			new TestCase(68,6, "Xt ::= OBJECT IDENTIFIER"),	// Object Identifier
			new TestCase(69,6, "Xt ::= RELATIVE-OID"),	// Relative OID
			new TestCase(70,6, "Xt ::= EMBEDDED PDV"),	// Embedded PDV
			new TestCase(71,6, "Xt ::= EXTERNAL"),	// External
			new TestCase(72,6, "Xt ::= CHARACTER STRING"),	// Character String
			new TestCase(73,6, "Xt ::= BMPString"),	// Restricted Character String Types
			new TestCase(74,6, "Xt ::= GeneralString"),
			new TestCase(75,6, "Xt ::= GraphicString"),
			new TestCase(76,6, "Xt ::= IA5String"),
			new TestCase(77,6, "Xt ::= ISO646String"),
			new TestCase(78,6, "Xt ::= NumericString"),
			new TestCase(79,6, "Xt ::= PrintableString"),
			new TestCase(80,6, "Xt ::= TeletexString"),
			new TestCase(81,6, "Xt ::= T61String"),
			new TestCase(82,6, "Xt ::= UniversalString"),
			new TestCase(83,6, "Xt ::= UTF8String"),
			new TestCase(84,6, "Xt ::= VideotexString"),
			new TestCase(85,6, "Xt ::= VisibleString"),
			new TestCase(86,6, "Xt ::= GeneralizedTime"),
			new TestCase(87,6, "Xt ::= UTCTime"),
			new TestCase(88,6, "Xt ::= ObjectDescriptor"),	// Object Descriptor
			// Constraints
			new TestCase(89,9, "Xt ::= INTEGER (4)"),	// Single Value
			new TestCase(90,11, "Xt ::= INTEGER (ALL EXCEPT 4)"),	// ALL Exclusions
			new TestCase(91,13, "Xt ::= INTEGER (ALL EXCEPT 4..10)"),	// ALL Exclusions	
			new TestCase(92,17, "Xt ::= INTEGER (4 UNION 6 INTERSECTION 3 | 5 ^ 6)"),	// UNIONS/INTERSECTIONS
			new TestCase(93,10, "Xt ::= INTEGER (INCLUDES Fv)"),	// ContainedSubType
			new TestCase(94,10, "Xt ::= INTEGER (Fv)"),	// ContainedSubType
			new TestCase(95,11, "Xt ::= INTEGER (4..5)"),	// ValueRange
			new TestCase(96,13, "Xt ::= INTEGER (4<..<5)"),	// ValueRange
			new TestCase(97,15, "Xt ::= SEQUENCE OF VisibleString (SIZE(1..64))"),		// SIZE constraint
			new TestCase(98,15, "Xt ::= SEQUENCE OF VisibleString (SIZE(MIN..MAX))"),	// SIZE Constraint
			new TestCase(99,17, "Xt ::= SEQUENCE OF VisibleString (SIZE(1<..<64))"),	// SIZE Constraint
			new TestCase(100,13, "Xt ::= CHARACTER STRING (FROM (Latin))"),	// Permitted Alphabet
			new TestCase(101,14, "Xt ::= a < Tu (WITH COMPONENTS {..., a ABSENT})"),	// Character String	// inner subtyping
			new TestCase(102,9, "Xt ::= UniversalString (PATTERN \"regexp\")"),	// Pattern constraint
			new TestCase(103,13, "Xt ::= INTEGER (0..10, ..., 12)"),	// Extension
			new TestCase(104,10, "Bt INTEGER ::= { 1..4 }"),	// value set type assignment
			// TODO: check BIT_STRING (CONTAINING xxx)
			// values
			new TestCase(105,7, "bt BOOLEAN ::= TRUE"),
			new TestCase(106,7, "bf BOOLEAN ::= FALSE"),
			new TestCase(107,7, "a INTEGER ::= 1"),
			new TestCase(108,7, "a INTEGER ::= b"),
			new TestCase(109,25, "a INTEGER ::= 1 Ta ::= INTEGER { a(2) } Tb ::= INTEGER { a(3), b(a) } c Tb ::= b d Tb ::= a "),
			new TestCase(110,13, "Xt ::= ENUMERATED {a, b} c Xt ::= a"),
			new TestCase(111,11, "c ENUMERATED {a, b} ::= a"),
			new TestCase(112,7, "a REAL ::= PLUS-INFINITY"),
			new TestCase(113,7, "a REAL ::= MINUS-INFINITY"),
			new TestCase(114,7, "a REAL ::= 0.59"),
			new TestCase(115,8, "a REAL ::= -0.59"),
			new TestCase(116,10, "a REAL ::= { mantissa 3157 base 10 exponent 20}"),
			new TestCase(117,20, "a REAL (WITH COMPONENTS { ..., base (10)})  ::= { mantissa 3157 base 10 exponent 20}"),
			new TestCase(118,7, "a  BIT STRING ::= '1010'B"),
			new TestCase(119,7, "a  BIT STRING ::= '898A'H"),
			new TestCase(120,14, "Xt ::= BIT STRING {a(2)} c Xt ::= { a }"),
			new TestCase(121,12, "a INTEGER ::= 1 c BIT STRING ::= { a }"),
			new TestCase(122,8, "ix BIT STRING ::= { }"),
			new TestCase(123,8, "ix BIT STRING ::= CONTAINING 3"),
			new TestCase(124,7, "a OCTET STRING ::= '898A'H"),
			new TestCase(125,7, "a OCTET STRING ::= '1010'B"),
			new TestCase(126,8, "a OCTET STRING ::= CONTAINING 3"),
			new TestCase(127,9, "Xt ::= OCTET STRING c Xt ::= '898B'H"),
			new TestCase(128,12, "Xt ::= NULL a Xt ::= NULL b NULL ::= NULL"),
			new TestCase(129,20, "Xt ::= SEQUENCE { a INTEGER , b INTEGER } z Xt ::= { a 3, b 4 }"),
			new TestCase(130,13, "Xt ::= SEQUENCE OF INTEGER z Xt ::= { 3, 4 }"),
			new TestCase(131,29, "Xt ::= SEQUENCE { a INTEGER, b INTEGER } x SEQUENCE OF Xt ::= { { a 3, b 4 }, { a 6, b 7} }"),
			new TestCase(132,20, "Xt ::= SET { a INTEGER , b INTEGER } z Xt ::= { a 3, b 4 }"),
			new TestCase(133,13, "Xt ::= SET OF INTEGER z Xt ::= { 3, 4 }"),
			new TestCase(134,29, "Xt ::= SET { a INTEGER, b INTEGER } x SET OF Xt ::= { { a 3, b 4 }, { a 6, b 7} }"),
			new TestCase(135,14, "z CHOICE { a INTEGER, b REAL} ::= a:3"),	// CHOICE
			new TestCase(136,16, "Xt ::= CHOICE { a INTEGER, b REAL} z Xt ::= a:3"),	// CHOICE
			// TODO: What's the value use case for selection type?
			new TestCase(137,12, "Xt ::= [4] EXPLICIT INTEGER z Xt ::= 4"),	// Tagged type
			new TestCase(138,12, "Xt ::= OBJECT IDENTIFIER z Xt ::= { 3 1 8}"),
			new TestCase(139,11, "a OBJECT IDENTIFIER ::= { iso m(2) 3 z}"),
			// TODO: EMBEDDED PDV value
			// TODO: EXTERNAL value
			new TestCase(140,14, "BasicLatin ::= BMPString(FROM (space..tilde))"),
			new TestCase(141,48,"FUNCTION ::= CLASS {\r\n" + 
					"&code INTEGER (0..MAX) UNIQUE,\r\n" + 
					"&Alphabet BMPString\r\n" + 
					"DEFAULT {Latin1 INTERSECTION Level1},\r\n" + 
					"&ArgumentType ,\r\n" + 
					"&SupportedArguments &ArgumentType OPTIONAL,\r\n" + 
					"&ResultType DEFAULT NULL,\r\n" + 
					"&result-if-error &ResultType DEFAULT NULL,\r\n" + 
					"&associated-function OTHER-FUNCTION OPTIONAL,\r\n" + 
					"&Errors ERROR DEFAULT\r\n" + 
					"{rejected-argument | memory-fault} }"),			
			new TestCase(142,87, "MATCHING-RULE ::= CLASS {\r\n" + 
					"&AssertionType OPTIONAL,\r\n" + 
					"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
					"WITH SYNTAX {\r\n" + 
					"[SYNTAX &AssertionType]\r\n" + 
					"ID &id }\r\n" +
					"MatchingRules MATCHING-RULE ::= {\r\n" + 
					"caseIgnoreMatch | booleanMatch | integerMatch }\r\n" +
					"caseIgnoreMatch MATCHING-RULE ::= {\r\n" + 
					"SYNTAX DirectoryString\r\n" + 
					"ID {id-mr 2} }\r\n" + 
					"id-mr OBJECT IDENTIFIER ::=\r\n" + 
					"{ joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
					"booleanMatch MATCHING-RULE ::= {\r\n" + 
					"SYNTAX BOOLEAN\r\n" + 
					"ID {id-mr 13} }\r\n" + 
					"-- id-mr OBJECT IDENTIFIER ::=\r\n" + 
					"-- { joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
					"integerMatch MATCHING-RULE ::= {\r\n" + 
					"SYNTAX INTEGER\r\n" + 
					"ID {id-mr 14} }\r\n" + 
					"-- id-mr OBJECT IDENTIFIER ::=\r\n" + 
					"-- { joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
					"LessMatchingRules MATCHING-RULE ::= {\r\n" + 
					"MatchingRules EXCEPT caseIgnoreMatch }\r\n" +
					"ExtensibleMatchingRules MATCHING-RULE ::= {\r\n" + 
					"caseIgnoreMatch | booleanMatch | integerMatch, ... }\r\n"),
			
			
			new TestCase(143,144,"ATTRIBUTE ::= CLASS {\r\n" + 
					"&derivation ATTRIBUTE OPTIONAL,\r\n" + 
					"&Type OPTIONAL,\r\n" + 
					"&equality-match MATCHING-RULE OPTIONAL,\r\n" + 
					"&ordering-match MATCHING-RULE OPTIONAL,\r\n" + 
					"&substrings-match MATCHING-RULE OPTIONAL,\r\n" + 
					"&single-valued BOOLEAN DEFAULT FALSE,\r\n" + 
					"&collective BOOLEAN DEFAULT FALSE,\r\n" + 
					"&no-user-modification BOOLEAN DEFAULT FALSE,\r\n" + 
					"&usage Attribute-Usage\r\n" + 
					"DEFAULT userApplications,\r\n" + 
					"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
					"WITH SYNTAX {\r\n" + 
					"[SUBTYPE OF &derivation]\r\n" + 
					"[WITH SYNTAX &Type]\r\n" + 
					"[EQUALITY MATCHING RULE &equality-match]\r\n" + 
					"[ORDERING MATCHING RULE &ordering-match]\r\n" + 
					"[SUBSTRINGS MATCHING RULE &substrings-match]\r\n"+
					"[SINGLE VALUE &single-valued]\r\n" + 
					"[COLLECTIVE &collective]\r\n" + 
					"[NO USER MODIFICATION &no-user-modification]\r\n" + 
					"[USAGE &usage]\r\n" + 
					"ID &id }\r\n" + 
					"AttributeUsage ::= ENUMERATED { userApplications(0),\r\n" + 
					"directoryOperation(1), distributedOperation(2),\r\n" + 
					"dSAOperation(3) }"	
					+ "surname ATTRIBUTE ::= { -- family name\r\n" + 
					"SUBTYPE OF name\r\n" + 
					"WITH SYNTAX DirectoryString\r\n" + 
					"ID id-at-surname }\r\n" + 
					"givenName ATTRIBUTE ::= { -- first name\r\n" + 
					"SUBTYPE OF name\r\n" + 
					"WITH SYNTAX DirectoryString\r\n" + 
					"ID id-at-givenName }\r\n" + 
					"countryName ATTRIBUTE ::= { -- country\r\n" + 
					"SUBTYPE OF name\r\n" + 
					"WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes\r\n" + 
					"SINGLE VALUE TRUE\r\n" + 
					"ID id-at-countryName}"),
			new TestCase(144,18,"CLASS1 ::= TYPE-IDENTIFIER\r\n"),	// useful types
			new TestCase(145,28,"class1 TYPE-IDENTIFIER ::= {BIT STRING IDENTIFIED BY {mhsBody 3}}\r\n"),
			new TestCase(146,30,"CLASS1 ::= TYPE-IDENTIFIER\r\n" +
					"class1 CLASS1 ::= {BIT STRING IDENTIFIED BY {mhsBody 3}}\r\n"),
			new TestCase(147,33,"CLASS1 ::= CLASS { &obj CLASS2 }\r\n" + 
					"CLASS2 ::= CLASS { &val INTEGER }\r\n" + 
					"object1 CLASS1 ::= { &obj object2 }\r\n" + 
					"object2 CLASS2 ::= { &val 5 }\r\n" + 
					"value INTEGER ::= object1.&obj.&val\r\n"),
			new TestCase(148,21,"CLASS1 ::= TYPE-IDENTIFIER\r\n"+
					"Foo ::= INSTANCE OF CLASS1\r\n"),
			
			/*			
			// TODO: Instance OF type, ObjectClassFieldType
			// new TestCase(0,"CT ::= TYPE-IDENTIFIER"),
			*/
	};
	
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}

	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.asn1.parser.AsnParser#parse(java.lang.String)}.
	 */
	@Test
	public void testHeaders() {
		TestCase [] tests0 = {
				new TestCase(0,2, "Tm DEFINITIONS ::= BEGIN END"),	// Empty module
				new TestCase(1,6, "Tm {iso 3 something(4)} DEFINITIONS ::= BEGIN END"), // OID
				new TestCase(2,3, "Tm DEFINITIONS EXPLICIT TAGS ::= BEGIN END"), // TAGS
				new TestCase(3,4, "Tm DEFINITIONS AUTOMATIC TAGS EXTENSIBILITY IMPLIED ::= BEGIN END"), // Extensibility
		};
		// run tests for initial module definitions
		for(TestCase s : tests0) {
			AsnParser parser = new AsnParser();
			try {
				System.out.println(s);
				AsnNode r = parser.parse(s.test);
				showTree(r,"");
				System.out.println("\n");
				System.out.flush();
				assertEquals(s.nodes,countNodes(r));
			} catch (ModelException e) {
				e.printStackTrace();
				fail();
			}
		}
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.asn1.parser.AsnParser#parse(java.lang.String)}.
	 */
	@Test
	public void testParseString() {
		for(TestCase t : tests) {
			AsnParser parser = new AsnParser();
			String s = "Tm DEFINITIONS ::= BEGIN "+t.test+" END";
			try {
				System.out.println(t);
				AsnNode r = parser.parse(s);
				LinkedHashMap<String,Symbol> symbols = parser.getSymbolTable().getSymbolTable();
				for(Entry<String, Symbol> e : symbols.entrySet()) {
					System.out.println(e.getKey()+" "+e.getValue());
				}
				showTree(r,"");
				System.out.println("\n");
				System.out.flush();
				assertEquals(t.nodes,countNodes(r));
			} catch (ModelException e) {
				e.printStackTrace();
				fail("Test "+t.sequence);
			}
		}
		return;
	}

	@Ignore
	@Test
	public void testCases() {
		// remaining tests
		for(TestCase t : testCase) {
			// System.out.println(t);
			AsnParser parser = new AsnParser();
			try {
				AsnNode r = parser.parse(t.test);
				// showTree(r,"");
				// System.out.println("\n");
				// System.out.flush();
				assertEquals(t.nodes,countNodes(r));
			} catch (ModelException e) {
				System.out.println(t);
				System.out.flush();
				e.printStackTrace();
				fail("Test "+t.sequence);
			}
		}
		return;
	}

	private int countNodes(AsnNode r) {
		int nodes = 1;
		for(AsnNode child : r.getChildren()) {
			nodes += countNodes(child);
		}
		return nodes;
	}

	/**
	 * Create a tree representation of all nodes in the query parse tree
	 * @param n - current node to be represented
	 * @param indent - current level of indent
	 * @return - string containing subtree
	 */
	private static void showTree(AsnNode n, String indent){
		System.out.print(indent);
		if(!indent.isEmpty()) System.out.print("-- ");
		System.out.print(n.toString());
		if(n.hasChildren()){
			for(AsnNode c : n.getChildren()){
				System.out.print("\n");
				if(c == null) {
					System.out.print("|-- Null");
				} else {
					showTree(c,indent+"  |");
				}
			}
		}
		return;
	}
	private static TestCase testCase[] = {
			new TestCase(0,211, "Module-order DEFINITIONS AUTOMATIC TAGS ::=\r\n" + 
					"BEGIN\r\n" + 
					"Order ::= SEQUENCE {\r\n" + 
					"header Order-header,\r\n" + 
					"items SEQUENCE OF Order-line}\r\n" +  
					"Order-header ::= SEQUENCE {\r\n" + 
					"number Order-number,\r\n" + 
					"date Date,\r\n" + 
					"client Client,\r\n" + 
					"payment Payment-method }\r\n" + 
					"Order-number ::= NumericString (SIZE (12))\r\n" + 
					"Date ::= NumericString (SIZE (8)) -- MMDDYYYY\r\n" + 
					"Client ::= SEQUENCE {\r\n" + 
					"name PrintableString (SIZE (1..20)),\r\n" + 
					"street PrintableString (SIZE (1..50)) OPTIONAL,\r\n" + 
					"postcode NumericString (SIZE (5)),\r\n" + 
					"town PrintableString (SIZE (1..30)),\r\n" + 
					"country PrintableString (SIZE (1..20))\r\n" + 
					"DEFAULT default-country }\r\n" + 
					"default-country PrintableString ::= \"France\"\r\n" + 
					"Payment-method ::= CHOICE {\r\n" + 
					"check NumericString (SIZE (15)),\r\n" + 
					"credit-card Credit-card,\r\n" + 
					"cash NULL }\r\n" + 
					"Credit-card ::= SEQUENCE {\r\n" + 
					"type Card-type,\r\n" + 
					"number NumericString (SIZE (20)),\r\n" + 
					"expiry-date NumericString (SIZE (6)) -- MMYYYY -- }\r\n" + 
					"Card-type ::= ENUMERATED { cb(0), visa(1), eurocard(2),\r\n" + 
					"diners(3), american-express(4) }\r\n" + 
					"Order-line ::= SEQUENCE {\r\n" + 
					"item-code Item-code,\r\n" + 
					"label Label,\r\n" + 
					"quantity Quantity,\r\n" + 
					"price Cents }\r\n" + 
					"Item-code ::= NumericString (SIZE (7))\r\n" + 
					"Label ::= PrintableString (SIZE (1..30))\r\n" + 
					"Quantity ::= CHOICE { unites INTEGER,\r\n" + 
					"millimeters INTEGER,\r\n" + 
					"milligrams INTEGER }\r\n" + 
					"Cents ::= INTEGER\r\n" + 
					"Delivery-report ::= SEQUENCE {\r\n" + 
					"order-number Order-number,\r\n" + 
					"delivery SEQUENCE OF Delivery-line }\r\n" + 
					"Delivery-line ::= SEQUENCE { item Item-code,\r\n" + 
					"quantity Quantity }\r\n" + 
					"END\r\n"),
			new TestCase(1,39, "Protocol DEFINITIONS AUTOMATIC TAGS ::=\r\n" + 
					"BEGIN\r\n" + 
					"IMPORTS Order, Delivery-report, Item-code, Quantity,\r\n" + 
					"Order-number FROM Module-order ;\r\n" + 
					"Pdu ::= CHOICE { question CHOICE {\r\n" + 
					"question1 Order,\r\n" + 
					"question2 Item-code,\r\n" + 
					"question3 Order-number,\r\n" + 
					"... },\r\n" + 
					"answer CHOICE {\r\n" + 
					"answer1 Delivery-report,\r\n" + 
					"answer2 Quantity,\r\n" + 
					"answer3 Delivery-report,\r\n" + 
					"... }}\r\n" + 
					"END\r\n"),
			new TestCase(2,62, "Misc DEFINITIONS ::=\r\n"+
					"BEGIN\r\n"+
					"Lottery-number ::= INTEGER (1..49)\r\n" + 
					"Lottery-draw ::= SEQUENCE SIZE (6) OF Lottery-number\r\n"
					+ "Upper-case-words ::= IA5String (FROM (\"A\"..\"Z\"))\r\n" + 
					"Phone-number ::= NumericString (FROM (\"0\"..\"9\"))(SIZE (10))\r\n" + 
					"Coordinates-in-plan ::=\r\n" + 
					"Coordinates (WITH COMPONENTS {..., z ABSENT})\r\n"+
					"Coordinates ::= SET { a INTEGER, z REAL }\r\n"+
					"END\r\n"),
			new TestCase(3,57, "Real DEFINITIONS ::=\r\n"+
					"BEGIN\r\n"+
					"pi REAL ::= { mantissa 314159, base 10, exponent -5 }\r\n" + 
					"e REAL ::= { mantissa 271828128459045235360287,\r\n" + 
					"base 10,\r\n" + 
					"exponent -23 }\r\n" + 
					"zero REAL ::= 0.0 BinaryReal ::= REAL (WITH COMPONENTS {..., base (2)})\r\n"+
					"RestrictedReal ::= REAL (WITH COMPONENTS {\r\n" + 
					"mantissa (-16777215..16777215),\r\n" + 
					"base (2),\r\n" + 
					"exponent (-125..128) })\r\n"+ "END"),
			new TestCase(4,115, "Module DEFINITIONS ::= BEGIN\r\n" +
				// 	"choice Afters ::= dessert:\"profiterolles\""+
					"RestrictedReal ::= REAL (WITH COMPONENTS {\r\n" + 
					"mantissa (-16777215..16777215),\r\n" + 
					"base (2),\r\n" + 
					"exponent (-125..128) })\r\n"+
					"StringOf32Bits ::= BIT STRING (SIZE (32))\r\n"+
					"Rights ::= BIT STRING { user-read(0), user-write(1),\r\n" + 
					"group-read(2), group-write(3),\r\n" + 
					"other-read(4), other-write(5) }\r\n" + 
					"group1 Rights ::= { group-read, group-write }\r\n"+
					"BooleanVector ::= BIT STRING { b1(0), b2(1), b3(2),\r\n" + 
					"b4(3), b5(4), b6(5) } all-wrong BooleanVector (SIZE (6)) ::= {}\r\n"+
					"StringOf5Octets ::= OCTET STRING (SIZE (5))\r\n"+
					"EDIBodyPartType ::= OBJECT IDENTIFIER\r\n"+
					"internet-id OBJECT IDENTIFIER ::=\r\n" + 
					"{iso(1) identified-organization(3) dod(6) internet(1) }\r\n" + 
					"francetelecom-id OBJECT IDENTIFIER ::=\r\n" + 
					"{ iso member-body f(250) type-org(1) ft(16) }\r\n" + 
					"ber-id OBJECT IDENTIFIER ::= { 2 1 1 }\r\n"+
					"END\r\n"),
			new TestCase(5,162, "Module DEFINITIONS ::= BEGIN\r\n" +
					"IMPORTS Tx FROM Homonym { iso member-body(2) f(250)\r\n" + 
					"type-org(1) ft(16) asn1-book(9)\r\n" + 
					"chapter10(2) homonym1(1) }\r\n" + 
					"Tx FROM Surname -- renaming -- { iso member-body(2)\r\n" + 
					"f(250) type-org(1) ft(16) asn1-book(9)\r\n" + 
					"chapter10(2) homonym2(2) };\r\n" + 
					"Wx ::= CHOICE { integer Homonym.Tx,\r\n" + 
					"real Surname.Tx -- local name --}\r\n"+
					"Roid ::= SEQUENCE {\r\n" + 
					"reference-node OBJECT IDENTIFIER DEFAULT { iso\r\n" + 
					"member-body(2) f(250) type-org(1)\r\n" + 
					"ft(16) asn1-book(9) },\r\n" + 
					"relative-oids SEQUENCE OF RELATIVE-OID\r\n" + 
					"-- relative to reference-node -- }\r\n"+
					"Form ::= SET {\r\n" + 
					"name Surname,\r\n" + 
					"first-name First-name,\r\n" + 
					"phone-number [2] NumericString }\r\n" + 
					"Surname ::= [0] VisibleString\r\n" + 
					"First-name ::= [1] VisibleString\r\n"+
					"A-possible-type ::= SET {\r\n" + 
					"integer [0] CHOICE {\r\n" + 
					"a [0] INTEGER,\r\n" + 
					"b [1] INTEGER },\r\n" + 
					"boolean [1] CHOICE {\r\n" + 
					"a [0] BOOLEAN,\r\n" + 
					"b [1] BOOLEAN }}\r\n"+
					"Description ::= SEQUENCE { surname IA5String,\r\n" + 
					"first-name IA5String,\r\n" + 
					"age INTEGER }\r\n" +
					"johnny Description ::= { surname \"Smith\",\r\n" + 
					"first-name \"John\",\r\n" + 
					"age 40 }\r\n" + 
					"Description2 ::= SEQUENCE {\r\n" + 
					"surname IA5String,\r\n" + 
					"first-name IA5String OPTIONAL,\r\n" + 
					"age INTEGER DEFAULT 40 }\r\n"
					+ "Registration ::= SEQUENCE {\r\n" + 
					"COMPONENTS OF Description2,\r\n" + 
					"marital-status ENUMERATED {single, married, divorced,\r\n" + 
					"widowed} }\r\n"+
			/*		"SteepleChase ::= SEQUENCE OF INTEGER\r\n"
					+ "winningCombination SteepleChase ::= {5, 2, 12}\r\n"+
					"Afters ::= CHOICE { cheese [0] IA5String,\r\n" + 
					"dessert [1] IA5String } mine Afters ::= dessert:\"profiteroles\"\r\n"+
					"ROS ::= CHOICE {\r\n" + 
					"invoke [1] Invoke,\r\n" + 
					"returnResult [2] ReturnResult,\r\n" + 
					"returnError [3] ReturnError,\r\n" + 
					"reject [4] Reject }\r\n"+
					"Element ::= CHOICE { atomic-no INTEGER (1..103),\r\n" + 
					"symbol PrintableString }\r\n" + 
					"MendeleievTable ::=\r\n" + 
					"SEQUENCE SIZE (103) OF symbol < Element\r\n" + 
					"einsteinium symbol < Element ::= \"Es\"\r\n" +
					"v ANY ::= INTEGER:12\r\n" + 
					"T ::= SEQUENCE { a BOOLEAN,\r\n" + 
					"b REAL }\r\n" + 
					"w ANY ::= T:{ a TRUE,\r\n" + 
					"b {314, 10, -2} }\r\n"+
					"State ::= ENUMERATED {on, off, out-of-order, ...,\r\n" + 
					"stand-by} -- version 2\r\n" + 
					"Dimensions ::= SET { x INTEGER,\r\n" + 
					"y INTEGER,\r\n" + 
					"...,\r\n" + 
					"z INTEGER } -- version 2\r\n" + 
					"Afters ::= CHOICE { cheese IA5String,\r\n" + 
					"dessert IA5String,\r\n" + 
					"...,\r\n" + 
					"coffee NULL } -- version 2\r\n" + 
					"Afters ::= CHOICE { cheese IA5String,\r\n" + 
					"dessert IA5String,\r\n" + 
					"...,\r\n" + 
					"coffee NULL, -- version 2\r\n" + 
					"cognac IA5String } -- version 3\r\n"+
					"Description ::= SEQUENCE {\r\n" + 
					"surname IA5String,\r\n" + 
					"first-name IA5String,\r\n" + 
					"age INTEGER,\r\n" + 
					"...!extended-description }\r\n" + 
					"extended-description INTEGER ::= 1\r\n" + 
					"Dimensions ::= SET { x INTEGER,\r\n" + 
					"y INTEGER,\r\n" + 
					"... !IA5String:\"dimension error\"}\r\n" + 
					"Afters ::= CHOICE { cheese IA5String,\r\n" + 
					"dessert IA5String,\r\n" + 
					"...!ExtensionPb:greedy,\r\n" + 
					"coffee NULL,\r\n" + 
					"cognac IA5String }\r\n" + 
					"ExtensionPb::= ENUMERATED {greedy, ...}\r\n"+
					"Person ::= SET {\r\n" + 
					"surname [0] IA5String,\r\n" + 
					"first-name [1] IA5String,\r\n" + 
					"contact CHOICE { phone-number [2] NumericString,\r\n" + 
					"e-mail-address [3] NumericString,\r\n" + 
					"... },\r\n" + 
					"info CHOICE { age [4] INTEGER,\r\n" + 
					"... } }\r\n"+
					"WeekEnd ::= Day (saturday|sunday)\r\n" + 
					"PushButtonDial ::= IA5String (\"0\"|\"1\"|\"2\"|\"3\"|\r\n" + 
					"\"4\"|\"5\"|\"6\"|\"7\"|\"8\"|\"9\"|\"*\"|\"#\")\r\n"+
					"T ::= REAL (0..<{mantissa 5,base 10,exponent 0})\r\n" + 
					"U ::= T ({mantissa 2,base 10,exponent 0}..MAX)\r\n"+
					"Exactly31BitsString ::= BIT STRING (SIZE (31))\r\n" + 
					"StringOf31BitsAtTheMost ::= BIT STRING (SIZE (0..31))\r\n" + 
					"EvenNumber ::= INTEGER (2|4|6|8|10)\r\n" + 
					"EvenLengthString ::=\r\n" + 
					"IA5String (SIZE (INCLUDES EvenNumber))\r\n" + 
					"NonEmptyString ::= OCTET STRING (SIZE (1..MAX))\r\n" +
					"FUNCTION ::= CLASS {\r\n" + 
					"&ArgumentType ,\r\n" + 
					"&ResultType DEFAULT NULL,\r\n" + 
					"&Errors ERROR OPTIONAL,\r\n" + 
					"&code INTEGER UNIQUE }\r\n" + 
					"addition-of-2-integers FUNCTION ::= {\r\n" + 
					"&ArgumentType SEQUENCE { a INTEGER, b INTEGER },\r\n" + 
					"&ResultType INTEGER,\r\n" + 
					"-- empty error list by default\r\n" + 
					"&code 1 }"+*/
					"END\r\n"), 
			new TestCase(6,2, "Module DEFINITIONS ::= BEGIN\r\n" +
					"END\r\n"),
			new TestCase(7,62, "Module DEFINITIONS ::= BEGIN\r\n"
					+ "-- See Rec. X.680 p53. Assumes AUTOMATIC tagging\r\n" + 
					" ExternalType ::= SEQUENCE {\r\n" + 
					" 	identification	CHOICE {\r\n" + 
					" 	-- Abstract and transfer syntax object identifiers\r\n" + 
					" 		syntaxes	SEQUENCE {\r\n" + 
					" 			abstract	OBJECT IDENTIFIER,\r\n" + 
					" 			transfer	OBJECT IDENTIFIER\r\n" + 
					" 		},\r\n" + 
					" 		-- A single object identifier for identification of\r\n" + 
					" 		-- Abstract and transfer syntaxes\r\n" + 
					"		syntax	OBJECT IDENTIFIER,\r\n" + 
					"		-- OSI only. Negotiated OSI presentation context id\r\n" + 
					"		presentation-context-id	INTEGER,\r\n" + 
					"		-- OSI only. Context negotiation in progress\r\n" + 
					"		context-negotiation	SEQUENCE {\r\n" + 
					"			presentation-context-id	INTEGER,\r\n" + 
					"			transfer-syntax	OBJECT IDENTIFIER\r\n" + 
					"		},\r\n" + 
					"		-- The type of value is fixed by the application designer\r\n" + 
					"		-- Provided to support selective field encyption\r\n" + 
					"		transfer-syntax	OBJECT IDENTIFIER,\r\n" + 
					"		-- Data value is of a fixed ASN.1 type, and known to both the \r\n" + 
					"		-- sender and the receiver \r\n" + 
					"		fixed	NULL,\r\n" + 
					"		-- not for transmission. Provides a human readable description\r\n" + 
					"		data-value-descriptor	ObjectDescriptor OPTIONAL,\r\n" + 
					"		-- actual value\r\n" + 
					"		data-value	OCTET STRING\r\n" + 
					" 	} (WITH COMPONENTS { ... , \r\n" + 
					" 		identification (WITH COMPONENTS { ... ,\r\n" + 
					" 			syntaxes	ABSENT,\r\n" + 
					" 			transfer-syntax	ABSENT,\r\n" + 
					" 			fixed	ABSENT} ) } )\r\n" +
					"  }\r\n"
					+ "END"),
			new TestCase(8,93, "Module DEFINITIONS ::= BEGIN\r\n" +
					"DirectoryString{INTEGER:maxSize} ::= CHOICE {\r\n" + 
					"teletexString TeletexString (SIZE (1..maxSize)),\r\n" + 
					"printableString PrintableString (SIZE (1..maxSize)),\r\n" + 
					"universalString UniversalString (SIZE (1..maxSize)),\r\n" + 
					"bmpString BMPString (SIZE (1..maxSize)),\r\n" + 
					"utf8String UTF8String (SIZE (1..maxSize)) }\r\n" +
					"SubstringAssertion{INTEGER:ub-match} ::= SEQUENCE OF\r\n" + 
					"CHOICE { initial [0] DirectoryString{ub-match},\r\n" + 
					"any [1] DirectoryString{ub-match},\r\n" + 
					"final [2] DirectoryString{ub-match} }\r\n"+
					"END\r\n"),
			new TestCase(9,35, "Module DEFINITIONS ::= BEGIN\r\n" +
					"Choice{Tx} ::= CHOICE { a [0] Tx,\r\n" + 
					"b INTEGER }\r\n" + 
					"Structure{Tx} ::= SEQUENCE { a INTEGER,\r\n" + 
					"b [0] Tx OPTIONAL,\r\n" + 
					"c INTEGER }\r\n"+
					"END\r\n"),
			new TestCase(10,19, "Module DEFINITIONS ::= BEGIN\r\n" +
					"GeneralForm{Tx, Tx:val} ::= SEQUENCE {\r\n" + 
					"info Tx DEFAULT val,\r\n" + 
					"comments IA5String }\r\n"+
					"END\r\n"),
			new TestCase(11,19, "Module DEFINITIONS ::= BEGIN\r\n" +
					"pariTierce{INTEGER:first, INTEGER:second,\r\n" + 
					"INTEGER:third} SEQUENCE OF INTEGER ::=\r\n" + 
					"{ first, second, third }\r\n"+
					"END\r\n"),
			new TestCase(12,35, "Module DEFINITIONS ::= BEGIN\r\n" +
					"FUNCTION ::= CLASS {\r\n" + 
					"&ArgumentType ,\r\n" + 
					"&ResultType DEFAULT NULL,\r\n" + 
					"&Errors ERROR OPTIONAL,\r\n" + 
					"&code INTEGER UNIQUE }\r\n" +
					"addition-of-2-integers FUNCTION ::= {\r\n" + 
					"&ArgumentType SEQUENCE { a INTEGER, b INTEGER },\r\n" + 
					"&ResultType INTEGER,\r\n" + 
					"-- empty error list by default\r\n" + 
					"&code 1 }"+
					"END\r\n"),
			new TestCase(13,152, "Module DEFINITIONS ::= BEGIN\r\n" +
					"ERROR ::= CLASS { &errorCode INTEGER, &errorReason VisibleString }\r\n"+
					"OTHER-FUNCTION ::= CLASS {\r\n" + 
					"	&code INTEGER (0..MAX) UNIQUE,\r\n" + 
					"	&Alphabet BMPString DEFAULT {Latin1 INTERSECTION Level1},\r\n" + 
					"	&ArgumentType ,\r\n" +
					"	&SupportedArguments &ArgumentType OPTIONAL,\r\n" + 
					"	&ResultType DEFAULT NULL,\r\n" + 
					"	&result-if-error &ResultType DEFAULT NULL,\r\n" + 
					"	&associated-function OTHER-FUNCTION OPTIONAL,\r\n" + 
					"	&Errors ERROR DEFAULT {rejected-argument | memory-fault}" +
					"}\r\n" + 
					"rejected-argument ERROR ::=\r\n" + 
					"{ &errorCode 1, &errorReason \"argument rejected\" }\r\n" + 
					"memory-fault ERROR ::=\r\n" + 
					"{&errorCode 2, &errorReason \"memory fault\" }\r\n" +
					"other-addition-of-2-integers OTHER-FUNCTION ::= {\r\n" + 
					"	&ArgumentType Pair,\r\n" + 
					"	&SupportedArguments {PosPair | NegPair},\r\n" + 
					"	&ResultType INTEGER,\r\n" + 
					"	&result-if-error 0,\r\n" + 
					"	&code 1\r\n" +
					"}\r\n" + 
					"Pair ::= SEQUENCE {a INTEGER, b INTEGER}\r\n" + 
					"PosPair ::= Pair (WITH COMPONENTS {a(0..MAX), b(0..MAX)})\r\n" + 
					"NegPair ::= Pair (WITH COMPONENTS {a(MIN..0), b(MIN..0)})\r\n" +
					"END\r\n"),
			new TestCase(14,119, "Module DEFINITIONS ::= BEGIN\r\n" +
					"ERROR ::= CLASS { &errorCode INTEGER, &errorReason VisibleString }\r\n"+
					"OTHER-FUNCTION ::= CLASS {\r\n" + 
					"&code INTEGER (0..MAX) UNIQUE,\r\n" + 
					"&Alphabet BMPString DEFAULT {\"default\"},\r\n" + 
					"&ArgumentType ,\r\n" + 
					"&SupportedArguments &ArgumentType OPTIONAL,\r\n" + 
					"&ResultType DEFAULT NULL,\r\n" + 
					"&result-if-error &ResultType DEFAULT NULL,\r\n" + 
					"&associated-function OTHER-FUNCTION OPTIONAL,\r\n" + 
					"&Errors ERROR DEFAULT\r\n" + 
					"{rejected-argument|memory-fault} }" +
					"WITH SYNTAX {\r\n" + 
					"ARGUMENT TYPE &ArgumentType,\r\n" + 
					"[SUPPORTED ARGUMENTS &SupportedArguments,]\r\n" + 
					"[RESULT TYPE &ResultType,\r\n" + 
					"[RETURNS &result-if-error IN CASE OF ERROR,]]\r\n" + 
					"[ERRORS &Errors,]\r\n" + 
					"[MESSAGE ALPHABET &Alphabet,]\r\n" + 
					"[ASSOCIATED FUNCTION &associated-function,]\r\n" + 
					"CODE &code }\r\n" + 
					"memory-fault ERROR ::= {-- object definition --}\r\n"+
					"addition-of-2-integers OTHER-FUNCTION ::= {\r\n" + 
					"ARGUMENT TYPE Pair,\r\n" + 
					"SUPPORTED ARGUMENTS {PosPair | NegPair},\r\n" + 
					"RESULT TYPE INTEGER,\r\n" + 
					"RETURNS 0 IN CASE OF ERROR,\r\n" + 
					"CODE 1 }\r\n"+
					"END"),
			new TestCase(15,87, "Module DEFINITIONS ::= BEGIN\r\n" +
					"MATCHING-RULE ::= CLASS {\r\n" + 
					"&AssertionType OPTIONAL,\r\n" + 
					"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
					"WITH SYNTAX {\r\n" + 
					"[SYNTAX &AssertionType]\r\n" + 
					"ID &id }\r\n" +
					"MatchingRules MATCHING-RULE ::= {\r\n" + 
					"caseIgnoreMatch | booleanMatch | integerMatch }\r\n" +
					"caseIgnoreMatch MATCHING-RULE ::= {\r\n" + 
					"SYNTAX DirectoryString\r\n" + 
					"ID {id-mr 2} }\r\n" + 
					"id-mr OBJECT IDENTIFIER ::=\r\n" + 
					"{ joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
					"booleanMatch MATCHING-RULE ::= {\r\n" + 
					"SYNTAX BOOLEAN\r\n" + 
					"ID {id-mr 13} }\r\n" + 
					"-- id-mr OBJECT IDENTIFIER ::=\r\n" + 
					"-- { joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
					"integerMatch MATCHING-RULE ::= {\r\n" + 
					"SYNTAX INTEGER\r\n" + 
					"ID {id-mr 14} }\r\n" + 
					"-- id-mr OBJECT IDENTIFIER ::=\r\n" + 
					"-- { joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
					"LessMatchingRules MATCHING-RULE ::= {\r\n" + 
					"MatchingRules EXCEPT caseIgnoreMatch }\r\n" +
					"ExtensibleMatchingRules MATCHING-RULE ::= {\r\n" + 
					"caseIgnoreMatch | booleanMatch | integerMatch, ... }\r\n"+
					"END\r\n"),
			new TestCase(16,199, "Module DEFINITIONS ::= BEGIN\r\n" +
					"ATTRIBUTE ::= CLASS {\r\n" + 
					"&derivation ATTRIBUTE OPTIONAL,\r\n" + 
					"&Type OPTIONAL,\r\n" + 
					"&equality-match MATCHING-RULE OPTIONAL,\r\n" + 
					"&ordering-match MATCHING-RULE OPTIONAL,\r\n" + 
					"&substrings-match MATCHING-RULE OPTIONAL,\r\n" + 
					"&single-valued BOOLEAN DEFAULT FALSE,\r\n" + 
					"&collective BOOLEAN DEFAULT FALSE,\r\n" + 
					"&no-user-modification BOOLEAN DEFAULT FALSE,\r\n" + 
					"&usage Attribute-Usage\r\n" + 
					"DEFAULT userApplications,\r\n" + 
					"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
					"WITH SYNTAX {\r\n" + 
					"[SUBTYPE OF &derivation]\r\n" + 
					"[WITH SYNTAX &Type]\r\n" + 
					"[EQUALITY MATCHING RULE &equality-match]\r\n" + 
					"[ORDERING MATCHING RULE &ordering-match]\r\n" + 
					"[SUBSTRINGS MATCHING RULE &substrings-match]" +
					"[SINGLE VALUE &single-valued]\r\n" + 
					"[COLLECTIVE &collective]\r\n" + 
					"[NO USER MODIFICATION &no-user-modification]\r\n" + 
					"[USAGE &usage]\r\n" + 
					"ID &id }\r\n" + 
					"AttributeUsage ::= ENUMERATED { userApplications(0),\r\n" + 
					"directoryOperation(1), distributedOperation(2),\r\n" + 
					"dSAOperation(3) }" +
					"MATCHING-RULE ::= CLASS {\r\n" + 
					"&AssertionType OPTIONAL,\r\n" + 
					"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
					"WITH SYNTAX {\r\n" + 
					"[SYNTAX &AssertionType]\r\n" + 
					"ID &id }\r\n"+
					"name ATTRIBUTE ::= {\r\n" + 
					"WITH SYNTAX DirectoryString\r\n" + 
					"EQUALITY MATCHING RULE caseIgnoreMatch\r\n" + 
					"ID {joint-iso-itu-t ds(5) attributeType(4) 2} }\r\n" +
					"DirectoryString ::= CHOICE {\r\n" + 
					"teletexString TeletexString (SIZE (1..maxSize)),\r\n" + 
					"printableString PrintableString (SIZE (1..maxSize)),\r\n" + 
					"universalString UniversalString (SIZE (1..maxSize)),\r\n" + 
					"bmpString BMPString (SIZE (1..maxSize)),\r\n" + 
					"utf8String UTF8String (SIZE (1..maxSize)) }\r\n" +
					"maxSize INTEGER ::= 25\r\n" + 
					"caseIgnoreMatch MATCHING-RULE ::= {\r\n" + 
					"SYNTAX DirectoryString\r\n" + 
					"ID {id-mr 2} }\r\n" + 
					"id-mr OBJECT IDENTIFIER ::=\r\n" + 
					"{ joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
					"END"),
			 new TestCase(17,40, "Module DEFINITIONS ::= BEGIN\r\n" +
						"MATCHING-RULE ::= CLASS {\r\n" + 
						"&AssertionType OPTIONAL,\r\n" + 
						"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
						"WITH SYNTAX {\r\n" + 
						"[SYNTAX &AssertionType]\r\n" + 
						"ID &id }\r\n" +
						"MatchingRules MATCHING-RULE ::= {\r\n" + 
						"caseIgnoreMatch | booleanMatch | integerMatch }\r\n"+
						"LessMatchingRules MATCHING-RULE ::= {\r\n" + 
						"MatchingRules EXCEPT caseIgnoreMatch }\r\n"+
						"ExtensibleMatchingRules MATCHING-RULE ::= {\r\n" + 
						" ... }\r\n"+
						"END\r\n"),
			 new TestCase(18,63,"Module DEFINITIONS ::= BEGIN\r\n" +
						"PrimeNumbers INTEGER ::= { 2 | 3 | 5 | 7 | 11 | 13 }\r\n" +
						"Lottery-number ::= INTEGER (1..54)\r\n"+
						"counter Lottery-number ::= 45\r\n" + 
						"Lottery-draw ::= SEQUENCE OF INTEGER\r\n"+
						"sextuple Lottery-draw ::= { 7, 12, 23, 31, 33, 41 }\r\n" + 
						"Coordinates ::= SET { x [0] INTEGER, y [0] INTEGER }\r\n"+
						"pair Coordinates ::= { x 5, y -3 }\r\n"+
						"END\r\n"),
			 new TestCase(19,66,"Module DEFINITIONS ::= BEGIN\r\n" +
						"MECHANISM-NAME ::= TYPE-IDENTIFIER\r\n" +
						"Authentication-value ::= CHOICE {\r\n" + 
						"charstring [0] IMPLICIT GraphicString,\r\n" + 
						"bitstring [1] BIT STRING,\r\n" + 
						"external [2] EXTERNAL,\r\n" + 
						"other [3] IMPLICIT SEQUENCE {\r\n" + 
						"other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),\r\n" + 
						"other-mechanism-value MECHANISM-NAME.&Type\r\n" + 
						"({ObjectSet}{@.other-mechanism-name}) }}\r\n"+
						"END\r\n"),

				new TestCase(20,33,"Module DEFINITIONS ::= BEGIN\r\n" +
						"CLASS1 ::= CLASS { &obj CLASS2 }\r\n" + 
						"CLASS2 ::= CLASS { &val INTEGER }\r\n" + 
						"object1 CLASS1 ::= { &obj object2 }\r\n" + 
						"object2 CLASS2 ::= { &val 5 }\r\n" + 
						"value INTEGER ::= object1.&obj.&val\r\n"+
						"END\r\n"),
				
				new TestCase(21,431,"Module DEFINITIONS ::= BEGIN\r\n" +			
						"ATTRIBUTE ::= CLASS {\r\n" + 
						"&derivation ATTRIBUTE OPTIONAL,\r\n" + 
						"&Type OPTIONAL,\r\n" + 
						"&equality-match MATCHING-RULE OPTIONAL,\r\n" + 
						"&ordering-match MATCHING-RULE OPTIONAL,\r\n" + 
						"&substrings-match MATCHING-RULE OPTIONAL,\r\n" + 
						"&single-valued BOOLEAN DEFAULT FALSE,\r\n" + 
						"&collective BOOLEAN DEFAULT FALSE,\r\n" + 
						"&no-user-modification BOOLEAN DEFAULT FALSE,\r\n" + 
						"&usage Attribute-Usage\r\n" + 
						"DEFAULT userApplications,\r\n" + 
						"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
						"WITH SYNTAX {\r\n" + 
						"[SUBTYPE OF &derivation]\r\n" + 
						"[WITH SYNTAX &Type]\r\n" + 
						"[EQUALITY MATCHING RULE &equality-match]\r\n" + 
						"[ORDERING MATCHING RULE &ordering-match]\r\n" + 
						"[SUBSTRINGS MATCHING RULE &substrings-match]\r\n"+
						"[SINGLE VALUE &single-valued]\r\n" + 
						"[COLLECTIVE &collective]\r\n" + 
						"[NO USER MODIFICATION &no-user-modification]\r\n" + 
						"[USAGE &usage]\r\n" + 
						"ID &id }\r\n" + 
						"AttributeUsage ::= ENUMERATED { userApplications(0),\r\n" + 
						"directoryOperation(1), distributedOperation(2),\r\n" + 
						"dSAOperation(3) }\r\n"+
						
						"MATCHING-RULE ::= CLASS {\r\n" + 
						"&AssertionType OPTIONAL,\r\n" + 
						"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
						"WITH SYNTAX {\r\n" + 
						"[SYNTAX &AssertionType]\r\n" + 
						"ID &id }\r\n" +
						
						"MatchingRules MATCHING-RULE ::= {\r\n" + 
						"caseIgnoreMatch | booleanMatch | integerMatch }\r\n"+
						
						"caseIgnoreMatch MATCHING-RULE ::= {\r\n" + 
						"SYNTAX DirectoryString\r\n" + 
						"ID {id-mr 2} }\r\n" + 
						
						"id-mr OBJECT IDENTIFIER ::=\r\n" + 
						"{ joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
						
						"surname ATTRIBUTE ::= { -- family name\r\n" + 
						"SUBTYPE OF name\r\n" + 
						"WITH SYNTAX DirectoryString\r\n" + 
						"ID id-at-surname }\r\n" + 
						
						"givenName ATTRIBUTE ::= { -- first name\r\n" + 
						"SUBTYPE OF name\r\n" + 
						"WITH SYNTAX DirectoryString\r\n" + 
						"ID id-at-givenName }\r\n" + 
						
						"countryName ATTRIBUTE ::= { -- country\r\n" + 
						"SUBTYPE OF name\r\n" + 
						"WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes\r\n" + 
						"SINGLE VALUE TRUE\r\n" + 
						"ID id-at-countryName}\r\n"+
						
						"SupportedAttributes ATTRIBUTE ::=\r\n" + 
						"{surname | givenName | countryName}\r\n"+
						
						"AttributeIdAndValue1 ::= SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id,\r\n" + 
						"value ATTRIBUTE.&Type }\r\n" +
						
						"AttributeIdAndValue2 ::= SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id({SupportedAttributes}),\r\n" + 
						"value ATTRIBUTE.&Type({SupportedAttributes}) }\r\n"+
						
						"AttributeIdAndValue3 ::= SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id({SupportedAttributes}),\r\n" + 
						"value ATTRIBUTE.&Type({SupportedAttributes}{@.ident}) }\r\n" +	
						
						"AttributeValueAssertion ::= SEQUENCE {\r\n" + 
						"type ATTRIBUTE.&Id({SupportedAttributes}),\r\n" + 
						"assertion ATTRIBUTE.&equality-match.&AssertionType\r\n" + 
						"({SupportedAttributes}{@type}) }\r\n"+
						"FilterItem ::= CHOICE {\r\n" + 
						"equality [0] AttributeValueAssertion,\r\n" + 
						"substrings [1] SEQUENCE {\r\n" + 
						"type ATTRIBUTE.&id({SupportedAttributes}),\r\n" + 
						"strings SEQUENCE OF CHOICE {\r\n" + 
						"initial [0] ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@substrings.type}),\r\n" + 
						"any [1] ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@substrings.type}),\r\n" + 
						"final [2] ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@substrings.type}) }},\r\n" + 
						"greaterOrEqual [2] AttributeValueAssertion,\r\n" + 
						"lessOrEqual [3] AttributeValueAssertion,\r\n" + 
						"present [4] AttributeType,\r\n" + 
						"approximateMatch [5] AttributeValueAssertion,\r\n" + 
						"extensibleMatch [6] MatchingRuleAssertion }\r\n"+
						
						"Attribute-desc ::= SEQUENCE {\r\n" + 
						"usage ATTRIBUTE.&usage({SupportedAttributes}),\r\n" + 
						"list SEQUENCE OF SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id({SupportedAttributes}{@usage}),\r\n" + 
						"value ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@usage,@.ident}) }}\r\n"+
						
						"-- att-desc Attribute-desc ::= {\r\n" + 
						"-- usage userApplications,\r\n" + 
						"-- list { { ident id-at-objectClass,\r\n" + 
						"-- value oid },\r\n" + 
						"-- { ident id-at-aliasedEntryName,\r\n" + 
						"-- value distinguishedName }}}\r\n"+
						"END\r\n"),
				new TestCase(22,38,"M1 DEFINITIONS ::= BEGIN\r\n"+
						"Ty ::= INTEGER\r\n"+
						"v Ty ::= 4\r\n"+
						"A ::= CLASS { &Type }\r\n"+
						"a A ::= { &Type INTEGER }\r\n"+
						"As A ::= { a }\r\n"+
						"Vs Ty ::= { 3 }\r\n"+
						"END\r\n"+
						"Module DEFINITIONS ::= BEGIN\r\n" +
						"EXPORTS ALL;\r\n"+
						"IMPORTS Ty,v,A,a,As FROM M1;\r\n"+
						"END\r\n"),
				new TestCase(23,66,"Module DEFINITIONS ::= BEGIN\r\n" +
						"MECHANISM-NAME ::= TYPE-IDENTIFIER\r\n" +
						"Authentication-value ::= CHOICE {\r\n" + 
						"charstring [0] IMPLICIT GraphicString,\r\n" + 
						"bitstring [1] BIT STRING,\r\n" + 
						"external [2] EXTERNAL,\r\n" + 
						"other [3] IMPLICIT SEQUENCE {\r\n" + 
						"other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),\r\n" + 
						"other-mechanism-value MECHANISM-NAME.&Type\r\n" + 
						"({ObjectSet}{@.other-mechanism-name}) }}\r\n"+
						"END\r\n"),
				new TestCase(24,33,"Module DEFINITIONS ::= BEGIN\r\n" +
						"CLASS1 ::= CLASS { &obj CLASS2 }\r\n" + 
						"CLASS2 ::= CLASS { &val INTEGER }\r\n" + 
						"object1 CLASS1 ::= { &obj object2 }\r\n" + 
						"object2 CLASS2 ::= { &val 5 }\r\n" + 
						"value INTEGER ::= object1.&obj.&val\r\n"+
						"END\r\n"),
				new TestCase(25,431,"Module DEFINITIONS ::= BEGIN\r\n" +			
						"ATTRIBUTE ::= CLASS {\r\n" + 
						"&derivation ATTRIBUTE OPTIONAL,\r\n" + 
						"&Type OPTIONAL,\r\n" + 
						"&equality-match MATCHING-RULE OPTIONAL,\r\n" + 
						"&ordering-match MATCHING-RULE OPTIONAL,\r\n" + 
						"&substrings-match MATCHING-RULE OPTIONAL,\r\n" + 
						"&single-valued BOOLEAN DEFAULT FALSE,\r\n" + 
						"&collective BOOLEAN DEFAULT FALSE,\r\n" + 
						"&no-user-modification BOOLEAN DEFAULT FALSE,\r\n" + 
						"&usage Attribute-Usage\r\n" + 
						"DEFAULT userApplications,\r\n" + 
						"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
						"WITH SYNTAX {\r\n" + 
						"[SUBTYPE OF &derivation]\r\n" + 
						"[WITH SYNTAX &Type]\r\n" + 
						"[EQUALITY MATCHING RULE &equality-match]\r\n" + 
						"[ORDERING MATCHING RULE &ordering-match]\r\n" + 
						"[SUBSTRINGS MATCHING RULE &substrings-match]\r\n"+
						"[SINGLE VALUE &single-valued]\r\n" + 
						"[COLLECTIVE &collective]\r\n" + 
						"[NO USER MODIFICATION &no-user-modification]\r\n" + 
						"[USAGE &usage]\r\n" + 
						"ID &id }\r\n" + 
						"AttributeUsage ::= ENUMERATED { userApplications(0),\r\n" + 
						"directoryOperation(1), distributedOperation(2),\r\n" + 
						"dSAOperation(3) }\r\n"+
						
						"MATCHING-RULE ::= CLASS {\r\n" + 
						"&AssertionType OPTIONAL,\r\n" + 
						"&id OBJECT IDENTIFIER UNIQUE }\r\n" + 
						"WITH SYNTAX {\r\n" + 
						"[SYNTAX &AssertionType]\r\n" + 
						"ID &id }\r\n" +
						
						"MatchingRules MATCHING-RULE ::= {\r\n" + 
						"caseIgnoreMatch | booleanMatch | integerMatch }\r\n"+
						
						"caseIgnoreMatch MATCHING-RULE ::= {\r\n" + 
						"SYNTAX DirectoryString\r\n" + 
						"ID {id-mr 2} }\r\n" + 
						
						"id-mr OBJECT IDENTIFIER ::=\r\n" + 
						"{ joint-iso-itu-t ds(5) matchingRule(13) }\r\n"+
						
						"surname ATTRIBUTE ::= { -- family name\r\n" + 
						"SUBTYPE OF name\r\n" + 
						"WITH SYNTAX DirectoryString\r\n" + 
						"ID id-at-surname }\r\n" + 
						
						"givenName ATTRIBUTE ::= { -- first name\r\n" + 
						"SUBTYPE OF name\r\n" + 
						"WITH SYNTAX DirectoryString\r\n" + 
						"ID id-at-givenName }\r\n" + 
						
						"countryName ATTRIBUTE ::= { -- country\r\n" + 
						"SUBTYPE OF name\r\n" + 
						"WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes\r\n" + 
						"SINGLE VALUE TRUE\r\n" + 
						"ID id-at-countryName}\r\n"+
						
						"SupportedAttributes ATTRIBUTE ::=\r\n" + 
						"{surname | givenName | countryName}\r\n"+
						
						"AttributeIdAndValue1 ::= SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id,\r\n" + 
						"value ATTRIBUTE.&Type }\r\n" +
						
						"AttributeIdAndValue2 ::= SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id({SupportedAttributes}),\r\n" + 
						"value ATTRIBUTE.&Type({SupportedAttributes}) }\r\n"+
						
						"AttributeIdAndValue3 ::= SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id({SupportedAttributes}),\r\n" + 
						"value ATTRIBUTE.&Type({SupportedAttributes}{@.ident}) }\r\n" +	
						
						"AttributeValueAssertion ::= SEQUENCE {\r\n" + 
						"type ATTRIBUTE.&Id({SupportedAttributes}),\r\n" + 
						"assertion ATTRIBUTE.&equality-match.&AssertionType\r\n" + 
						"({SupportedAttributes}{@type}) }\r\n"+
						"FilterItem ::= CHOICE {\r\n" + 
						"equality [0] AttributeValueAssertion,\r\n" + 
						"substrings [1] SEQUENCE {\r\n" + 
						"type ATTRIBUTE.&id({SupportedAttributes}),\r\n" + 
						"strings SEQUENCE OF CHOICE {\r\n" + 
						"initial [0] ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@substrings.type}),\r\n" + 
						"any [1] ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@substrings.type}),\r\n" + 
						"final [2] ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@substrings.type}) }},\r\n" + 
						"greaterOrEqual [2] AttributeValueAssertion,\r\n" + 
						"lessOrEqual [3] AttributeValueAssertion,\r\n" + 
						"present [4] AttributeType,\r\n" + 
						"approximateMatch [5] AttributeValueAssertion,\r\n" + 
						"extensibleMatch [6] MatchingRuleAssertion }\r\n"+
						
						"Attribute-desc ::= SEQUENCE {\r\n" + 
						"usage ATTRIBUTE.&usage({SupportedAttributes}),\r\n" + 
						"list SEQUENCE OF SEQUENCE {\r\n" + 
						"ident ATTRIBUTE.&id({SupportedAttributes}{@usage}),\r\n" + 
						"value ATTRIBUTE.&Type\r\n" + 
						"({SupportedAttributes}{@usage,@.ident}) }}\r\n"+
						
						"-- att-desc Attribute-desc ::= {\r\n" + 
						"-- usage userApplications,\r\n" + 
						"-- list { { ident id-at-objectClass,\r\n" + 
						"-- value oid },\r\n" + 
						"-- { ident id-at-aliasedEntryName,\r\n" + 
						"-- value distinguishedName }}}\r\n"+
						"END\r\n")

	};

}
