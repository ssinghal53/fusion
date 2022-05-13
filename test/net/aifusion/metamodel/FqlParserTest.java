/**
 * Copyright 2021 Sharad Singhal. All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
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
 * Created Dec 10, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test the Fql Parser
 * @author Sharad Singhal
 */
public class FqlParserTest {
	class TestCase {
		String fql;
		String tree;
		boolean valid;
		int code;
		public TestCase(String fql, String tree, boolean valid, int code) {
			this.fql = fql;
			this.tree = tree;
			this.valid = valid;
			this.code = code;
		}
	}

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("FqlParserTest");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
	}
	TestCase [] testCases = {
			new TestCase("'literal'","CONSTANT[STRING literal]",true,0),	// String literal
			new TestCase("32","CONSTANT[SINT64 32]",true,0),				// Integer literal
			new TestCase("0x42","CONSTANT[SINT64 66]",true,0),				// Hex literal
			new TestCase("01001B","CONSTANT[SINT64 9]",true,0),				// Binary literal
			new TestCase("42.0","CONSTANT[REAL64 42.0]",true,0),			// Real literal
			new TestCase("true","CONSTANT[BOOLEAN true]",true,0),			// Boolean literal
			new TestCase("p1","IDENTIFIER (p1)",true,0),					// property identifier
			new TestCase("func('foo','bar')","FUNCTION (func)\n"
					+ "  |-- CONSTANT[STRING foo]\n"
					+ "  |-- CONSTANT[STRING bar]",true,0),					// function identifier
			new TestCase("p1[]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- CONSTANT[STRING *]",true,0),				// array property (no property range)
			new TestCase("p1[*]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- CONSTANT[STRING *]",true,0),				// star range
			new TestCase("p1[3]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- CONSTANT[SINT64 3]",true,0),				// constant index
			new TestCase("p1[3,5]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- CONSTANT[SINT64 3]\n"
					+ "  |  |-- CONSTANT[SINT64 5]",true,0),				// selected indexes
			new TestCase("p1[3..5]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- RANGE\n"
					+ "  |  |  |-- CONSTANT[SINT64 3]\n"
					+ "  |  |  |-- CONSTANT[SINT64 5]",true,0),				// range indexes
			new TestCase("p1[..5]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- RANGE\n"
					+ "  |  |  |-- CONSTANT[SINT64 0]\n"
					+ "  |  |  |-- CONSTANT[SINT64 5]",true,0),				// missing start index
			new TestCase("p1[3..]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- RANGE\n"
					+ "  |  |  |-- CONSTANT[SINT64 3]\n"
					+ "  |  |  |-- CONSTANT[STRING *]",true,0),				// missing end index
			new TestCase("p1[2..5,7..9]","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- RANGE\n"
					+ "  |  |  |-- CONSTANT[SINT64 2]\n"
					+ "  |  |  |-- CONSTANT[SINT64 5]\n"
					+ "  |  |-- RANGE\n"
					+ "  |  |  |-- CONSTANT[SINT64 7]\n"
					+ "  |  |  |-- CONSTANT[SINT64 9]",true,0),				// multiple ranges
			new TestCase("p1+p2*p3-p4/p5||p3","SUBTRACT\n"
					+ "  |-- ADD\n"
					+ "  |  |-- IDENTIFIER (p1)\n"
					+ "  |  |-- MULTIPLY\n"
					+ "  |  |  |-- IDENTIFIER (p2)\n"
					+ "  |  |  |-- IDENTIFIER (p3)\n"
					+ "  |-- DIVIDE\n"
					+ "  |  |-- IDENTIFIER (p4)\n"
					+ "  |  |-- CONCAT\n"
					+ "  |  |  |-- IDENTIFIER (p5)\n"
					+ "  |  |  |-- IDENTIFIER (p3)",true,0),				// arith
			new TestCase("p1 is null","ISNULL\n"
					+ "  |-- IDENTIFIER (p1)",true,0),		// comp is NULL
			new TestCase("p1 is not null","ISNOTNULL\n"
					+ "  |-- IDENTIFIER (p1)",true,0),		// is not null
			new TestCase("p1 like 'foo*'","LIKE\n"
					+ "  |-- IDENTIFIER (p1)\n"
					+ "  |-- CONSTANT[STRING foo*]",true,0),// like
			new TestCase("p1 not like 'foo*'","NOTLIKE\n"
					+ "  |-- IDENTIFIER (p1)\n"
					+ "  |-- CONSTANT[STRING foo*]",true,0),	// not like
			new TestCase("p1 > p4","GT\n"
					+ "  |-- IDENTIFIER (p1)\n"
					+ "  |-- IDENTIFIER (p4)",true,0),	// comparison
			new TestCase("any p1 > p4","ANY\n"
					+ "  |-- GT\n"
					+ "  |  |-- IDENTIFIER (p1)\n"
					+ "  |  |-- IDENTIFIER (p4)",true,0),	// comparison
			new TestCase("not any p1 > p4","NOT\n"
					+ "  |-- ANY\n"
					+ "  |  |-- GT\n"
					+ "  |  |  |-- IDENTIFIER (p1)\n"
					+ "  |  |  |-- IDENTIFIER (p4)",true,0),	// comparison
			new TestCase("a or b and c or not d","OR\n"
					+ "  |-- IDENTIFIER (a)\n"
					+ "  |-- AND\n"
					+ "  |  |-- IDENTIFIER (b)\n"
					+ "  |  |-- IDENTIFIER (c)\n"
					+ "  |-- NOT\n"
					+ "  |  |-- IDENTIFIER (d)",true,0),	// logical
			
			// extensions
			
			
			new TestCase("any p1 in {4, 5} satisfies ( p1 > p4 )","SATISFIES\n"
					+ "  |-- ANY (p1)\n"
					+ "  |  |-- CONSTANT[SINT64_ARRAY { 4, 5 }]\n"
					+ "  |-- GT\n"
					+ "  |  |-- IDENTIFIER (p1)\n"
					+ "  |  |-- IDENTIFIER (p4)",true,0),	// satisfies comparison
			new TestCase("schema_class","IDENTIFIER (schema_class)",true,0), // identifier
			new TestCase("p1.p2.p3","IDENTIFIER (p1)\n"						
					+ "  |-- PERIOD\n"
					+ "  |  |-- IDENTIFIER (p2)\n"
					+ "  |-- PERIOD\n"
					+ "  |  |-- IDENTIFIER (p3)",true,0),						// identifier . identifier
			new TestCase("p1.p2[]","IDENTIFIER (p1)\n"
					+ "  |-- PERIOD\n"
					+ "  |  |-- IDENTIFIER (p2)\n"
					+ "  |  |  |-- INDEX\n"
					+ "  |  |  |  |-- CONSTANT[STRING *]",true,0),				// identifier . identifier []
			new TestCase("p1[3].p2","IDENTIFIER (p1)\n"
					+ "  |-- INDEX\n"
					+ "  |  |-- CONSTANT[SINT64 3]\n"
					+ "  |-- PERIOD\n"
					+ "  |  |-- IDENTIFIER (p2)",true,0),						// identifier [] . identifier
			new TestCase("/structure/foo:class_name.p1","CLASS_PATH (/structure/foo:class_name)\n"
					+ "  |-- IDENTIFIER (p1)",true,0),	// classpath . propName
			new TestCase("schema_class.prop","IDENTIFIER (schema_class)\n"
					+ "  |-- PERIOD\n"
					+ "  |  |-- IDENTIFIER (prop)",true,0), 					// classPath . propertyName
			new TestCase("schema_class.prop1.prop2","IDENTIFIER (schema_class)\n"
					+ "  |-- PERIOD\n"
					+ "  |  |-- IDENTIFIER (prop1)\n"
					+ "  |-- PERIOD\n"
					+ "  |  |-- IDENTIFIER (prop2)",true,0), 				// classPath . propertyName . propertyName
			

	};
	
	/**
	 * Test method for {@link net.aifusion.metamodel.FqlParser#FqlParser(java.lang.String)}.
	 */
	@Test
	public void testFqlParser() {
		for(TestCase t : testCases) {
			try {
				FqlParser p = new FqlParser(t.fql);
				assertNotNull(p);
//				System.out.println(p.getParseTree().toTree(""));
				assertTrue(t.valid);
				assertEquals(t.tree,p.getParseTree().toTree(""));
			} catch(ModelException e) {
				System.out.println(e);
				assertFalse(t.valid);
				assertEquals(t.code,e.getReason().getCode());
			}
		}
	}
}
