/**
 * Copyright 2016, Sharad Singhal, All Rights Reserved
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
 * Created Sep 10, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test for CQL Token Types
 * @author Sharad Singhal
 */
public class TokenTypeTest {
	public static String [] name = {
			"AND","ANY","AS","ASC","BOOLEAN","BY","CLASSQUALIFIER","COMMA","DESC","DISTINCT","EOF","EQUALS","EVERY",
			"FALSE","FIRST","FROM","GE","GT","IDENTIFIER","IN","IS","ISNOT","ISA","LE","LBRACE","LIKE","LPAREN",
			"LT","NE","NOT","NULL","OR","ORDER","PERIOD","PROPERTYQUALIFIER","QUALIFIER","RBRACE","RPAREN",
			"SATISFIES","SELECT","STAR","TRUE","WHERE","COMPARISON","WHITE_SPACE","COMMENT","SEMICOLON","LBRACKET",
			"RBRACKET","COLON","QUOTED_CHARACTER","ERROR","STRING_VALUE","HASH","CHARACTER","NUMBER","INTEGER",
			"REAL","BINARY","HEX","DATETIMETOMICROSECONDS","STRINGTOUINT","STRINGTOSINT","STRINGTOREAL","UPPERCASE",
			"NUMERICTOSTRING","REFERENCETOSTRING","INSTANCEOF","CLASSPATH","OBJECTPATH","CURRENTDATETIME",
			"DATETIME","MICROSECONDTOTIMESTAMP","MICROSECONDTOINTERVAL","SCOPE","SLASH","PLUS","MINUS","RANGE","CONCAT","SIGN"
	};
	
	public static String [] value = {
			"and","any","as","asc",null,"by","classqualifier",",","desc","distinct",null,"=","every",
			"false","first","from",">=",">",null,"in","is","isnot","isa","<=","{","like","(",
			"<","<>","not","null","or","order",".","propertyQualifier","Qualifier","}",")",
			"satisfies","select","*","true","where",null,null,null,";","[",
			"]",":",null,null,null,"#",null,null,null,null,null,null,"dateTimeToMicroseconds","stringToUint",
			"stringToSint","stringToReal","uppercase","numerictostring","referencetostring","instanceof",
			"classpath","objectpath","currentdatetime","datetime","microsecondtotimestamp","microsecondtointerval","::",
			"/","+","-","..","||",null
	};
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("TokenType ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}
	
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}

	@Test
	public final void test() {
		assertEquals(name.length,TokenType.values().length);
		for(int i = 0; i < name.length; i++){
			TokenType t = TokenType.valueOf(name[i]);
			assertNotNull(t);
			assertEquals(value[i],t.getValue());
		}
	}

}
