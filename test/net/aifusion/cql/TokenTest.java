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
 * Test cases for CQL Token. This test depends on TokenTypeTest
 * @author Sharad Singhal
 *
 */
public class TokenTest {
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Token ");
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
	/**
	 * Test method for {@link net.aifusion.cql.Token#Token(net.aifusion.cql.TokenType)}.
	 */
	@Test
	public final void testTokenTokenType() {
		for(int i = 0; i < TokenTypeTest.name.length; i++){
			TokenType t = TokenType.valueOf(TokenTypeTest.name[i]);
			try {
				Token token = new Token(t);
				assertNotNull(token);
				assertEquals(t.getValue(),token.value());
			} catch (Exception e){
				assertNull(t.getValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Token#Token(net.aifusion.cql.TokenType, java.lang.String)}.
	 */
	@Test
	public final void testTokenTokenTypeString() {
		for(int i = 0; i < TokenTypeTest.name.length; i++){
			TokenType t = TokenType.valueOf(TokenTypeTest.name[i]);
			// System.out.println("Checking TokenType "+t);
			try {
				Token token = new Token(t,"foo");
				assertNotNull(token);
				assertEquals("foo",token.value());
				assertNull(t.getValue());
			} catch (Exception e){
				// System.out.println(e.toString());
				assertNotNull(t.getValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Token#is(net.aifusion.cql.TokenType)}.
	 */
	@Test
	public final void testIs() {
		for(int i = 0; i < TokenTypeTest.name.length; i++){
			TokenType ti = TokenType.valueOf(TokenTypeTest.name[i]);
			assertNotNull(ti);
			Token token = ti.getValue() == null ? new Token(ti,"foo") : new Token(ti);
			assertNotNull(token);
			for(int j = 0; j < TokenTypeTest.name.length; j++){
				TokenType tj = TokenType.valueOf(TokenTypeTest.name[j]);
				boolean match = token.is(tj);
				// System.out.println("Testing Token "+token+" against type "+tj+" match = "+match);
				if(i == j){
					assertTrue(match);
				} else {
					switch(ti){
					case EQUALS:	// these operators match COMPARISON
					case NE:
					case GT:
					case GE:
					case LE:
					case LT:
					case LIKE:
						if(match) assertTrue(tj == TokenType.COMPARISON);
						break;
					case TRUE:		// these match BOOLEAN
					case FALSE:
						if(match) assertTrue(tj == TokenType.BOOLEAN);
						break;
					case INTEGER:	// these match NUMBER
					case HEX:
					case BINARY:
					case REAL:
						if(match) assertTrue(tj == TokenType.NUMBER);
						break;
					case PLUS:
					case MINUS:
						if(match) assertTrue(tj == TokenType.SIGN);
						break;
					default:
						if(tj == TokenType.IDENTIFIER){
							ti.isIdentifier();
						} else {
							assertFalse(match);
						}
						break;
					}
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Token#type()}.
	 */
	@Test
	public final void testType() {
		for(int i = 0; i < TokenTypeTest.name.length; i++){
			TokenType t = TokenType.valueOf(TokenTypeTest.name[i]);
			Token token = t.getValue() == null ? new Token(t,"foo") : new Token(t);
			assertEquals(t,token.type());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Token#value()}.
	 */
	@Test
	public final void testValue() {
		for(int i = 0; i < TokenTypeTest.name.length; i++){
			TokenType t = TokenType.valueOf(TokenTypeTest.name[i]);
			Token token = t.getValue() == null ? new Token(t,"foo") : new Token(t);
			assertEquals(t.getValue() == null ? "foo" : t.getValue(),token.value());
		}
	}

}
