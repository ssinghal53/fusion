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
 * Created Mar 10, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
/**
 * Unit test for Boolean Values
 * @author Sharad Singhal
 *
 */
public class BooleanValueTest {
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("BooleanValueTest");
	}
	
	@AfterClass
	public static void tearDownAfter() throws Exception {
		System.out.print("\n");
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
	 * Test method for {@link net.aifusion.asn.BooleanValue#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertEquals(Boolean.valueOf(b).hashCode(),bv.hashCode());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#getEncodedValue()}.
	 */
	@Test
	public final void testGetEncoded() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertArrayEquals(b ? new byte[] {1,1,-1} : new byte[] {1,1,0},bv.getEncodedValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#BooleanValue(boolean)}.
	 */
	@Test
	public final void testAsn1Boolean() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertNotNull(bv);
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#create(byte[], int, int)}
	 */
	@Test
	public final void testCreate() {
		byte [] buffer = {1,1,0};
		BooleanValue v = BooleanValue.create(buffer, buffer.length, 0);
		assertArrayEquals(buffer,v.getEncodedValue());
		assertEquals(false,v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertEquals(b,bv.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#toString()}.
	 */
	@Test
	public final void testToString() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertEquals(b ? "TRUE" : "FALSE",bv.toString());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		BooleanValue bt1 = new BooleanValue(true);
		BooleanValue bt2 = new BooleanValue(true);
		BooleanValue bf = new BooleanValue(false);
		assertFalse(bt1.equals(null));
		assertFalse(bt1.equals(bf));
		assertTrue(bt1.equals(bt2));
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#toAsnString(String)}.
	 */
	@Test
	public final void  testGenAsnString() {
		BooleanValue b = new BooleanValue(true);
		assertEquals("[BOOLEAN] ::= TRUE\n",b.toAsnString(""));
	}

}
