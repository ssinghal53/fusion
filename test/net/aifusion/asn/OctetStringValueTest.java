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
 * Created Mar 10, 2018 by sharad
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Unit tests for OctetString
 * @author Sharad Singhal
 */
public class OctetStringValueTest {
	static byte [] input = {-128,3,5,7,127};
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("OctetStringValueTest");
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
	 * Test method for {@link net.aifusion.asn.OctetStringValue#getEncoded()}.
	 */
	@Test
	public final void testGetEncoded() {
		OctetStringValue s = new OctetStringValue(input);
		assertArrayEquals(new byte[] {4,5,-128,3,5,7,127},s.getEncodedValue());
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#create(byte[], int, int)};
	 */
	@Test
	public final void testCreate() {
		byte [] buffer = new byte[] {4,5,-128,3,5,7,127};
		OctetStringValue v = OctetStringValue.create(buffer, buffer.length, 0);
		assertArrayEquals(buffer, v.getEncodedValue());
		assertArrayEquals(input,v.getValue());
	}
	
	@Test
	public final void testToAsnValue() {
		OctetStringValue s = new OctetStringValue(input);
		assertEquals("[OCTET_STRING] ::= { -128 3 5 7 127 }\n",s.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#OctetString(byte[])}.
	 */
	@Test
	public final void testOctetString() {
		OctetStringValue s = new OctetStringValue(input);
		assertNotNull(s);
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		OctetStringValue s = new OctetStringValue(input);
		assertArrayEquals(input,s.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		OctetStringValue s1 = new OctetStringValue(input);
		OctetStringValue s2 = new OctetStringValue(input);
		OctetStringValue s3 = new OctetStringValue(new byte[] {54});
		assertEquals(s1,s2);
		assertNotEquals(s1,s3);
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#toString()}.
	 */
	@Test
	public final void testToString() {
		OctetStringValue s = new OctetStringValue(input);
		assertEquals("{ -128 3 5 7 127 }",s.toString());
	}

}
