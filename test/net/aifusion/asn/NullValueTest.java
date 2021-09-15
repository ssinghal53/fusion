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
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test to check the ASN.1 Null value
 * @author Sharad Singhal
 *
 */
public class NullValueTest {
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("NullValueTest");
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
	 * Test method for {@link net.aifusion.asn.NullValue#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		assertEquals(7,new NullValue().hashCode());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#getEncodedValue()}.
	 */
	@Test
	public final void testGetEncoded() {
		assertArrayEquals(new byte[] {5,0},new NullValue().getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#NullValue()}.
	 */
	@Test
	public final void testNull() {
		NullValue n = new NullValue();
		assertNotNull(n);
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		NullValue n = new NullValue();
		assertNull(n.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		NullValue n = new NullValue();
		NullValue n2 = new NullValue();
		assertEquals(n,n);
		assertEquals(n,n2);
		assertNotEquals(null, n);
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#toString()}.
	 */
	@Test
	public final void testToString() {
		assertEquals("NULL",new NullValue().toString());
	}

}
