/**
 * Copyright 2020 Hewlett Packard Enterprise Development LP
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
 * Created Mar 21, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
/**
 * Class to test an enumerated value
 * @author Sharad Singhal
 *
 */
public class EnumeratedValueTest {
	private static int [] values = new int[] {0,1,Integer.MAX_VALUE};
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
	 * Test method for {@link net.aifusion.asn.EnumeratedValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		for(int i = 0; i < values.length; i++) {
			EnumeratedValue v1 = new EnumeratedValue(values[i]);
			for(int j = 0; j < values.length; j++) {
				EnumeratedValue v2 = new EnumeratedValue(values[j]);
				if(v1.getValue() == v2.getValue()) assertEquals(v1.hashCode(),v2.hashCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.EnumeratedValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [][] expected = {
				{10,1,0},
				{10,1,1},
				{10,4,127,-1,-1,-1}
		};
		assertEquals(expected.length,values.length);
		for(int i = 0; i < values.length; i++) {
			EnumeratedValue v = new EnumeratedValue(values[i]);
			assertNotNull(v);
			assertArrayEquals(expected[i],v.getEncodedValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.EnumeratedValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		EnumeratedValue v = new EnumeratedValue(3);
		assertEquals("[ENUMERATED] ::= 3\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.EnumeratedValue#EnumeratedValue(int)}.
	 */
	@Test
	public void testEnumeratedValue() {
		for(int i : values) {
			EnumeratedValue v = new EnumeratedValue(i);
			assertNotNull(v);
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.EnumeratedValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {10,4,127,-1,-1,-1};
		EnumeratedValue v = EnumeratedValue.create(buffer, buffer.length, 0);
		assertNotNull(v);
		assertEquals(Integer.MAX_VALUE,v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.EnumeratedValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		for(int i = 0; i < values.length; i++) {
			EnumeratedValue v1 = new EnumeratedValue(values[i]);
			for(int j= 0; j < values.length; j++) {
				EnumeratedValue v2 = new EnumeratedValue(values[j]);
				if(v1.getValue() == v2.getValue()) {
					assertEquals(v1,v2);
				} else {
					assertNotEquals(v1, v2);
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.EnumeratedValue#toString()}.
	 */
	@Test
	public void testToString() {
		assertEquals("3",new EnumeratedValue(3).toString());
	}

}
