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
import org.junit.Before;
import org.junit.Test;

/**
 * Unit Tests for Integer Value
 * @author Sharad Singhal
 *
 */
public class IntegerValueTest {
	private static long [] values = new long[] {0,1,Long.MAX_VALUE,Long.MIN_VALUE};
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
	 * Test method for {@link net.aifusion.asn.IntegerValue#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		for(int i = 0; i < values.length; i++) {
			IntegerValue v1 = new IntegerValue(values[i]);
			for(int j = 0; j < values.length; j++) {
				IntegerValue v2 = new IntegerValue(values[j]);
				if(v1.getValue() == v2.getValue()) assertEquals(v1.hashCode(),v2.hashCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.IntegerValue#getEncoded()}.
	 */
	@Test
	public final void testGetEncoded() {
		byte [][] expected = {
				{2,1,0},
				{2,1,1},
				{2,8,127,-1,-1,-1,-1,-1,-1,-1},
				{2,8,-128,0,0,0,0,0,0,0}
		};
		assertEquals(expected.length,values.length);
		for(int i = 0; i < values.length; i++) {
			IntegerValue v = new IntegerValue(values[i]);
			assertNotNull(v);
			assertArrayEquals(expected[i],v.getEncodedValue());
			
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.IntegerValue#IntegerValue(long)}.
	 */
	@Test
	public final void testIntegerValue() {
		for(int i = 0; i < values.length; i++) {
			IntegerValue v = new IntegerValue(values[i]);
			assertNotNull(v);
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.IntegerValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(int i = 0; i < values.length; i++) {
			IntegerValue v = new IntegerValue(values[i]);
			assertEquals(values[i],v.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.IntegerValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		IntegerValue v1 = new IntegerValue(0);
		assertFalse(v1.equals(null));
		assertTrue(v1.equals(new IntegerValue(0)));
		assertFalse(v1.equals(new IntegerValue(-1)));
	}

	/**
	 * Test method for {@link net.aifusion.asn.IntegerValue#toString()}.
	 */
	@Test
	public final void testToString() {
		String [] expected = new String[]{"0","1","9223372036854775807","-9223372036854775808"};
		assertEquals(expected.length,values.length);
		for(int i = 0; i < values.length; i++) {
			IntegerValue v = new IntegerValue(values[i]);
			assertEquals(expected[i],v.toString());
		}
	}

}
