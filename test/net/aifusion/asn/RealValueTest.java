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
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.metamodel.ModelException;

/**
 * Unit Tests for Real values
 * @author Sharad Singhal
 *
 */
public class RealValueTest {
	private static double [] values = new double[] {0,1,Double.MAX_VALUE,Double.MIN_VALUE,Double.MIN_NORMAL, Double.NEGATIVE_INFINITY,Double.POSITIVE_INFINITY};
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
	 * Test method for {@link net.aifusion.asn.RealValue#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		for(int i = 0; i < values.length; i++) {
			RealValue v = new RealValue(values[i]);
			assertNotNull(v);
			assertEquals(Double.valueOf(values[i]).hashCode(),v.hashCode());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.RealValue#getEncoded()}.
	 */
	@Test
	public final void testGetEncoded() {
		byte [][] expect = new byte[][] {
			{9,0},{9,3,(byte) 0x80,0,1},
			{9, 10, -127, 3, -53, 31, -1, -1, -1, -1, -1, -1,},
			{9,4,-127, -5, -51, 1},
			{9,4,-127,-4,2,1},
			{9,1,65},{9,1,64}
		};
		assertEquals(expect.length,values.length);
		for(int i = 0; i < values.length; i++) {
			RealValue v = new RealValue(values[i]);
			// System.out.println(AsnValue.toHex(v.getEncoded()));
			assertArrayEquals(expect[i],v.getEncodedValue());
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.RealValue#RealValue(double)}.
	 */
	@Test
	public final void testRealValue() {
		for(int i = 0; i < values.length; i++) {
			RealValue v = new RealValue(values[i]);
			assertNotNull(v);
		}
		try {
			new RealValue(Double.NaN);
			fail("Should not succeed");
		} catch(ModelException e) {
			assertEquals(4,e.getReason().getCode());
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.RealValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(int i = 0; i < values.length; i++) {
			RealValue v = new RealValue(values[i]);
			assertNotNull(v);
			assertEquals(values[i],v.getValue(),Double.MIN_NORMAL);
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.RealValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		RealValue v1 = new RealValue(1);
		RealValue v2 = new RealValue(1);
		RealValue v3 = new RealValue(2);
		assertFalse(v1.equals(null));
		assertFalse(v1.equals(v3));
		assertTrue(v1.equals(v2));
	}

	/**
	 * Test method for {@link net.aifusion.asn.RealValue#toString()}.
	 */
	@Test
	public final void testToString() {
		String[] expect = new String[] {"0.0","1.0","1.7976931348623157E308","4.9E-324","2.2250738585072014E-308","-Infinity","Infinity"};
		assertEquals(expect.length,values.length);
		for(int i = 0; i < values.length; i++) {
			RealValue v = new RealValue(values[i]);
			// System.out.println(v.toString());
			assertEquals(expect[i],v.toString());
		}
	}
}
