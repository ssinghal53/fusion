/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 10, 2018 by sharad
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

import net.aifusion.asn.IntegerValue;

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
