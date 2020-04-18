/**
 * Copyright 2020 Hewlett Packard Laboratories
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

import net.aifusion.asn.EnumeratedValue;
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
