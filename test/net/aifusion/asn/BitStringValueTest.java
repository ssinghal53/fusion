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

import net.aifusion.asn.BitStringValue;

/**
 * Class to test BitStrings
 * @author Sharad Singhal
 */
public class BitStringValueTest {
	static String [] input = {"'00001010001110110101111100101001000111001101'B","'0A3B5F291CD'H"};
	
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
	 * Test method for {@link net.aifusion.asn.BitStringValue#getEncoded()}.
	 */
	@Test
	public final void testGetEncoded() {
		byte [] expected = new byte[] {3,7,4,10,59,95,41,28,-48};
		for(int i = 0; i < input.length; i++) {
			BitStringValue v = new BitStringValue(input[i]);
			assertArrayEquals(expected,v.getEncodedValue());
		}
		BitStringValue v = new BitStringValue("'00'B");
		assertArrayEquals(new byte[] {3,1,0},v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#BitString(java.lang.String)}.
	 */
	@Test
	public final void testBitString() {
		for(String s : input) {
			BitStringValue v = new BitStringValue(s);
			assertNotNull(v);
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		BitStringValue bs = new BitStringValue(input[0]);
		assertEquals(input[0],bs.toString());
		BitStringValue v = new BitStringValue("'00'B");
		assertEquals("'00'B",v.toString());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		BitStringValue bs1 = new BitStringValue(input[0]);
		BitStringValue bs2 = new BitStringValue(input[1]);
		assertEquals(bs1,bs2);
	}

	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#toString()}.
	 */
	@Test
	public final void testToString() {
		for(String s : input) {
			BitStringValue v = new BitStringValue(s);
			assertEquals(s,v.toString());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#toAsnString(String)}.
	 */
	@Test
	public void testToAsnStringString() {
		BitStringValue v = new BitStringValue(input[0]);
		assertEquals("[BIT_STRING]  ::= '00001010001110110101111100101001000111001101'B\n",v.toAsnString(""));
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#isSet(int)}.
	 */
	@Test
	public void testIsSetInt() {
		BitStringValue v = new BitStringValue(input[0]);
		assertTrue(v.isSet(4));
		assertFalse(v.isSet(0));
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#getLength()}.
	 */
	@Test
	public void testGetLength() {
		BitStringValue v = new BitStringValue(input[0]);
		assertEquals(44,v.getLength());
		v = new BitStringValue("'00'B");
		assertEquals(0,v.getLength());
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.BitStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreate() {
		byte [] buffer = {0x03,0x07,0x04,0x0A,0x3B,0x5F,0x29,0x1C,(byte) 0xD0};
		BitStringValue v = BitStringValue.create(buffer, buffer.length, 0);
		assertEquals(input[0],v.getValue());
	}
}
