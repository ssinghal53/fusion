/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 16, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.BMPStringValue;

/**
 * Class to test BMPString
 * @author Sharad Singhal
 */
public class BMPStringValueTest {
	
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
	 * Test method for {@link net.aifusion.asn.BMPStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expected = {0x1E, 0x16, 0x00, 0x54, 0x00, 0x65, 0x00, 0x73, 0x00, 0x74, 0x00, 0x20, 0x00, 0x53, 0x00, 0x74, 0x00, 0x72, 0x00, 0x69, 0x00, 0x6E, 0x00, 0x67};
		BMPStringValue v = new BMPStringValue("Test String");
		// System.out.println(Utilities.toHex(v.getEncodedValue()));
		assertArrayEquals(expected,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		BMPStringValue v = new BMPStringValue("Test String");
		assertEquals("[BMP_STRING] ::= \"Test String\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#BMPStringValue(java.lang.String)}.
	 */
	@Test
	public void testBMPStringValue() {
		BMPStringValue v = new BMPStringValue("Test String");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		BMPStringValue v = new BMPStringValue("Test String");
		assertEquals("Test String",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x1E, 0x16, 0x00, 0x54, 0x00, 0x65, 0x00, 0x73, 0x00, 0x74, 0x00, 0x20, 0x00, 0x53, 0x00, 0x74, 0x00, 0x72, 0x00, 0x69, 0x00, 0x6E, 0x00, 0x67};
		BMPStringValue v = BMPStringValue.create(buffer, buffer.length, 0);
		assertNotNull(v);
		assertEquals("Test String",v.getValue());
	}

}
