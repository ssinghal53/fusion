/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 19, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.NumericStringValue;

/**
 * Test cases for NumericStringValue
 * @author Sharad Singhal
 */
public class NumericStringValueTest {
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
	 * Test method for {@link net.aifusion.asn.NumericStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x12, 0x08, 0x33, 0x31, 0x32, 0x35, 0x30, 0x39, 0x38, 0x30};
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
		assertEquals("[NUMERIC_STRING] ::= \"31250980\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#NumericStringValue(java.lang.String)}.
	 */
	@Test
	public void testNumericStringValue() {
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x12, 0x08, 0x33, 0x31, 0x32, 0x35, 0x30, 0x39, 0x38, 0x30};
		NumericStringValue v = NumericStringValue.create(buffer, buffer.length, 0);
		assertEquals("31250980",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
		assertEquals("31250980",v.getValue());
	}

}
