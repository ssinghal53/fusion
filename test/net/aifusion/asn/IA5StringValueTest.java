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

import net.aifusion.asn.IA5StringValue;
/**
 * Test cases for IA5StringValue
 * @author Sharad Singhal
 */
public class IA5StringValueTest {
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
	 * Test method for {@link net.aifusion.asn.IA5StringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x16, 0x0B, 0x54, 0x65, 0x73, 0x74, 0x20, 0x53, 0x74, 0x72, 0x69, 0x6E, 0x67};
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
		assertEquals("[IA5_STRING]  ::= \"Test String\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#IA5StringValue(java.lang.String)}.
	 */
	@Test
	public void testIA5StringValue() {
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x16, 0x0B, 0x54, 0x65, 0x73, 0x74, 0x20, 0x53, 0x74, 0x72, 0x69, 0x6E, 0x67};
		IA5StringValue v = IA5StringValue.create(buffer, buffer.length, 0);
		assertEquals("Test String",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
		assertEquals("Test String",v.getValue());
	}

}
