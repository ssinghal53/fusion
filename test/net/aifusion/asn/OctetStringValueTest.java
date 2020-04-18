/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 10, 2018 by sharad
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.OctetStringValue;

/**
 * Unit tests for OctetString
 * @author Sharad Singhal
 */
public class OctetStringValueTest {
	static byte [] input = {-128,3,5,7,127};
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
	 * Test method for {@link net.aifusion.asn.OctetStringValue#getEncoded()}.
	 */
	@Test
	public final void testGetEncoded() {
		OctetStringValue s = new OctetStringValue(input);
		assertArrayEquals(new byte[] {4,5,-128,3,5,7,127},s.getEncodedValue());
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#create(byte[], int, int)};
	 */
	@Test
	public final void testCreate() {
		byte [] buffer = new byte[] {4,5,-128,3,5,7,127};
		OctetStringValue v = OctetStringValue.create(buffer, buffer.length, 0);
		assertArrayEquals(buffer, v.getEncodedValue());
		assertArrayEquals(input,v.getValue());
	}
	
	@Test
	public final void testToAsnValue() {
		OctetStringValue s = new OctetStringValue(input);
		assertEquals("[OCTET_STRING] ::= { -128 3 5 7 127 }\n",s.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#OctetString(byte[])}.
	 */
	@Test
	public final void testOctetString() {
		OctetStringValue s = new OctetStringValue(input);
		assertNotNull(s);
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		OctetStringValue s = new OctetStringValue(input);
		assertArrayEquals(input,s.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		OctetStringValue s1 = new OctetStringValue(input);
		OctetStringValue s2 = new OctetStringValue(input);
		OctetStringValue s3 = new OctetStringValue(new byte[] {54});
		assertEquals(s1,s2);
		assertNotEquals(s1,s3);
	}

	/**
	 * Test method for {@link net.aifusion.asn.OctetStringValue#toString()}.
	 */
	@Test
	public final void testToString() {
		OctetStringValue s = new OctetStringValue(input);
		assertEquals("{ -128 3 5 7 127 }",s.toString());
	}

}
