/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 19, 2020 by sharad
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.math.BigInteger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.LargeInteger;

/**
 * @author sharad
 *
 */
public class LargeIntegerTest {
	static BigInteger bigInt = new BigInteger("123456789012345678901234567890");
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
	 * Test method for {@link net.aifusion.asn.LargeInteger#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x02, 0x0D, 0x01, (byte) 0x8E, (byte)0xE9, 0x0F, (byte)0xF6, (byte)0xC3, 0x73, (byte)0xE0, (byte)0xEE, 0x4E, 0x3F, 0x0A, (byte)0xD2};
		LargeInteger v = new LargeInteger(bigInt);
		assertNotNull(v);
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.LargeInteger#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		LargeInteger v = new LargeInteger(bigInt);
		assertNotNull(v);
		assertEquals("[INTEGER]  ::= 123456789012345678901234567890\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.LargeInteger#LargeInteger(java.math.BigInteger)}.
	 */
	@Test
	public void testLargeInteger() {
		LargeInteger v = new LargeInteger(bigInt);
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.LargeInteger#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x02, 0x0D, 0x01, (byte) 0x8E, (byte)0xE9, 0x0F, (byte)0xF6, (byte)0xC3, 0x73, (byte)0xE0, (byte)0xEE, 0x4E, 0x3F, 0x0A, (byte)0xD2};
		LargeInteger v = LargeInteger.create(buffer, buffer.length, 0);
		assertEquals(bigInt,v.getvalue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.LargeInteger#getvalue()}.
	 */
	@Test
	public void testGetvalue() {
		LargeInteger v = new LargeInteger(bigInt);
		assertEquals(bigInt,v.getvalue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.LargeInteger#toString()}.
	 */
	@Test
	public void testToString() {
		LargeInteger v = new LargeInteger(bigInt);
		assertEquals(bigInt.toString(),v.toString());
	}

}
