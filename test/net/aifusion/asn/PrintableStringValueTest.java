/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 21, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.PrintableStringValue;

/**
 * Class to test Visible String Value
 * @author Sharad Singhal
 *
 */
public class PrintableStringValueTest {
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
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			PrintableStringValue v1 = new PrintableStringValue(s1);
			for(String s2 : tests) {
				PrintableStringValue v2 = new PrintableStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1.hashCode(),v2.hashCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x13,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		PrintableStringValue v = new PrintableStringValue("Jones");
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		PrintableStringValue v = new PrintableStringValue("Jones");
		assertEquals("[PRINTABLE_STRING] ::= \"Jones\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#PrintableStringValue(java.lang.String)}.
	 */
	@Test
	public void testPrintableStringValue() {
		PrintableStringValue v = new PrintableStringValue("Jim Jones-Zeta Jr.");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		PrintableStringValue v = new PrintableStringValue("Jones");
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x13,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		PrintableStringValue v = PrintableStringValue.create(buffer, buffer.length, 0);
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			PrintableStringValue v1 = new PrintableStringValue(s1);
			for(String s2 : tests) {
				PrintableStringValue v2 = new PrintableStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1,v2);
				else assertNotSame(v1, v2);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#toString()}.
	 */
	@Test
	public void testToString() {
		PrintableStringValue v1 = new PrintableStringValue("Jones");
		assertEquals("Jones",v1.toString());
	}

}
