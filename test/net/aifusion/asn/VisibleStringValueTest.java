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

import net.aifusion.asn.VisibleStringValue;

/**
 * Class to test Visible String Value
 * @author Sharad Singhal
 *
 */
public class VisibleStringValueTest {
	
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
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			VisibleStringValue v1 = new VisibleStringValue(s1);
			for(String s2 : tests) {
				VisibleStringValue v2 = new VisibleStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1.hashCode(),v2.hashCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x1A,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertEquals("[VISIBLE_STRING] ::= \"Jones\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#VisibleStringValue(java.lang.String)}.
	 */
	@Test
	public void testVisibleStringValue() {
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x1A,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		VisibleStringValue v = VisibleStringValue.create(buffer, buffer.length, 0);
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			VisibleStringValue v1 = new VisibleStringValue(s1);
			for(String s2 : tests) {
				VisibleStringValue v2 = new VisibleStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1,v2);
				else assertNotSame(v1, v2);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#toString()}.
	 */
	@Test
	public void testToString() {
		VisibleStringValue v1 = new VisibleStringValue("Jones");
		assertEquals("Jones",v1.toString());
	}

}
