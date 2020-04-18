/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 21, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.RelativeOidValue;

/**
 * Test for OID value
 * @author Sharad Singhal
 */
public class RelativeOidValueTest {
	static long [][] values = {
			{1,0,1,0},
			{1,0,1,1},
			{1,0,0,0}
	};
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
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		for(long [] v1 : values) {
			RelativeOidValue o1 = new RelativeOidValue(v1);
			for(long [] v2 : values) {
				RelativeOidValue o2 = new RelativeOidValue(v2);
				if(Arrays.equals(v1, v2)){
					assertEquals(o1.hashCode(),o2.hashCode());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x0D, 0x06, 0x01, 0x01, 0x02,(byte) 0x081, (byte) 0x0A3, 0x7B};
		RelativeOidValue v = new RelativeOidValue(new long[] {1, 1, 2, 20987});
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		RelativeOidValue v = new RelativeOidValue(new long[] {1, 1, 2, 20987});
		assertEquals("[RELATIVE_OID] ::= {1 1 2 20987}\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#RelativeOidValue(long[])}.
	 */
	@Test
	public void testRelativeOidValue() {
		for(long [] v : values) {
			RelativeOidValue o = new RelativeOidValue(v);
			assertNotNull(o);
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x0D, 0x06, 0x01, 0x01, 0x02,(byte) 0x081, (byte) 0x0A3, 0x7B};
		RelativeOidValue v = RelativeOidValue.create(buffer, buffer.length, 0);
		assertEquals("{1 1 2 20987}",v.toString());
	}

	/**
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		for(long [] v : values) {
			RelativeOidValue o = new RelativeOidValue(v);
			assertArrayEquals(v,o.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		for(long [] v1 : values) {
			RelativeOidValue o1 = new RelativeOidValue(v1);
			for(long [] v2 : values) {
				RelativeOidValue o2 = new RelativeOidValue(v2);
				if(Arrays.equals(v1, v2)){
					assertEquals(o1,o2);
				} else {
					assertFalse(o1.equals(o2));
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.RelativeOidValue#toString()}.
	 */
	@Test
	public void testToString() {
		RelativeOidValue v = new RelativeOidValue(new long[] {1, 1, 2, 20987});
		assertEquals("{1 1 2 20987}",v.toString());
	}

}
