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

import net.aifusion.asn.OidValue;

/**
 * Test for OID value
 * @author Sharad Singhal
 */
public class OidValueTest {
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
	 * Test method for {@link net.aifusion.asn.OidValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		for(long [] v1 : values) {
			OidValue o1 = new OidValue(v1);
			for(long [] v2 : values) {
				OidValue o2 = new OidValue(v2);
				if(Arrays.equals(v1, v2)){
					assertEquals(o1.hashCode(),o2.hashCode());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x06, 0x05, 0x29, 0x02,(byte) 0x081, (byte) 0x0A3, 0x7B};
		OidValue v = new OidValue(new long[] {1, 1, 2, 20987});
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		OidValue v = new OidValue(new long[] {1, 1, 2, 20987});
		assertEquals("[OBJECT_IDENTIFIER]  ::= {1 1 2 20987}\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#OidValue(long[])}.
	 */
	@Test
	public void testOidValue() {
		for(long [] v : values) {
			OidValue o = new OidValue(v);
			assertNotNull(o);
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x06, 0x05, 0x29, 0x02,(byte) 0x081, (byte) 0x0A3, 0x7B};
		OidValue v = OidValue.create(buffer, buffer.length, 0);
		assertEquals("{1 1 2 20987}",v.toString());
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		for(long [] v : values) {
			OidValue o = new OidValue(v);
			assertArrayEquals(v,o.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		for(long [] v1 : values) {
			OidValue o1 = new OidValue(v1);
			for(long [] v2 : values) {
				OidValue o2 = new OidValue(v2);
				if(Arrays.equals(v1, v2)){
					assertEquals(o1,o2);
				} else {
					assertFalse(o1.equals(o2));
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#toString()}.
	 */
	@Test
	public void testToString() {
		OidValue v = new OidValue(new long[] {1, 1, 2, 20987});
		assertEquals("{1 1 2 20987}",v.toString());
	}

}
