/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 10, 2018 by sharad
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.NullValue;

/**
 * Test to check the ASN.1 Null value
 * @author Sharad Singhal
 *
 */
public class NullValueTest {
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
	 * Test method for {@link net.aifusion.asn.NullValue#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		assertEquals(7,new NullValue().hashCode());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#getEncoded()}.
	 */
	@Test
	public final void testGetEncoded() {
		assertArrayEquals(new byte[] {5,0},new NullValue().getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#Null()}.
	 */
	@Test
	public final void testNull() {
		NullValue n = new NullValue();
		assertNotNull(n);
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		NullValue n = new NullValue();
		assertNull(n.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		NullValue n = new NullValue();
		NullValue n2 = new NullValue();
		assertEquals(n,n);
		assertEquals(n,n2);
		assertNotEquals(null, n);
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.NullValue#toString()}.
	 */
	@Test
	public final void testToString() {
		assertEquals("NULL",new NullValue().toString());
	}

}
