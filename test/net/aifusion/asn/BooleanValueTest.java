/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 10, 2018 by sharad
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.aifusion.asn.BooleanValue;
/**
 * Unit test for Boolean Values
 * @author Sharad Singhal
 *
 */
public class BooleanValueTest {
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
	 * Test method for {@link net.aifusion.asn.BooleanValue#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertEquals(Boolean.valueOf(b).hashCode(),bv.hashCode());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#getEncoded()}.
	 */
	@Test
	public final void testGetEncoded() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertArrayEquals(b ? new byte[] {1,1,-1} : new byte[] {1,1,0},bv.getEncodedValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#Asn1Boolean(boolean)}.
	 */
	@Test
	public final void testAsn1Boolean() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertNotNull(bv);
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.ast.BooleanValue#
	 */
	@Test
	public final void testCreate() {
		byte [] buffer = {1,1,0};
		BooleanValue v = BooleanValue.create(buffer, buffer.length, 0);
		assertArrayEquals(buffer,v.getEncodedValue());
		assertEquals(false,v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertEquals(b,bv.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#toString()}.
	 */
	@Test
	public final void testToString() {
		for(boolean b : new boolean[] {true,false}){
			BooleanValue bv = new BooleanValue(b);
			assertEquals(b ? "TRUE" : "FALSE",bv.toString());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		BooleanValue bt1 = new BooleanValue(true);
		BooleanValue bt2 = new BooleanValue(true);
		BooleanValue bf = new BooleanValue(false);
		assertFalse(bt1.equals(null));
		assertFalse(bt1.equals(bf));
		assertTrue(bt1.equals(bt2));
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.BooleanValue#toAsnString(String)}.
	 */
	@Test
	public final void  testGenAsnString() {
		BooleanValue b = new BooleanValue(true);
		assertEquals("[BOOLEAN] ::= TRUE\n",b.toAsnString(""));
	}

}
