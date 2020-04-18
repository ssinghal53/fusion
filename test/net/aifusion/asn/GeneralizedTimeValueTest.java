/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 17, 2020 by sharad
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.time.ZonedDateTime;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.asn.GeneralizedTimeValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Test class for GeneralizedTimeValue
 * @author Sharad Singhal
 *
 */
public class GeneralizedTimeValueTest {
	static String [] testCases = {
			"19851106210627",
			"19851106210627Z",
			"19851106210627+0500",
			"19851106210627.3",
			"19851106210627.3Z",
			"19851106210627.3+0500"};
	static boolean [] isValid = {true,true,true,true,true,true};
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
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		assertEquals(testCases.length,isValid.length);
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
		assertArrayEquals(new byte [] {0x18, 0x0F, 0x31, 0x39, 0x38, 0x35, 0x31, 0x31, 0x30, 0x36, 0x32, 0x31, 0x30, 0x36, 0x32, 0x37, 0x5A},v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
		assertEquals("[GENERALIZED_TIME] ::= \"19851106210627Z\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#GeneralizedTimeValue(java.lang.String)}.
	 */
	@Test
	public void testGeneralizedTimeValue() {
		for(int i = 0; i < testCases.length; i++) {
			// System.out.println(testCases[i]);
			if(isValid[i]) {
				GeneralizedTimeValue v = new GeneralizedTimeValue(testCases[i]);
				assertNotNull(v);
			} else {
				try {
					new GeneralizedTimeValue(testCases[i]);
					fail(testCases[i]+" : Should not succeed");
				} catch (ModelException e) {
					assertEquals(4,e.getReason().getCode());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = new byte [] {0x18, 0x0F, 0x31, 0x39, 0x38, 0x35, 0x31, 0x31, 0x30, 0x36, 0x32, 0x31, 0x30, 0x36, 0x32, 0x37, 0x5A};
		GeneralizedTimeValue v = GeneralizedTimeValue.create(buffer, buffer.length, 0);
		assertArrayEquals(buffer,v.getEncodedValue());
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		try {
			ZonedDateTime date = ZonedDateTime.parse("1985-11-06T21:06:27+00:00");
			GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
			assertEquals(date,v.getValue());
		} catch (Exception e) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Invalid date format ");
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#toString()}.
	 */
	@Test
	public void testToString() {
		GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
		assertEquals("19851106210627Z",v.toString());
	}
}
