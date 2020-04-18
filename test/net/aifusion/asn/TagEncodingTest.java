/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 4, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.asn.TagEncoding;

/**
 * Tests for encoding
 * @author Sharad Singhal
 *
 */
public class TagEncodingTest {
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
		assertEquals(2,TagEncoding.values().length);
	}

	/**
	 * Test method for {@link net.aifusion.asn.TagEncoding#getTagEncoding()}.
	 */
	@Test
	public void testGetEncoding() {
		for(TagEncoding e : TagEncoding.values()) {
			switch(e) {
			case CONSTRUCTED:
				assertEquals(0x020,e.getTagEncoding());
				break;
			case PRIMITIVE:
				assertEquals(0,e.getTagEncoding());
				break;
			default:
				fail("Unknown encoding "+e);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.TagEncoding#isPrimitive(long)}.
	 */
	@Test
	public void testIsPrimitive() {
		assertTrue(TagEncoding.isPrimitive(0));
		assertFalse(TagEncoding.isPrimitive(0x20));
	}

	/**
	 * Test method for {@link net.aifusion.asn.TagEncoding#isConstructed(long)}.
	 */
	@Test
	public void testIsConstructed() {
		assertTrue(TagEncoding.isConstructed(0x20));
		assertFalse(TagEncoding.isConstructed(0));
	}

}
