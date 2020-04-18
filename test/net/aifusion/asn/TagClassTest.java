/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 10, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.asn.TagClass;

/**
 * Unit Tests for TagClass
 * @author Sharad Singhal
 *
 */
public class TagClassTest {
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
		assertEquals(4,TagClass.values().length);
	}

	/**
	 * Test method for {@link net.aifusion.asn.TagClass#getTagClass()}.
	 */
	@Test
	public final void testGetTagClass() {
		for(TagClass c : TagClass.values()) {
			switch(c) {
			case APPLICATION:
				assertEquals(0x040,c.getTagClass());
				break;
			case CONTEXT_SPECIFIC:
				assertEquals(0x080,c.getTagClass());
				break;
			case PRIVATE:
				assertEquals(0x0C0,c.getTagClass());
				break;
			case UNIVERSAL:
				assertEquals(0,c.getTagClass());
				break;
			default:
				fail("Unimplemented test case for "+c);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.TagClass#getTagClass(long)}.
	 */
	@Test
	public final void testGetTagClassLong() {
		for(int i : new int[] {0,0x040,0x080,0x0C0}) {
			switch(i) {
			case 0:
				assertEquals(TagClass.UNIVERSAL,TagClass.getTagClass((byte) i));
				break;
			case 0x40:
				assertEquals(TagClass.APPLICATION,TagClass.getTagClass((byte) i));
				break;
			case 0x80:
				assertEquals(TagClass.CONTEXT_SPECIFIC,TagClass.getTagClass((byte) i));
				break;
			case 0xC0:
				assertEquals(TagClass.PRIVATE,TagClass.getTagClass((byte) i));
				break;
			default:
				fail("Unimplemented test case for "+i);
			}
		}
	}
}
