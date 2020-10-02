/**
 * Copyright 2020 Hewlett Packard Enterprise Development LP
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *    
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * Created Mar 4, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests for encoding
 * @author Sharad Singhal
 *
 */
public class TagEncodingTest {
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("TagEncodingTest");
	}
	
	@AfterClass
	public static void tearDownAfter() throws Exception {
		System.out.print("\n");
	}
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
