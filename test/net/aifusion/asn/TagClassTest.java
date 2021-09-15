/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
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
 * Created Mar 10, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Unit Tests for TagClass
 * @author Sharad Singhal
 *
 */
public class TagClassTest {
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("TagClassTest");
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
	 * Test method for {@link net.aifusion.asn.TagClass#getTagClass(byte)}.
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
