/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Dec 5, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Sharad Singhal
 *
 */
public class OctetStringTest {
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("OctetString ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
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
	 * Test method for {@link net.aifusion.metamodel.OctetString#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		OctetString o = new OctetString("0x3fff7AB6");
		assertNotNull(o);
		assertEquals("0x3fff7ab6".hashCode(),o.hashCode());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.OctetString#OctetString(java.lang.String)}.
	 */
	@Test
	public final void testOctetString() {
		OctetString o = new OctetString("0x3fff7AB6");
		assertNotNull(o);
		String [] invalids = new String [] {"3fff7AB6", "0x3fff7AB", "0x3fff7ABZ" };
		for(String s : invalids){
			try {
				o = new OctetString(s);
				fail("Illegal String succeeded : "+s);
			} catch (ModelException e){
				assertEquals(4,e.getReason().getCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.OctetString#OctetString(byte[])}.
	 */
	@Test
	public final void testOctetStringBytes() {
		OctetString o = new OctetString(new byte[]{1,2,3,10,11,12});
		assertNotNull(o);
		assertEquals("0x0102030a0b0c",o.toString());
		assertArrayEquals(new byte[]{1,2,3,10,11,12},o.getValue());
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.OctetString#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		byte [] expect = new byte [] {Byte.parseByte("63"),Byte.parseByte("-1"),Byte.parseByte("122"),Byte.parseByte("-74")};
		OctetString o = new OctetString("0x3fff7AB6");
		assertNotNull(o);
		assertArrayEquals(expect, o.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.OctetString#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		OctetString o = new OctetString("0x3fff7AB6");
		assertNotNull(o);
		assertEquals(new OctetString("0x3fff7AB6"),o);
		assertNotEquals(new OctetString("0x3fff7AB7"), o);
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.OctetString#toString()}.
	 */
	@Test
	public final void testToString() {
		OctetString o = new OctetString("0x3fff7AB6");
		assertNotNull(o);
		assertEquals("0x3fff7ab6",o.toString());
	}

}
