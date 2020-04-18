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
 * Created Mar 21, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Class to test Visible String Value
 * @author Sharad Singhal
 *
 */
public class VisibleStringValueTest {
	
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
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			VisibleStringValue v1 = new VisibleStringValue(s1);
			for(String s2 : tests) {
				VisibleStringValue v2 = new VisibleStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1.hashCode(),v2.hashCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x1A,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertEquals("[VISIBLE_STRING] ::= \"Jones\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#VisibleStringValue(java.lang.String)}.
	 */
	@Test
	public void testVisibleStringValue() {
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		VisibleStringValue v = new VisibleStringValue("Jones");
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x1A,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		VisibleStringValue v = VisibleStringValue.create(buffer, buffer.length, 0);
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			VisibleStringValue v1 = new VisibleStringValue(s1);
			for(String s2 : tests) {
				VisibleStringValue v2 = new VisibleStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1,v2);
				else assertNotSame(v1, v2);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.VisibleStringValue#toString()}.
	 */
	@Test
	public void testToString() {
		VisibleStringValue v1 = new VisibleStringValue("Jones");
		assertEquals("Jones",v1.toString());
	}

}
