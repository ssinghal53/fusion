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
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test Visible String Value
 * @author Sharad Singhal
 *
 */
public class PrintableStringValueTest {
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("PrintableStringValueTest");
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
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			PrintableStringValue v1 = new PrintableStringValue(s1);
			for(String s2 : tests) {
				PrintableStringValue v2 = new PrintableStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1.hashCode(),v2.hashCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x13,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		PrintableStringValue v = new PrintableStringValue("Jones");
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		PrintableStringValue v = new PrintableStringValue("Jones");
		assertEquals("[PRINTABLE_STRING] ::= \"Jones\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#PrintableStringValue(java.lang.String)}.
	 */
	@Test
	public void testPrintableStringValue() {
		PrintableStringValue v = new PrintableStringValue("Jim Jones-Zeta Jr.");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		PrintableStringValue v = new PrintableStringValue("Jones");
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x13,0x05,0x4A,0x6F,0x6E,0x65,0x73};
		PrintableStringValue v = PrintableStringValue.create(buffer, buffer.length, 0);
		assertEquals("Jones",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		String [] tests = {"Jones", "Smith", "James"};
		for(String s1 : tests) {
			PrintableStringValue v1 = new PrintableStringValue(s1);
			for(String s2 : tests) {
				PrintableStringValue v2 = new PrintableStringValue(s2);
				if(s1.equals(s2)) assertEquals(v1,v2);
				else assertNotSame(v1, v2);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.PrintableStringValue#toString()}.
	 */
	@Test
	public void testToString() {
		PrintableStringValue v1 = new PrintableStringValue("Jones");
		assertEquals("Jones",v1.toString());
	}

}
