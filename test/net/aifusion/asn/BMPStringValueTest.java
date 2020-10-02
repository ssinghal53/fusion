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
 * Created Mar 16, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test BMPString
 * @author Sharad Singhal
 */
public class BMPStringValueTest {
	
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("BMPStringValueTest");
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
	 * Test method for {@link net.aifusion.asn.BMPStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expected = {0x1E, 0x16, 0x00, 0x54, 0x00, 0x65, 0x00, 0x73, 0x00, 0x74, 0x00, 0x20, 0x00, 0x53, 0x00, 0x74, 0x00, 0x72, 0x00, 0x69, 0x00, 0x6E, 0x00, 0x67};
		BMPStringValue v = new BMPStringValue("Test String");
		// System.out.println(Utilities.toHex(v.getEncodedValue()));
		assertArrayEquals(expected,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		BMPStringValue v = new BMPStringValue("Test String");
		assertEquals("[BMP_STRING] ::= \"Test String\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#BMPStringValue(java.lang.String)}.
	 */
	@Test
	public void testBMPStringValue() {
		BMPStringValue v = new BMPStringValue("Test String");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		BMPStringValue v = new BMPStringValue("Test String");
		assertEquals("Test String",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.BMPStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x1E, 0x16, 0x00, 0x54, 0x00, 0x65, 0x00, 0x73, 0x00, 0x74, 0x00, 0x20, 0x00, 0x53, 0x00, 0x74, 0x00, 0x72, 0x00, 0x69, 0x00, 0x6E, 0x00, 0x67};
		BMPStringValue v = BMPStringValue.create(buffer, buffer.length, 0);
		assertNotNull(v);
		assertEquals("Test String",v.getValue());
	}

}
