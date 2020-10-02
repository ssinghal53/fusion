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
 * Created Mar 19, 2020 by Sharad Singhal
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
 * Test cases for IA5StringValue
 * @author Sharad Singhal
 */
public class IA5StringValueTest {
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("IA5StringValueTest");
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
	 * Test method for {@link net.aifusion.asn.IA5StringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x16, 0x0B, 0x54, 0x65, 0x73, 0x74, 0x20, 0x53, 0x74, 0x72, 0x69, 0x6E, 0x67};
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
		assertEquals("[IA5_STRING]  ::= \"Test String\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#IA5StringValue(java.lang.String)}.
	 */
	@Test
	public void testIA5StringValue() {
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x16, 0x0B, 0x54, 0x65, 0x73, 0x74, 0x20, 0x53, 0x74, 0x72, 0x69, 0x6E, 0x67};
		IA5StringValue v = IA5StringValue.create(buffer, buffer.length, 0);
		assertEquals("Test String",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.IA5StringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		IA5StringValue v = new IA5StringValue("Test String");
		assertNotNull(v);
		assertEquals("Test String",v.getValue());
	}

}
