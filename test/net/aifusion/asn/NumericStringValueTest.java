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
import org.junit.Before;
import org.junit.Test;

/**
 * Test cases for NumericStringValue
 * @author Sharad Singhal
 */
public class NumericStringValueTest {
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
	 * Test method for {@link net.aifusion.asn.NumericStringValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x12, 0x08, 0x33, 0x31, 0x32, 0x35, 0x30, 0x39, 0x38, 0x30};
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
		assertEquals("[NUMERIC_STRING] ::= \"31250980\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#NumericStringValue(java.lang.String)}.
	 */
	@Test
	public void testNumericStringValue() {
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x12, 0x08, 0x33, 0x31, 0x32, 0x35, 0x30, 0x39, 0x38, 0x30};
		NumericStringValue v = NumericStringValue.create(buffer, buffer.length, 0);
		assertEquals("31250980",v.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.NumericStringValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		NumericStringValue v = new NumericStringValue("31250980");
		assertNotNull(v);
		assertEquals("31250980",v.getValue());
	}

}
