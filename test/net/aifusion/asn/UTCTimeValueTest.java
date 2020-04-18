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
 * Created Mar 20, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.time.ZonedDateTime;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test UTCTimeValue
 * @author sharad
 */
public class UTCTimeValueTest {
	// TODO: These are legal patterns. Fix when GeneralizedTimeValue is fixed
	static String [] testCases = {
			"231106210627",
			"851106210627",
			"851106210627Z",
			"851106210627+0500",
			"851106210627.3",
			"851106210627.3Z",
	"851106210627.3+0500"};
	static ZonedDateTime [] expect = {
			ZonedDateTime.parse("2023-11-06T21:06:27+00:00"),
			ZonedDateTime.parse("1985-11-06T21:06:27+00:00"),
			ZonedDateTime.parse("1985-11-06T21:06:27+00:00"),
			ZonedDateTime.parse("1985-11-06T21:06:27+05:00"),
			ZonedDateTime.parse("1985-11-06T21:06:27.3+00:00"),
			ZonedDateTime.parse("1985-11-06T21:06:27.3+00:00"),
			ZonedDateTime.parse("1985-11-06T21:06:27.3+05:00")
	};

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		assertEquals(testCases.length,expect.length);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
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
	 * Test method for {@link net.aifusion.asn.UTCTimeValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		UTCTimeValue v = new UTCTimeValue("851106210627Z");
		assertArrayEquals(new byte [] {0x17, 0x0D, 0x38, 0x35, 0x31, 0x31, 0x30, 0x36, 0x32, 0x31, 0x30, 0x36, 0x32, 0x37, 0x5A},v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.UTCTimeValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		UTCTimeValue v = new UTCTimeValue("851106210627Z");
		assertEquals("[UTC_TIME] ::= \"851106210627Z\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.UTCTimeValue#UTCTimeValue(java.lang.String)}.
	 */
	@Test
	public void testUTCTimeValue() {
		for(int i = 0; i < testCases.length; i++) {
			UTCTimeValue v = new UTCTimeValue(testCases[i]);
			assertNotNull(v);
			assertEquals(expect[i],v.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.UTCTimeValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = new byte [] {0x17, 0x0D, 0x38, 0x35, 0x31, 0x31, 0x30, 0x36, 0x32, 0x31, 0x30, 0x36, 0x32, 0x37, 0x5A};
		UTCTimeValue v = UTCTimeValue.create(buffer, buffer.length, 0);
		assertArrayEquals(buffer,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.UTCTimeValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		for(int i = 0; i < testCases.length; i++) {
			UTCTimeValue v = new UTCTimeValue(testCases[i]);
			assertEquals(expect[i],v.getValue());
		}
	}

}
