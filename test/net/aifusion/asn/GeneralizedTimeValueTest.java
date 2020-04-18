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
 * Created Mar 17, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.time.ZonedDateTime;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Test class for GeneralizedTimeValue
 * @author Sharad Singhal
 *
 */
public class GeneralizedTimeValueTest {
	static String [] testCases = {
			"19851106210627",
			"19851106210627Z",
			"19851106210627+0500",
			"19851106210627.3",
			"19851106210627.3Z",
			"19851106210627.3+0500"};
	static boolean [] isValid = {true,true,true,true,true,true};
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
		assertEquals(testCases.length,isValid.length);
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
		assertArrayEquals(new byte [] {0x18, 0x0F, 0x31, 0x39, 0x38, 0x35, 0x31, 0x31, 0x30, 0x36, 0x32, 0x31, 0x30, 0x36, 0x32, 0x37, 0x5A},v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
		assertEquals("[GENERALIZED_TIME] ::= \"19851106210627Z\"\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#GeneralizedTimeValue(java.lang.String)}.
	 */
	@Test
	public void testGeneralizedTimeValue() {
		for(int i = 0; i < testCases.length; i++) {
			// System.out.println(testCases[i]);
			if(isValid[i]) {
				GeneralizedTimeValue v = new GeneralizedTimeValue(testCases[i]);
				assertNotNull(v);
			} else {
				try {
					new GeneralizedTimeValue(testCases[i]);
					fail(testCases[i]+" : Should not succeed");
				} catch (ModelException e) {
					assertEquals(4,e.getReason().getCode());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = new byte [] {0x18, 0x0F, 0x31, 0x39, 0x38, 0x35, 0x31, 0x31, 0x30, 0x36, 0x32, 0x31, 0x30, 0x36, 0x32, 0x37, 0x5A};
		GeneralizedTimeValue v = GeneralizedTimeValue.create(buffer, buffer.length, 0);
		assertArrayEquals(buffer,v.getEncodedValue());
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		try {
			ZonedDateTime date = ZonedDateTime.parse("1985-11-06T21:06:27+00:00");
			GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
			assertEquals(date,v.getValue());
		} catch (Exception e) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Invalid date format ");
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.GeneralizedTimeValue#toString()}.
	 */
	@Test
	public void testToString() {
		GeneralizedTimeValue v = new GeneralizedTimeValue("19851106210627Z");
		assertEquals("19851106210627Z",v.toString());
	}
}
