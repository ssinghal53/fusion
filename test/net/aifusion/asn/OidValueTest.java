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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test for OID value
 * @author Sharad Singhal
 */
public class OidValueTest {
	static long [][] values = {
			{1,0,1,0},
			{1,0,1,1},
			{1,0,0,0}
	};
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("OidValueTest");
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
	 * Test method for {@link net.aifusion.asn.OidValue#hashCode()}.
	 */
	@Test
	public void testHashCode() {
		for(long [] v1 : values) {
			OidValue o1 = new OidValue(v1);
			for(long [] v2 : values) {
				OidValue o2 = new OidValue(v2);
				if(Arrays.equals(v1, v2)){
					assertEquals(o1.hashCode(),o2.hashCode());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		byte [] expect = {0x06, 0x05, 0x29, 0x02,(byte) 0x081, (byte) 0x0A3, 0x7B};
		OidValue v = new OidValue(new long[] {1, 1, 2, 20987});
		assertArrayEquals(expect,v.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		OidValue v = new OidValue(new long[] {1, 1, 2, 20987});
		assertEquals("[OBJECT_IDENTIFIER]  ::= {1 1 2 20987}\n",v.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#OidValue(long[])}.
	 */
	@Test
	public void testOidValue() {
		for(long [] v : values) {
			OidValue o = new OidValue(v);
			assertNotNull(o);
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = {0x06, 0x05, 0x29, 0x02,(byte) 0x081, (byte) 0x0A3, 0x7B};
		OidValue v = OidValue.create(buffer, buffer.length, 0);
		assertEquals("{1 1 2 20987}",v.toString());
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#getValue()}.
	 */
	@Test
	public void testGetValue() {
		for(long [] v : values) {
			OidValue o = new OidValue(v);
			assertArrayEquals(v,o.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		for(long [] v1 : values) {
			OidValue o1 = new OidValue(v1);
			for(long [] v2 : values) {
				OidValue o2 = new OidValue(v2);
				if(Arrays.equals(v1, v2)){
					assertEquals(o1,o2);
				} else {
					assertFalse(o1.equals(o2));
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.OidValue#toString()}.
	 */
	@Test
	public void testToString() {
		OidValue v = new OidValue(new long[] {1, 1, 2, 20987});
		assertEquals("{1 1 2 20987}",v.toString());
	}

}
