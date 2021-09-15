/**
 * Copyright 2020 Sharad Singhal. All Rights Reserved.
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
 * Created Mar 29, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;

import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.OctetString;

/**
 * Class to test a sequence value
 * @author Sharad Singhal
 */
public class SequenceValueTest {
	static SequenceValue v1,v2;
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("SequenceValueTest");
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
		Vector<AsnValue> values1 = new Vector<AsnValue>();
		Vector<AsnValue> values2 = new Vector<AsnValue>();
		values1.add(new OidValue(new long[] {1L,2L,2950L,1L}));
		values1.add(new IntegerValue(55L));
		values2.add(new OidValue(new long[] {1L,2L,2950L,2L}));
		values2.add(new IntegerValue(55L));
		v1 = new SequenceValue(values1);
		v2 = new SequenceValue(values2);
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.SequenceValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncodedValue() {
		OctetString [] expect = {
				new OctetString("0x300906042a970601020137"),
				new OctetString("0x300906042a970602020137")
		};
		assertArrayEquals(expect[0].getValue(),v1.getEncodedValue());
		assertArrayEquals(expect[1].getValue(),v2.getEncodedValue());
	}

	/**
	 * Test method for {@link net.aifusion.asn.SequenceValue#toAsnString(java.lang.String)}.
	 */
	@Test
	public void testToAsnString() {
		assertEquals("[SEQUENCE] {\n  [OBJECT_IDENTIFIER]  ::= {1 2 2950 1}\n  [INTEGER]  ::= 55\n}\n",v1.toAsnString(""));
		assertEquals("[SEQUENCE] {\n  [OBJECT_IDENTIFIER]  ::= {1 2 2950 2}\n  [INTEGER]  ::= 55\n}\n",v2.toAsnString(""));
	}

	/**
	 * Test method for {@link net.aifusion.asn.SequenceValue#create(byte[], int, int)}.
	 */
	@Test
	public void testCreateByteArrayIntInt() {
		byte [] buffer = v1.getEncodedValue();
		SequenceValue v3 = SequenceValue.create(buffer, buffer.length, 0);
		assertNotNull(v3);
		assertEquals(v1,v3);
	}

	/**
	 * Test method for {@link net.aifusion.asn.SequenceValue#getContainedValues()}.
	 */
	@Test
	public void testGetContainedValues() {
		List<AsnValue> values = v1.getContainedValues();
		assertEquals(2,values.size());
		assertEquals("{1 2 2950 1}",values.get(0).toString());
		assertEquals("55",values.get(1).toString());
	}

	/**
	 * Test method for {@link net.aifusion.asn.SequenceValue#toString()}.
	 */
	@Test
	public void testToString() {
		assertEquals("[SEQUENCE] {\n  [OBJECT_IDENTIFIER]  ::= {1 2 2950 1}\n  [INTEGER]  ::= 55\n}\n",v1.toString());
		assertEquals("[SEQUENCE] {\n  [OBJECT_IDENTIFIER]  ::= {1 2 2950 2}\n  [INTEGER]  ::= 55\n}\n",v2.toString());
	}

	/**
	 * Test method for {@link java.lang.Object#hashCode()}.
	 * Test method for {@link java.lang.Object#equals(java.lang.Object)}.
	 */
	@Test
	public void testHashCode() {
		SequenceValue [] u = {v1,v2};
		for(int i = 0; i < u.length; i++) {
			for(int j = 0; j < u.length; j++) {
				if(i == j) {
					assertEquals(u[i].hashCode(),u[j].hashCode());
					assertEquals(u[i],u[j]);
				} else {
					assertNotSame(u[i],u[j]);
				}
			}
		}
	}
}
