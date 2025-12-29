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
 * Created Mar 5, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.OctetString;

/**
 * @author Sharad Singhal
 *
 */
public class AsnValueTest {
	/**
	 * AsnValue is abstract. Create a concrete class for testing
	 * @author sharad
	 *
	 */
	public class TestValue extends AsnValue {
		public TestValue(Tag tag) {
			super(tag);
		}
		public TestValue(TagClass tagClass, TagEncoding encoding, long tagNumber) {
			super(tagClass, encoding, tagNumber);
		}
		@Override
		public byte[] getEncodedValue() {
			if(getTag() != null) return getTag().getEncoded();
			return Tag.getEncoded(getTagClass(), getTagEncoding(), getTagNumber());
		}
	}
	
	static byte[] example = new OctetString("0x60818561101a044a6f686e"
			+ "1a01501a05536d697468"
			+ "a00a1a084469726563746f72420133a10a43083139373130393137"
			+ "a21261101a044d6172791a01541a05536d697468"
			+ "a342311f61111a0552616c70681a01541a05536d697468a00a43083139353731313131"
			+ "311f61111a05537573616e1a01421a054a6f6e6573a00a43083139353930373137").getValue();
	
	@BeforeClass
	public static void setupBefore() throws Exception {
		System.out.print("AsnValueTest");
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
	 * Test method for {@link net.aifusion.asn.AsnValue#AsnValue(Tag)}.
	 */
	@Test
	public void testAsnValueTag() {
		for(Tag t : Tag.values()) {
			TestValue v = new TestValue(t);
			assertNotNull(v);
			assertEquals(t,v.getTag());
			assertEquals(t.getTagClass(),v.getTagClass());
			assertEquals(t.getTagNumber(),v.getTagNumber());
			assertEquals(t.getTagEncoding(),v.getTagEncoding());
			assertArrayEquals(t.getEncoded(),v.getEncodedValue());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#AsnValue(net.aifusion.asn.TagClass, net.aifusion.asn.TagEncoding, long)}.
	 * Test method for {@link net.aifusion.asn.AsnValue#getTag()}.
	 * Test method for {@link net.aifusion.asn.AsnValue#getTagNumber()}.
	 * Test method for {@link net.aifusion.asn.AsnValue#getTagClass()}.
	 * Test method for {@link net.aifusion.asn.AsnValue#getTagEncoding()}.
	 */
	@Test
	public void testAsnValueLongTagClassEncoding() {
		for(long tagNumber : new long[] {4,5,30,31,64,128,4096,75000000}) {
			for(TagClass c : TagClass.values()) {
				for(TagEncoding e : TagEncoding.values()) {
					TestValue v = new TestValue(c,e,tagNumber);
					assertNotNull(v);
					assertEquals(tagNumber,v.getTagNumber());
					assertEquals(c,v.getTagClass());
					assertEquals(e,v.getTagEncoding());
					assertEquals(v.getTag(),Tag.locate(c, e, tagNumber));
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#getEncodedValue()}.
	 */
	@Test
	public void testGetEncoded() {
		long [] tagNumber = new long[] {16,32,64,128,256,512,1024,2048,4096,8192,16384};
		byte [][] expected = {
				new byte [] {(byte) 0xB0}, 				// 16
				new byte[] {(byte) 0xBF,0x20}, 			// 32
				new byte[] {(byte) 0xBF,0x40}, 			// 64
				new byte[] {(byte) 0xBF,(byte) 0x81, 0}, 	// 128
				new byte[] {(byte) 0xBF,(byte) 0x82, 0}, 	// 256
				new byte[] {(byte) 0xBF,(byte) 0x84, 0}, 	// 512
				new byte[] {(byte) 0xBF,(byte) 0x88, 0}, 	// 1024
				new byte[] {(byte) 0xBF,(byte) 0x90, 0}, 	// 2048
				new byte[] {(byte) 0xBF,(byte) 0xA0, 0}, 	// 4096
				new byte[] {(byte) 0xBF,(byte) 0xC0, 0}, 	// 8192
				new byte[] {(byte) 0xBF,(byte) 0x81, (byte) 0x80, 0}, 	// 16384	
		};
		assertEquals(tagNumber.length,expected.length);
		for(int i=0; i < tagNumber.length; i++) {
			TestValue v = new TestValue(TagClass.CONTEXT_SPECIFIC,TagEncoding.CONSTRUCTED,tagNumber[i]);
			// System.out.println(Utilities.toHex(v.getEncodedValue()));
			assertArrayEquals(expected[i],v.getEncodedValue());
			assertEquals(tagNumber[i],v.getTagNumber());
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#getRequiredLength(long)}.
	 */
	@Test
	public void testGetRequiredLengthLong() {
		long [] values = new long[] {0,Long.MAX_VALUE,Long.MIN_VALUE};
		int [] expected = new int[] {1,8,8};
		assertEquals(values.length,expected.length);
		for(int i = 0; i < values.length; i++) {
			assertEquals(expected[i],AsnValue.getRequiredLength(values[i]));
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#getRequiredLength(double)}.
	 */
	@Test
	public void testGetRequiredLengthDouble() {
		double [] values = new double[] {0,Double.MAX_VALUE,Double.MIN_VALUE,Double.MIN_NORMAL,Double.POSITIVE_INFINITY,Double.NEGATIVE_INFINITY};
		int [] expected = new int[] {0,10,4,4,1,1};
		assertEquals(values.length,expected.length);
		for(int i = 0; i < values.length; i++) {
			// System.out.println(i);
			assertEquals(expected[i],AsnValue.getRequiredLength(values[i]));
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#packLength(byte[], int, long)}.
	 */
	@Test
	public void testPackLength() {
		long [] values = new long[] {0,Long.MAX_VALUE,Long.MIN_VALUE};
		int [] expected = new int[] {1,9,1};
		assertEquals(values.length,expected.length);
		for(int i = 0; i < values.length; i++) {
			byte [] buffer = new byte[expected[i]];
			try {
				int cursor = AsnValue.packLength(buffer, 0, values[i]);
				// System.out.println(values[i]+" "+cursor+" "+AsnValue.toHex(buffer));
				assertEquals(expected[i],cursor);
			} catch(ModelException e) {
				assertEquals(4,e.getReason().getCode());	// negative value
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#packContent(byte[], int, long)}.
	 */
	@Test
	public void testPackContentByteArrayIntLong() {
		long [] values = new long[] {0,Long.MAX_VALUE,Long.MIN_VALUE};
		int [] expected = new int[] {1,9,1};
		assertEquals(values.length,expected.length);
		for(int i = 0; i < values.length; i++) {
			byte [] buffer = new byte[expected[i]];
			try {
				int cursor = AsnValue.packLength(buffer, 0, values[i]);
				// System.out.println(values[i]+" "+cursor+" "+AsnValue.toHex(buffer));
				assertEquals(expected[i],cursor);
			} catch(ModelException e) {
				assertEquals(4,e.getReason().getCode());	// negative value
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#packContent(byte[], int, byte[])}.
	 */
	@Test
	public void testPackContentByteArrayIntByteArray() {
		byte [] values = new byte[] {0,-128,127};
		byte [] buffer = new byte [4];
		int cursor = AsnValue.packContent(buffer, 0, values);
		assertEquals(4,cursor);
		assertArrayEquals(new byte[] {3,0,-128,127},buffer);
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#getTagNumber(byte[], int, int)}.
	 */
	@Test
	public void testGetTagNumber() {
		long [] expected = new long[] {16,32,64,128,256,512,1024,2048,4096,8192,16384};
		byte [][] buffer = {
				new byte [] {(byte) 0x90,0x03,01,01,00}, 					// 16
				new byte[] {(byte)0x9F,0x20,0x03,01,01,00}, 				// 32
				new byte[] {(byte)0x9F,0x40,0x03,01,01,00}, 				// 64
				new byte[] {(byte)0x9F,(byte) 0x81, 0,0x03,01,01,00}, 		// 128
				new byte[] {(byte)0x9F,(byte) 0x82, 0,0x03,01,01,00}, 		// 256
				new byte[] {(byte)0x9F,(byte) 0x84, 0,0x03,01,01,00}, 		// 512
				new byte[] {(byte)0x9F,(byte) 0x88, 0,0x03,01,01,00}, 		// 1024
				new byte[] {(byte)0x9F,(byte) 0x90, 0,0x03,01,01,00}, 		// 2048
				new byte[] {(byte)0x9F,(byte) 0xA0, 0,0x03,01,01,00}, 		// 4096
				new byte[] {(byte)0x9F,(byte) 0xC0, 0,0x03,01,01,00}, 		// 8192
				new byte[] {(byte)0x9F,(byte) 0x81, (byte) 0x80, 0}, 		// 16384	
		};
		assertEquals(buffer.length,expected.length);
		for(int i = 0; i < expected.length; i++) {
			assertEquals(TagClass.CONTEXT_SPECIFIC,TagClass.getTagClass(buffer[i][0]));
			if(i != 0) {
				assertEquals(Tag.USER_DEFINED,Tag.locate(buffer[i][0]));
			} else {
				// 0x090 is CONTEXT_SPECIFIC, SEQUENCE combination, and does not have a pre-defined
				// identifier. Tag.locate() will return null
				assertNull(Tag.locate(buffer[i][0]));
			}
			assertEquals(expected[i],AsnValue.getTagNumber(buffer[i], buffer[i].length, 0));
		}
	}
	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#skipPastTag(byte[], int, int)}.
	 */
	@Test
	public void testAdvancePastTag() {
		long [] expected = new long[] {1,2,3,4};
		byte [][] buffer = {
				new byte [] {(byte) 0x90,0x03,01,01,00}, 					// 16
				new byte[] {(byte)0x9F,0x20,0x03,01,01,00}, 				// 32
				new byte[] {(byte)0x9F,(byte) 0x81, 0,0x03,01,01,00}, 		// 128
				new byte[] {(byte)0x9F,(byte) 0x81, (byte) 0x80, 0}, 		// 16384	
		};
		assertEquals(buffer.length,expected.length);
		for(int i = 0; i < expected.length; i++) {
			assertEquals(expected[i],AsnValue.skipPastTag(buffer[i], buffer[i].length, 0));
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#getContentLength(byte[], int, int)}.
	 */
	@Test
	public void testGetContentLength() {
		long [] expected = new long[] {1,2,3,4};
		byte [][] buffer = {
				new byte [] {(byte) 0x90,0x03,01,01,00}, 					// 16
				new byte[] {(byte)0x9F,0x20,0x03,01,01,00}, 				// 32
				new byte[] {(byte)0x9F,(byte) 0x81, 0,0x03,01,01,00}, 		// 128
				new byte[] {(byte)0x9F,(byte) 0x81, (byte) 0x80, 0}, 		// 16384	
		};
		assertEquals(buffer.length,expected.length);
		for(int i = 0; i < expected.length; i++) {
			assertEquals(expected[i],AsnValue.skipPastTag(buffer[i], buffer[i].length, 0));
		}
	}
	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#skipPastTag(byte[], int, int)}.
	 */
	@Test
	public void testAdvancePastLength() {
		int [] cursor = new int[] {1,2,3,4};
		long [] expected = new long[] {2,3,4,5};
		byte [][] buffer = {
				new byte [] {(byte) 0x90,0x03,01,01,00}, 					// 16
				new byte[] {(byte)0x9F,0x20,0x03,01,01,00}, 				// 32
				new byte[] {(byte)0x9F,(byte) 0x81, 0,0x03,01,01,00}, 		// 128
				new byte[] {(byte)0x9F,(byte) 0x81, (byte) 0x80, 0,0x03,01,01,00}, 		// 16384	
		};
		assertEquals(buffer.length,expected.length);
		for(int i = 0; i < expected.length; i++) {
			// System.out.println(Utilities.toHex(buffer[i]));
			assertEquals(expected[i],AsnValue.skipPastTag(buffer[i], buffer[i].length, cursor[i]));
		}
	}
	/**
	 * Test method for {@link net.aifusion.asn.AsnValue#toAsnString(String)}.
	 */
	@Test
	public void testToAsnStringString() {
		TestValue v = new TestValue(Tag.INTEGER);
		assertEquals("\t[INTEGER] ",v.toAsnString("\t"));
		v = new TestValue(TagClass.APPLICATION,TagEncoding.PRIMITIVE,34);
		assertEquals("\t[34] [null] ",v.toAsnString("\t"));
	}

	/**
	 * Test for example in X.690 Annex A.
	 * Note that this example uses APPLICATION tags<pre>
	 * PersonnelRecord ::= [APPLICATION 0] IMPLICIT SET {
	 * 	Name Name,
	 *  title [0] VisibleString,
	 *  number EmployeeNumber,
	 *  dateOfHire [1] Date,
	 *  nameOfSpouse [2] Name,
	 *  children [3] IMPLICIT SEQUENCE OF ChildInformation DEFAULT {}
	 * }
	 * ChildInformation ::= SET {
	 * 	name Name,
	 * 	dateOfBirth [0] Date
	 * }
	 * Name ::= [APPLICATION 1] IMPLICIT SEQUENCE { 
	 * 	givenName VisibleString,
	 * 	initial VisibleString,
	 * 	familyName VisibleString
	 * }
	 * EmployeeNumber ::= [APPLICATION 2] IMPLICIT INTEGER
	 * Date ::= [APPLICATION 3] IMPLICIT VisibleString -- YYYYMMDD
	 * 
	 * -- and has the following record in it
	 * 
	 * { name {givenName "John",initial "P",familyName "Smith"},
	 * 	 title "Director",
	 *   number 51,
	 *   dateOfHire "19710917",
	 *   nameOfSpouse {givenName "Mary",initial "T",familyName "Smith"},
	 *   children {
	 *   	{name {givenName "Ralph",initial "T",familyName "Smith"}, dateOfBirth "19571111"},
	 *   	{name {givenName "Susan",initial "B",familyName "Jones"}, dateOfBirth "19590717"}
	 *   }
	 * }
	 * </pre>
	 */
	@Ignore
	@Test
	public void testExample() {
		AsnValue v = TestValue.create(example, example.length, 0);
		assertNotNull(v);
	}
}
