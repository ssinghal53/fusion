/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 4, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.asn.Tag;
import net.aifusion.asn.TagClass;
import net.aifusion.asn.TagEncoding;

/**
 * @author Sharad Singhal
 *
 */
public class TagTest {
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
		assertEquals(37,Tag.values().length);
	}

	/**
	 * Test method for {@link net.aifusion.asn.Tag#getTagNumber()}.<br>
	 * Test method for {@link net.aifusion.asn.Tag#getTagClass()}.<br>
	 * Test method for {@link net.aifusion.asn.Tag#getTagEncoding()}.<br>
	 * Test method for {@link net.aifusion.asn.Tag#getEncoded()}.<br>
	 * Test method for {@link net.aifusion.asn.Tag#getIdentifier()}.
	 */
	@Test
	public void testGetTagValues() {
		for(Tag t : Tag.values()) {
			// all predefined tags are UNIVERSAL, except USER_DEFINED tags, which are context specific
			assertEquals(t != Tag.USER_DEFINED ? TagClass.UNIVERSAL : TagClass.CONTEXT_SPECIFIC,t.getTagClass());
			switch(t) {
			case BIT_STRING:
				assertEquals(3,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(3,t.getIdentifier());
				assertArrayEquals(new byte[] {3},t.getEncoded());
				break;
			case BMP_STRING:
				assertEquals(30,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(30,t.getIdentifier());
				assertArrayEquals(new byte[] {30},t.getEncoded());
				break;
			case BOOLEAN:
				assertEquals(1,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(1,t.getIdentifier());
				assertArrayEquals(new byte[] {1},t.getEncoded());
				break;
			case CHARACTER_STRING:
				assertEquals(29,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(29,t.getIdentifier());
				assertArrayEquals(new byte[] {29},t.getEncoded());
				break;
			case EMBEDDED_PDV:
				assertEquals(11,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(11,t.getIdentifier());
				assertArrayEquals(new byte[] {11},t.getEncoded());
				break;
			case END_OF_CONTENT:
				assertEquals(0,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(0,t.getIdentifier());
				assertArrayEquals(new byte[] {0},t.getEncoded());
				break;
			case ENUMERATED:
				assertEquals(10,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(10,t.getIdentifier());
				assertArrayEquals(new byte[] {10},t.getEncoded());
				break;
			case GENERALIZED_TIME:
				assertEquals(24,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(24,t.getIdentifier());
				assertArrayEquals(new byte[] {24},t.getEncoded());
				break;
			case GENERAL_STRING:
				assertEquals(27,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertArrayEquals(new byte[] {27},t.getEncoded());
				break;
			case GRAPHIC_STRING:
				assertEquals(25,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(25,t.getIdentifier());
				assertArrayEquals(new byte[] {25},t.getEncoded());
				break;
			case IA5_STRING:
				assertEquals(22,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(22,t.getIdentifier());
				assertArrayEquals(new byte[] {22},t.getEncoded());
				break;
			case EXTERNAL:
			case INSTANCE_OF:
				assertEquals(8,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(8,t.getIdentifier());
				assertArrayEquals(new byte[] {8},t.getEncoded());
				break;
			case INTEGER:
				assertEquals(2,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(2,t.getIdentifier());
				assertArrayEquals(new byte[] {2},t.getEncoded());
				break;
			case NULL:
				assertEquals(5,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(5,t.getIdentifier());
				assertArrayEquals(new byte[] {5},t.getEncoded());
				break;
			case NUMERIC_STRING:
				assertEquals(18,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(18,t.getIdentifier());
				assertArrayEquals(new byte[] {18},t.getEncoded());
				break;
			case OBJECT_DESCRIPTOR:
				assertEquals(7,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(7,t.getIdentifier());
				assertArrayEquals(new byte[] {7},t.getEncoded());
				break;
			case OBJECT_IDENTIFIER:
				assertEquals(6,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(6,t.getIdentifier());
				assertArrayEquals(new byte[] {6},t.getEncoded());
				break;
			case OCTET_STRING:
				assertEquals(4,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(4,t.getIdentifier());
				assertArrayEquals(new byte[] {4},t.getEncoded());
				break;
			case PRINTABLE_STRING:
				assertEquals(19,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(19,t.getIdentifier());
				assertArrayEquals(new byte[] {19},t.getEncoded());
				break;
			case REAL:
				assertEquals(9,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(9,t.getIdentifier());
				assertArrayEquals(new byte[] {9},t.getEncoded());
				break;
			case RELATIVE_OID:
				assertEquals(13,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(13,t.getIdentifier());
				assertArrayEquals(new byte[] {13},t.getEncoded());
				break;
			case RESERVED_1:
				assertEquals(14,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(14,t.getIdentifier());
				assertArrayEquals(new byte[] {14},t.getEncoded());
				break;
			case RESERVED_2:
				assertEquals(15,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(15,t.getIdentifier());
				assertArrayEquals(new byte[] {15},t.getEncoded());
				break;
			case SEQUENCE:
			case SEQUENCE_OF:
				assertEquals(16,t.getTagNumber());
				assertEquals(TagEncoding.CONSTRUCTED,t.getTagEncoding());
				assertEquals(48,t.getIdentifier());
				assertArrayEquals(new byte[] {48},t.getEncoded());
				break;
			case SET:
			case SET_OF:
				assertEquals(17,t.getTagNumber());
				assertEquals(TagEncoding.CONSTRUCTED,t.getTagEncoding());
				assertEquals(49,t.getIdentifier());
				assertArrayEquals(new byte[] {49},t.getEncoded());
				break;
			case T61_STRING:
			case TELETEX_STRING:
				assertEquals(20,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(20,t.getIdentifier());
				assertArrayEquals(new byte[] {20},t.getEncoded());
				break;
			case UNIVERSAL_STRING:
				assertEquals(28,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(28,t.getIdentifier());
				assertArrayEquals(new byte[] {28},t.getEncoded());
				break;
			case UTC_TIME:
				assertEquals(23,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(23,t.getIdentifier());
				assertArrayEquals(new byte[] {23},t.getEncoded());
				break;
			case UTF8_STRING:
				assertEquals(12,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(12,t.getIdentifier());
				assertArrayEquals(new byte[] {12},t.getEncoded());
				break;
			case VIDEOTEX_STRING:
				assertEquals(21,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(21,t.getIdentifier());
				assertArrayEquals(new byte[] {21},t.getEncoded());
				break;
			case ISO646_STRING:
			case VISIBLE_STRING:
				assertEquals(26,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(26,t.getIdentifier());
				assertArrayEquals(new byte[] {26},t.getEncoded());
				break;
			case USER_DEFINED:	// 1001 1111 = -97
				assertEquals(31,t.getTagNumber());
				assertEquals(TagEncoding.PRIMITIVE,t.getTagEncoding());
				assertEquals(-97,t.getIdentifier());
				assertArrayEquals(new byte[] {-97},t.getEncoded());
				break;
			default:
				break;
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.Tag#getLength()}.
	 */
	@Test
	public void testGetLength() {
		// all predefined tags are 1-byte long
		for(Tag c : Tag.values()) {
			assertEquals(1,c.getLength());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.Tag#locate(long)}.
	 */
	@Test
	public void testLocate() {
		for(Tag c : Tag.values()) {
			long tagValue = c.getTagNumber();
			Tag found = Tag.locate((byte) tagValue);
			// handle tags that have duplicate tag numbers
			switch(c) {
			case EXTERNAL:
				assertEquals(Tag.INSTANCE_OF,found);
				break;
			case SEQUENCE_OF:
				assertEquals(Tag.SEQUENCE,found);
				break;
			case SET_OF:
				assertEquals(Tag.SET,found);
				break;
			case TELETEX_STRING:
				assertEquals(Tag.T61_STRING,found);
				break;
			case ISO646_STRING:
				assertEquals(Tag.VISIBLE_STRING,found);
				break;
			default:
				assertEquals(c,found);
				break;
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.Tag#getEncoded()}.
	 */
	@Test
	public void testGetEncoded() {
		for(Tag c : Tag.values()) {
			assertArrayEquals(new byte[] {c.getIdentifier()},c.getEncoded());
		}
	}

	/**
	 * Test method for {@link net.aifusion.asn.Tag#getEncoded(long, net.aifusion.asn.TagClass, net.aifusion.asn.TagEncoding)}.<br>
	 * Test method for {@link net.aifusion.asn.Tag#getTag(byte[], int)}.<br>
	 * Test method for {@link net.aifusion.asn.Tag#getRequiredLength(long)}.
	 */
	@Test
	public void testGetEncodedLongTagClassEncoding() {
		long [] tags = new long[] {16,32,64,128,256,512,1024,2048,4096,8192,16384};
		byte [][] expected = {
				new byte [] {0x10}, 				// 16
				new byte[] {0x1F,0x20}, 			// 32
				new byte[] {0x1F,0x40}, 			// 64
				new byte[] {0x1F,(byte) 0x081, 0}, 	// 128
				new byte[] {0x1F,(byte) 0x082, 0}, 	// 256
				new byte[] {0x1F,(byte) 0x084, 0}, 	// 512
				new byte[] {0x1F,(byte) 0x088, 0}, 	// 1024
				new byte[] {0x1F,(byte) 0x090, 0}, 	// 2048
				new byte[] {0x1F,(byte) 0x0A0, 0}, 	// 4096
				new byte[] {0x1F,(byte) 0x0C0, 0}, 	// 8192
				new byte[] {0x1F,(byte) 0x081, (byte) 0x80, 0}, 	// 16384	
		};
		assertEquals(tags.length,expected.length);
		for(int i=0; i < tags.length; i++) {
			byte [] encoded = Tag.getEncoded(tags[i], TagClass.UNIVERSAL, TagEncoding.PRIMITIVE);
			assertArrayEquals(expected[i],encoded);
			assertEquals(tags[i],Tag.getTagNumber(encoded, 0));
			assertEquals(expected[i].length,Tag.getRequiredLength(tags[i]));
		}
	}
}
