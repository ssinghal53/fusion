/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
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
 * Created Mar 10, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Enumeration to handle ASN.1 Tags
 * @author Sharad Singhal
 */
public enum Tag {
	// (tagNumber, tagClass, tagEncoding)
	/** End of content */
	END_OF_CONTENT(0,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** BOOLEAN value */
	BOOLEAN(1,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Integer value */
	INTEGER(2,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** an arbitrary string of bits (ones and zeroes) */
	BIT_STRING(3,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** an arbitrary string of octets (eight-bit values) */
	OCTET_STRING(4,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** a null value */
	NULL(5,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** an object identifier */
	OBJECT_IDENTIFIER(6,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Object Descriptor */
	OBJECT_DESCRIPTOR(7,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Instance of */
	INSTANCE_OF(8,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** External */
	EXTERNAL(8,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Real number */
	REAL(9,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Enumerated value */
	ENUMERATED(10,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Embedded PDV value */
	EMBEDDED_PDV(11,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** utf-8 string value */
	UTF8_STRING(12,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Relative OID */
	RELATIVE_OID(13,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	// 14 and 15 are reserved for future use
	/** Reserved */
	RESERVED_1(14,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Reserved */
	RESERVED_2(15,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** an ordered collection of zero or more types */
	SEQUENCE(16,TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED),
	/** an ordered collection of one or more occurrences of a given type */
	SEQUENCE_OF(16,TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED),
	/** an unordered collection of zero or more types */
	SET(17,TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED),
	/** an unordered collection of one or more types */
	SET_OF(17,TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED),
	/** Numeric string */
	NUMERIC_STRING(18,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** an arbitrary string of printable characters */
	PRINTABLE_STRING(19,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** an arbitrary string of T.61 (eight-bit) characters */
	T61_STRING(20,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Teletex string */
	TELETEX_STRING(20,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** VideoTex String */
	VIDEOTEX_STRING(21,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** an arbitrary string of IA5 (ASCII) characters */
	IA5_STRING(22,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** a "coordinated universal time" or Greenwich Mean Time (GMT) value */
	UTC_TIME(23,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Generalized Time value */
	GENERALIZED_TIME(24,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Graphic string */
	GRAPHIC_STRING(25,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Visible String */
	VISIBLE_STRING(26,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** ISO 646 String */
	ISO646_STRING(26,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** General String value */
	GENERAL_STRING(27,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Universal String value */
	UNIVERSAL_STRING(28,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** Character String value */
	CHARACTER_STRING(29,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** BMP String value. */
	BMP_STRING(30,TagClass.UNIVERSAL,TagEncoding.PRIMITIVE),
	/** User Defined tag */
	USER_DEFINED(31,TagClass.CONTEXT_SPECIFIC,TagEncoding.PRIMITIVE);

	/** Tag identifier for this tag */
	private final long tagNumber;
	/** Associated tag class */
	private final TagClass tagClass;
	/** Encoding for this tag */
	private final TagEncoding tagEncoding;
	/** EncodedValue for this tag */
	private byte identifier;

	/**
	 * Create the ASN tag for universal classes
	 * @param tagNumber - value associated with this tag
	 * @param tagClass - tag class associated with this type
	 * @param tagEncoding - tag encoding associated with this tag
	 */
	private Tag(long tagNumber,TagClass tagClass, TagEncoding tagEncoding){
		this.tagNumber = tagNumber;
		this.tagClass = tagClass;
		this.tagEncoding = tagEncoding;
		this.identifier = (byte) (tagNumber | tagClass.getTagClass() | tagEncoding.getTagEncoding());
		return;
	}

	/**
	 * Get the number associated with this tag
	 * @return - tag number [0..31]
	 */
	public long getTagNumber() {
		return tagNumber;
	}

	/**
	 * Get the class associated with this tag
	 * @return - UNIVERSAL or CONTEXT_SPECIFIC
	 */
	public TagClass getTagClass() {
		return tagClass;
	}

	/**
	 * Get the encoding associated with this tag
	 * @return - encoding CONSTRUCTED or PRIMITIVE
	 */
	public TagEncoding getTagEncoding() {
		return tagEncoding;
	}

	/**
	 * Return the identifier associated with this tag
	 * @return - byte including tag class, encoding and tag number
	 */
	public byte getIdentifier() {
		return identifier;
	}

	/**
	 * Get the length needed to encode this tag
	 * @return - number of bytes needed to encode this tag
	 */
	public long getLength() {
		return 1;
	}
	
	/**
	 * Get the encoded value of this tag
	 * @return - encoded value for this tag
	 */
	public byte [] getEncoded() {
		return new byte[] {identifier};
	}

	/**
	 * Locate the tag corresponding a given identifier<br>
	 * Note that in some cases (e.g., SEQUENCE and SEQUENCE_OF) two tags have the same identifier.
	 * In that case the first tag in the enum will be returned.
	 * @param identifier - identifier for the tag to locate
	 * @return - corresponding tag, if pre-defined
	 */
	public static Tag locate(byte identifier) {
		long tagNumber = identifier & 0x01F;
		for(Tag t : Tag.values()) {
			if(t.tagNumber == tagNumber) return t;
		}
		return null; // should not happen, since all numbers [0..31] are defined
	}

	/**
	 * Get the number of bytes needed to encode a given tag number<br>
	 * Note that the standard allows the tag number to be an arbitrary precision unsigned integer.
	 * We restrict the tag number to be no more than 63 bits to fit in a long.
	 * @param tagNumber - value of the tag (must be a positive number)
	 * @return - number of bytes needed to encode the tag (identifier+tagNumber)
	 */
	public static int getRequiredLength(long tagNumber) {
		if(tagNumber < 0) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Tag cannot be negative, found "+tagNumber);
		return tagNumber < 31 ? 1 : 2+(63-Long.numberOfLeadingZeros(tagNumber))/7;
	}

	/**
	 * Tag Encoding<br><pre>
	 *  ____________________________________________________________________
	 * |Octet 1                          | Octet 2 onwards                  |
	 * |8 | 7    | 6 | 5 | 4 | 3 | 2 | 1 | 8    | 7 | 6 | 5 | 4 | 3 | 2 | 1 |
	 * |Tag Class|P/C| Tag Number(0-30)  | NA                               |
	 * |         |   | 31                | more | Tag Number                |
	 * |_________|___|___________________|__________________________________|</pre>
	 * Tag class can be {Universal, Application, ContextSensitive, or Private}<br>
	 * Encoding can {Primitive, Constructed}<br>
	 * Tag Number is encoded either as part of the first byte (for tag values &lt; 31), or as the first byte followed
	 * by one or more bytes using the long form. The initial octet encodes the tag class and encoding as before,
	 * and bits 1..5 are 1. The tag number is encoded in the following octets, where bit 8 of each is 1 if there are
	 * more octets, and bits 1..7 encode the tag number. The tag number bits combined, big-endian, encode the tag number.
	 * The least number of following octets should be encoded; that is, bits 1..7 should not all be 0 in the first following octet.<br>
	 * Note that the standard allows the tag number to be an arbitrary precision unsigned integer.
	 * We restrict the tag length to be no more than 63 bits to fit in a long.
	 * @param tagNumber - tag number
	 * @param c - tag class
	 * @param e - encoding type
	 * @return - byte sequence containing the encoded value of the tag (identifier + tagNumber)
	 */
	public static byte [] getEncoded(long tagNumber, TagClass c, TagEncoding e) {
		if(tagNumber < 0) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Tag cannot be negative, found "+tagNumber);
		if(tagNumber < 31) {
			return new byte[] {(byte) (tagNumber | c.getTagClass() | e.getTagEncoding())};
		}
		int length = 1+(63-Long.numberOfLeadingZeros(tagNumber))/7;		// number of bytes needed for tag number
		byte [] bytes = new byte[length+1];								// space for (identifier + tag number)
		bytes[0] = (byte) (0x1F | c.getTagClass() | e.getTagEncoding());	// pack identifier
		for(int i=0; i< length; i++) {									// pack tag number
			bytes[length-i] = i != 0 ? (byte) (((tagNumber >> 7*i) & 0x7F) | 0x80)  : (byte) (tagNumber & 0x7F);
		}
		return bytes;
	}

	/**
	 * Get the tag number from an encoded value
	 * Note that the standard allows the tag number to be an arbitrary precision unsigned integer.
	 * We restrict the tag length to be no more than 63 bits to fit in a long.
	 * @param encoding - byte array containing the encoded value of the tag
	 * @param cursor - starting offset within the array
	 * @return - tag number
	 */
	public static long getTagNumber(byte [] encoding, int cursor) {
		if(encoding == null || encoding.length <= cursor) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Null or short-length encoding");
		if((encoding[cursor] & 0x1F) < 31) {
			return encoding[cursor] & 0x01F;
		}
		long tag = 0;
		int tagLength = 0;
		for(int i = cursor+1; i < encoding.length; i++) {
			tag = (tag << 7) | encoding[i] & 0x07F;
			if((encoding[i] & 0x080) != 0) {
				if(++tagLength > 9) throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Tag lengths > 63 bits are not supported");
				continue;
			}
			return tag;
		}
		throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Unterminated encoding tag");
	}
}
