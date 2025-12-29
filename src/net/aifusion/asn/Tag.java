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
	END_OF_CONTENT(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,0),
	/** BOOLEAN value */
	BOOLEAN(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,1),
	/** Integer value */
	INTEGER(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,2),
	/** an arbitrary string of bits (ones and zeroes) */
	BIT_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,3),
	/** an arbitrary string of octets (eight-bit values) */
	OCTET_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,4),
	/** a null value */
	NULL(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,5),
	/** an object identifier */
	OBJECT_IDENTIFIER(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,6),
	/** Object Descriptor */
	OBJECT_DESCRIPTOR(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,7),
	/** Instance of */
	INSTANCE_OF(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,8),
	/** External */
	EXTERNAL(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,8),
	/** Real number */
	REAL(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,9),
	/** Enumerated value */
	ENUMERATED(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,10),
	/** Embedded PDV value */
	EMBEDDED_PDV(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,11),
	/** utf-8 string value */
	UTF8_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,12),
	/** Relative OID */
	RELATIVE_OID(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,13),
	// 14 and 15 are reserved for future use
	/** Reserved */
	RESERVED_1(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,14),
	/** Reserved */
	RESERVED_2(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,15),
	/** an ordered collection of zero or more types */
	SEQUENCE(TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED,16),
	/** an ordered collection of one or more occurrences of a given type */
	SEQUENCE_OF(TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED,16),
	/** an unordered collection of zero or more types */
	SET(TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED,17),
	/** an unordered collection of one or more types */
	SET_OF(TagClass.UNIVERSAL,TagEncoding.CONSTRUCTED,17),
	/** Numeric string */
	NUMERIC_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,18),
	/** an arbitrary string of printable characters */
	PRINTABLE_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,19),
	/** an arbitrary string of T.61 (eight-bit) characters */
	T61_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,20),
	/** Teletex string */
	TELETEX_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,20),
	/** VideoTex String */
	VIDEOTEX_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,21),
	/** an arbitrary string of IA5 (ASCII) characters */
	IA5_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,22),
	/** a "coordinated universal time" or Greenwich Mean Time (GMT) value */
	UTC_TIME(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,23),
	/** Generalized Time value */
	GENERALIZED_TIME(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,24),
	/** Graphic string */
	GRAPHIC_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,25),
	/** Visible String */
	VISIBLE_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,26),
	/** ISO 646 String */
	ISO646_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,26),
	/** General String value */
	GENERAL_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,27),
	/** Universal String value */
	UNIVERSAL_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,28),
	/** Character String value */
	CHARACTER_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,29),
	/** BMP String value. */
	BMP_STRING(TagClass.UNIVERSAL,TagEncoding.PRIMITIVE,30),
	/** User Defined tag */
	USER_DEFINED(TagClass.CONTEXT_SPECIFIC,TagEncoding.PRIMITIVE,31),
	
	/* *************************************************************
	 * Fusion specific tags
	 * 
	 * We will use TagClass.PRIVATE for our tags.
	 * We will use TagEncoding.PRIMITIVE for non-array tags, and
	 * TagEncoding.CONTEXT_SPECIFIC to indicate array tags. This will
	 * allow us to use 6 bits (5 tag bits + PRIMITIVE/CONTEXT_SPECIFIC)
	 * for tag numbers and all Fusion tags will fit in one byte.
	 * 
	 * TODO: Since many of the Fusion tags represent fixed length values,
	 * we will also change the TLV structure to omit L values in the
	 * encoding as necessary.
	 * 
	 * TODO: In Fusion, we want to separate out the overloaded use of tags
	 * specified in ASN.1-- that's garbage-- if we have to have the
	 * class definition for decoding, all this TLV stuff is no longer
	 * self-describing. 
	 * *************************************************************
	 */
	VOID(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 0),
	/** Boolean type (Boolean | boolean) */
	BOOL(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 1),
	/** Unsigned 8-bit Integer (UInt8) */
	UINT8(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 2),
	/** Unsigned 16-bit Integer (UInt16) */
	UINT16(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 3),
	/** Unsigned 32-bit Integer (UInt32) */
	UINT32(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 4),
	/** Unsigned 64-bit Integer (UInt64) */
	UINT64(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 5),
	/** Signed 8-bit Integer (Byte | byte) */
	SINT8(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 6),
	/** Signed 16-bit Integer (Short | short) */
	SINT16(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 7),
	/** Signed 32-bit Integer (Integer | int) */
	SINT32(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 8),
	/** Signed 64-bit Integer (Long | long) */
	SINT64(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 9),
	/** IEEE 4-byte real number (Float | float) */
	REAL32(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 10),
	/** IEEE 8-byte real number (Double | double) */
	REAL64(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 11),
	/** 16-bit UCS-2 Character (Character | char) */
	CHAR16(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 12),
	/** 16-bit UCS-2 String (String) */
	STRING(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 13),
	/** Date time  (DateTime) */
	DATETIME(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 14),
	/** OctetString value (OctetString) */
	OCTETSTRING(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 15),
	/** Object Name (ObjectPath) */
	OBJECTPATH(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 16),
	/** Enumeration value (EnumerationValue) */
	ENUMERATIONVALUE(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 17),
	/** CimStructure value (StructureValue) */
	STRUCTUREVALUE(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 18),
	/** Instance value (CimInstance) */
	INSTANCEVALUE(TagClass.PRIVATE,TagEncoding.PRIMITIVE, 19),
	
	// tags <PRIVATE,PRIMITIVE {20-31}> are undefined
	
	/** Boolean array (Boolean[] | boolean[]) */
	BOOL_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 1),
	/** Unsigned 8-bit Integer array (UInt8[]) */
	UINT8_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 2),
	/** Unsigned 16-bit Integer array (UInt16[]) */
	UINT16_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 3),
	/** Unsigned 32-bit Integer array (UInt32[]) */
	UINT32_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 4),
	/** Unsigned 64-bit Integer array (UInt64[]) */
	UINT64_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 5),
	/** Signed 8-bit Integer array (Byte[] | byte[]) */
	SINT8_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 6),
	/** Signed 16-bit Integer array (Short[] | short[]) */
	SINT16_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 7),
	/** Signed 32-bit Integer array (Integer[] | int[]) */
	SINT32_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 8),
	/** Signed 64-bit Integer array (Long[] | long[]) */
	SINT64_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 9),
	/** IEEE 4-byte real number array (Float[] | float[]) */
	REAL32_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 10),
	/** IEEE 8-byte real number array (Double[] | double[]) */
	REAL64_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 11),
	/** 16-bit UCS-2 Character array (Character[] | char[]) */
	CHAR16_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 12),
	/** 16-bit UCS-2 String array (String[]) */
	STRING_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 13),
	/** Date time array (DateTime[]) */
	DATETIME_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 14),
	/** OctetString array value (OctetString[]) */
	OCTETSTRING_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 15),
	/** Object Name array (ObjectPath[]) */
	OBJECTPATH_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 16),
	/** Enumeration value array (EnumerationValue) */
	ENUMERATIONVALUE_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 17),
	/** CimStructure value (StructureValue) */
	STRUCTUREVALUE_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 18),
	/** Instance value (CimInstance) */
	INSTANCEVALUE_ARRAY(TagClass.PRIVATE,TagEncoding.CONSTRUCTED, 19);
	
	// Tags <PRIVATE,CONSTRUCTED, {20-31}> are undefined

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
	 * @param tagClass - tag class associated with this type
	 * @param tagEncoding - tag encoding associated with this tag
	 * @param tagNumber - value associated with this tag
	 */
	private Tag(TagClass tagClass,TagEncoding tagEncoding, long tagNumber){
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
	 * @return - UNIVERSAL, APPLICATION, CONTEXT_SPECIFIC, or PRIVATE
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
	 * @return - corresponding tag, if pre-defined. Null otherwise
	 */
	public static Tag locate(byte identifier) {
		for(Tag t : Tag.values()) {
			if(t.identifier== identifier) return t;
		}
		return null;
	}
	
	/**
	 * Locate the tag corresponding to a given class, encoding, and number
	 * @param tagClass associated tag class
	 * @param tagEncoding associated tag encoding
	 * @param tagNumber tag number
	 * @return corresponding tag. Null returned if the tag is not predefined
	 */
	public static Tag locate(TagClass tagClass, TagEncoding tagEncoding, long tagNumber) {
		return locate(getEncoded(tagClass,tagEncoding,tagNumber)[0]);
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
	 * 
	 * For Fusion tags, we will use TagClass = PRIVATE. TagEncoding.PRIMITIVE will indicate non-array
	 * values, and TagEncoding.CONSTRUCTED will indicate array values
	 * @param c - tag class
	 * @param e - encoding type
	 * @param tagNumber - tag number
	 * @return - byte sequence containing the encoded value of the tag (identifier + tagNumber)
	 */
	public static byte [] getEncoded(TagClass c, TagEncoding e, long tagNumber) {
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
		if((encoding[cursor] & 0x01F) < 31) {
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
