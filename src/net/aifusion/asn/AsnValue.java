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
 * Created Mar 5, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Wrapper class for all ASN.1 values. This class manages the tag, and provides common methods used in subclasses, 
 * while individual subclasses handle the actual values.
 * @author Sharad Singhal
 */
public abstract class AsnValue {
	// We retain both the tag and the tag value, since multiple tags may map to the same tag value (e.g., SEQUENCE & SEQUENCE_OF)
	/** Tag associated with this value, if any */
	private Tag tag = null;
	/** ASN.1 tag number associated with this value */
	private long tagNumber;
	/** ASN.1 Tag class (Universal, Application, Private, Context_Sensitive) */
	private TagClass tagClass;
	/** ASN.1 Encoding flag (Primitive, Constructed) */
	private TagEncoding tagEncoding;
	/** Debugging flag */
	static boolean debug = false;
	
	/*
	 * *************
	 * Constructors
	 * *************
	 */

	/**
	 * Create an ASN.1 value using a given tag
	 * @param tag
	 */
	protected AsnValue(Tag tag) {
		this.tag = tag;
		this.tagNumber = tag.getTagNumber();
		this.tagClass = tag.getTagClass();
		this.tagEncoding = tag.getTagEncoding();
		return;
	}

	/**
	 * Values with given tag number, class, and encoding
	 * @param tagClass - ASN.1 TagClass (Universal, Application, Context_Sensitive, Private)
	 * @param encoding - ASN.1 Encoding (Primitive, Constructed}
	 * @param tagNumber - ASN.1 Tag number associated with this value
	 */
	protected AsnValue(TagClass tagClass, TagEncoding encoding, long tagNumber) {
		this.tagNumber = tagNumber;
		this.tagClass = tagClass;
		this.tagEncoding = encoding;
		tag = Tag.locate(tagClass,tagEncoding,tagNumber);
		return;
	}
	
	/*
	 * ***********************
	 * Common accessor methods
	 * ***********************
	 */

	/**
	 * Get the ASN.1 predefined (UNIVERSAL) tag associated with this value
	 * @return - associated tag, if any. Null if the tag is not a pre-defined tag
	 */
	public Tag getTag() {
		return tag;
	}

	/**
	 * Get the value of the tag associated with this value
	 * @return - long value containing the tag number
	 */
	public long getTagNumber() {
		return tagNumber;
	}

	/**
	 * Get the ASN.1 Tag class for this value
	 * @return - ASN.1 Tag Class
	 */
	public TagClass getTagClass() {
		return tagClass;
	}

	/**
	 * Get the ASN.1 Tag Encoding for this value
	 * @return - ASN.1 Tag Encoding
	 */
	public TagEncoding getTagEncoding() {
		return tagEncoding;
	}
	
	/**
	 * Get the encoded length of this value
	 * @return - encoded length of the value
	 */
	public int getLength() {
		return getEncodedValue().length;
	}

	/*
	 * ****************************************************
	 * Method(s) to be implemented by concrete sub-classes
	 * ****************************************************
	 */

	/**
	 * Get the encoded value for this ASN.1 value. This method should
	 * be implemented by subclasses to provide the encoded ASN.1 value
	 * @return byte [] containing encoded value for this ASN.1 value (including tag, length, content)
	 */
	public abstract byte [] getEncodedValue();

	/**
	 * Create an ASN.1 value from a serialized value. This method dispatches the 
	 * call to subclasses which should also implement the create method to construct
	 * and return the proper AsnValue.
	 * @param buffer - input buffer
	 * @param blen - length of buffer
	 * @param cursor - offset pointing to the first byte (tag) for the value
	 * @return - parsed ASN.1 value
	 */
	public static AsnValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// get the tag and tag class information
		Tag tag = Tag.locate(buffer[cursor]);
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);

		if(debug) {
			TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
			long tagNumber = getTagNumber(buffer,blen,cursor);
			cursor = skipPastTag(buffer, blen, cursor);
			long contentLength = getContentLength(buffer, blen, cursor);
			cursor = skipPastLength(buffer, blen, cursor);
			System.out.println("Cursor["+saved+"] Buffer "+toHex(buffer, saved, blen));
			System.out.print("["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+tag);
			System.out.println(" : Length "+contentLength+"\n");
			cursor = saved;	// reset cursor
		}
		switch(tagClass) {
		case UNIVERSAL:
			switch(tag) {
			case END_OF_CONTENT:			// NO-OP
				if(buffer[cursor+1] != 0)	// check for contentLength == 0
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected 0 length for END_OF_CONTENT at "+(cursor+1)+
						" found "+buffer[cursor+1]);
				return new NoOpValue();
			case SEQUENCE:
			case SEQUENCE_OF:
				return SequenceValue.create(buffer, blen, cursor);
			case SET:
			case SET_OF:
				return SetValue.create(buffer, blen, cursor);
			case INTEGER:
				cursor = skipPastTag(buffer,blen,cursor);
				long contentLength = getContentLength(buffer,blen,cursor);
				return contentLength <= 8 ? IntegerValue.create(buffer, blen, saved) : LargeInteger.create(buffer, blen, saved);
			case OBJECT_IDENTIFIER:
				return OidValue.create(buffer, blen, cursor);
			case UTC_TIME:
				return UTCTimeValue.create(buffer, blen, cursor);
			case BIT_STRING:
				return BitStringValue.create(buffer, blen, cursor);
			case BOOLEAN:
				if(buffer[cursor+1] != 1) throw new ModelException(ExceptionReason.INVALID_PARAMETER, "Expected length = 1 for Boolean, found "+buffer[cursor+1]);
				return (buffer[cursor+2] & 0x0FF) == 0 ? new BooleanValue(false) : new BooleanValue(true);
			case NULL:
				return new NullValue();
			case ENUMERATED:
				return EnumeratedValue.create(buffer, blen, cursor);
			case REAL:
				return RealValue.create(buffer, blen, cursor);
			case GENERALIZED_TIME:
				return GeneralizedTimeValue.create(buffer,blen,cursor);
			case IA5_STRING:
				return IA5StringValue.create(buffer, blen, cursor);
			case BMP_STRING:
				return BMPStringValue.create(buffer, blen, cursor);
			case NUMERIC_STRING:
				return NumericStringValue.create(buffer, blen, cursor);
			case OCTET_STRING:
				return OctetStringValue.create(buffer, blen, cursor);
			case PRINTABLE_STRING:
				return PrintableStringValue.create(buffer,blen,cursor);
			case RELATIVE_OID:
				return RelativeOidValue.create(buffer, blen, cursor);
			case ISO646_STRING:
			case VISIBLE_STRING:
				return VisibleStringValue.create(buffer, blen, cursor);
			// unimplemented types
			case UTF8_STRING:	// Dubuisson page 188 - same as universal string, but with different encoding. See RFC3629
			case T61_STRING:
			case TELETEX_STRING:
			case EXTERNAL:
			case INSTANCE_OF:
			case GRAPHIC_STRING:	// Dubuisson page 181. Not used any more 
			case UNIVERSAL_STRING:	// iso10646-1	Dubuisson all characters are 4 bytes. Use UTF8 instead
			case VIDEOTEX_STRING:	// Dubuisson page 181. Not used any more
			case CHARACTER_STRING:
			case GENERAL_STRING:	// Dubuisson page 182. Not used any more.
			case EMBEDDED_PDV:
			case OBJECT_DESCRIPTOR:	// Dubuisson page 226: [Universal 7] IMPLICIT GraphicString (not generally used-- use OID instead).
			default:
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,tag+": Tag not yet handled");
			}
		case CONTEXT_SPECIFIC:
			return TaggedValue.create(buffer, blen, cursor);
		case APPLICATION:
		case PRIVATE:
		default:
			throw new ModelException(tagClass+": Tag class not yet handled");
		}
	}
	
	/*
	 * Additional method to be implemented by most subclasses (if subclass is T)
	 * public T getValue();
	 */
	
	/**
	 * Get a (semi) ASN.1 notation for this value.
	 * @param prefix - prefix to add to each line
	 * @return - string containing the ASN.1 value
	 */
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder();
		b.append(prefix);
		if(tag == null || (tag != null && tag.getTagNumber() != tagNumber)) {
			b.append("[").append(tagNumber).append("] ");
		}
		b.append("[").append(tag).append("] ");
		return b.toString();
	}

	/*
	 * **********************************************
	 * Helper methods used by subclasses of Asn1Value
	 * In general, the expected sequence of calls is:
	 * 1. GetTagNumber
	 * 2. SkipPastTag
	 * 3. GetContentLength
	 * 4. SkipPastLength
	 * **********************************************
	 */

	/**
	 * Get the tag number for the current tag in the buffer
	 * @param buffer - buffer containing encoded values
	 * @param blen - length of the buffer
	 * @param cursor - cursor pointing at the tag in the buffer
	 * @return - tag number
	 */
	static long getTagNumber(byte [] buffer, int blen, int cursor) {
		if(buffer == null || blen <= cursor) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Null or short-length encoding");
		if((buffer[cursor] & 0x01F) < 31) {
			return buffer[cursor] & 0x01F;
		}
		long tag = 0;
		int tagLength = 0;
		for(int i = cursor+1; i < blen; i++) {
			tag = (tag << 7) | buffer[i] & 0x07F;
			if((buffer[i] & 0x080) != 0) {
				if(++tagLength > 9) throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Tag lengths > 63 bits are not supported");
				continue;
			}
			return tag;
		}
		throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Unterminated encoding tag");
	}

	/**
	 * Skip past the tag in a buffer
	 * @param buffer - buffer containing encoded values
	 * @param blen - length of the buffer
	 * @param cursor - cursor pointing to the tag of the encoded value
	 * @return - cursor value past the tag (including tag and tag number)
	 */
	static int skipPastTag(byte [] buffer, int blen, int cursor) {
		if(buffer == null || blen <= cursor) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Null or short-length encoding");
		if((buffer[cursor] & 0x1F) < 31) {
			return cursor+1;
		}
		int tagLength = 0;
		for(int i = cursor+1; i < blen; i++) {
			if((buffer[i] & 0x80) != 0) {
				if(++tagLength > 9) throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Tag lengths > 63 bits are not supported");
				continue;
			}
			return i+1;
		}
		throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Unterminated encoding tag");
	}

	/**
	 * Get the length of the content 
	 * @param buffer - buffer holding the serialized value
	 * @param blen - length of the buffer
	 * @param cursor - cursor pointing to the length octets
	 * @return - length of the content.
	 */
	static long getContentLength(byte [] buffer, int blen, int cursor) {
		long contentLength = 0;
		if((buffer[cursor] & 0x080) == 0) {
			// short form. Content length is [0..127]
			contentLength = buffer[cursor++] & 0x07F;
			// System.out.println("Short form "+Utilities.toHex(buffer[cursor-1])+" "+contentLength);
		} else {
			// long or indefinite forms
			contentLength = buffer[cursor++] & 0x07F;
			if(contentLength > 0) {
				// long form. Content length is formed by the next nBytes
				int nBytes = (int) contentLength;
				// System.out.println("Long form "+Utilities.toHex(buffer[cursor])+" "+nBytes);
				// note that the standard allows up to 63 bytes
				if(nBytes <= 8) {
					contentLength = 0;
					for(int i = 0; i < nBytes; i++) {
						contentLength = (contentLength << 8) | (buffer[cursor++] & 0x0FF);
					}
				} else {
					// length of content is greater than will fit in a long
					throw new ModelException(nBytes+" : Content length is > 8 bytes in definite form");
				}
			} else {
				// indefinite form. cursor points to first content byte. Scan buffer for two 0 octets
				int saved = cursor;
				while(cursor < blen-1) {
					if(buffer[cursor] == 0 && buffer[cursor+1] == 0) {
						// found the end of content tags
						// contentLength includes end-of-content marker
						contentLength = cursor-saved+2;
						return contentLength;
					}
					cursor++;
				}
				throw new ModelException(ExceptionReason.NOT_FOUND,"End of content tags not found in buffer");
			}			
		}
		return contentLength;
	}

	/**
	 * Advance the cursor past the length octets
	 * @param buffer - buffer holding the serialized value
	 * @param blen - length of the buffer
	 * @param cursor  - offset to the length octets
	 * @return - new cursor value (past the length octets)
	 */
	static int skipPastLength(byte [] buffer, int blen, int cursor) {
		if((buffer[cursor] & 0x080) != 0) {
			// long or indefinite form
			// note that in the indefinite form, the content will include a no-op value at the end
			cursor += (buffer[cursor] & 0x07F);
		}
		return cursor+1;
	}

	/**
	 * Get the length needed to encode a long (signed) integer value
	 * @param value - value to be encoded
	 * @return - number of bytes needed to encode the value
	 */
	static int getRequiredLength(long value) {
		long bitMask = 0xff80000000000000L;
		int length;	// encoding length. Minimum length is 1
		for(length = 8; length > 1 ; length--) {
			long v = (value & bitMask) >>> 8*length-9;
			if(v == 0 || v == 0x1FF) {
				// all 9 bits are either 0 or 1, we can omit
				bitMask >>>= 8;
				continue;
			}
			break;
		}
		return length;
	}

	/**
	 * Get the length of the encoding needed for a double value.<br>
	 * Assumes that the hardware implements IEEE 754-2008.<br>
	 * Bit 63 (the bit that is selected by the mask 0x8000000000000000L) represents the sign of the floating-point number.<br> 
	 * Bits 62-52 (the bits that are selected by the mask 0x7ff0000000000000L) represent the exponent.<br> 
	 * Bits 51-0 (the bits that are selected by the mask 0x000fffffffffffffL) represent the significand of the floating-point number.
	 * @param value - double value
	 * @return - length of the ASN.1 encoding in bytes
	 */
	static int getRequiredLength(double value){
		// zero value => no content octets
		if(value == 0) return 0;
		// PLUS or MINUS INFINITY is encoded as a single byte
		if(value == Double.POSITIVE_INFINITY || value == Double.NEGATIVE_INFINITY) return 1;
		if(value == Double.NaN) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Value is a NAN; cannot be transmitted");

		// get the exponent and the significand. Do not need the sign to compute length
		long bitValue = Double.doubleToRawLongBits(value);
		long exponent = (0x7FF0000000000000L & bitValue) >> 52;
		long significand = 0x000FFFFFFFFFFFFFL & bitValue;

		if (exponent == 0) {
			// Unnormalized value. Exponent will need 2 bytes (it is between -1023 and -1075)
			// remove zero bits behind the decimal point
			while ((significand & 1) == 0) significand >>= 1;
			// Number of bytes == 1 header + 2 exponent + bytes for significand
			return 3 + getRequiredLength(significand);
		} else {
			// Normalized value. Convert the exponent and significand.
			exponent -= 1075;
			// IEEE 754 - add the implict MSB in the significand
			significand |= 0x0010000000000000L;
			// remove trailing bits in significand
			while ((significand & 1) == 0) {
				significand >>= 1;
				exponent += 1;
			}
			// Number of bytes = 1 header + [1|2] exponent + bytes for significand
			return getRequiredLength(significand) + (((exponent < -128) || (exponent > 127)) ? 3 : 2);
		}
	}
	
	/**
	 * Get the number of bytes needed to encode a length value using the short/long form
	 * @param length length value to encode
	 * @return - number of bytes needed to encode length value
	 */
	static int getRequiredBytesForLength(long length) {
		if(length < 0) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Negative Length not allowed: "+length);
		if(length >= 128) {
			int nBytes = 0;
			do {
				nBytes++;
				length >>>= 8; 
			} while(length > 0);
			return nBytes+1;
		}
		return 1;
	}

	/**
	 * Pack a length value into the encoded buffer using the long/short form
	 * @param buffer - encoded value buffer
	 * @param cursor - starting index in buffer
	 * @param length - long value (> 0) containing length
	 * @return - next cursor location in buffer after length has been packed
	 */
	static int packLength(byte[] buffer, int cursor, long length) {
		if(length < 0) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Negative Length not allowed: "+length);
		if(length < 128) {
			// short form
			buffer[cursor++] = (byte) length;
		} else {
			// long form
			long saved = length;
			int nBytes = 0;
			do {
				nBytes++;
				length >>>= 8; 
			} while(length > 0);
			// reset length
			length = saved;
			buffer[cursor++] = (byte)(0x080 | nBytes);	// number of additional bytes
			for(int i = nBytes; i > 0; i--){	// add additional length bytes
				buffer[cursor+i-1] = (byte)(length & 0xff);
				length = length >>> 8;
			}
			cursor += nBytes;
		}
		return cursor;
	}

	/**
	 * Pack a long value into the buffer.
	 * @param buffer - output buffer
	 * @param cursor - starting location in the output buffer 
	 * @param input - input to be packed in 2's complement form
	 * @return - ending cursor
	 */
	static int packContent(byte [] buffer, int cursor, long input) {
		int length = getRequiredLength(input);
		long bitMask = 0x0FF;
		for(int i = 0; i <length; i++) {
			buffer[cursor+length-i-1] = (byte)((input >> 8*i) & bitMask);
		}
		cursor += length;
		return cursor;
	}

	/**
	 * Pack some bytes into the buffer. Packs the length of the input, followed by the bytes
	 * @param buffer - encoded value buffer
	 * @param cursor - index into the buffer at which the input is to be packed
	 * @param input - byte array containing the input
	 * @return - cursor after the packing is finished
	 */
	static int packContent(byte [] buffer, int cursor, byte [] input){
		cursor = packLength(buffer, cursor, input.length);
		for(int i = 0; i < input.length; i++){	// add content bytes
			buffer[cursor++] = input[i];
		}
		return cursor;
	}
	
	/*
	 * *******************
	 * Debugging code - discard after finished
	 * *******************
	 */
	public static String toHex(byte [] bytes){
		StringBuilder hex = new StringBuilder();
		for(byte b : bytes) {
			hex.append(toHex(b)).append(" ");
		}
		hex.setLength(hex.length()-1);
		return hex.toString();
	}
	
	public static String toHex(byte [] bytes, int start, int end){
		StringBuilder hex = new StringBuilder();
		for(int i = start; i < end && i < bytes.length; i++) {
			hex.append(toHex(bytes[i])).append(" ");
		}
		hex.setLength(hex.length()-1);
		return hex.toString();
	}
	
	public static String toHex(byte b) {
		char [] hex = new char[]{'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'}; 
		StringBuilder sb = new StringBuilder();
		sb.append(hex[(b & 0x00f0) >>> 4]).append(hex[(b & 0x000f)]);
		return sb.toString();
	}
	
	public static String toHex(long value) {
		return Long.toHexString(value);
	}
}
