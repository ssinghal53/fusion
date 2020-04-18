/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 5, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;
import java.util.BitSet;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to represent an ASN.1 BitString
 * @author Sharad Singhal
 */
public class BitStringValue extends AsnValue {
	/** BitSet value for this bit string */
	private BitSet bitSet = null;
	/** String value for this bit string */
	private String value;
	/** Encoded ASN.1 value for this bit string */
	private byte [] encodedValue;
	
	/**
	 * Create a BitString from a byte []. Note that bits[bits.length-1] is the MSB in the bitString
	 * @param bits - byte array containing bits in the Bit String
	 */
	public BitStringValue(byte [] bits) {
		super(Tag.BIT_STRING.getTagNumber(),Tag.BIT_STRING.getTagClass(),Tag.BIT_STRING.getTagEncoding());
		bitSet = BitSet.valueOf(bits);
		StringBuilder b = new StringBuilder("'");
		for(int i = 0; i < bitSet.length(); i += 8) {
			int val = 0;
			for(int j = 0; j < 8; j++) {
				val = (val << 1) | (bitSet.get(i+j) ? 1 : 0);
			}
			b.append(toHex((byte)val));
		}
		value = b.toString();
		return;
	}
	
	/**
	 * Create a BitString. Note that java BitSets are little-endian, while ASN.1 are big-endian. Bit 0 is the
	 * left-most bit in the string representation
	 * @param bitString - bit string using the ASN.1 bit string format 'nnn...'[B|H]
	 */
	public BitStringValue(String bitString) {
		super(Tag.BIT_STRING.getTagNumber(),Tag.BIT_STRING.getTagClass(),Tag.BIT_STRING.getTagEncoding());
		this.value = bitString;
		bitSet = new BitSet();
		if(bitString.startsWith("'") && bitString.endsWith("'B")){
			for(int i=1; i < bitString.length()-2; i++) {
				if('1' == bitString.charAt(i)) bitSet.set(i-1);
				else if('0' != bitString.charAt(i))
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected binary in bitString, found "+bitString.charAt(i));	
			}
		} else if(bitString.startsWith("'") && bitString.endsWith("'H")) {
			for(int i = 1, j = 0; i < bitString.length()-2; i++) {
				char hex = bitString.charAt(i);
				int val = 0;
				if(hex >= '0' && hex <= '9') {
					val = hex - '0';
				} else if(hex >= 'A' && hex <= 'F') {
					val = hex - 'A' +10;
				}
				int bitMask = 0x08;
				for(int k = 0; k < 4; k++) {
					if((val & bitMask) != 0) bitSet.set(j);
					j++;
					bitMask >>>= 1;
				}
			}
		} else {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"bitString must be of form 'xxx...'[B|H] "+bitString);	
		}
		return;
	}
	
	private BitStringValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}
	
	/**
	 * Create a bit string from an encoded value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - current position of the cursor in the input buffer
	 * @return - bit string value
	 */
	public static BitStringValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.BIT_STRING) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected BIT_STRING, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		BitStringValue v = new BitStringValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		int extraBits = buffer[cursor];
		int bits = (int) ((contentLength-1)*8-extraBits);
		// System.out.println("NBits "+bits+" ExtraBits "+extraBits);
		// note that if bits is a multiple of 4, we can use Hex notation to reduce length of value
		v.bitSet = new BitSet(bits);
		StringBuilder val = new StringBuilder("'");
		int bitToSet = bits-1;
		for(int i = 1; i < contentLength; i++) {
			byte b = buffer[cursor+i];
			int min = (i == contentLength-1) ? extraBits : 0;
			int mask = 0x080;
			for(int j = 7; j >= min; j--) {
				if((b & mask) != 0) {
					val.append("1");
					v.bitSet.set(bitToSet);
				} else {
					val.append("0");
				}
				mask >>>= 1;
				bitToSet--;
			}
		}
		val.append("'B");
		v.value = val.toString();
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"\n");
		}
		return v;
	}

	/**
	 * Get the value of this bit string.
	 * @return value of this string
	 */
	public String getValue() {
		return value;
	}
	
	/**
	 * Get the size of this bit string
	 * @return length of this bit string
	 */
	public int getLength() {
		return bitSet.length();
	}
	
	/**
	 * Check if a particular bit in this bitSet is set
	 * @param index - index of the bit to check. Bit 0 is the left-most bit.
	 * @return - true if the bit is set, false otherwise
	 */
	public boolean isSet(int index) {
		return bitSet.get(index);
	}
	
	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		int length = bitSet.length();
		if(length == 0) {
			// zero length - return {tag = BIT_STRING(UNIVERSAL,PRIMITIVE), length = 1, ContentHeader = 0}
			return new byte[] {Tag.BIT_STRING.getIdentifier(),1,0};
		}
		int contentLength = (length+7)/8;	// round up to the nearest 8
		int extraBits = contentLength * 8 -length;// number of extra bits
		// required size is 1(tag) + lengthBytes + 1 (unused bits) + nBytes
		encodedValue = new byte[1+getRequiredBytesForLength(contentLength+1)+1+contentLength];
		int cursor = 0;
		encodedValue[cursor++] = Tag.BIT_STRING.getIdentifier();
		cursor = packLength(encodedValue, cursor, contentLength+1);
		encodedValue[cursor++] = (byte) extraBits;
		for(int i = 0; i < length; i++) {
			if(bitSet.get(i)) {
				int byteIndex = i/8;
				int bitIndex = i%8;
				encodedValue[cursor+byteIndex] |= 1 << 7-bitIndex;
			}
		}
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append(" ::= ").append(value).append("\n");
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return bitSet.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof BitStringValue)) return false;
		BitStringValue other = (BitStringValue) obj;
		return bitSet.equals(other.bitSet);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return value;
	}
}
