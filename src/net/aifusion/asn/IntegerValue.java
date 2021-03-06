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

import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * ASN.1 Integer value (&lt;64 bits)
 * @author Sharad Singhal
 * @see LargeInteger
 */
public class IntegerValue extends AsnValue {
	/** Value of this integer. */
	private long value;
	/** Encoded integer value */
	private byte [] encodedValue;

	/**
	 * ASN.1 Integer value
	 * @param value - value to encode
	 * @see LargeInteger
	 */
	public IntegerValue(long value) {
		super(Tag.INTEGER.getTagNumber(),Tag.INTEGER.getTagClass(),Tag.INTEGER.getTagEncoding());
		this.value = value;
		int length = getRequiredLength(value);	// encoding length 1 <= length <= 8
		// coded as tag, length, MSB .. LSB
		encodedValue = new byte[2+length];
		encodedValue[0] = Tag.INTEGER.getIdentifier();
		encodedValue[1] = (byte)(length);
		long bitMask = 0xFF << 8 * (length-1);
		for(int i = 2; i < encodedValue.length; i++) {
			long v = (value & bitMask) >>> 8 * (--length);
			encodedValue[i] = (byte) v;
			bitMask >>>= 8;
		}
		return;
	}
	
	/**
	 * Create an integer value
	 * @param tagNumber - tag number
	 * @param tagClass - tag class
	 * @param encoding - tag encoding
	 */
	private IntegerValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}

	/**
	 * Create an integer value (&lt;= 64 bits) from a serialized value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - starting location for the integer value 
	 * @return - IntegerValue
	 * @see LargeInteger
	 */
	public static IntegerValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.INTEGER) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected INTEGER, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		IntegerValue v = new IntegerValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		for(int i = 0; i < contentLength; i++) {
			v.value = (v.value << 8) | (buffer[cursor+i] & 0x0FF);
		}
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"\n");
		}
		return v;
	}
	
	/**
	 * Get the value associated with this integer
	 * @return - value of this integer
	 */
	public long getValue() {
		return value;
	}
	
	@Override
	public byte[] getEncodedValue() {
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof IntegerValue)) return false;
		IntegerValue other = (IntegerValue) obj;
		return value == other.value;
	}

	@Override
	public int hashCode() {
		return Long.valueOf(value).hashCode();
	}

	@Override
	public String toString() {
		return Long.toString(value);
	}

	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append(" ::= ").append(toString()).append("\n");
		return b.toString();
	}
}
