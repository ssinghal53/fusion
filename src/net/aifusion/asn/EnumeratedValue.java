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
 * Created Mar 11, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage an Enumerated Value
 * @author Sharad Singhal
 */
public class EnumeratedValue extends AsnValue {
	/** Value of this enumerated value */
	private int value;
	/** Encoding of this enumerated value */
	private byte[] encodedValue;
	
	/**
	 * Create an enumerated value
	 * @param value - integer value associated with the Enumerated Value
	 */
	public EnumeratedValue(int value) {
		super(Tag.ENUMERATED.getTagNumber(),Tag.ENUMERATED.getTagClass(),Tag.ENUMERATED.getTagEncoding());
		if(value < 0) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected positive integer, found "+value);
		this.value = value;
		int length = getRequiredLength(value);	// encoding length 1 <= length <= 8
		// coded as tag, length, MSB .. LSB
		encodedValue = new byte[2+length];
		encodedValue[0] = Tag.ENUMERATED.getIdentifier();
		encodedValue[1] = (byte)(length);
		long bitMask = 0xFF << 8 * (length-1);
		for(int i = 2; i < encodedValue.length; i++) {
			long v = (value & bitMask) >>> 8 * (--length);
			encodedValue[i] = (byte) v;
			bitMask >>>= 8;
		}
		return;
	}

	private EnumeratedValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		int length = getRequiredLength(value);	// encoding length 1 <= length <= 8
		// coded as tag, length, MSB .. LSB
		encodedValue = new byte[2+length];
		encodedValue[0] = Tag.ENUMERATED.getIdentifier();
		encodedValue[1] = (byte)(length);
		long bitMask = 0xFF << 8 * (length-1);
		for(int i = 2; i < encodedValue.length; i++) {
			long v = (value & bitMask) >>> 8 * (--length);
			encodedValue[i] = (byte) v;
			bitMask >>>= 8;
		}
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	/**
	 * Create an enumerated value from an input buffer
	 * @param buffer - buffer containing input bytes
	 * @param blen - length of the buffer
	 * @param cursor - current position in the buffer
	 * @return - enumerated value
	 */
	public static EnumeratedValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.ENUMERATED) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected ENUMERATED, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		EnumeratedValue v = new EnumeratedValue(tagNumber, tagClass, tagEncoding);
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
	 * Get the value associated with this integer value
	 * @return - value associated with this enumerated value
	 */
	public int getValue() {
		return value;
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append("::= ").append(value).append("\n");
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return value;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof EnumeratedValue)) return false;
		EnumeratedValue other = (EnumeratedValue) obj;
		return value == other.value;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return String.valueOf(value);
	}
}
