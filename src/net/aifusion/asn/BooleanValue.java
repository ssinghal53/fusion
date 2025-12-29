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
 * Created Mar 3, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * ASN.1 Boolean value
 * @author Sharad Singhal
 */
public class BooleanValue extends AsnValue {
	/** Wrapped boolean value */
	private boolean b;
	/** Encoded value */
	private byte [] encodedValue;
	
	/**
	 * ASN.1 Boolean value
	 * @param b - value for this Boolean value
	 */
	public BooleanValue(boolean b) {
		super(Tag.BOOLEAN);
		this.b = b;
		return;
	}
	
	private BooleanValue(long tagNumber, TagClass tagClass, TagEncoding tagEncoding) {
		super(tagClass, tagEncoding,tagNumber);
		return;
	}

	/**
	 * Create a Boolean value from a serialized value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - starting location for the integer value 
	 * @return - IntegerValue
	 * @see LargeInteger
	 */
	public static BooleanValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.BOOLEAN) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected BOOLEAN, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);
		
		BooleanValue v = new BooleanValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		
		byte value = buffer[cursor];
		v.b = value != 0 ? true : false;
		
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.b+"\n");
		}
		return v;
	}
	
	/**
	 * Get the value of this Boolean
	 * @return true if this Boolean is true, false otherwise
	 */
	public boolean getValue() {
		return b;
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append("::= ").append(toString()).append("\n");
		return b.toString();
	}

	@Override
	public String toString() {
		return b ? "TRUE" : "FALSE";
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		return new byte [] {Tag.BOOLEAN.getIdentifier(), 1, (byte) (b ? 0xFF : 0)};
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof BooleanValue)) return false;
		BooleanValue other = (BooleanValue)obj;
		return b == other.b;
	}

	@Override
	public int hashCode() {
		return b ? Boolean.TRUE.hashCode() : Boolean.FALSE.hashCode();
	}
	
}
