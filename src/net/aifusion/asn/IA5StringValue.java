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
 * Created Mar 8, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage an IA5 (ASCII) String
 * @author Sharad Singhal
 */
public class IA5StringValue extends AsnValue {
	private String value;
	private byte [] encodedValue;
	/**
	 * Create an ASCII (IA5) string
	 * @param value - input String
	 */
	public IA5StringValue(String value) {
		super(Tag.IA5_STRING);
		this.value = value;
		for(int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if(c >= 0 && c <= 0x7f) continue;
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected IA5String found "+value);
		}
		return;
	}
	
	private IA5StringValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
		return;
	}
	
	/**
	 * Create an IA5 (ASCII) String from a serialized value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - starting location for the integer value 
	 * @return - BMPStringValue
	 */
	public static IA5StringValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.IA5_STRING) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected IA5_STRING, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		IA5StringValue v = new IA5StringValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		char [] chars = new char[(int) (contentLength)];
		int charsIndex = 0, bufferIndex = cursor;
		while(bufferIndex < cursor+contentLength) {
			int codePoint = buffer[bufferIndex++] & 0x0FF;
			charsIndex += Character.toChars(codePoint, chars, charsIndex);
		}
		v.value = String.valueOf(chars);
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"\n");
		}
		return v;
	}

	/**
	 * Get the value of this string
	 * @return value of the string
	 */
	public String getValue() {
		return value;
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		byte [] header = Tag.getEncoded(getTagNumber(), getTagClass(), getTagEncoding());
		byte[] content;
		content = value.getBytes();
		long bytesInContentLength = getRequiredBytesForLength(content.length);
		encodedValue = new byte[(int) (header.length+bytesInContentLength+content.length)];
		int cursor = 0;
		while(cursor < header.length) {
			encodedValue[cursor] = header[cursor++];
		}
		packContent(encodedValue, cursor, content);
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append(" ::= \"").append(value).append("\"\n");
		return b.toString();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return value.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof IA5StringValue)) return false;
		IA5StringValue other = (IA5StringValue) obj;
		return value.equals(other.value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return value;
	}
	
}
