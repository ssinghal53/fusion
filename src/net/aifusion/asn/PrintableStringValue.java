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

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage a printable String
 * @author Sharad Singhal
 */
public class PrintableStringValue extends AsnValue {
	private String value;
	private byte [] encodedValue;
	/**
	 * Create a printable string
	 * @param value - input stream
	 */
	public PrintableStringValue(String value) {
		super(Tag.PRINTABLE_STRING);
		this.value = value;
		for(int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if(c >='a' && c <='z' || 
					c >='A' && c <='Z' || 
					c >='0' && c <='9' ||
					" '()+,-./:=?".indexOf(c) != -1) continue;
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected printable string found "+value);
		}
		return;
	}
	
	private PrintableStringValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}
	
	/**
	 * Get a printable string from an input buffer
	 * @param buffer - buffer containing encoded value
	 * @param blen - length of buffer
	 * @param cursor - current cursor
	 * @return - Printable String
	 */
	public static PrintableStringValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.PRINTABLE_STRING) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected PRINTABLE_STRING, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		PrintableStringValue v = new PrintableStringValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		StringBuilder b = new StringBuilder();
		for(int i = 0; i < contentLength; i++) {
			b.append(Character.valueOf((char) (buffer[cursor+i] & 0x0FF)));
		}
		v.value = b.toString();
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"("+v.value+")\n");
		}
		return v;
	}

	/**
	 * Return the value of this printable String
	 * @return - printable string
	 */
	public String getValue() {
		return value;
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append("::= \"").append(value).append("\"\n");
		return b.toString();
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		byte [] header = Tag.getEncoded(getTagNumber(), getTagClass(), getTagEncoding());
		byte[] content;
		content = value.getBytes(StandardCharsets.US_ASCII);
		long bytesInContentLength = getRequiredBytesForLength(content.length);
		encodedValue = new byte[(int) (header.length+bytesInContentLength+content.length)];
		int cursor = 0;
		while(cursor < header.length) {
			encodedValue[cursor] = header[cursor++];
		}
		packContent(encodedValue, cursor, content);
		return Arrays.copyOf(encodedValue, encodedValue.length);
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
		if(obj == null || !(obj instanceof PrintableStringValue)) return false;
		PrintableStringValue other = (PrintableStringValue) obj;
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
