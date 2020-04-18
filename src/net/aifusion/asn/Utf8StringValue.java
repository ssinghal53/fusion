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
 * Class to manage a UTF-8 String
 * This non-normative example demonstrates using SmtpUTF8Mailbox as an
   otherName in GeneralName to encode the email address
   "u+8001u+5E2B@example.com".

      The hexadecimal DER encoding of the email address is:
      A022060A 2B060105 05070012 0809A014 0C12E880 81E5B8AB 40657861
      6D706C65 2E636F6D

      The text decoding is:
        0  34: [0] {
        2  10:   OBJECT IDENTIFIER '1 3 6 1 5 5 7 0 18 8 9'
       14  20:   [0] {
       16  18:     UTF8String '..@example.com'
             :     }
             :   }
 * @author Sharad Singhal
 */
public class Utf8StringValue extends AsnValue {
	private String value;
	private byte [] encodedValue;
	/**
	 * Create a UTF-8 string
	 * @param value - input String
	 */
	public Utf8StringValue(String value) {
		super(Tag.UTF8_STRING);
		this.value = value;
		return;
	}
	
	private Utf8StringValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
		return;
	}
	
	/**
	 * Get the value of this string
	 * @return - string value
	 */
	public String getValue() {
		return value;
	}
	
	/**
	 * Create a UTF8 String from a serialized value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - starting location for the integer value 
	 * @return - UTF8StringValue
	 */
	public static Utf8StringValue create(byte [] buffer, int blen, int cursor) {
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,"UTF8 Strings are not yet supported");
		/*
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.UTF8_STRING) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected UTF8_STRING, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		Utf8StringValue v = new Utf8StringValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		char [] chars = new char[(int) (contentLength/2)];
		int charsIndex = 0, bufferIndex = cursor;
		while(bufferIndex < cursor+contentLength) {
			int codePoint = buffer[bufferIndex++] << 8 | buffer[bufferIndex++];
			charsIndex += Character.toChars(codePoint, chars, charsIndex);
		}
		v.value = String.valueOf(chars);
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"\n");
		}
		return v;
		*/
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
		content = value.getBytes(StandardCharsets.UTF_8);	// UTF-8
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
		if(obj == null || !(obj instanceof Utf8StringValue)) return false;
		Utf8StringValue other = (Utf8StringValue) obj;
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
