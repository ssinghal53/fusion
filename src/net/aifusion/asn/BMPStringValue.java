/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 8, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage an BMP (Basic Multilingual Plan) String
 * @author Sharad Singhal
 */
public class BMPStringValue extends AsnValue {
	private String value;
	private byte [] encodedValue;
	/**
	 * Create a BMP (Basic Multilingual Plan) string (contains two-byte values in the range 0:FFFF). Same as UTF-16
	 * @param value - input String
	 */
	public BMPStringValue(String value) {
		super(Tag.BMP_STRING);
		this.value = value;
		return;
	}
	
	private BMPStringValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
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
	 * Create a BMP String from a serialized value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - starting location for the integer value 
	 * @return - BMPStringValue
	 */
	public static BMPStringValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.BMP_STRING) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected BMP_STRING, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		BMPStringValue v = new BMPStringValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		// See ISO 690-207 Sect.8.21.8
		// each character is written out as 2 octets (00..FF), so contentLength must be even
		// Java char is a BMP character
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
		content = value.getBytes(StandardCharsets.UTF_16BE);	// UTF-16, Big Endian
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
		if(obj == null || !(obj instanceof BMPStringValue)) return false;
		BMPStringValue other = (BMPStringValue) obj;
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
