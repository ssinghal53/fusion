/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 5, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import java.math.BigInteger;
import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to hold a large (>64 bit) Integer
 * @author Sharad Singhal
 */
public class LargeInteger extends AsnValue {
	private BigInteger value;
	private byte [] encodedValue;

	/**
	 * Create a large Integer
	 * @param tag
	 */
	public LargeInteger(BigInteger value) {
		super(Tag.INTEGER.getTagNumber(),Tag.INTEGER.getTagClass(),Tag.INTEGER.getTagEncoding());
		this.value = value;
	}

	/**
	 * Create a large Integer
	 * @param tagNumber - tag for this value
	 * @param tagClass - tagClass for this value
	 * @param encoding - encoding for this value
	 */
	private LargeInteger(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.ast.codec.AsnValue#getEncoded()
	 */
	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return encodedValue;
		byte [] header = Tag.getEncoded(getTagNumber(), getTagClass(), getTagEncoding());
		byte[] content = value.toByteArray();
		long bytesInContentLength = getRequiredBytesForLength(content.length);
		encodedValue = new byte[(int) (header.length+bytesInContentLength+content.length)];
		int cursor = 0;
		while(cursor < header.length) {
			encodedValue[cursor] = header[cursor++];
		}
		packContent(encodedValue, cursor, content);
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}

	/**
	 * Create a Large Integer value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - current location in input buffer
	 * @return - Large Integer value
	 */
	public static LargeInteger create(byte[] buffer, int blen, int cursor) {
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
		
		LargeInteger v = new LargeInteger(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		byte [] intValue = new byte[(int) contentLength];
		for(int i = 0; i < contentLength; i++) {
			intValue[i] = buffer[cursor+i];
		}
		v.value = new BigInteger(intValue);
		
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"\n");
		}
		return v;
	}
	
	BigInteger getvalue() {
		return value;
	}
	
	@Override
	public String toString() {
		return value.toString();
	}

	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append(" ::= ").append(toString()).append("\n");
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
		if(obj == null || !(obj instanceof LargeInteger)) return false;
		LargeInteger other = (LargeInteger) obj;
		return value.equals(other.value);
	}

}
