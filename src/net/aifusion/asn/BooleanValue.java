/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
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
		super(tagNumber, tagClass,tagEncoding);
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
