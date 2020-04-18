/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 7, 2018 by sharad
 */
package net.aifusion.asn;

import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to wrap an ASN.1 Octet String
 * @author Sharad Singhal
 */
public class OctetStringValue extends AsnValue {
	private byte [] value;
	private byte [] encodedValue;

	/**
	 * Create an ASN.1 Octet String
	 * @param value - byte [] containing the octets
	 */
	public OctetStringValue(byte [] value) {
		super(Tag.OCTET_STRING.getTagNumber(),Tag.OCTET_STRING.getTagClass(),Tag.OCTET_STRING.getTagEncoding());
		this.value = Arrays.copyOf(value, value.length);
		return;
	}
	
	private OctetStringValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}
	
	/**
	 * Create an OctetString value
	 * @param buffer - input buffer containing serialized value
	 * @param blen - length of input buffer
	 * @param cursor - current cursor
	 * @return - OctetStringValue
	 */
	public static OctetStringValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.OCTET_STRING) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected OCTET_STRING, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		OctetStringValue v = new OctetStringValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		v.value = Arrays.copyOfRange(buffer, cursor, (int) (cursor+contentLength));
		return v;
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		encodedValue = new byte[1+getRequiredBytesForLength(value.length)+value.length];
		encodedValue[0] = Tag.OCTET_STRING.getIdentifier();
		packContent(encodedValue,1,value);
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	public byte [] getValue() {
		return Arrays.copyOf(value, value.length);
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append("::= ").append(toString()).append("\n");
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return Arrays.hashCode(encodedValue);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof OctetStringValue)) return false;
		OctetStringValue other = (OctetStringValue) obj;
		return Arrays.equals(value, other.value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder("{ ");
		for(byte v : value) {
			b.append(Byte.valueOf(v).toString()).append(" ");
		}
		b.append("}");
		return b.toString();
	}
	
}
