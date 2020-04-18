/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 5, 2018 by sharad
 */
package net.aifusion.asn;

/**
 * ASN.1 Null value
 * @author Sharad Singhal
 */
public final class NullValue extends AsnValue {

	/**
	 * ASN.1 Null Value
	 */
	public NullValue() {
		super(Tag.NULL.getTagNumber(),Tag.NULL.getTagClass(),Tag.NULL.getTagEncoding());
		return;
	}
	
	/**
	 * Get the value of this Null value
	 * @return null
	 */
	public Object getValue() {
		return null;
	}

	@Override
	public byte[] getEncodedValue() {
		return new byte[] {Tag.NULL.getIdentifier(),0};
	}

	@Override
	public boolean equals(Object obj) {
		return obj != null && (obj instanceof NullValue);
	}

	@Override
	public int hashCode() {
		return 7;
	}

	@Override
	public String toString() {
		return "NULL";
	}
}
