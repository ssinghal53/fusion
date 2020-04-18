/**
 * Copyright 2020 Hewlett Packard Laboratories
 * Created Mar 22, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

/**
 * Class to hold a NO-OP ([UNIVERSAL 0](Primitive) value
 * @author Sharad Singhal
 */
public class NoOpValue extends AsnValue {

	/**
	 * Class to hold a NO-OP value
	 */
	protected NoOpValue() {
		super(Tag.END_OF_CONTENT);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.ast.codec.AsnValue#getEncodedValue()
	 */
	@Override
	public byte[] getEncodedValue() {
		return new byte[] {0,0};
	}
}
