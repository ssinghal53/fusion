/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 7, 2018 by sharad
 */
package net.aifusion.asn;

import java.util.Arrays;
import java.util.Vector;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to pack a relative OID
 * @author Sharad Singhal
 */
public class RelativeOidValue extends AsnValue {
	private long [] oidValues;
	private byte [] encodedValue;
	
	/**
	 * Create a Relative OID value
	 * @param oidValues - array containing relative OID components, with oidValues[0] being the leftmost component
	 */
	public RelativeOidValue(long [] oidValues) {
		super(Tag.RELATIVE_OID.getTagNumber(),Tag.RELATIVE_OID.getTagClass(),Tag.RELATIVE_OID.getTagEncoding());
		if(oidValues.length < 1) throw new ModelException(ExceptionReason.INVALID_PARAMETER,
				this+" Expected at least 1 component. Found "+oidValues.length);
		this.oidValues = oidValues;
		Vector<Byte> encoded = new Vector<Byte>();
		for(int j=0;j < oidValues.length; j++) {
			long identifier = oidValues[j];
			int length = (int) Long.highestOneBit(identifier);
			int nBytes = 0;
			do {
				nBytes++;
				length >>= 7;
			} while(length > 0);
			for(int i = nBytes-1; i >=0; i--) {
				byte b = (byte)((identifier >> 7*i) & 0x07F);
				if(i != 0) b |= 0x80;
				encoded.add(Byte.valueOf(b));
			}
		}
		byte [] tagBytes = Tag.getEncoded(getTagNumber(), getTagClass(), getTagEncoding());
		encodedValue = new byte [tagBytes.length+getRequiredBytesForLength(encoded.size())+encoded.size()];
		int cursor = 0;
		for(; cursor < tagBytes.length; cursor++) {
			encodedValue[cursor] = tagBytes[cursor];
		}
		cursor = packContent(encodedValue, cursor, encoded.size());
		for(Byte b : encoded) {
			encodedValue[cursor++] = b;
		}
		return;
	}
	
	private RelativeOidValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}
	
	public static RelativeOidValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create OIDValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.RELATIVE_OID) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected RELATIVE_OID, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		RelativeOidValue v = new RelativeOidValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		Vector<Long> identifiers = new Vector<Long>();
		long identifier = 0;
		for(int i = 0; i < contentLength; i++) {
			byte b = buffer[cursor+i];
			identifier = (identifier << 7) | (b & 0x07F);
			if((b & 0x080) != 0) continue;
			identifiers.add(Long.valueOf(identifier));
			identifier = 0;
		}
		v.oidValues = new long[identifiers.size()];
		for(int i = 0; i < v.oidValues.length; i++) {
			v.oidValues[i] = identifiers.get(i);
		}
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.print("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+"{ Length: "+contentLength+" Cursor "+cursor+"} :: = {");
			for(int i = 0; i < v.oidValues.length; i++) {
				System.out.print(" "+v.oidValues[i]);
			}
			System.out.println(" }\n");
		}
		return v;
	}
	
	/**
	 * Get the value of this OID
	 * @return - array containing OID values. Element 0 is the root OID
	 */
	public long [] getValue() {
		return Arrays.copyOf(oidValues, oidValues.length);
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append("::= ").append(toString()).append("\n");
		return b.toString();
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof RelativeOidValue)) return false;
		RelativeOidValue other = (RelativeOidValue) obj;
		return Arrays.equals(oidValues, other.oidValues);
	}

	@Override
	public int hashCode() {
		return Arrays.hashCode(oidValues);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.asn1.Asn1Value#getEncoded()
	 */
	@Override
	public byte[] getEncodedValue() {
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder("{");
		for(long l : oidValues) {
			b.append(l).append(" ");
		}
		b.setLength(b.length()-1);
		b.append("}");
		return b.toString();
	}
}
