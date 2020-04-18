/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 24, 2018 by sharad
 */
package net.aifusion.asn;

import java.io.UnsupportedEncodingException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to represent a UTC Time String
 * @author Sharad Singhal
 *
 */
public class UTCTimeValue extends AsnValue {
	private byte [] encodedValue;
	private String value;
	private ZonedDateTime date;
	
	/**
	 * Create a UTC time value
	 * @param value - string value in UTC time format ("yyMMddHHmmssZ")
	 * @see GeneralizedTimeValue
	 */
	public UTCTimeValue(String value) {
		super(Tag.UTC_TIME.getTagNumber(),Tag.UTC_TIME.getTagClass(),Tag.UTC_TIME.getTagEncoding());
		this.value = value;
		try {
			date = getDate(value);
		} catch (Exception e) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Invalid date format "+value);
		}
		return;
	}
	
	private UTCTimeValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}

	/**
	 * Create a UTC Time value
	 * @param buffer - input buffer
	 * @param blen - length of the input buffer
	 * @param cursor - cursor within the buffer
	 * @return - UTC time value
	 */
	public static UTCTimeValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.UTC_TIME) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected UTC_TIME, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		UTCTimeValue v = new UTCTimeValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		StringBuilder b = new StringBuilder();
		for(int i = 0; i < contentLength; i++) {
			b.append(Character.valueOf((char) (buffer[cursor+i] & 0x0FF)));
		}
		v.value = b.toString();
		v.date = getDate(v.value);
		
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"("+v.date+")\n");
		}
		return v;
	}
	
	public ZonedDateTime getValue() {
		return date;
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		byte [] header = Tag.getEncoded(getTagNumber(), getTagClass(), getTagEncoding());
		byte[] content;
		try {
			content = value.getBytes("UTF-8");
			long bytesInContentLength = getRequiredBytesForLength(content.length);
			encodedValue = new byte[(int) (header.length+bytesInContentLength+content.length)];
			int cursor = 0;
			while(cursor < header.length) {
				encodedValue[cursor] = header[cursor++];
			}
			packContent(encodedValue, cursor, content);
		} catch (UnsupportedEncodingException e) {
			throw new ModelException("Internal Error converting encoded value to String ["+toHex(encodedValue)+"]",e);
		}
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(super.toAsnString(prefix));
		b.append("::= \"").append(value).append("\"\n");
		return b.toString();
	}
	
	
	private static ZonedDateTime getDate(String dateString) {
		// yyMMddHHmmss - local time with no time zone
		// yyMMddHHmmss.fraction - local time with fractional seconds
		// yyMMddHHmmssZ - UTC time
		// yyMMddHHmmss.fractionZ - UTC fractional time
		// yyMMddHHmmss.fraction[+-]xxxx - where offset is offset in hhmm from UTC
		int year = Integer.parseInt(dateString.substring(0,2));
		if(year < 50) {
			year += 2000; // 2000 - 2049
		} else {
			year += 1900; // 1950 - 1999
		}
		int month = Integer.parseInt(dateString.substring(2,4));
		int day = Integer.parseInt(dateString.substring(4,6));
		int hour = Integer.parseInt(dateString.substring(6,8));
		int minute = Integer.parseInt(dateString.substring(8,10));
		int second = Integer.parseInt(dateString.substring(10,12));
		int nanoSecond = 0;
		ZoneId zone = ZoneId.of("Z");
		if(dateString.length() > 12) {
			int i = 12;
			if(dateString.charAt(i) == '.') {
				while(++i < dateString.length() && Character.isDigit(dateString.charAt(i)));
				double fraction = Double.parseDouble(dateString.substring(12, i));
				nanoSecond = (int) (fraction * 1000000000);
			}
			if(dateString.length() > i) {
				zone = ZoneId.of(dateString.substring(i));
			}
		}
		return ZonedDateTime.of(year, month, day, hour, minute, second, nanoSecond, zone);
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof UTCTimeValue)) return false;
		UTCTimeValue other = (UTCTimeValue) obj;
		return date.equals(other.date);
	}

	@Override
	public int hashCode() {
		return date.hashCode();
	}

	@Override
	public String toString() {
		return date.toString();
	}
	
	

}
