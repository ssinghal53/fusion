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
 * Created May 7, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.io.UnsupportedEncodingException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to represent a Generalized Time Value
 * @author Sharad Singhal
 */
public class GeneralizedTimeValue extends AsnValue {
	private String value;
	private byte [] encodedValue;
	private ZonedDateTime date;

	/**
	 * Create a Generalized time value
	 * @param value - String representation of the time value ("yyyyMMddHHmmssZ")
	 */
	public GeneralizedTimeValue(String value) {
		super(Tag.GENERALIZED_TIME.getTagClass(),Tag.GENERALIZED_TIME.getTagEncoding(),Tag.GENERALIZED_TIME.getTagNumber());
		this.value = value;
		try {
			date = getDate(value);
		} catch (Exception e) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Invalid date format "+value);
		}
		return;
	}
	
	private GeneralizedTimeValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagClass, encoding, tagNumber);
	}
	
	/**
	 * Create a Generalized Time value
	 * @param buffer - input buffer
	 * @param blen - length of the input buffer
	 * @param cursor - cursor within the buffer
	 * @return - Generalized time value
	 */
	public static GeneralizedTimeValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create IntegerValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.GENERALIZED_TIME) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected GENERALIZED_TIME, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		GeneralizedTimeValue v = new GeneralizedTimeValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		StringBuilder b = new StringBuilder();
		for(int i = 0; i < contentLength; i++) {
			b.append(Character.valueOf((char) (buffer[cursor+i] & 0x0FF)));
		}
		v.value = b.toString();
		try {
			v.date = getDate(v.value);
		} catch (Exception e) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Invalid date format "+v.value);
		}
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
		byte [] header = Tag.getEncoded(getTagClass(), getTagEncoding(), getTagNumber());
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
		// yyyyMMddHHmmss - local time with no time zone
		// yyyyMMddHHmmss.fraction - local time with fractional seconds
		// yyyyMMddHHmmssZ - UTC time
		// yyyyMMddHHmmss.fractionZ - UTC fractional time
		// yyyyMMddHHmmss.fraction[+-]xxxx - where offset is offset in hhmm from UTC
		int year = Integer.parseInt(dateString.substring(0,4));
		int month = Integer.parseInt(dateString.substring(4,6));
		int day = Integer.parseInt(dateString.substring(6,8));
		int hour = Integer.parseInt(dateString.substring(8,10));
		int minute = Integer.parseInt(dateString.substring(10,12));
		int second = Integer.parseInt(dateString.substring(12,14));
		int nanoSecond = 0;
		ZoneId zone = ZoneId.of("Z");
		if(dateString.length() > 14) {
			int i = 14;
			if(dateString.charAt(i) == '.') {
				while(++i < dateString.length() && Character.isDigit(dateString.charAt(i)));
				double fraction = Double.parseDouble(dateString.substring(14, i));
				nanoSecond = (int) (fraction * 1000000000);
			}
			if(dateString.length() > i) {
				zone = ZoneId.of(dateString.substring(i));
			}
		}
		return ZonedDateTime.of(year, month, day, hour, minute, second, nanoSecond, zone);
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
		if(obj == null || !(obj instanceof GeneralizedTimeValue)) return false;
		GeneralizedTimeValue other = (GeneralizedTimeValue) obj;
		return value.equals(other.value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return value.toString();
	}
}
