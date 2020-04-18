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
 * Created Mar 5, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;

import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.ExceptionReason;

/**
 * Wrapper class for ASN.1 real values
 * @author Sharad Singhal
 */
public class RealValue extends AsnValue {
	/** Real value being encoded */
	private double value;
	/** encoding of the real value */
	private byte [] encodedValue;

	/**
	 * Create a real value
	 * @param value - value to use
	 */
	public RealValue(double value) {
		super(Tag.REAL.getTagNumber(),Tag.REAL.getTagClass(),Tag.REAL.getTagEncoding());
		this.value = value;
		if(value == 0) {
			// zero value is encoded as a zero length value with tag REAL
			encodedValue = new byte[] {Tag.REAL.getIdentifier(),0};
		} else if(value == Double.POSITIVE_INFINITY) {
			// Positive Infinity is one byte with value 0x40
			encodedValue = new byte[] {Tag.REAL.getIdentifier(),1,0x40};
		} else if(value == Double.NEGATIVE_INFINITY) {
			// Negative Infinity is one byte with value 0x41
			encodedValue = new byte[] {Tag.REAL.getIdentifier(),1,0x41};
		} else {
			// get the bits comprising the double value
			long bitValue = Double.doubleToLongBits(value);
			// Check for NaN
			 if(bitValue == 0x7ff8000000000000L)
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,"NaN value is not supported in ASN.1");
			
			 // parse the bits to get {sign, exponent, significand}
			boolean sign = bitValue < 0;	// sign is bit 63 (1 bit)
			long exponent = (0x7FF0000000000000L & bitValue) >> 52;	// exponent is bits 52:63 (11 bits)
			long significand = 0x000FFFFFFFFFFFFFL & bitValue;	// significand is bits 0:51 (52 bits)
			
			int header = sign ? 0x0C0: 0x080;	// binary encoding (bit 8 = 1) + sign (bit 7 = 1 or 0)	
			// System.out.println("Value = "+toHex(bitValue));
			if(exponent == 0) {
				// unnormalized form value = (S, emin = -Bias, 2^(1-p) * Significand)
				while ((significand & 1) == 0) significand >>= 1;
				int i;
				for (i = 51; i > 0; --i) {      // find bit number of first 1
					if ((significand & (1L << i)) != 0) break;
				}
				exponent = i - 1075;	// remove the bias
				// header will be two bytes
				header |= 0x01;
			} else {
				// Normalized double-point value = (S, E-1023, 1+2^(1-p) * Significand
				// we divide significand by 2 (and adjust exponent) until the significand is a whole number
				significand |= 0x0010000000000000L;	// add the implict 1.xxx
				exponent -= 1075;          			// IEEE 754 bias (1023) + 52 bits in significand
				// the significand is now a whole number
				while ((significand & 1) == 0) {
					significand >>= 1;	// divide significand by 2 to remove trailing 0
					exponent += 1;		// increment exponent by 2 to retain value
				}
				// if exponent will take two bytes, add 01 flag to header byte
				if ((exponent < -128) || (exponent > 127)) {
					header |= 0x01;
				}
			}
			// 1 byte tag + 1 byte length + content length
			int length = getRequiredLength(value);
			encodedValue = new byte[2+length];			
			// System.out.println("Header = "+header+" Exponent = "+toHex(exponent)+" significand = "+toHex(significand)+" bytes = "+getContentLength(value));
			encodedValue[0] = Tag.REAL.getIdentifier();
			int cursor = packLength(encodedValue, 1, length);
			encodedValue[cursor++] = (byte)header;		
			cursor = packContent(encodedValue, cursor, exponent);
			cursor = packContent(encodedValue, cursor, significand);
		}
		return;
	}
	
	private RealValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}



	public static RealValue create(byte [] buffer, int blen, int cursor) {
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
		
		RealValue v = new RealValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		// zero value is encoded as no contents
		if(contentLength == 0) {
			v.value = 0.0;
		} else if(contentLength == 1) {
			if(buffer[cursor] == 0x40) v.value = Double.POSITIVE_INFINITY;
			else if(buffer[cursor] == 0x41) v.value = Double.NEGATIVE_INFINITY;
			else throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Error decoding Real value - Unknown special value "+buffer[cursor]);
		} else {
			throw new ModelException("Real value not yet implemented");
		}
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+
					"{ Length: "+contentLength+" Cursor "+cursor+"} ::= "+v.value+"\n");
		}
		return v;
	}

	/**
	 * Get the value for this real value
	 * @return - value of this real value
	 */
	public double getValue() {
		return value;
	}

	@Override
	public byte[] getEncodedValue() {
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof RealValue)) return false;
		RealValue other = (RealValue) obj;
		return Double.doubleToLongBits(value) ==  Double.doubleToLongBits(other.value);
	}

	@Override
	public int hashCode() {
		return Double.valueOf(value).hashCode();
	}

	@Override
	public String toString() {
		return Double.valueOf(value).toString();
	}
}
