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
 * Created Mar 7, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage a set of value
 * @author Sharad Singhal
 */
public class SetValue extends AsnValue {
	private static int nextId = 0;
	private int setId = 0;
	private Set<AsnValue> values;
	private byte [] encodedValue;
	
	/**
	 * Create a value set
	 * @param values - values to include in set
	 */
	public SetValue(Set<AsnValue> values) {
		super(Tag.SET.getTagNumber(),Tag.SET.getTagClass(),Tag.SET.getTagEncoding());
		this.values = values;
		setId = nextId++;
		encodedValue = getEncodedValue();
		return;
	}
	
	private SetValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
		setId = nextId++;
	}


	/**
	 * Create a set value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - current cursor in the buffer
	 * @return - Set value
	 */
	public static SetValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create SetValue
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag != Tag.SET && tag != Tag.SET_OF) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected SET, found "+tag);
		
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);

		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		SetValue v = new SetValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\t"+tag+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+"{ Length: "+contentLength+" Cursor "+cursor+" id "+v.setId+"}\n");
		}
		
		// obtain embedded values
		v.values = new LinkedHashSet<AsnValue>();
		do {
			AsnValue c = AsnValue.create(buffer, blen, cursor);
			cursor += c.getEncodedValue().length;
			v.values.add(c);
			if(debug) System.out.println("\tAdded "+toHex(c.getEncodedValue())+" to Set "+v.setId);
		} while(cursor < v.encodedValue.length);
		if(debug) System.out.println("\n");
		return v;
	}

	/**
	 * Get the value of this sequence
	 * @return - list containing the values in this Set
	 */
	public List<AsnValue> getContainedValues(){
		Vector<AsnValue> v = new Vector<AsnValue>();
		for(AsnValue av : values) {
			v.add(av);
		}
		return v;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.asn1.Asn1Value#getEncoded()
	 */
	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return Arrays.copyOf(encodedValue, encodedValue.length);
		Vector<byte[]> codings = new Vector<byte[]>();
		long length = 0;
		for(AsnValue v : values) {
			byte [] coding = v.getEncodedValue();
			length += coding.length;
			codings.add(coding);
		}
		encodedValue = new byte[(int) (1+getRequiredBytesForLength(length)+length)];
		int cursor = 0;
		encodedValue[cursor++] = Tag.SET.getIdentifier();
		cursor = packLength(encodedValue, cursor, length);
		for(byte [] coding : codings) {
			for(int i = 0; i < coding.length; i++) {
				encodedValue[cursor++] = coding[i];
			}
		}
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder();
		b.append(super.toAsnString(prefix)).append("{\n");
		String prefix1 = prefix+"  ";
		for(AsnValue v : values) {
			b.append(v.toAsnString(prefix1));
		}
		b.append(prefix).append("}\n");
		return b.toString();
	}

	@Override
	public String toString() {
		return toAsnString("");
	}
	
	@Override
	public int hashCode() {
		return values.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof SetValue)) return false;
		SetValue other = (SetValue) obj;
		if(other.values.size() != values.size()) return false;
		for(AsnValue v : values) {
			if(!other.values.contains(v)) return false;
		}
		return true;
	}
}
