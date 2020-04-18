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
 * Created Mar 11, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage a tagged value
 * @author Sharad Singhal
 *
 */
public class TaggedValue extends AsnValue {
	private byte [] encodedValue;
	private AsnValue value;
	
	/*
	 * TODO: Note that since we require the ASN definitions to decode IMPLICIT mode values, the implementation here only uses the
	 * Explicit mode encoder/decoder.
	 * 
	 * From Dubuisson Page 409
	 * 18.2.16 Tagged value
	 * If a type is tagged in IMPLICIT mode (or if the module includes the IMPLICIT TAGS or AUTOMATIC TAGS clause in its header):
	 * 	v [1] IMPLICIT INTEGER ::= -38
	 * only the tag that appears on the left-hand side of the IMPLICIT keyword is encoded in the tag Field T:
	 * [CS. 1]  |     L    |     V
	 * 10000001 | 00000001 | 11011010
	 * 
	 * If the type is tagged in EXPLICIT mode (or if the module includes the EXPLICIT TAGS clause in its header):
	 * 	v [APPLICATION 0] EXPLICIT INTEGER ::= 38
	 * the value is encoded in constructed form as a series of TLV triplets where the tag fields T contain all 
	 * the subsequent tags until the UNIVERSAL class tag of the type is encountered; this tag must be included in the encoding
	 * (see rules <2> and <3> on page 216):
	 * [APPL. 0] | L        | [UNIV. 2] | L        | V
	 * 01100000  | 00000011 | 00000010  | 00000001 | 00100110
	 * 
	 * <2> If a type is tagged, directly or indirectly, in the EXPLICIT mode (i.e. the tag is marked EXPLICIT, or it is not marked but, either the module
	 * contains the clause EXPLICIT TAGS in its header, either it contains no global-tagging clause), all the tags preceding the keyword EXPLICIT and
	 * those which follow (potentially until the next tag marked, directly or indirectly, IMPLICIT), including the tag of class UNIVERSAL associated
	 * by default with the type, are encoded in BER, whatever their class (UNIVERSAL, context-specific, APPLICATION or PRIVATE).
	 * 
	 * <3> If a type is tagged, directly or indirectly, in the IMPLICIT mode (i.e. the tag is marked IMPLICIT, or it is not marked but the module contains
	 * the clause IMPLICIT TAGS in its header), only the tags preceding the keyword IMPLICIT are transmitted in BER (in particular, the default tag,
	 * of class UNIVERSAL, is not encoded except if a tag marked, directly or indirectly, EXPLICIT is found after the tag marked IMPLICIT).
	 * 
	 * <4> For a definition such as T ::= [1] IMPLICIT [0] EXPLICIT INTEGER, both the tags [1] and [UNIVERSAL 2] are transmitted by a BER encoder
	 * whereas for T ::= [1] EXPLICIT [0] IMPLICIT INTEGER, only the tags [1] and [0] are transmitted.
	 * 
	 * From Larmouth Page 267-268
	 * 
	 * integer1 INTEGER ::= 72
	 * integer2 [1] IMPLICIT INTEGER ::= 72
	 * integer3 [APPLICATION 27] EXPLICIT INTEGER ::= 72
	 * 
	 * are coded as
	 *           T L  V
	 * integer1 02 01 48		// we would not come to the TaggedValue for decoding this
	 * integer2 C1 01 48		// TagClass = Context Sensitive, Encoding = Primitive TagNumber = 1
	 * integer3 7B 03 T  L  V	// TagClass = Application, Encoding = Constructed, TagNumber = 27
	 *                02 01 48
	 * where the 7B is made up, in binary, as follows:
	 * Class         P/C       Number
	 * APPLICATION Constructed 27
	 * 01              1       11011 = 01111011 = 7B
	 * 
	 */
	
	
	public TaggedValue(long tagNumber, AsnValue value) {
		super(tagNumber,TagClass.CONTEXT_SPECIFIC,TagEncoding.CONSTRUCTED);
		this.value = value;
		encodedValue = getEncodedValue();
		return;
	}
	
	private TaggedValue(long tagNumber, TagClass tagClass, TagEncoding encoding) {
		super(tagNumber, tagClass, encoding);
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue != null) return encodedValue;
		
		byte [] content = value.getEncodedValue();
		byte [] encodedTag = Tag.getEncoded(getTagNumber(), getTagClass(), getTagEncoding());
		encodedValue = new byte[encodedTag.length+getRequiredBytesForLength(content.length)+content.length];
		
		for(int i = 0; i < encodedTag.length; i++) {
			encodedValue[i] = encodedTag[i];
		}
		packContent(encodedValue, encodedTag.length, content);
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	/**
	 * Get the underlying value associated with this tagged value
	 * @return value of this tagged value
	 */
	public AsnValue getValue() {
		return value;
	}
	
	
	/**
	 * Create a tagged value
	 * @param buffer - input buffer
	 * @param blen - length of input buffer
	 * @param cursor - current cursor in buffer
	 * @return - tagged value
	 */
	public static TaggedValue create(byte [] buffer, int blen, int cursor) {
		int saved = cursor;
		// check tag and create tagged value
		Tag tag = Tag.locate(buffer[cursor]);
		TagClass tagClass = TagClass.getTagClass(buffer[cursor]);
		if(tagClass != TagClass.CONTEXT_SPECIFIC) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected CONTEXT_SPECIFIC found "+tagClass);
		
		TagEncoding tagEncoding = TagEncoding.getTagEncoding(buffer[cursor]);
		long tagNumber = getTagNumber(buffer, blen, cursor);
		cursor = skipPastTag(buffer, blen, cursor);
		long contentLength = getContentLength(buffer, blen, cursor);
		cursor = skipPastLength(buffer, blen, cursor);		
		
		TaggedValue v = new TaggedValue(tagNumber, tagClass, tagEncoding);
		v.encodedValue = Arrays.copyOfRange(buffer, saved, (int) (cursor+contentLength));
		
		if(debug) {
			System.out.println("\t"+toHex(v.encodedValue));
			System.out.println("\tTAGGED_VALUE"+" ["+tagClass+" "+tagNumber+"]("+tagEncoding+") "+tag+" { Length: "+contentLength+" Cursor "+cursor+"}\n");
		}
		v.value = AsnValue.create(buffer, blen, cursor);
		return v;
	}

	@Override
	public String toAsnString(String prefix) {
		StringBuilder b = new StringBuilder(prefix).append("[");
		if(value.getTag() != null && value.getTag() != Tag.NULL) b.append(value.getTag()).append(" ");
		b.append(getTagNumber()).append("] ::= ").append(value.toString()).append("\n");
		return b.toString();
	}
}
