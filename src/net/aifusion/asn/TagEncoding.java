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
 * Created Mar 10, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

/**
 * Enumeration to handle Tag Encoding Flag
 * @author Sharad Singhal
 *
 */
public enum TagEncoding {
	/** Primitive encoding */
	PRIMITIVE(0),
	/** Constructed encoding */
	CONSTRUCTED(0x020);
	
	/** Flag associated with the encoding */
	private long encoding;
	
	/**
	 * Create the encoding
	 * @param encoding -flag associated with the encoding
	 */
	private TagEncoding(long encoding) {
		this.encoding = encoding;
		return;
	}
	
	/**
	 * Get the encoding from an identifier
	 * @param identifier - tag identifier
	 * @return - associated encoding
	 */
	public static TagEncoding getTagEncoding(byte identifier) {
		return (identifier & 0x020) == 0 ? PRIMITIVE : CONSTRUCTED;
	}
	
	/**
	 * Get the flag associated with this encoding
	 * @return - flag associated with this encoding
	 */
	public long getTagEncoding() {
		return encoding;
	}
	
	/**
	 * Test if a tag encoding is primitive
	 * @param tagValue - tag value to test
	 * @return - true if the encoding is primitive, false otherwise
	 */
	public static boolean isPrimitive(long tagValue) {
		return (tagValue & 0x020) == PRIMITIVE.encoding;
	}
	
	/**
	 * Test if a tag encoding is Constructed
	 * @param tagValue - tag value to test
	 * @return - true if the encoding is constructed, false otherwise
	 */
	public static boolean isConstructed(long tagValue) {
		return (tagValue & 0x020) == CONSTRUCTED.encoding;
	}
}
