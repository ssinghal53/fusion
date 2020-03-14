/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Dec 5, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.Arrays;

/**
 * Class to represent a CIM OctetString
 * An octetString is a wrapper around a byte array
 * @author Sharad Singhal
 */
public class OctetString {
	final private static String hex = "0123456789abcdef";
	final private static char[] hexValues = hex.toCharArray();
	private String octetString;
	private byte [] value;

	/**
	 * Create an OctetString
	 * @param octets - String representation of a byte array in hex representation
	 */
	public OctetString(String octets) {
		if(octets == null) return;
		octetString = octets.toLowerCase();
		if(!octetString.startsWith("0x")) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected string starting with 0x. Found "+octets);
		int nBytes = octetString.length()/2 - 1;
		if(octetString.length() != (nBytes * 2 + 2)) throw new ModelException(ExceptionReason.INVALID_PARAMETER,
				"OctetString expected "+(nBytes * 2 + 2)+" characters found "+octetString.length());
		value = new byte[nBytes];
		for(int i=0, j=2; i < nBytes; i++, j += 2){
			int upperNibble = hex.indexOf(octetString.charAt(j));
			int lowerNibble = hex.indexOf(octetString.charAt(j+1));
			if(upperNibble < 0 || lowerNibble < 0) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"NonHex character in OctetString "+octetString.toString());
			value[i] = (byte) (upperNibble << 4 | lowerNibble);
		}
		return;
	}
	
	/**
	 * Create an OctetString from a byte array
	 * @param octets - array containing the bytes
	 */
	public OctetString(byte [] octets){
		if(octets == null) return;
		value = octets;
		StringBuilder b = new StringBuilder("0x");
		for ( int j = 0; j < octets.length; j++ ) {
			int v = octets[j] & 0xFF;
			b.append(hexValues[v >>> 4]);
			b.append(hexValues[v & 0x0F]);
		}
		octetString = b.toString();
		return;
	}
	
	/**
	 * Get the value of this octetString as a byte []
	 * @return - byte [] containing the value of this octetString. Null is returned if the underlying octetString has no value
	 */
	public byte [] getValue(){
		if(value == null) return null;
		return Arrays.copyOf(value, value.length);
	}
	
	@Override
	public int hashCode() {
		return octetString.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof OctetString)) return false;
		OctetString other = (OctetString) obj;
		if(octetString != null) return octetString.equals(other.octetString);
		return other.octetString == null ? true : false;
	}

	@Override
	public String toString() {
		return octetString;
	}
	
}
