/**
 * Copyright 2013, Sharad Singhal, All Rights Reserved
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
 * Created Dec 20, 2013 by Sharad Singhal
 */

package net.aifusion.metamodel;

import java.io.Serializable;

/**
 * Class to represent an unsigned 16-bit integer
 * @author Sharad Singhal
 *
 */
public class UInt16 extends Number implements Serializable {
	
	/** serial version UID */
	private static final long serialVersionUID = 1L;
	/** maximum dataValue possible for the unsigned integer */
	public static final int MAX_VALUE = 0xffff; // 2^16-1
	/** minimum dataValue possible for the unsigned integer */
	public static final int MIN_VALUE = 0; // 0
	/** dataValue of this UInt8 */
	private Integer value;
	
	/**
	 * Create an unsigned 16-bit integer from a string dataValue
	 * @param val - string containing unsigned 16-bit integer
	 */
	public UInt16(String val) {
		value = Integer.valueOf(val);
		if(value < MIN_VALUE || value > MAX_VALUE){
			throw new NumberFormatException();
		}
		return;
	}

	/**
	 * Create an unsigned 16-bit integer from an integer dataValue
	 * @param i - integer dataValue containing 16 bit integer
	 */
	public UInt16(int i) {
		value = i;
		if(value < MIN_VALUE || value > MAX_VALUE){
			throw new NumberFormatException();
		}
		return;
	}

	public int intValue(){
		return value;
	}
	
	public long longValue() {
		return value;
	}

	
	public double doubleValue() {
		return value;
	}
	
	public float floatValue() {
		return value;
	}
	
	public boolean equals(Object o){
		if (o == null || !(o instanceof UInt16)) {
			return false;
		}
		return ((UInt16)o).value == value;
	}
	
	public int hashCode() {
		return value.hashCode();
	}
	
	public String toString() {
		return Integer.toString(value);
	}
}
