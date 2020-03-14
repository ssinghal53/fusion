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
 * Class to represent an unsigned 32-bit integer
 * @author Sharad Singhal
 */
public class UInt32 extends Number implements Serializable {

	/** Serial version UID */
	private static final long serialVersionUID = 1L;
	/** maximum dataValue possible for the unsigned integer */
	public static final long MAX_VALUE = 0xffffffffL; // 2^32-1
	/** minimum dataValue possible for the unsigned integer */
	public static final long MIN_VALUE = 0; // 0
	/** dataValue of this UnsignedInt32 */
	private Long value;
	
	/**
	 * Create an unsigned 32 number from a long dataValue
	 * @param val - dataValue for this unsigned integer. Must be 0 &lt;= val &lt; 2^32
	 */
	public UInt32(long val){
		value = val;
		if(value < MIN_VALUE || value > MAX_VALUE){
			throw new NumberFormatException();
		}
		return;
	}
	
	/**
	 * Create an unsigned 64 number from a string
	 * @param val - input string containing integer dataValue 0 &lt;= val &lt; 2^32
	 */
	public UInt32(String val) {
		value = Long.parseLong(val);
		if(value < MIN_VALUE || value > MAX_VALUE){
			throw new NumberFormatException();
		}
		return;
	}
	
	/**
	 * Return the dataValue of this unsigned integer as a long dataValue
	 * @return dataValue of the integer
	 */
	public long longValue(){
		return value;
	}
	
	/**
	 * Return the dataValue of this unsigned integer as a double dataValue
	 * @return - double dataValue containing this unsigned integer
	 */
	public double doubleValue() {
		return (double)value;
	}
	
	
	/**
	 * Compares this unsigned 64-bit integer object with the specified object
	 * for equality
	 * 
	 * @param o the object to compare
	 * @return true if the specified object is an unsigned 64-bit integer
	 *         object. Otherwise, false.
	 */ 
	@Override
	public boolean equals(Object o) {
		if (o == null || !(o instanceof UInt32)) {
			return false;
		}
		return ((UInt32)o).value == value;
	}

	@Override
	public String toString() {
		return Long.toString(value);
	}
	
	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public int intValue() {
		return value.intValue();
	}

	@Override
	public float floatValue() {
		return value.floatValue();
	}
	
}
