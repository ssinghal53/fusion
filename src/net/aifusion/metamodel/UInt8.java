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
 * Class to represent an unsigned 8-bit integer
 * @author Sharad Singhal
 *
 */
public class UInt8 extends Number implements Serializable {
	
	private static final long serialVersionUID = 1L;
	/** maximum dataValue possible for the unsigned integer */
	public static final short MAX_VALUE = 0xff; // 2^8 - 1
	/** minimum dataValue possible for the unsigned integer */
	public static final short MIN_VALUE = 0; // 0
	/** dataValue of this UInt8 */
	private Short value;
	

	/**
	 * Create an unsigned 8-bit integer
	 * @param val - string containing an unsigned 8 bit integer 0 &lt;= dataValue &lt; 2^8
	 */
	public UInt8(String val) {
		value = Short.valueOf(val);
		if(value < MIN_VALUE || value > MAX_VALUE){
			throw new NumberFormatException();
		}
		return;
	}

	/**
	 * Create an unsigned 8-bit integer from a short dataValue
	 * @param s - short dataValue containing an unsigned 8-bit integer 0 &lt;= dataValue &lt; 2^8
	 */
	public UInt8(short s) {
		value = s;
		if(value < MIN_VALUE || value > MAX_VALUE){
			throw new NumberFormatException();
		}
		return;
	}

	/**
	 * Return the dataValue of this unsigned 8-bit integer as a short dataValue
	 * @return - short containing dataValue
	 */
	public short shortValue(){
		return value;
	}
	/**
	 * Return the dataValue of this unsigned 8-bit integer as an integer dataValue
	 * @return - int containing dataValue
	 */
	public int intValue(){
		return value;
	}
	
	/**
	 * Return the dataValue of this unsigned 8-bit integer as a long dataValue
	 * @return - long containing dataValue
	 */
	public long longValue() {
		return value;
	}

	/**
	 * Return the dataValue of this unsigned 8-bit integer as a double dataValue
	 * @return - double containing dataValue
	 */
	public double doubleValue() {
		return value;
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o){
		if (o == null || !(o instanceof UInt8)) {
			return false;
		}
		return ((UInt8)o).value == value;
	}
	
	@Override
	public String toString() {
		return Short.toString(value);
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public float floatValue() {
		return value.floatValue();
	}
}
