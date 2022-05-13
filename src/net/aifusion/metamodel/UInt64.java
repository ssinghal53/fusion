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

import java.math.BigInteger;

/**
 * The UnsignedInt64 class represents an unsigned 64-bit number
 * @author Sharad Singhal
 */
public class UInt64 extends BigInteger {
	
	/** Serial version UID */
	private static final long serialVersionUID = 2975918635159311032L;
	/** maximum dataValue possible for the unsigned integer */
	public static final BigInteger MAX_VALUE = new BigInteger("18446744073709551615"); // 2^64-1
	/** minimum dataValue possible for the unsigned integer */
	public static final BigInteger MIN_VALUE = new BigInteger("0"); // 0

	/**
	 * Create an unsigned 64 number from a string
	 * @param val - input string containing integer dataValue 0 &lt; val &lt; 2^64
	 */
	public UInt64(String val) {
		super(val);
		if(compareTo(MIN_VALUE)<0 || compareTo(MAX_VALUE)>0){
			throw new NumberFormatException();
		}
		return;
	}
	
	public UInt64(long val) {
		this(Long.toString(val));
		return;
	}
	
	/**
	 * Compares this unsigned 64-bit integer object with the specified object
	 * for equality
	 * 
	 * @param o the object to compare
	 * @return true if the specified object is an unsigned 64-bit integer equals to this integer
	 *         Otherwise, false.
	 */ 
	@Override
	public boolean equals(Object o) {
		if (o == null || !(o instanceof UInt64)) {
			return false;
		}
		return super.equals(o);
	}
	
}
