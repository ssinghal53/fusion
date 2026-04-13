/**
 * Copyright 2025 Sharad Singhal. All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
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
 * Created Dec 28, 2025 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.Arrays;

import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to wrap a CIM DataValue in an ASN.1 wrapper
 * @author Sharad Singhal
 */
public class CimDataValue extends AsnValue {
	private DataValue value;
	private byte [] encodedValue = null;
	
	public CimDataValue(DataValue value) {
		super(Tag.locate(value.getType()));
		this.value = value;
		return;
	}

	@Override
	public byte[] getEncodedValue() {
		if(encodedValue == null) encodedValue = CimCodec.encode(value);
		return Arrays.copyOf(encodedValue, encodedValue.length);
	}
	
	public static CimDataValue create(byte [] buffer, int blen, int cursor) {
		throw new ModelException(ExceptionReason.NOT_SUPPORTED);
	}

}
