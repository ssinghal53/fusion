/**
 * Copyright 2020 Sharad Singhal. All Rights Reserved.
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
 * Created Mar 29, 2020 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.List;
import java.util.Map;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to represent a tag context for context sensitive tags<br>
 * A tag context defines the &lt;tagNumber,tag&gt; mapping for context sensitive tags used
 * for encoding or decoding BER when IMPLICIT or AUTO tag definitions are being used.
 * @author Sharad Singhal
 */
public class TagContext {
	private int cursor = 0;
	private List<Map<Integer,Tag>> context;
	
	/**
	 * Create a tag context that defines the tagged types in an ASN.1 Encoding
	 * @param context - context to be used within the current definition
	 */
	public TagContext(List<Map<Integer,Tag>> context) {
		if(context == null || context.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Context must not be null or empty");
		this.context = context;
		return;
	}
	
	/**
	 * advance the mapping to the next mapping in the ASN.1 context definition
	 */
	public void advance() {
		cursor++;
	}
	
	/**
	 * Get the expected Tag type for a tag number in the current context
	 * @param tagNumber - tag number to look up
	 * @return - Tag representing expected data type
	 */
	public Tag getExpectedTag(int tagNumber) {
		if(cursor >= context.size()) throw new ModelException(ExceptionReason.INVALID_ENUMERATION_CONTEXT,"Cursor is past the context size "+context.size());
		Map<Integer,Tag> currentContext = context.get(cursor);
		if(!currentContext.containsKey(tagNumber))
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Context does not define tag number "+tagNumber);
		return currentContext.get(tagNumber); 
	}
}
