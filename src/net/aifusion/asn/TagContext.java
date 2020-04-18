/**
 * Copyright 2020 Sharad Singhal. All Rights Reserved.
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
