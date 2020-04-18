/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 10, 2018 by sharad
 */
package net.aifusion.asn;

/**
 * Enumeration to handle Tag Classes
 * @author Sharad Singhal
 *
 */
public enum TagClass {
	/** Universal Tag Class */
	UNIVERSAL(0),
	/** Application Tag Class */
	APPLICATION(0x040),
	/** Context-specific Tag Class */
	CONTEXT_SPECIFIC(0x080),
	/** Private Tag Class */
	PRIVATE(0x0C0);
	
	/** Class for the current TagClass */
	private long tagClass;
	
	/**
	 * Create a tag class
	 * @param tagClass
	 */
	private TagClass(long tagClass) {
		this.tagClass = tagClass;
		return;
	}
	
	/**
	 * Get the flag associated with this tagClass 
	 * @return - flag associated with this class
	 */
	public long getTagClass() {
		return tagClass;
	}
	
	/**
	 * Get the tagClass associated with a tag value
	 * @param tagIdentifier - tag identifier (containing tag number/Class/Encoding) 
	 * @return - associated tag class
	 */
	public static TagClass getTagClass(byte tagIdentifier) {
		long tagClass = tagIdentifier & 0x0C0;
		for(TagClass c : TagClass.values()) {
			if(tagClass == c.tagClass) return c;
		}
		return null;	// should not happen
	}
}
