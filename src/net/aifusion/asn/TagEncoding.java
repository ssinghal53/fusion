/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
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
