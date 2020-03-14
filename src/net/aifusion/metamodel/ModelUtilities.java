/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Apr 19, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.HashMap;
import java.util.Random;
import java.util.Vector;

/**
 * This class contains a number of static methods used in the metamodel package
 * @author Sharad Singhal
 */
public class ModelUtilities {

	/**
	 * prevent instances of this class
	 */
	private ModelUtilities() {
		return;
	}
	
	/**
	 * Escapes special characters in a string to generate a MOF formatted string
	 * @param inp the string to process
	 * @return The string with all of the special characters escaped.
	 * @see #unquote(String)
	 */
	public static String quote(String inp) {
		StringBuilder sb = new StringBuilder(inp.length());
		sb.append('\"');
		sb.append(escape(inp));
		sb.append('\"');
		return sb.toString();
	}
	
	/**
	 * Removes the first level of quotes and escapes from a string
	 *
	 * @param value - the string to unquote
	 * @return the unquoted string
	 * @see #quote(String)
	 */
	public static String unquote(String value) {
		if(value.startsWith("\"")) {
			if(value.endsWith("\"")) {
				value = unescapeString(value.substring(1, value.length() - 1));
			} else {
				throw new IllegalArgumentException("String literal " + value + " is not properly enclosed by double quotes.");
			}
		}
		return value;
	}

	/**
	 * Escapes special characters in a string for MOF strings
	 * @param str the string to process
	 * @return The string with all of the special characters escaped
	 * @see #unescapeString(String)
	 */
	public static String escape(String str) {
		int size = str.length();
		StringBuilder sb = new StringBuilder(size);
		for (int i = 0; i < size; i++) {
			char ch = str.charAt(i);
			switch (ch) {
			case 0 :
				continue;
			case '\n':
				sb.append("\\n");
				break;
			case '\t': 
				sb.append("\\t");
				break;
			case '\b': 
				sb.append("\\b");
				break;
			case '\r': 
				sb.append("\\r");
				break;
			case '\f': 
				sb.append("\\f");
				break;
			case '\\': 
				sb.append("\\\\");
				break;
			case '\'': 
				sb.append("\\\'");
				break;
			case '\"': 
				sb.append("\\\"");
				break;
			default :
				if ((ch = str.charAt(i)) < 0x20 || ch > 0x7e) {
					String s = Integer.toString(ch, 16);
					sb.append("\\x").append("0000".substring(s.length() - 4)).append(s);
				} else {
					sb.append(ch);
				}
				break;
			}
		}
		return sb.toString();
	}

	/**
	 * Removes the first level of escapes from a MOF string
	 * @param inp the string to unescape
	 * @return the unescaped string
	 * @see #escape(String)
	 */
	public static String unescapeString(String inp)
	{
		StringBuilder sb = new StringBuilder();
		int size = inp.length();
		for (int i = 0; i < size; i++) {
			char ch = inp.charAt(i);
			if(ch == '\\') {
				i++;
				if(i >= size) {
					throw new IllegalArgumentException(
							"String ended with an escape, but there was no subsequent character to escape");
				}
				ch = inp.charAt(i);
				switch (ch) {
				case 'n': sb.append('\n');
				break;
				case 't': sb.append('\t');
				break;
				case 'b': sb.append('\b');
				break;
				case 'r': sb.append('\r');
				break;
				case 'f': sb.append('\f');
				break;
				case '\\': 
				case '\'': 
				case '\"': sb.append(ch);
				break;
				case 'X': 
				case 'x':
					sb.append("\\x"); 
					break;
				default :
					throw new IllegalArgumentException(
						"Invalid escape sequence '" + ch + 
				"' (valid sequences are  \\b  \\t  \\n  \\f  \\r  \\\"  \\\'  \\\\ \\x0000 \\X0000 )");
				}
			}
			else {
				sb.append(ch);
			}
		}
		return sb.toString();
	}
	
	/**
	 * Return the string representation of a dataValue singleton
	 * @param type - data type of dataValue
	 * @param value - underlying java object containing dataValue 
	 * @return - string representation
	 */
	public static String toMOFString(DataType type, Object value){
		if(value == null) return "null";
		if(!type.matches(value)) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Type "+type+" does not match "+value);
		// TODO: This must be completed for DataTypes EnumerationValue, StructureValue, CimInstance, and annotated java classes
		switch(type){
		case OBJECTPATH:
			if(value.toString().startsWith("$")) return value.toString();
		case OCTETSTRING:
		case DATETIME:
		case STRING:
			StringBuilder b = new StringBuilder("\"");
			String s = value.toString();
			for(int i=0; i<s.length(); i++){
				char c = s.charAt(i);
				if(c == '"'){
					b.append("\\\"");
				} else if(c == '\''){
					b.append("\\\'");
				} else if(c == '\\'){
					b.append("\\\\");
				} else if(c > 31 && c < 127){
					b.append(c);
				} else if(c == '\b'){	// ascii backspace
					b.append("\\b");
				} else if(c == '\t'){	// ascii tab
					b.append("\\t");
				} else if(c == '\n'){	// new line
					b.append("\\n");
				} else if(c == '\f'){
					b.append("\\f");
				} else if(c == '\r'){
					b.append("\\r");
				} else {
					b.append("\\X");
					b.append(String.format("%04x", (int) c));
				}
			}
			b.append("\"");
			return b.toString();
		case CHAR16:
			b = new StringBuilder("\'");
			char c = value.toString().charAt(0);
			if (c == '"') {
				b.append("\\\"");
			} else if (c == '\'') {
				b.append("\\\'");
			} else if (c == '\\') {
				b.append("\\\\");
			} else if (c > 31 && c < 127) {
				b.append(c);
			} else if (c == '\b') { // ascii backspace
				b.append("\\b");
			} else if (c == '\t') { // ascii tab
				b.append("\\t");
			} else if (c == '\n') { // new line
				b.append("\\n");
			} else if (c == '\f') {
				b.append("\\f");
			} else if (c == '\r') {
				b.append("\\r");
			} else {
				b.append("\\X");
				b.append(String.format("%04x", (int) c));
			}
			b.append("\'");
			return b.toString();
		case INSTANCEVALUE:
			String v = value.toString();
			return v.substring(0, v.length()-1);	// trim trailing ";"
		default:
			break;
		}
		return value.toString();
	}
	
	/** legal string values for base64 operations. Character at index i is the coding for byte i */
	private static final String code =
			"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	
	/**
	 * Encode a byte array to a base64 encoded string
	 * @param bytes - array of bytes to be encoded
	 * @return - string representation of bytes
	 * @see #base64Decode(String)
	 */

	public static String base64Encode(byte[] bytes) {
		int val, i;
		int slen = bytes.length;
		// we anticipate a 4/3 expansion when we convert to a string
		StringBuilder b = new StringBuilder((slen * 4) / 3 + 2);
		for (i = 0; i < slen; i += 3) {
			val = (int) bytes[i] & 0x00ff;
			b.append(code.charAt(val >> 2));
			val = (val & 0x3) << 8;
			if (i + 1 >= slen) {
				b.append(code.charAt(val >> 4));
				b.append("==");
				break;
			}
			val |= (int) bytes[i + 1] & 0x00ff;
			b.append(code.charAt(val >> 4));
			val = (val & 0xf) << 8;
			if (i + 2 >= slen) {
				b.append(code.charAt(val >> 6));
				b.append("=");
				break;
			}
			val |= (int) bytes[i + 2] & 0x00ff;
			b.append(code.charAt(val >> 6));
			b.append(code.charAt(val & 0x3f));
		}
		return b.toString();
	}
	
	/**
	 * Encode a byte array to a base64 encoded string
	 * @param bytes - array containing bytes to be encoded
	 * @param offset - offset within array where transformation starts
	 * @param length - number of bytes to be encoded
	 * @return - string containing base64 representation of the array
	 * @throws ModelException if the operation goes past either end of the array
	 */
	public static String base64Encode(byte[] bytes, int offset, int length){
		int val, i;
		int slen = length;
		if(offset < 0 || length < 0 || bytes.length < offset+length){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Operation goes past end of array ["+bytes.length+"] offset: "+offset+" length: "+length);
		}
		// we anticipate a 4/3 expansion when we convert to a string
		StringBuilder b = new StringBuilder((slen * 4) / 3 + 2);
		for (i = offset; i < offset+slen; i += 3) {
			val = (int) bytes[i] & 0x00ff;
			b.append(code.charAt(val >> 2));
			val = (val & 0x3) << 8;
			if (i + 1 >= slen) {
				b.append(code.charAt(val >> 4));
				b.append("==");
				break;
			}
			val |= (int) bytes[i + 1] & 0x00ff;
			b.append(code.charAt(val >> 4));
			val = (val & 0xf) << 8;
			if (i + 2 >= slen) {
				b.append(code.charAt(val >> 6));
				b.append("=");
				break;
			}
			val |= (int) bytes[i + 2] & 0x00ff;
			b.append(code.charAt(val >> 6));
			b.append(code.charAt(val & 0x3f));
		}
		return b.toString();
	}
	
	/**
	 * Decode a base64 encoded string to a byte array
	 * @param s - string to be decoded
	 * @return - decoded byte array
	 * @see #base64Encode(byte[])
	 */
	public static byte[] base64Decode(String s) {
	    if (s == null) {
	        return null;
	    }
	    if(s.contains("\n") || s.contains("\r") || s.contains("-") || s.contains("_")){
	    	// strip out external characters
	    	StringBuilder b = new StringBuilder(s.length());
	    	for(int i = 0; i < s.length(); i++){
	    		char c = s.charAt(i);
	    		if(c == '\n' || c == '\r') continue;
	    		b.append(c == '-' ? '+' : c == '_' ? '/' : c);
	    	}
	    	s = b.toString();
	    }
		int bmax = s.length();
		byte[] bytes = new byte[bmax];
		int blen = 0;
		int end = -1;
		// note that the encoded string must be a multiple of 4
		// if not, up to the last three characters may be ignored
		// in this conversion
		for (int i = blen = 0; i < s.length(); i += 4) {
			// System.out.print(s.substring(i,i+4)+" : ");
			int c0 = code.indexOf(s.charAt(i));
			int c1 = code.indexOf(s.charAt(i + 1));
			int c2 = code.indexOf(s.charAt(i + 2));
			int c3 = code.indexOf(s.charAt(i + 3));
			// System.out.print(c0+","+c1+","+c2+","+c3+" : ");
			bytes[blen++] = (byte) ((c0 << 2) | (c1 >> 4));
			if (c2 == end)
				break;
			bytes[blen++] = (byte) (((c1 & 0x0f) << 4) | (c2 >> 2));
			if (c3 == end)
				break;
			bytes[blen++] = (byte) (((c2 & 0x03) << 6) | c3);
			// System.out.println(bytes[blen-3]+","+bytes[blen-2]+","+bytes[blen-1]);
		}
		byte[] b = new byte[blen];
		for (int i = 0; i < blen; i++) {
			b[i] = bytes[i];
		}
		return b;
	}
	
	/**
	 * Normalize a local path to remove . and .. segments.
	 * @param localPath - local path to be checked
	 * @return - local path with all "." and ".." segments removed. An empty local path will return "/"
	 */
	public static String normalizePath(String localPath){
		Vector<String> pathElements = getPathElements(localPath);
		if(pathElements.size() > 0){
			StringBuilder b = new StringBuilder();
			for(String s : pathElements){
				b.append("/");
				b.append(s);
			}
			return b.toString();
		} else
			return "/";
	}
	
	/**
	 * Get path elements from a local path definition. The path is split using "/" or "\" characters.
	 * All '.' and empty segments are ignored, and all ".." segments move up the hierarchy without going past 
	 * the initial level. All path elements are converted to lower case
	 * @param localPath - local path to be broken
	 * @return - vector containing path elements
	 */
	public static Vector<String> getPathElements(String localPath){
		Vector<String> pathElements = new Vector<String>();
		if (localPath != null) {
			String[] elements = localPath.split("[/\\\\]");
			for (int i = 0; i < elements.length; i++) {
				if(elements[i] == null) continue;
				String element = elements[i].trim().toLowerCase();
				if (element.length() == 0 || element.equals(".")) continue;
				if(element.equals("..")){
					if(pathElements.size() > 0){
						pathElements.setSize(pathElements.size()-1);
						continue;
					} else {
						// attempt to go above the path root. Throw an exception
						throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Path "+localPath+"resolves above /");
					}
				}
				pathElements.add(element);
			}
		}
		return pathElements;
	}
	
	/**
	 * Get a hashmap containing {name, value} pairs passed as an argument list.
	 * Each {name, value} pair is represented as [-name value] ... in the argument list. In
	 * case the value contains spaces, it can either be quoted, or be enclosed in braces.
	 * The returned Hashmap is keyed by name (sans the '-') and contains the corresponding
	 * value. Note that names are converted to lowercase, so are case insensitive
	 * @param argv - string array to be parsed
	 * @return - argument map
	 */
	public static HashMap<String,String> getArgs(String [] argv){
		HashMap<String,String> args = new HashMap<String,String>();
		for(int i=0; i<argv.length-1; i++ ){
			String token = argv[i];
			if(!token.startsWith("-")) continue;
			token = token.substring(1); // strip the - sign in front
			String value = argv[++i];
			if(value.equals("\\*")) value = "*";
			if(value.startsWith("{")){	// have a quoted value
				StringBuffer b = new StringBuffer(value);
				if(!value.endsWith("}")){
					while(i < argv.length-1){
						b.append(" ");
						b.append(argv[++i]);
						if(argv[i].endsWith("}")) break;
					}
				}
				value = b.substring(1,b.length()-1);	// strip the braces from value
			}
			args.put(token.toLowerCase(), value);
		}
		return args;
	}
	
	/**
	 * Get a random string of ascii characters
	 * @param len - length of string
	 * @return random string
	 */
	public static String getRandomString(int len){
		String ascii = "!#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~";
		StringBuilder b = new StringBuilder();
		Random r = new Random();
		for(int i = 0; i < len; i++){
			b.append(ascii.charAt(r.nextInt(ascii.length())));
		}
		return b.toString();
	}
}
