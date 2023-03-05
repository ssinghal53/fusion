/**
 * Copyright 2013, 2018, Sharad Singhal, All Rights Reserved
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
 * Created Dec 29, 2013 by Sharad Singhal
 * Last Modified Jan 2, 2018
 */
package net.aifusion.metamodel;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to represent a CIM ObjectPath. An object path represents the globally unique name of a named element, and can be used to access that
 * element.
 * @author Sharad Singhal
 */
public class ObjectPath {
	/** Type of the Element represented by this object path (required) */
	private ElementType type;
	/** Name of the object (required) */
	private String objectName;
    /** Alias if any */
    private String alias = null;
    /** Key dataValue pairs in this object name (for instances) */
    private TreeMap<String, DataValue> keyValues = null;
    /** NameSpace Path for this object */
    private NameSpacePath nameSpacePath = null;
    /** Pattern to parse the ObjectName (in DSP0004 format) */
    private static Pattern pattern = Pattern.compile("^(([a-zA-Z0-9+-]+):)?(//([^/]+))?((/[^:]*):)?(([a-zA-Z0-9]+_)?\\w+)(\\.(.+))?$");
	
    
    /**
     * Create an object path based on its type, name, name space path, and key-value pairs
     * @param type - type of named object (must be non-null)
     * @param objectName - name of the object (must be non-null)
     * @param path - Name space path for this object (must be non-null)
     * @param keys - keys associated with this object (must be non-null for instances; must be null or empty for all other element types)
     * @param alias - alias, if any defined on this object.
     */
	public ObjectPath(ElementType type, String objectName, NameSpacePath path, Map<String,DataValue> keys, String alias) {
		if(type == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected non-null ElementType, found null");
		if(path == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected non-null NameSpacePath, found null");
		if(objectName == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected non-null objectName, found null");
		if(type == ElementType.INSTANCE && (keys == null || keys.isEmpty())) throw new ModelException(ExceptionReason.INVALID_PARAMETER,
				"Instance paths must declare key values");
		this.type = type;
		this.objectName = objectName;
		this.nameSpacePath = path;
		this.alias = alias;
		if(keys != null && !keys.isEmpty()){
			if(type != ElementType.INSTANCE && type != ElementType.STRUCTUREVALUE) 
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,objectName+
					"Only instances or structure values can have keys defined - found type "+type);
			keyValues = new TreeMap<String,DataValue>();
			for(String key : keys.keySet()){
				DataValue keyValue = keys.get(key);
				DataType keyType = keyValue.getType();
				if(keyType.isArray() || !(keyType.isPrimitive() || keyType.isReference())) 
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,objectName+
						" objectPath can only have non-array primitive or reference key values, found "+keyValue);
				keyValues.put(key.toLowerCase(), keyValue);
			}
		}
		return;
	}
	
	/**
	 * Create an object path based on a class name and an instance alias, where alias is of the form "$" [a-zA-Z0-9_]+<br>
	 * @param elementType Type of this element (instance or structurevalue)
	 * @param className - name of class or structure
	 * @param alias - alias for the instance of the class, or value of the structure
	 */
	public ObjectPath(ElementType elementType, String className, String alias){
		if(className == null || className.trim().isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected non-empty className, found "+className);
		if(alias == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected non-null alias, found null");
		if(elementType != ElementType.INSTANCE && elementType != ElementType.STRUCTUREVALUE)
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected INSTANCE/STRUCTUREVALUE, found "+elementType);
		this.type = elementType;
		this.objectName = className;
		this.alias = alias;
		if(!alias.matches("^\\$[a-zA-Z0-9_]+$")){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected alias, found "+alias);
		}
		return;
	}
    
	/**
     * Create an object path from a path specification as defined in DSP0004 and extended for CimV3. The following forms are recognized<br>
     * className: schemaName "_" identifier<br>
     * instanceName : className "." key "=" dataValue [, key "=" dataValue]*<br>
     * localClassPath : ["/elementType"] "/" pathName ["/" pathName]* ":" className<br>
     * localInstancePath : ["/instanceType"] "/" pathName ["/" pathName]* ":" instanceName<br>
     * classPath : scheme "://" authority localClassPath<br>
     * instancePath : scheme "://" authority localInstancePath<br>
     * where<br>
     * schemaName : [A-Za-z0-9]+<br>
     * identifier : [A-Za-z0-9_]+<br>
     * scheme : [A-Za-z0-9+-.]+<br>
     * authority : ascii char set not including "/"<br>
     * The term "/elementType" above can take the forms "/" ["class" | "structure" | "enumeration" | "qualifiertype" | "interface"]<br>
     * The term "/instanceType" above can take the forms "/" ["instance" | "structurevalue"] 
     * @param objectName - string dataValue containing object name
     */
	public ObjectPath(String objectName){
		// null dataValue is illegal
		if(objectName == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"ObjectPath: ObjectName must be non-null");
		// check for a valid name dataValue
        Matcher m = pattern.matcher(objectName);
        if (!m.matches() || m.groupCount() != 10) {
            throw new ModelException(ExceptionReason.INVALID_PARAMETER, "ObjectPath: Malformed reference " + objectName);
        }
        // for(int i=0; i<=9;i++) System.out.println("["+i+"] "+m.group(i));
        // this will throw exception if localPath is null
        String path = m.group(6);
        if(path != null) {
        	String [] pathElements = m.group(6).split("/");
        	switch(pathElements[1]) {
        	case "class":
        	case "instance":
        	case "structure":
        	case "enumeration":
        	case "qualifiertype":
        	case "interface":
        	case "structurevalue":
        		type = ElementType.valueOf(pathElements[1].toUpperCase());
        		path = path.substring(pathElements[1].length()+1);
        		break;
        	default:
        		break;
        	}
        	// for(String s : pathElements) System.out.print("["+s+"]");
        	// System.out.println(type+" "+path);
        }
        this.nameSpacePath = new NameSpacePath(m.group(2) /* scheme */,m.group(4) /*authority*/,path/*m.group(6) localPath*/);
        String className = m.group(7); // className - must not be null
        if (className == null) {
            throw new ModelException(ExceptionReason.INVALID_PARAMETER, "ObjectPath: className not found in " + objectName);
        } else {
        	this.objectName = className;
        }
        // objectName must contain _ unless it is a qualifierType
        if(!ElementType.QUALIFIERTYPE.equals(type) && !objectName.contains("_")) {
        	throw new ModelException(ExceptionReason.INVALID_PARAMETER, "ObjectPath: ObjectName must be of form schema_name, found " + objectName);
        }
        
        String value = m.group(10); // keyValue pairs
        // System.out.println(dataValue);
        // note that care is needed to parse, since separators can occur in quoted strings
        if (value != null) {
            StringBuilder b = new StringBuilder();
            int i = 0;
            char c;
            while (i < value.length()) {
            	// get the key name
                b.setLength(0);
                while (i < value.length()) {
                    c = value.charAt(i++);
                    if (c == '=')
                        break;
                    b.append(c);
                }
                String key = b.toString().toLowerCase();
                DataValue val = null;
                // System.out.println("Key: "+key);
                // get the key dataValue
                b.setLength(0);
                while (i < value.length()) {
                    c = value.charAt(i++);
                    if (c == '"') {
                        // read a string dataValue (includes dates).
                    	// TODO: need to interpret escape characters in strings
                        while (i < value.length()) {
                            c = value.charAt(i++);
                            if (c == '"')
                                break;
                            if (c == '\\')
                                c = value.charAt(i++);
                            b.append(c);
                        }
                        val = new DataValue(b.toString());
                        break;
                    } else if (value.regionMatches(true, i - 1, "true", 0, 4)) {
                        // boolean true
                        val = new DataValue(true);
                        i += 3;
                        break;
                    } else if (value.regionMatches(true, i - 1, "false", 0, 5)) {
                        // boolean false
                        val = new DataValue(false);
                        i += 4;
                        break;
                    } else if (c == '+' || c == '-' || (c >= '0' && c <= '9')) {
                        // numerical dataValue
                        val = getNumericalValue(i - 1, value, b);
                        i += b.length() - 1;
                        break;
                    } else {
                        throw new ModelException(ExceptionReason.INVALID_PARAMETER, "ObjectPath: Malformed reference " + objectName);
                    }
                }
                if (key != null && value != null)
               //  System.out.println("***Found key,dataValue pair ["+val.getType()+"] "+key+"="+val);
                addKey(key, val);
                if (i < value.length()) {
                    if(value.charAt(i) != ',') throw new ModelException(ExceptionReason.INVALID_PARAMETER, "ObjectPath: Malformed reference " + objectName);
                    i++;
                }
            }
        }
        if(type == null) type = keyValues == null ? ElementType.CLASS : ElementType.INSTANCE;
        return;
	}
	
	/**
	 * Construct an object path from a URI
	 * @param uri - Properly constructed URI. In the URI, the localPath is expected to be of the form:<br>
	 * uri = '/' elementType '/' localPathElement [ ('/' localPathElement)* ] '/' ElementName [InstanceLocator]
	 * instanceLocator = '?' keyName ',' dataType '=' value (['&amp;' keyName ',' dataType '=' value)*
	 */
	public ObjectPath(URI uri){
		if(uri == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Null URI passed to ObjectPath");
		// TODO: Need to add error checks here
		String uriPath = uri.getPath();
		String [] pathElements = uriPath.split("/");
		// object name is the last element in the URI path
		objectName = pathElements[pathElements.length-1];
		// element type is the first element in the URI path
		type = pathElements[0].isEmpty() ? ElementType.valueOf(pathElements[1].toUpperCase()) : ElementType.valueOf(pathElements[0].toUpperCase());
		// localPath elements are the middle elements in the URI path
		StringBuilder b = new StringBuilder();
		for(int i = pathElements[0].isEmpty() ? 2 : 1; i < pathElements.length-1; i++){
			b.append("/");
			b.append(pathElements[i]);
		}
		nameSpacePath = new NameSpacePath(uri.getScheme(),uri.getAuthority(),b.toString());
		// if elementType is instance or structureValue, we must have a query defining the instance keys
		if(type == ElementType.INSTANCE || type == ElementType.STRUCTUREVALUE){
			keyValues = new TreeMap<String,DataValue>();
			String query = uri.getQuery();
			String [] queryElements = query.split("&");
			for(String queryElement : queryElements){
				if(queryElement.isEmpty()) continue;
				String [] keyValue = queryElement.split("[,=]",3);
				keyValues.put(keyValue[0].trim().toLowerCase(), new DataValue(keyValue[1],ModelUtilities.unquote(keyValue[2])));
			}
		}
		return;
	}
	
	/**
     * Scan a numerical dataValue from a string
     * @param linePosition position in the string to start scanning from
     * @param currentLine - input string
     * @param b - string builder to construct dataValue
     * @return - DataValue containing numerical dataValue
     */
    private DataValue getNumericalValue(int linePosition, String currentLine, StringBuilder b) {
        boolean isHex = false, seenDecimal = false, signEnabled = true;
        boolean needDigit = true, isBool = true, isInt = true;

        int lineLength = currentLine.length();
        int initPosition = linePosition;
        // initial + or -
        while (linePosition < lineLength) {
            char c = currentLine.charAt(linePosition);
            if (Character.isDigit(c)) {
                b.append(c);
                signEnabled = needDigit = false;
                if (isBool && (c != '0' && c != '1')) {
                    isBool = false;
                }

            } else if (isHex && (c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f')) {
                b.append(c);
                needDigit = signEnabled = isBool = false;

            } else if ((c == 'x' || c == 'X')) {
                if ((b.length() == 1 && b.charAt(0) == '0')
                    || (b.length() == 2 && b.charAt(1) == '0' && (b.charAt(0) == '+' || b
                        .charAt(0) == '-'))) {
                    // we ignore the 'x' in the output
                    // b.append(c);
                    needDigit = isHex = true;
                    signEnabled = isBool = false;
                } else {
                    break;
                }

            } else if (c == '.') {
                // check for decimal rules
                if (isHex || seenDecimal)
                    break;
                b.append(c);
                signEnabled = isBool = isHex = false;
                needDigit = seenDecimal = true;

            } else if (signEnabled && (c == '+' || c == '-')) {
                // sign can only be at the beginning or after (e | E)
                // we ignore the '+' sign for the output
                if (c == '-')
                    b.append(c);
                signEnabled = false;

            } else if (!needDigit && seenDecimal && (c == 'E' || c == 'e')) {
                b.append(c);
                signEnabled = needDigit = true;

            } else if (isBool && (c == 'B' || c == 'b')) {
                // boolean termination reached. change isInt = false
                // b.append(c);
            	isInt = false;
                linePosition++;
                break;

            } else {
                // reached a non-number character
                break;
            }
            linePosition++;
        }
        linePosition -= initPosition;
        if(isInt && isBool) isBool = false;
        DataValue value = null;
        if (isHex) {
            value = new DataValue(Integer.parseInt(b.toString(), 16));
        } else if (isBool) {
            value = new DataValue(Integer.parseInt(b.toString(), 2));
        } else if (seenDecimal) {
            value = new DataValue(Double.parseDouble(b.toString()));
        } else {
        	// TODO: Check if integer dataValue is needed, or if a long dataValue is needed
        	// dataValue = new DataValue(Long.parseLong(b.toString()));
           value = new DataValue(Integer.parseInt(b.toString()));
        }
        b.setLength(linePosition);
        // System.out.println("Number Token ["+b.toString()+"] length "+linePosition+":"+b.length());
        return value;
    }
    
    /**
     * Add a {Key,Value} pair from some key property to this object path
     * @param key - (String) name of property
     * @param dataValue - (DataValue) dataValue of the property
     */
    private void addKey(String key, DataValue value) {
        if (keyValues == null)
            keyValues = new TreeMap<String, DataValue>();
        keyValues.put(key.toLowerCase(), value);
        return;
    }

	/**
	 * Get the element type represented in this object path
	 * @return - element type represented by this path
	 */
	public ElementType getElementType(){
		return type;
	}
	
	/**
	 * Get the name of the element in this path
	 * @return - name of the element in the path
	 */
	public String getName(){
		return objectName;
	}
	
	/**
	 * Get the normalized case name of the element in this path
	 * @return - lower case name of the element in the path
	 */
	public String getLowerCaseName(){
		return objectName.toLowerCase();
	}
	
	/**
	 * Get the name space path associated with this object path
	 * @return - name space path associated with this object path. Null is returned if 
	 * no namespace path is associated with the object path (e.g., for alias values)
	 */
	public NameSpacePath getNameSpacePath(){
		return nameSpacePath;
	}
	
	/**
	 * Get the localPath corresponding to this objectPath
	 * @return - local path for this objectPath. Default is returned if none specified
	 */
	public String getLocalPath() {
		NameSpacePath ns = getNameSpacePath();
		return ns != null ? nameSpacePath.getLocalPath() : Constants.defaultLocalPath;
	}
	
	/**
	 * Get the alias value associated with this object path
	 * @return - alias value. Null if no alias is set in this object path
	 */
	public String getAlias(){
		return alias;
	}
	
	/**
	 * Get the value associated with a given key in this object path
	 * @param keyName - case insensitive name of the key property
	 * @return - value of the key. Null if this path is not a structure/instancevalue, or if no such key exists
	 */
	public DataValue getKeyValue(String keyName) {
		return keyValues != null && keyName !=null ? keyValues.get(keyName.toLowerCase()) : null;
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.NameSpacePath#toString()
	 */
	@Override
	public String toString() {
		// if an alias is given with no keyValues this is an alias
		if (alias != null && keyValues == null) {
			return alias;
		}
		try {
			URI objectPath = new URI(nameSpacePath.getScheme(),nameSpacePath.getAuthority(),"/"+type.toString().toLowerCase()+nameSpacePath.getLocalPath(),null,null);
			StringBuilder b = new StringBuilder(objectPath.toString());
			if (b.length() > 0 && b.charAt(b.length() - 1) != ':')
				b.append(":");
			b.append(objectName);
			if (keyValues != null && !keyValues.isEmpty()) {
				b.append(".");
				for (String key : keyValues.keySet()) {
					b.append(key);
					b.append("=");
					DataValue v = keyValues.get(key);
					b.append(v != null ? v.toMOF() : "\"null\"");
					b.append(",");
				}
				b.setLength(b.length() - 1);
			}
			return b.toString();
		} catch (URISyntaxException e) {
			throw new ModelException("Internal Error - conversion failed",e);
		}
	}
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.NameSpacePath#hashCode()
	 */
	@Override
	public int hashCode() {
		return getUUID().hashCode();
	}
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.NameSpacePath#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof ObjectPath)) return false;
		ObjectPath other = (ObjectPath) obj;
		if(type != other.type) return false;
		return getUUID().equals(other.getUUID());
	}

	/**
	 * get a UUID value based on this objectPath. Note that the UUID depends on the element type, local path, and object name and keys,
	 * but does NOT depend on the hostname and authority in the namespace
	 * @return - Type 3 (Name-based) UUID based on this objectPath
	 */
	public UUID getUUID() {
		StringBuilder b = new StringBuilder(getElementType().toString());
		b.append(":");
		b.append(getLocalPath().toLowerCase());
		b.append(":");
		b.append(objectName.toLowerCase());
		if (keyValues != null && !keyValues.isEmpty()) {
			b.append(".");
			for (String key : keyValues.keySet()) {
				b.append(key);
				b.append("=");
				DataValue v = keyValues.get(key);
				b.append(v != null ? v.toMOF() : "\"null\"");
				b.append(",");
			}
			b.setLength(b.length() - 1);
		} else if((type == ElementType.INSTANCE || type == ElementType.STRUCTUREVALUE) && alias != null){
			b.append(".");
			b.append(alias);
		}
		return UUID.nameUUIDFromBytes(b.toString().getBytes());
	}
	
	/**
	 * Create a URL based on this object path. The scheme/authority are extracted from this path, or replaced by defaults if null
	 * @return - URL based on this object path
	 */
	public URL toURL(){
		NameSpacePath ns = nameSpacePath != null ? nameSpacePath : Constants.defaultNameSpacePath;
		return getURL(ns.getScheme(),ns.getAuthority());
	}
	
	/**
	 * Create a URL for this objectPath targeted at a given host
	 * @param scheme - scheme used for this URL. Replaced by a default if null
	 * @param authority - authority used for this URL. Replaced by a default if null
	 * @return - URL constructed based on this ObjectPath
	 * @see Constants#defaultScheme
	 * @see Constants#defaulAuthority
	 */
	public URL getURL(String scheme, String authority) {
		if(scheme == null) scheme = Constants.defaultScheme;
		if(authority == null) authority = Constants.defaulAuthority;
		// elementType localPath and className are embedded in the URL path as /elementType/localPath/className
		String localPath = "/" + getElementType().toString().toLowerCase() + getLocalPath() + "/" + objectName;
		// query represents {key,value} pairs needed to locate instances
		// note that the language allows only non-array primitive types and object references as keys
		String query = null;
		if(getElementType() == ElementType.INSTANCE || getElementType() == ElementType.STRUCTUREVALUE){
			StringBuilder b = new StringBuilder();
			// query is keyName ',' datatype '=' value ['&' keyName ',' datatype '=' value]*
			// for aliased instances/structureValues with no keys defined, query is alias
			if(keyValues != null && !keyValues.isEmpty()){
				for (String key : keyValues.keySet()) {
					b.append(key);
					DataValue v = keyValues.get(key);
					b.append(",");
					b.append(v.getType().toString().toLowerCase());
					b.append("=");
					b.append(v != null ? v.toMOF() : "\"null\"");
					b.append("&");
				}
				b.setLength(b.length() - 1);
				query = b.toString();
			} else if(alias != null){
				query = alias;
			} else {
				throw new ModelException("ObjectPath defined with no keys or alias for "+objectName+"["+type+"]");
			}
		}
		try {
			// construct the URI, and return it
			return new URI(scheme,authority,localPath,query,null).toURL();
		} catch(URISyntaxException | MalformedURLException e){
			throw new ModelException("ObjectName error in constructing URL " + scheme +"://"+authority + localPath+"?" + query);
		}
	}
}
