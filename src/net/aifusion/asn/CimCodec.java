/**
 * Copyright 2025,2026 Sharad Singhal. All Rights Reserved.
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
 * Last modified Jan 12, 2026 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.UUID;

import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.providers.Provider;

/**
 * Class to manage CIM to binary translations and vice versa
 * @author Sharad Singhal
 */
public class CimCodec {
	Provider provider  = null;
	

	/**
	 * 
	 */
	public CimCodec(Provider provider) {
		this.provider = provider;
		return;
	}

	public byte [] encode(NamedElement e) {
		byte [] encoded = null;
		if(e == null) {
			return (new NullValue()).getEncodedValue();
		}
		ElementType et = e.getElementType();

		switch(et) {
		// definitions extending NamedElement
		case ASSOCIATION:
		case CLASS:
		case INTERFACE:
		case ENUMERATION:
		case QUALIFIERTYPE:
		case STRUCTURE:
			// simply encode as a variable length string?

			// elements containing values
		case INSTANCE:
		case STRUCTUREVALUE:
			// iterate over all properties. Encode as OID/ROID/objectPath and a
			// set of tagged values	
		case ENUMERATIONVALUE:
			// encode as OID/OID/ObjectPath and a value
			break;
			// indications have properties, but are not named elements
		case INDICATION:
			// elements that extend QualifiedElement
			break;
		case METHOD:
		case PARAMETER:
		case PROPERTY:
		case QUALIFIER:
		case REFERENCE:
			// Schema is currently unused in Fusion, and probably not needed. It can
			// be represented as a CimStructure
		case SCHEMA:
		default:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,e.getName()+" is not a named element");
		}
		return encoded;
	}

	/**
	 * Decode the next CIM element from the incoming buffer
	 * @param buffer
	 * @param cursor
	 * @param cimElements
	 * @return updated cursor value
	 */
	public int decode(byte [] buffer, int cursor,ArrayList<NamedElement> cimElements) {
		if(cursor < 0 || cursor >= buffer.length) throw new ModelException(ExceptionReason.INVALID_PARAMETER, 
				"cursor "+cursor+" must be in range [0,"+buffer.length+")");
		Tag tag = Tag.locate(buffer[cursor]);
		if(tag == Tag.CLASS_DEFINED) {
			// class_defined tag will require a repository?
			throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE, 
					"Buffer["+cursor+"] Class_defined tags are not yet implemented");

		} else {
			DataType dt = Tag.getDataType(buffer[cursor]);
			if(dt == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER, 
					"Buffer["+cursor+"] does not point to a valid CIM identifier");
			switch(dt) {
			case BOOLEAN:
				break;
			case BOOLEAN_ARRAY:
				break;
			case CHAR16:
				break;
			case CHAR16_ARRAY:
				break;
			case DATETIME:
				break;
			case DATETIME_ARRAY:
				break;
			case ENUMERATIONVALUE:
				break;
			case ENUMERATIONVALUE_ARRAY:
				break;
			case INSTANCEVALUE:
				break;
			case INSTANCEVALUE_ARRAY:
				break;
			case OBJECTPATH:
				break;
			case OBJECTPATH_ARRAY:
				break;
			case OCTETSTRING:
				break;
			case OCTETSTRING_ARRAY:
				break;
			case REAL32:
				break;
			case REAL32_ARRAY:
				break;
			case REAL64:
				break;
			case REAL64_ARRAY:
				break;
			case SINT16:
				break;
			case SINT16_ARRAY:
				break;
			case SINT32:
				break;
			case SINT32_ARRAY:
				break;
			case SINT64:
				break;
			case SINT64_ARRAY:
				break;
			case SINT8:
				break;
			case SINT8_ARRAY:
				break;
			case STRING:
				break;
			case STRING_ARRAY:
				break;
			case STRUCTUREVALUE:
				break;
			case STRUCTUREVALUE_ARRAY:
				break;
			case UINT16:
				break;
			case UINT16_ARRAY:
				break;
			case UINT32:
				break;
			case UINT32_ARRAY:
				break;
			case UINT64:
				break;
			case UINT64_ARRAY:
				break;
			case UINT8:
				break;
			case UINT8_ARRAY:
				break;
			case VOID:
				break;
			default:
				break;
			}
		}
		return cursor;
	}

	public static byte[] encode(DataValue v) {
		// null data values are CIM no-value constructs
		// while a v.getValue() == null represents an explicit null value
		if(v == null) {
			// null data values (no value) is encoded as an ASN.1 UNIVERSAL Null value
			return (new NullValue()).getEncodedValue();
		}
		DataType dt = v.getType();		
		Object value = v.getValue();
		if(value == null) {
			// have an embedded null value (as opposed to a no-value)
			// we'll encode it as a CIM void value. Since only methods are
			// declared as void in CIM, this is unambiguous
			return new byte [] {Tag.locate(dt).getIdentifier(),0};
		}
		switch(dt) {
		case VOID:
			return new byte [] {Tag.VOID.getIdentifier(),0};
		case BOOLEAN:
			// we can also use Tag.BOOLEAN (universal) and tlv structure.
			Boolean b = (Boolean) value;
			return new byte [] {Tag.BOOL.getIdentifier(), 1,(byte) (b ? 0xFF : 0)};
		case BOOLEAN_ARRAY:
			break;
		case CHAR16:
			char c = ((Character) value).charValue();
			return new byte [] {Tag.CHAR16.getIdentifier(), 2,(byte)( (c & 0x0FF00) >> 8), (byte) (c & 0x0FF)};
		case CHAR16_ARRAY:
			break;
		case DATETIME:
			// encode as fixed length character string
			byte [] encoded = new byte[27];
			encoded[0] = Tag.DATETIME.getIdentifier();
			encoded[1] = 25;
			String dateTime = v.getValue().toString();
			for(int i = 2; i < encoded.length; i++) {
				encoded[i] = (byte) dateTime.charAt(i-2);
			}
			return encoded;
		case DATETIME_ARRAY:
			break;
		case ENUMERATIONVALUE:
			// encode as {identifier} valueTag, value
			break;
		case ENUMERATIONVALUE_ARRAY:
			break;
		case INSTANCEVALUE:
			break;
		case INSTANCEVALUE_ARRAY:
			break;
		case OBJECTPATH:
			// encode as a variable length string containing the URI form
			break;
		case OBJECTPATH_ARRAY:
			break;
		case OCTETSTRING:
			// encode as a variable length byte array
			break;
		case OCTETSTRING_ARRAY:
			break;
		case REAL32:
			break;
		case REAL32_ARRAY:
			break;
		case REAL64:
			break;
		case REAL64_ARRAY:
			break;
		case SINT16:
			break;
		case SINT16_ARRAY:
			break;
		case SINT32:
			break;
		case SINT32_ARRAY:
			break;
		case SINT64:
			break;
		case SINT64_ARRAY:
			break;
		case SINT8:
			break;
		case SINT8_ARRAY:
			break;
		case STRING:
			break;
		case STRING_ARRAY:
			break;
		case STRUCTUREVALUE:
			break;
		case STRUCTUREVALUE_ARRAY:
			break;
		case UINT16:
			break;
		case UINT16_ARRAY:
			break;
		case UINT32:
			break;
		case UINT32_ARRAY:
			break;
		case UINT64:
			break;
		case UINT64_ARRAY:
			break;
		case UINT8:
			break;
		case UINT8_ARRAY:
			break;
		default:
			break;
		}
		return null;
	}


	private static OidValue stringToOid(String prefix, UUID uid) {
		String [] prefixStrings = prefix.split("\\.");
		long [] oidValues = new long[prefixStrings.length+4];

		for(int i = 0; i < prefixStrings.length; i++) {
			oidValues[i] = Long.valueOf(prefixStrings[i]);
		}

		StringBuilder b = new StringBuilder();
		//		StringBuilder b = new StringBuilder("0x0");
		String uidString = uid.toString();

		for(int i = 0, j = prefixStrings.length; i < uidString.length(); i++) {
			if(uidString.charAt(i) == '-') continue;
			b.append(uidString.charAt(i));
			if(b.length()%8 == 0) {
				//	if(b.length()%11 == 0) {
				oidValues[j++] = Long.valueOf(b.toString(), 16);
				b.setLength(0);
			}
		}
		System.out.println(Arrays.toString(oidValues));
		return new OidValue(oidValues);
	}
	private static OidValue stringToOid(String prefix) {
		String [] prefixStrings = prefix.split("\\.");
		long [] oidValues = new long[prefixStrings.length];

		for(int i = 0; i < prefixStrings.length; i++) {
			oidValues[i] = Long.valueOf(prefixStrings[i]);
		}
		System.out.println(Arrays.toString(oidValues));
		return new OidValue(oidValues);
	}

	private static RelativeOidValue stringToRelativeOid(UUID uid) {
		long [] oidValues = new long[4];

		StringBuilder b = new StringBuilder();
		String uidString = uid.toString();

		for(int i = 0, j = 0; i < uidString.length(); i++) {
			if(uidString.charAt(i) == '-') continue;
			b.append(uidString.charAt(i));
			if(b.length()%8 == 0) {
				//	if(b.length()%11 == 0) {
				oidValues[j++] = Long.valueOf(b.toString(), 16);
				b.setLength(0);
			}
		}
		System.out.println(Arrays.toString(oidValues));
		return new RelativeOidValue(oidValues);

	}

	public static void main(String [] args) {
		UUID x = UUID.randomUUID();
		System.out.println(Constants.fusionOid+" "+x);
		OidValue z = stringToOid(Constants.fusionOid);
		System.out.println(z);
		System.out.println(Arrays.toString(z.getEncodedValue()));

		OidValue v = stringToOid(Constants.fusionOid,x);
		System.out.println(v);
		System.out.println(Arrays.toString(v.getEncodedValue()));
		//		System.out.println(Constants.fusionOid.matches("^\\d+(\\.\\d+)*$"));
		//		System.out.println(Arrays.toString(stringToOid(Constants.fusionOid)));
		RelativeOidValue u = stringToRelativeOid(x);
		System.out.println(u);
		System.out.println(Arrays.toString(u.getEncodedValue()));
	}
}
