/**
 * Copyright 2014-2026, Sharad Singhal, All Rights Reserved
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
 * Created Jun 1, 2014 by Sharad Singhal
 * Updated Jan 11, 2026 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Standard CIM Qualifier Types defined in the meta model. This Enum provides "built-in" qualifier types
 * that are defined in a standard way. Note that unlike the <code>QualifierType</code> class, this enumeration does not
 * accommodate user defined qualifier types loaded into a Repository, and has all information hard-coded in it. Qualifier
 * Types defined in this class are available by default in the implementation, and do not need to be defined explicitly
 * in MOF.<br>
 * @see QualifierType
 * @author Sharad Singhal
 */
public enum StandardQualifierType {
	// QualifierType(String name, DataType type, DataValue defaultValue, Scope[] scopes, Policy policy)
	// note that Policy = null ==> Policy = ENABLEOVERRIDE, so is left as null in this class
	
	/** Qualifier ABSTRACT : Boolean = false, Scope(CLASS, ASSOCIATION, ENUMERATION, STRUCTURE, INTERFACE), Policy(RESTRICTED) */
	ABSTRACT("Abstract",DataType.BOOLEAN,new DataValue(false),new Scope[]{Scope.CLASS,Scope.ASSOCIATION,
			Scope.ENUMERATION,Scope.STRUCTURE,Scope.INTERFACE},Policy.RESTRICTED),
		
	/** Qualifier AggregationKind : String = "None", Scope(REFERENCE), Policy(DISABLEOVERRIDE) */
	AGGREGATIONKIND("AggregationKind",DataType.STRING,new DataValue("None"),new Scope[]{Scope.REFERENCE},
			Policy.DISABLEOVERRIDE),
	
	/** Qualifier ARRAYTYPE : String = "Bag", Scope(PROPERTY, PARAMETER, METHOD, REFERENCE), Policy(DISABLEOVERRIDE); */
	ARRAYTYPE("ArrayType",DataType.STRING,new DataValue("Bag"), new Scope[]{Scope.PROPERTY,Scope.PARAMETER,
			Scope.METHOD,Scope.REFERENCE},Policy.DISABLEOVERRIDE),
	
	/** Qualifier BITMAP : String[], Scope(METHOD, PROPERTY, PARAMETER), Policy(ENABLEOVERRIDE) */
	BITMAP("BitMap",DataType.STRING_ARRAY,null, new Scope[]{Scope.METHOD,Scope.PROPERTY,Scope.PARAMETER}, null),
	
	/** Qualifier BITVALUES : String[], Scope(METHOD, PROPERTY, PARAMETER), Policy(ENABLEOVERRIDE); */
	BITVALUES("BitValues",DataType.STRING_ARRAY,null, new Scope[]{Scope.METHOD,Scope.PROPERTY,Scope.PARAMETER}, null),
	
	/** Qualifier COUNTER : Boolean = false, Scope(METHOD, PROPERTY, PARAMETER), Policy(DISABLEOVERIDE) */
	COUNTER("Counter",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.METHOD,Scope.PROPERTY,Scope.PARAMETER},
			Policy.DISABLEOVERRIDE),
	
	/** Qualifier DEPRECATED : String[], Scope(ANY), Policy(RESTRICTED) */
	DEPRECATED("Deprecated",DataType.STRING_ARRAY,null, new Scope[]{Scope.ANY}, Policy.RESTRICTED),
	
	/** Qualifier DESCRIPTION : String, Scope(ANY), Policy(ENABLEOVERIDE) */
	DESCRIPTION("Description",DataType.STRING,null, new Scope[]{Scope.ANY},null),
	
	/** Qualifier EMBEDDEDOBJECT : Boolean = false, Scope(METHOD, PROPERTY, PARAMETER), Policy(DISABLEOVERRIDE) */
	EMBEDDEDOBJECT("EmbeddedObject",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.METHOD,
			Scope.PROPERTY,Scope.PARAMETER}, Policy.DISABLEOVERRIDE),
	
	/** Qualifier EXPERIMENTAL : Boolean = false, Scope(ANY), Policy(RESTRICTED) */
	EXPERIMENTAL("Experimental",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.ANY}, Policy.RESTRICTED),
	
	/** Qualifier GAUGE : Boolean = false, Scope(METHOD, PROPERTY, PARAMETER)), Policy(DISABLEOVERRIDE) */
	GAUGE("Gauge",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.METHOD,Scope.PROPERTY,Scope.PARAMETER},
			Policy.DISABLEOVERRIDE),
	
	// WARNING: This definition differs from DSP004-- the default value in DSP004 is false, which implies
	// that all input parameters have to be explicitly labeled in MOF. We choose the default as true, which
	// is the normal case, and thus input parameters do not require the [IN] qualifier to be specified
	/** Qualifier IN : Boolean = true, Scope(PARAMETER), Policy(DISABLEOVERRIDE) */
	IN("In",DataType.BOOLEAN,new DataValue(true), new Scope[]{Scope.PARAMETER}, Policy.DISABLEOVERRIDE),
	
	/** Qualifier ISPUNIT : Boolean = false, Scope(METHOD, PROPERTY, PARAMETER), Policy(ENABLEOVERRIDE) */
	ISPUNIT("IsPUnit",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.METHOD,Scope.PROPERTY,
			Scope.PARAMETER}, null),
	
	/** Qualifier KEY : Boolean = false, Scope(PROPERTY, REFERENCE), Policy(DISABLEOVERRIDE) */
	KEY("Key",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.PROPERTY,Scope.REFERENCE},
			Policy.DISABLEOVERRIDE),
	
	/** Qualifier MAPPINGSTRINGS : String[], Scope(ANY), Policy(ENABLEOVERRIDE) */
	MAPPINGSTRINGS("MappingStrings",DataType.STRING_ARRAY,null, new Scope[]{Scope.ANY}, null),
	
	/** Qualifier MAX : UnsignedInt32, Scope(REFERENCE), Policy(ENABLEOVERRIDE) */
	MAX("Max",DataType.UINT32,null, new Scope[]{Scope.REFERENCE}, null),
	
	/** Qualifier MIN : UnsignedInt32 = 0, Scope(REFERENCE), Policy(ENABLEOVERRIDE) */
	MIN("Min",DataType.UINT32,new DataValue(new UInt32(0)), new Scope[]{Scope.REFERENCE}, null),
	
	/** Qualifier MODELCORRESPONDENCE : String[], Scope(ANY), Policy(ENABLEOVERRIDE) */
	MODELCORRESPONDENCE("ModelCorrespondence",DataType.STRING_ARRAY,null, new Scope[]{Scope.ANY}, null),
	
	/** Qualifier OCL : String[], Scope(CLASS, ASSOCIATION, STRUCTURE, PROPERTY, METHOD, PARAMETER), Policy(ENABLEOVERRIDE) */
	OCL("OCL",DataType.STRING_ARRAY,null,new Scope[]{Scope.CLASS,Scope.ASSOCIATION, Scope.STRUCTURE, Scope.INTERFACE,
			Scope.PROPERTY,Scope.METHOD,Scope.PARAMETER}, null),
	
	/** Qualifier OUT : Boolean = false, Scope(PARAMETER), Policy(DISABLEOVERRIDE) */
	OUT("Out",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.PARAMETER}, Policy.DISABLEOVERRIDE),
	
	/** Qualifier OVERRIDE : Boolean = false, Scope(METHOD, PROPERTY, REFERENCE, PARAMETER), Policy(RESTRICTED) */
	OVERRIDE("Override",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.METHOD,Scope.PROPERTY,
			Scope.REFERENCE,Scope.PARAMETER}, Policy.RESTRICTED),
	
	/** Qualifier PACKAGEPATH : String, Scope(ASSOCIATION, CLASS, ENUMERATION,STRUCTURE), Policy(ENABLEOVERRIDE) */
	PACKAGEPATH("PackagePath",DataType.STRING,null,new Scope[]{Scope.CLASS,Scope.ENUMERATION,Scope.STRUCTURE,
			Scope.INTERFACE}, null),
	
	/** Qualifier PUNIT : String, Scope(METHOD, PROPERTY, PARAMETER), Policy(ENABLEOVERRIDE) */
	PUNIT("PUnit",DataType.STRING,null, new Scope[]{Scope.METHOD,Scope.PROPERTY,Scope.PARAMETER}, null),
	
	/** Qualifier READ : Boolean = true, Scope(PROPERTY,REFERENCE), Policy(ENABLEOVERRIDE) */
	READ("Read",DataType.BOOLEAN,new DataValue(true), new Scope[]{Scope.PROPERTY,Scope.REFERENCE}, null),
	
	/** Qualifier REQUIRED : Boolean = false, Scope(METHOD, PROPERTY, REFERENCE, PARAMETER), Policy(DISABLEOVERRIDE) */
	REQUIRED("Required",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.METHOD,Scope.PROPERTY,
			Scope.REFERENCE,Scope.PARAMETER}, Policy.DISABLEOVERRIDE),
	
	/** Qualifier STATIC : Boolean = false, Scope(METHOD, PROPERTY), Policy(DISABLEOVERRIDE) */
	STATIC("Static",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.METHOD,Scope.PROPERTY,
			Scope.REFERENCE}, Policy.DISABLEOVERRIDE),
	
	/** Qualifier TERMINAL : Boolean = false, Scope(CLASS, ASSOCIATION, ENUMERATION, STRUCTURE), Policy(ENABLEOVERRIDE) */
	TERMINAL("Terminal",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.CLASS,Scope.ENUMERATION,
			Scope.STRUCTURE}, null),
	
	/** Qualifier VERSION : String, Scope(INTERFACE, CLASS, ASSOCIATION, ENUMERATION,STRUCTURE), Policy(RESTRICTED) */
	VERSION("Version",DataType.STRING,null, new Scope[]{Scope.INTERFACE,Scope.CLASS,Scope.ASSOCIATION,
		Scope.ENUMERATION,Scope.STRUCTURE}, Policy.RESTRICTED),
	
	// This definition differs from DSP0004, which indicates that the value is null.
	/** Qualifier WRITE : Boolean = false, Scope(PROPERTY,REFERENCE), Policy(ENABLEOVERRIDE) */
	WRITE("Write",DataType.BOOLEAN,new DataValue(false), new Scope[]{Scope.PROPERTY,Scope.REFERENCE}, null),
	
	/** Qualifier XMLNAMESPACENAME : String = null, Scope(PARAMETER,PROPERTY,REFERENCE,METHOD), Policy(ENABLEOVERRIDE) */
	XMLNAMESPACENAME("XMLNamespaceName",DataType.STRING,new DataValue(DataType.STRING,null),
			new Scope[]{Scope.PARAMETER,Scope.PROPERTY,Scope.REFERENCE,Scope.METHOD},null),
	
	// CIM.v2 Qualifiers that are different from V3.
	// The MOF Parser will translate OVERIDE(STRING) to OVERRIDE(Boolean)
	// and translate UMLPackagePath to PackagePath
	
	// additional Fusion related qualifiers
	/** Qualifier IMPLEMENTS : String[], Scope(CLASS, ASSOCIATION), Policy(RESTRICTED) */
	IMPLEMENTS("Implements",DataType.STRING_ARRAY,null, new Scope[]{Scope.CLASS,Scope.STRUCTURE}, Policy.RESTRICTED),
	/** Qualifier OID : STRING, Scope(STRUCTURE,CLASS,INTERFACE,ENUMERATION),Policy(DISABLEOVERRIDE) */
	OID("OID",DataType.STRING,null,new Scope[] {Scope.STRUCTURE,Scope.CLASS,Scope.INTERFACE,
			Scope.ENUMERATION},Policy.DISABLEOVERRIDE),
	/** Qualifier TAG : Int32,null, Scope(Property,Reference,EnumerationValue), Policy(EnableOverride) */
	TAG("Tag",DataType.SINT32,null,new Scope[]{Scope.PROPERTY,Scope.REFERENCE,Scope.ENUMERATIONVALUE},null);
	
	/** MOF name for this qualifier type */
	private final String mofName;
	/** Data type for this qualifier type */
	private final DataType type;
	/** Default dataValue for this qualifier type */
	private final DataValue defaultValue;
	/** Scopes for this qualifier type */
	private final List<Scope> scopes;
	/** Default policy for this qualifier type */
	private final Policy policy;
	/** QualifierType for this enum value */
	private final QualifierType qualifierType;

	/**
	 * Create a new Qualifier Type
	 * @param mofName - MOF name for this qualifier type
	 * @param type - data type for this qualifier type
	 * @param defaultValue - default dataValue for this qualifier type
	 * @param scopes - scopes for this qualifier type
	 * @param policy - default policy for this qualifier type
	 */
	private StandardQualifierType(String mofName, DataType type, DataValue defaultValue, Scope[] scopes, Policy policy){
		this.mofName = mofName;
		this.type = type;
		this.defaultValue = defaultValue;
		this.scopes = Arrays.asList(scopes);
		this.policy = policy;
		this.qualifierType = 
				new QualifierType(mofName,type,defaultValue,this.scopes,policy,null, Constants.defaultNameSpacePath);
		return;
	}

	/**
	 * Check if the qualifier type has a default dataValue specified
	 * @return - true if the qualifier type has a default dataValue, false otherwise
	 */
	public boolean hasDefaultValue(){
		return defaultValue != null;
	}
	
	/**   
	 * Get the default dataValue for this qualifier type
	 * @return The CIM dataValue for this qualifier type. Null if this qualifier type does not have a value
	 */
	public DataValue getDefaultValue() {
		return defaultValue;
	}
	
	/**
	 * Check if this qualifier type has an array dataValue
	 * @return - true if this qualifier type has an array dataValue, false otherwise
	 */
	public boolean hasArrayValue(){
		return type.isArray();
	}

	/**
	 * Get the scopes to which this qualifier type can be applied.
	 * @return set of CIM element scopes for which this qualifier type is applicable.
	 */
	public List<Scope> getScopes() {
		return new ArrayList<Scope>(scopes);
	}
	
	/**
	 * Get the policy to which this qualifier type can be applied.
	 * @return set of policy for which this qualifier type is applicable.
	 */
	public Policy getPolicy() {
		return policy;
	}

	/** 
	 * Get the CIM data type of this qualifier type
	 * @return 	The data type of this qualifier type.
	 */
	public DataType getDataType() {
		return type;
	}
	
	/**
	 * Get the MOF name associated with this qualifier type
	 * @return - MOF name associated with this qualifier type
	 */
	public String getMofName() {
		return mofName;
	}
	
	/**
	 * Check if this qualifierType applies to a given elementType
	 * @param elementType - type of element to check
	 * @return - true if the standardQualifierType applies to this element type, false otherwise
	 */
	public boolean appliesTo(ElementType elementType){
		for(Scope s : scopes){
			if(s.appliesTo(elementType)) return true;
		}
		return false;
	}
	
	/**
	 * Check if this qualifierType has a given scope
	 * @param s - scope to check
	 * @return - true if the qualifierType has the given scope, false otherwise
	 */
	public boolean hasScope(Scope s){
		return scopes.contains(s);
	}
	
	
	/**
	 * Check if this qualifier type has restricted propagation policy
	 * @return - true if qualifier type is restricted, false otherwise
	 * @see #isOverridable()
	 */
	public boolean isRestricted(){
		return policy != null && policy == Policy.RESTRICTED ? true : false;
	}
	
	/**
	 * Check if this qualifier type can be overridden in subclasses
	 * @return - true if the qualifier type can be overridden, false otherwise.
	 * Note that the returned dataValue is meaningful only if the qualifier type is not restricted, 
	 * i.e. {@link #isRestricted()} returns false.
	 * @see #isRestricted()
	 */
	public boolean isOverridable(){
		return policy == null || policy == Policy.ENABLEOVERRIDE ? true : false;
	}
		
	/**
	 * Return a MOF representation of this Qualifier Type
	 * @return - String containing MOF representation of this Qualifier Type
	 */
	public String toMOF(){
		return qualifierType.toMOF("");
	}


	/**
	 * Create a new Qualifier instance based on this qualifier type
	 * @param value - dataValue of the qualifier (java object or DataValue)
	 * @param path - namespace path for the qualifier. If null, a default dataValue is used
	 * @return - new qualifier instance
	 * @see Constants#defaultNameSpacePath
	 */
	public Qualifier getQualifier(Object value, NameSpacePath path){
		Qualifier q = value instanceof DataValue ? new Qualifier(getQualifierType(path),(DataValue) value) :
					new Qualifier(getQualifierType(path),new DataValue(type,value));
		return q;
	}
	
	/**
	 * Create a new Qualifier instance based on this qualifier type with the default namespace.
	 * The Qualifier has default dataValue and policy as defined in the qualifier type
	 * @return - new qualifier of this type
	 * @see Constants#defaultNameSpacePath
	 */
	public Qualifier getQualifier(){
		return new Qualifier(getQualifierType());
	}
	
	/**
	 * Get all pre-defined qualifier types. Does NOT include user defined qualifier types
	 * @return - array containing all pre-defined Cim QualifierTypes within the default namespace
	 * @see Constants#defaultNameSpacePath
	 */
	public static QualifierType[] getQualifierTypes(){
		StandardQualifierType [] types =  StandardQualifierType.class.getEnumConstants();
		QualifierType [] cimTypes = new QualifierType[types.length];
		for(int i=0; i<types.length; i++){
			cimTypes[i] = types[i].getQualifierType();
		}
		return cimTypes;
	}
	
	/**
	 * Check if a given name is a standard qualifier type
	 * @param name - name to be checked
	 * @return - true if name is a standard qualifier type, false otherwise
	 */
	public static boolean isKnownType(String name){
		if(name == null || name.trim().isEmpty()) return false;
		StandardQualifierType [] types =  StandardQualifierType.class.getEnumConstants();
		for(StandardQualifierType t : types){
			if(t.toString().equalsIgnoreCase(name)) return true;
		}
		return false;
	}
	
	/**
	 * Get the QualifierType corresponding to this CimStandardQualifier
	 * @return - qualifier type for this Standard Qualifier Type
	 * @see Constants#defaultNameSpacePath
	 */
	public QualifierType getQualifierType(){
		return qualifierType;
	}
	
	/**
	 * Get the QualifierType corresponding to this StandardQualifier within a given nameSpace
	 * @param path - name space path to use for the qualifier type. If null, a default name space is used
	 * @return - qualifier type corresponding to this standard qualifier type
	 * @see Constants#defaultNameSpacePath
	 */
	public QualifierType getQualifierType(NameSpacePath path){
		return new QualifierType(mofName,type,defaultValue,scopes,policy,null,path != null ? path : Constants.defaultNameSpacePath);
	}	
}
