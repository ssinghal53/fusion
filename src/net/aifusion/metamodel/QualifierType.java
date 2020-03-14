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
 * Created Dec 29, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.List;

/**
 * Element to represent a Qualifier Type
 * @author Sharad Singhal
 */
public class QualifierType extends NamedElement {
	/** data type for this qualifier type */
	private DataType type;
	/** default dataValue (if any) for this qualifier type */
	private DataValue defaultValue;
	/** applicable scopes */
	private List<Scope> scopes;
	/** propagation policy */
	private Policy policy;
	
	/**
	 * Create a new qualifier type
	 * @param name - name of the qualifier type
	 * @param type - data type for this qualifier type
	 * @param defaultValue - default dataValue for this qualifier type
	 * @param scopes - element scopes to which this qualifier type applies
	 * @param policy - propagation policy for this qualifier type
	 * @param qualifiers - qualifiers associated with this qualifier type
	 * @param path - NameSpace path for this qualifier type
	 */
	protected QualifierType(String name, DataType type, DataValue defaultValue, List<Scope> scopes, Policy policy, List<Qualifier> qualifiers, NameSpacePath path){
		// qualifier types do not have superTypes
		super(ElementType.QUALIFIERTYPE,name,null, qualifiers,path, null, null);
		if(type == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": Expected non-null data type, found null");
		this.type = type;
		if(scopes == null || scopes.isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": Expected non-empty scope, found null or empty");
		this.scopes = scopes;
		this.defaultValue = defaultValue;
		this.policy = policy;
		if(!type.isPrimitive() && !type.isEnumerationValue())
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": Only primitive and enumeration types are allowed - found "+type);
		// Note that enumeration types must have a non-null default value
		if(type.isEnumerationValue() && (defaultValue == null || defaultValue.getValue() == null))
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": Expected non-null default value, found null or empty");
		// for non-null default values, data type given for the default value must match the given data type
		if(defaultValue != null && defaultValue.getType() != type){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,"Expected "+type+" for default value, found "+defaultValue.getType());
		}
		return;
	}
	
	/**
	 * Get the data type associated with this qualifier type
	 * @return - data type associated with this qualifier type
	 */
	public DataType getDataType(){
		return type;
	}
	
	/**
	 * Check if this qualifier type has a default dataValue
	 * @return - true if this qualifier type has a default dataValue, false otherwise
	 */
	public boolean hasDefaultValue(){
		return defaultValue != null;
	}
	
	/**
	 * Return the default dataValue associated with this qualifier type
	 * @return - default dataValue for this qualifier type. Null is returned if no default dataValue is specified. 
	 * If a null dataValue is specified as the default, A non-null data dataValue with a null in it is returned 
	 */
	public DataValue getDefaultValue(){
		return defaultValue;
	}
	
	/**
	 * Check if this qualifier type applies to a particular element type
	 * @param scope - scope to check
	 * @return - true if the qualifier type applies to the scope, false otherwise
	 */
	public boolean hasScope(Scope scope){
		return scopes.contains(scope) || scopes.contains(Scope.ANY);
	}
	
	/**
	 * Check if this qualifierType applies to a given elementType
	 * @param elementType - element type to be checked
	 * @return - true if this qualifier type applies to the given element type, false otherwise
	 */
	public boolean appliesTo(ElementType elementType){
		for(Scope s : scopes){
			if(s.appliesTo(elementType)) return true;
		}
		return false;
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
	
	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Element#toMOF()
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(super.toMOF(prefix));
		if(b.length() > 0 && b.charAt(b.length()-1) == ']'){
			b.append("\n");
			b.append(prefix);
		}
		b.append("Qualifier ");
		b.append(getName());
		b.append(" : ");
		if(type.isEnumerationValue()){
			if(type.isArray()){
				EnumerationValue [] enums = (EnumerationValue[]) defaultValue.getValue();
				b.append(enums[0].getEnumName());
			} else {
				b.append(((EnumerationValue) defaultValue.getValue()).getEnumName());
			}
		} else {
			b.append(type.toMOF());
		}
		
		if(type.isArray()){
			b.append("[]");
		}
		if(hasDefaultValue()){
			b.append(" = ");
			b.append(defaultValue.toMOF());	
		}
		if(!scopes.isEmpty()){
			b.append(" Scope(");
			for(Scope s : scopes){
				b.append(s.toMOF());
				b.append(", ");
			}
			b.setLength(b.length()-2);
			b.append(")");
		}
		if(policy != null){
			b.append(" Policy(");
			b.append(policy.toMOF());
			b.append(")");
		}
		b.append(";\n");
		return b.toString();
	}
	
	/**
	 * Check if this qualifierType is the same as some other qualifier type.
	 * A qualifier type is considered the same as some other qualifier type, if it has the same name, scope(s),
	 * default flavors, data type and default dataValue
	 * @param o - other qualifier type
	 * @return - true if this qualifier type is the same as the other qualifier type, false otherwise
	 */
	@Override
	public boolean equals(Object o){
		// check for null or name mismatch
		if(!super.equals(o)) return false;
		
		// check for class type
		if(!(o instanceof QualifierType)) return false;
		QualifierType q = (QualifierType) o;
		
		// check for equal scopes
		for(Scope scope : scopes){
			if(!q.hasScope(scope)) return false;
		}
		for(Scope scope : q.scopes){
			if(!hasScope(scope)) return false;
		}
		
		
		// check if the same policy applies
		if(policy != null){
			switch(policy){			
			case RESTRICTED:	// both must be RESTRICTED
				if(q.policy != Policy.RESTRICTED) return false;
				break;
			case DISABLEOVERRIDE:	// both must be DISABLEOVERRIDE
				if(q.policy != Policy.DISABLEOVERRIDE) return false;
				break;
			case ENABLEOVERRIDE:	// ENABLEOVERRIDE is same as null dataValue
			default:
				if(q.policy != null && q.policy != Policy.ENABLEOVERRIDE) return false;
			}
		} else {
			// we have a null policy. q must have a null or enableOverRide policy
			if(q.policy != null && q.policy != Policy.ENABLEOVERRIDE) return false;
		}
		
		// check for equal data types
		if(type != q.type) return false;
		
		// check for equal default values
		if(defaultValue != null) return defaultValue.equals(q.defaultValue);
		return q.defaultValue == null ? true : false;
	}
}
