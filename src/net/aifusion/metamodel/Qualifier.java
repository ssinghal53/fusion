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

/**
 * Element to represent a CIM Qualifier. Qualifiers are immutable once constructed
 * @author Sharad Singhal
 */
public class Qualifier extends Element {
	/** Value of this qualifier */
	private DataValue value;
	/** QualifierType corresponding to this qualifier */
	private QualifierType type;

	/**
	 * Create a CIM Qualifier based on a given qualifier type
	 * @param type - qualifier type that acts as a template for this qualifier (must not be null)
	 */
	Qualifier(QualifierType type) {
		// Qualifiers have the same name as their qualifier type
		super(ElementType.QUALIFIER, type.getName());
		// note that type must be non-null (else the statement above will throw a null-pointer exception
		this.type = type;
		return;
	}
	
	/**
	 * Create a CIM qualifier from the given type and dataValue	
	 * @param type - qualifier type to use for the qualifier
	 * @param value - dataValue of the qualifier
	 */
	Qualifier(QualifierType type, DataValue value){
		this(type);
		// ensure that the data type of the given value matches that of the qualifier type
		if(value != null && value.getType() != type.getDataType()) 
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": Expected data type "+type.getDataType()+", found "+value);
		this.value = value;
		return;
	}
	
	/**
	 * Get the data type of this qualifier dataValue
	 * @return - data type associated with this qualifier
	 */
	public DataType getDataType(){
		return type.getDataType();
	}
	
	/**
	 * Get the dataValue of this qualifier. If no dataValue was specified, the default dataValue from the qualifier type is returned
	 * @return - dataValue of this qualifier. Return null if no default is given. A non-null dataValue containing a null is returned
	 * if a nullvalue is declared either for the qualifer or the qualifier type.
	 * @see #hasValue()
	 * @see #hasNonNullValue()
	 */
	public DataValue getValue(){
		// note that boolean qualifiers declared with no value are assumed to be TRUE
		return value != null ? value : type.getDataType() == DataType.BOOLEAN ? new DataValue(true) : type.getDefaultValue();
	}
	
	/**
	 * Check if this qualifier has a dataValue
	 * @return - true if this qualifier has a dataValue defined in it, false otherwise
	 * @see #hasNonNullValue()
	 */
	public boolean hasValue(){
		return value != null;
	}
	
	/**
	 * Check if this Qualifier has a non-null dataValue
	 * @return - true if this qualifier has a non-null dataValue defined in it
	 * @see #hasValue()
	 */
	public boolean hasNonNullValue(){
		return value != null && value.getValue() != null;
		
	}
	
	/**
	 * Return the qualifier type associated with this qualifier
	 * @return - qualifier type associated with this qualifier
	 */
	public QualifierType getQualifierType(){
		return type;
	}
	
	/**
	 * Check if this qualifier has a given scope
	 * @param scope - scope for which this qualifier is being checked
	 * @return - true if this qualifier has the given scope type, else false
	 */
	public boolean hasScope(Scope scope) {
		return type.hasScope(scope);
	}
	
	/**
	 * Check if this qualifier applies to a given elementType
	 * @param elementType - elementType to check
	 * @return - true if this qualifier can be applied to the given element type, false otherwise
	 */
	public boolean appliesTo(ElementType elementType){
		return type.appliesTo(elementType);
	}
	
	/**
	 * Returns a MOF representation of the Qualifier.
	 * @return MOF formatted representation of this qualifier
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder buffer = new StringBuilder(prefix);
		buffer.append(getName());
		if(hasValue()) {
			boolean isArray = type.getDataType().isArray();
			if(!isArray){
				buffer.append("(");
				buffer.append(value.toMOF());
				buffer.append(")");
			} else {
				buffer.append(value.toMOF());
			}
		}
		return buffer.toString();
	}
	
	/**
	 * Check if this qualifier is the same as some other qualifier. A qualifier is considered
	 * the same as some other qualifier, if it has the same qualifierType and value
	 * @param o - other qualifier
	 * @return - true if both qualifiers are the same, else return false
	 */
	@Override
	public boolean equals(Object o){
		// check for null and name match
		if(!super.equals(o)) return false;
		// check for class match
		if(!(o instanceof Qualifier)) return false;
		Qualifier q = (Qualifier) o;
		// check for same qualifier type. Note that this implies scope and policy equality
		if(!type.equals(q.type)) return false;
		
		// check values match
		if(value != null){
			// we have a non-null dataValue
			return value.equals(q.value);
		} else if(q.value != null){
			// other dataValue is non-null
			return q.value.equals(value);
		}
		// both are null, we match
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.Element#hashCode()
	 */
	@Override
	public int hashCode() {
		return type.hashCode()+super.hashCode();
	}
	
	
}
