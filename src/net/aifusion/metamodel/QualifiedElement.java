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
 * Created Jan 19, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.HashSet;
import java.util.List;
import java.util.Vector;

/**
 * CIM Elements that can have qualifiers and (optionally) superTypes
 * @author Sharad Singhal
 *
 */
public abstract class QualifiedElement extends Element {
	/** qualifiers (if any) associated with this element */
	private List<Qualifier> qualifiers = null;
	
	/**
	 * Create a new qualified element
	 * @param elementType - type of element
	 * @param name - name of element
	 * @param qualifiers - list of qualifiers associated with this element. Null (or empty) if none defined.
	 */
	protected QualifiedElement(ElementType elementType, String name, List<Qualifier> qualifiers) {
		super(elementType, name);
		// validate that all qualifiers given apply to this element type
		if(qualifiers != null && !qualifiers.isEmpty()){
			for(Qualifier q : qualifiers){
				if(!q.appliesTo(elementType)) throw new ModelException(ExceptionReason.INVALID_PARAMETER,name+": Qualifier "+q.getName()+" does not apply to "+elementType);
			}
			this.qualifiers = qualifiers;
		}
		return;
	}
	
	/**
	 * Get the dataValue of a qualifier declared on this qualified element or the default defined in standard qualifiers. 
	 * @param qualifierName - name of the qualifier to search
	 * @return - dataValue of the qualifier or the default in a standard qualifier. Null if qualifier does not exist or
	 * does not have a default value
	 * @see StandardQualifierType
	 */
	public DataValue getQualifierValue(String qualifierName){
		if(qualifierName == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER, getName()+": getQualifierValue() Expected qualifier name, found null");
		// search for qualifiers defined on this element
		if(qualifiers != null){
			for(Qualifier q : qualifiers){
				if(!qualifierName.equalsIgnoreCase(q.getName())) continue;
				return q.getValue();
			}
		}
		// check standard qualifier values
		for(StandardQualifierType qt : StandardQualifierType.values()){
			if(qt.name().equalsIgnoreCase(qualifierName) && qt.appliesTo(getElementType())){
				return qt.getDefaultValue();
			}
		}
		return null;
	}

	/**
	 * Get the list of qualifiers declared on this qualified element
	 * @return - list of qualifiers. Empty list is returned if none are declared
	 */
	public List<Qualifier> getQualifiers(){
		// we return a copy to prevent the qualifiers from being modified in the current element
		// note that qualifiers are immutable, so only a shallow copy is needed
		Vector<Qualifier> q = new Vector<Qualifier>();
		if(qualifiers != null) q.addAll(qualifiers);
		return q;
	}

	/**
	 * Check if this qualified element has a particular qualifier declared in it
	 * @param qName - name of the qualifier
	 * @return - true if the qualified element has a qualifier declared in it, false otherwise
	 */
	public boolean hasQualifier(String qName){
		if(qName == null || qName.isEmpty()) return false;
		// check qualifiers defined on this element
		if(qualifiers != null) {
			for(Qualifier q : qualifiers){
				if(q.getName().equalsIgnoreCase(qName)) return true;
			}
		}
		return false;
	}
		
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.Element#toMOF()
	 */
	@Override
	protected String toMOF(String prefix) {
		StringBuilder b = new StringBuilder(prefix);
		if(qualifiers != null){
			b.append("[");
			for(Qualifier q : qualifiers){
				b.append(q.toMOF(""));
				b.append(", ");
			}
			b.setLength(b.length()-2);
			b.append("]");
		}
		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.Element#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		// check for null, qualified element name, and element type
		if(!super.equals(o) || !(o instanceof QualifiedElement)) return false;
		QualifiedElement other = (QualifiedElement) o;
		HashSet<String> seen = new HashSet<String>();
		// check all our qualifiers are contained in the other element
		if(qualifiers != null){
			for(Qualifier q : qualifiers){
				String name = q.getLowerCaseName();
				DataValue qv = q.getValue();
				DataValue ov = other.getQualifierValue(name);
				boolean match = qv != null ? qv.equals(ov) : ov == null ? true : false;
				if(!match) return false;
				seen.add(name);
			}
		}
		// check all other element's qualifiers are contained this element
		if(other.qualifiers != null){
			for(Qualifier q : other.qualifiers){
				String name = q.getLowerCaseName();
				if(seen.contains(name)) continue;
				DataValue ov = q.getValue();
				DataValue qv = getQualifierValue(name);
				boolean match = qv != null ? qv.equals(ov) : ov == null ? true : false;
				if(!match) return false;
				seen.add(name);
			}
		}
		return true;
	}


}
