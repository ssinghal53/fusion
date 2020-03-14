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
 * Created Dec 31, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Scope of CIM Qualifiers. The scope defines cim elements that can be annotated with a given qualifier
 * @author Sharad Singhal
 */
public enum Scope {
	/** The qualifier can be applied to a structure */
	STRUCTURE("Structure",ElementType.STRUCTURE),
	/** The qualifier can be applied to a structure value */
	STRUCTUREVALUE("StructureValue",ElementType.STRUCTUREVALUE),
	/** The qualifier applies to an enumeration */
	ENUMERATION("Enumeration",ElementType.ENUMERATION),
	/** The qualifier applies to an enumeration dataValue */
	ENUMERATIONVALUE("EnumerationValue",ElementType.ENUMERATIONVALUE),
	/** The qualifier can be applied to an interface */
	INTERFACE("Interface",ElementType.INTERFACE),
	/** The qualifier can be applied to a class */
	CLASS("Class",ElementType.CLASS),
	/** The qualifier can be applied to an instance */
	INSTANCE("Instance",ElementType.INSTANCE),
	/** The qualifier can be applied to a qualifier type */
	QUALIFIERTYPE("Qualifiertype",ElementType.QUALIFIERTYPE),
	/** The qualifier can be applied to a method parameter */
	PARAMETER("Parameter",ElementType.PARAMETER),
	/** The qualifier can be applied to a method */
	METHOD("Method",ElementType.METHOD),
	/** The qualifier can be applied to a property */
	PROPERTY("Property",ElementType.PROPERTY),
	/** The qualifier can be applied to a reference property */
	REFERENCE("Reference",ElementType.REFERENCE),
	/** The qualifier can be applied to the Schema */
	SCHEMA("Schema",ElementType.SCHEMA),
	/** The qualifier can be applied to associations (needed for CimV.2 parser)*/
	ASSOCIATION("Association",ElementType.ASSOCIATION),
	/** The qualifier can be applied to an indication (needed for CimV.2 parser) */
	INDICATION("Indication",ElementType.INDICATION),
	/** The qualifier can be applied to any meta element */
	ANY("Any",null);
	
	// Note that ElementType.QUALIFIER does not have a scope, and Scope ANY applies to all ElementType values
	
	/** mof name for this scope */
	private final String mofName;
	/** type of element to which this scope applies */
	private final ElementType type;
	
	private Scope(String mofName, ElementType type){
		this.mofName = mofName;
		this.type = type;
		return;
	}
	
	/**
	 * Check if this scope applies to a given element type
	 * @param elementType - type of element to be checked
	 * @return - true if this scope matches the given element type, false otherwise
	 */
	public final boolean appliesTo(ElementType elementType){
		return this.type == elementType || this == ANY;
	}
	
	/**
	 * Mof definition of scope
	 * @return - mixed case dataValue of scope
	 */
	public final String toMOF(){
		return mofName;
	}
}
