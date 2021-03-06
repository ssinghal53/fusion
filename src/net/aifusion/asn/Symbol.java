/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
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
 * Created May 20, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to manage a symbol
 * @author Sharad Singhal
 */
public class Symbol {
	/** Name of the symbol */
	private String name;
	/** Production that defines the type of this symbol (e.g., INTEGER_TYPE) */
	private AsnProduction type;
	/** Name of the defining symbol if it is a subtype */
	private String definingType;
	/** Parent of this symbol (enclosing symbol) */
	private Symbol parent;
	/** Definition for this symbol */
	private AsnNode definition;
	
	/**
	 * Create a symbol
	 * @param name - name of the symbol (must not be null)
	 * @param type - type of this symbol
	 * @param parent - enclosing symbol, if any
	 * @param definingType - name of the defining type, if not a built-in type
	 */
	public Symbol(String name, AsnProduction type, Symbol parent, String definingType) {
		this.name = name;
		this.parent = parent;
		this.type = type;
		this.definingType = definingType;
	}
	
	/**
	 * Get the name of this symbol
	 * @return - name of this symbol
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Get the fully qualified name of this symbol
	 * @return - fully qualified name in the form "@" moduleName ( "." symbol)* 
	 */
	public String getFullName() {
		if(parent == null) {
			if(AsnProduction.MODULE_IDENTIFIER.equals(type)) {
				return "@"+name;
			}
			return name;
		}
		StringBuilder b = new StringBuilder();
		b.append(parent.getFullName()).append(".").append(name);
		return b.toString();
	}
	
	/**
	 * Get the production associated with this symbol
	 * @return - AST Production associated with the symbol
	 */
	public AsnProduction getProduction() {
		return type;
	}
	
	/**
	 * Get the parent of this symbol, if any
	 * @return - parent of this symbol. Null if none defined
	 */
	public Symbol getParent() {
		return parent;
	}
	
	/**
	 * Get the name of the defining type, if any
	 * @return - name of the defining type. Null if none exists
	 */
	public String getDefiningType() {
		return definingType;
	}
	
	/**
	 * Set the definition for this symbol
	 * @param definition - AST node defining this symbol
	 */
	public void setDefinition(AsnNode definition) {
		if(this.definition != null) throw new ModelException(ExceptionReason.ALREADY_EXISTS,"Attempt to reset definition for "+name);
		this.definition = definition;
		return;
	}
	
	/**
	 * Get the definition for this symbol
	 * @return - definition. Null if the symbol is not yet defined
	 */
	public AsnNode getDefinition() {
		return definition;
	}
	
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append(getName()).append(" ").append(type);
		if(definingType != null) b.append(" [").append(definingType).append("]");
		if(definition != null) b.append(" {").append(definition).append("}");
		// if(parent != null) b.append(" {").append(parent.toString()).append("}");
		return b.toString();
	}
}
