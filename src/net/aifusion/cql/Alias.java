/**
 * Copyright 2017 Sharad Singhal, All Rights Reserved
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
 * Created Nov 12, 2017 by Sharad Singhal
 */
package net.aifusion.cql;

/**
 * Node to represent an Alias
 * @author Sharad Singhal
 */
class Alias extends Node {

	/**
	 * Create an ALIAS node
	 */
	Alias() {
		super(Operator.ALIAS, null, null);
	}

	@Override
	void evaluate() {
		return;
	}
	
	@Override
	void resolve(Alias alias){
		return;
	}
	
	/**
	 * Get the node which defines a given alias value
	 * @param alias - alias to be located
	 * @return - node containing the alias definition. Null if no such node exists
	 */
	Node locateAliasNode(String alias){
		return locateAliasNode(alias,this);
	}
	
	/**
	 * Get the className corresponding to an alias
	 * @param alias - alias to be searched
	 * @return - className corresponding to the alias. Null if no such value
	 */
	String getClassName(String alias){
		Node n = locateAliasNode(alias);
		if(n == null) return null;
		switch(n.getOperator()){
		case CLASS_PATH:
			return ((ClassPath)n).getClassName();
		case PROPERTY_NAME:
			return ((PropertyName)n).getClassName();
		case SELECT:
			return n.getAlias();
		default:
			return null;
		}
	}
	
	/**
	 * Get the local path corresponding to an alias
	 * @param alias - alias to be searched
	 * @return - local path corresponding to the alias. Null if no such value.
	 */
	String getLocalPath(String alias){
		Node n = locateAliasNode(alias);
		if(n == null) return null;
		switch(n.getOperator()){
		case CLASS_PATH:
			return ((ClassPath)n).getLocalPath();
		case PROPERTY_NAME:
			return ((PropertyName)n).getLocalPath();
		default:
			return null;
		}
	}
	
	/**
	 * Get the property name corresponding to an alias value
	 * @param alias - alias value to be searched
	 * @return - name of the corresponding property. Null if no such value.
	 */
	String getPropertyName(String alias){
		Node n = locateAliasNode(alias);
		if(n == null) return null;
		switch(n.getOperator()){
		case PROPERTY_NAME:
			return ((PropertyName)n).getPropertyName();
		case CLASS_PATH:
		case SELECT:
			return null;
		default:
			return alias;
		}
	}
	
	/**
	 * Search a subtree for the alias
	 * @param alias - alias to be searched
	 * @param node - root of the subtree
	 * @return - node defining an alias. Null of no such node
	 */
	private Node locateAliasNode(String alias, Node node){
		if(alias.equals(node.getAlias())) return node;
		for(Node child : node.getChildren()){
			Node found = locateAliasNode(alias, child);
			if(found != null) return found;
		}
		return null;
	}
	
}
