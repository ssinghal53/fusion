/**
 * Copyright 2021, Sharad Singhal, All Rights Reserved
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
 * Created Sept 19, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Class to implement a query for elements in a repository
 * @author Sharad Singhal
 */
public class CimFilter {
	/** input string */
	private String query;
	/** Root of the query parse tree */
	private FQLNode select;
	
	/**
	 * Create a CIM query
	 * @param query query in CQL format
	 */
	public CimFilter(String query) {
		this.query = query;
		FQLParser parser = new FQLParser();
		select = (FQLNode) parser.parse(query);
		select.resolve();
		return;
	}
	
	/**
	 * Create a tree representation of all nodes in the query parse tree
	 * @param n - current node to be represented
	 * @param indent - current level of indent
	 * @return - string containing subtree
	 */
	private String toTree(FQLNode n, String indent){
		StringBuilder b = new StringBuilder(indent);
		if(!indent.isEmpty()) b.append("-- ");
		b.append(n.toString());
		if(n.hasChildren()){
			for(FQLNode c : n.getChildren()){
				b.append("\n");
				b.append(c == null ? "|-- Null" : toTree(c,indent+"  |"));
			}
			
		}
		return b.toString();
	}
	
	/**
	 * Get a string representation of this query containing the query string and the parse tree
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder(query);
		b.append("\n");
		if(select != null){
			b.append(toTree(select,""));
		}
		return b.toString();
	}
	
	/**
	 * Set the value of a lazy constant in this query
	 * @param variable - name of the lazy constant
	 * @param value - value to set
	 */
	public void setVariable(String variable, DataValue value){
		select.setVariable(variable, value);
		return;
	}
	
	/**
	 * Evaluate the filter on a CIM Structure Value
	 * @param structValue - CIM StructureValue to test
	 * @return true if the structure value matches the filter, false otherwise
	 */
	public boolean isValid(StructureValue structValue) {
		select.evaluate(structValue);
		return select.getBooleanValue();
	}
}
