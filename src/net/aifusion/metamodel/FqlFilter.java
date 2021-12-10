/**
 * Copyright 2021 Hewlett Packard Enterprise Development LP
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
 * Created Dec 4, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Create a CimFilter based on DMTF FQL specification. See DSP0212
 * @author Sharad Singhal
 */
public class FqlFilter implements CimFilter {
	private String fql;
	private FqlNode root;
	private ObjectPath path;
	/**
	 * Create a CimFilter based on DMTF FQL (DSP0212)
	 */
	public FqlFilter(ObjectPath structurePath, String fql) {
		if(fql == null) throw new ModelException(ExceptionReason.INVALID_QUERY,"Query cannot be null");
		this.fql = fql;
		if(structurePath == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"structurePath cannot be null");
		this.path = structurePath;
		FqlParser p = new FqlParser(fql);
		root = p.getParseTree();
		return;
	}

	@Override
	public boolean satisfies(StructureValue value, Repository repository) {
		root.evaluate(value, repository);
		DataValue v = root.getValue();
		if(v == null || v.getValue() == null || v.getType() != DataType.BOOLEAN) {
			throw new ModelException(ExceptionReason.INVALID_QUERY,"Query does not evaluate to a valid boolean value");
		}
		return (Boolean) v.getValue();
	}
	
	@Override
	public void setVariable(String variableName, DataValue value) {
		root.setVariable(variableName, value);
		return;
	}
	
	/**
	 * Create a tree representation of all nodes in the query parse tree
	 * @param n - current node to be represented
	 * @param indent - current level of indent
	 * @return - string containing subtree
	 */
	private String toTree(FqlNode n, String indent){
		StringBuilder b = new StringBuilder(indent);
		if(!indent.isEmpty()) b.append("-- ");
		b.append(n.toString());
		if(n.hasChildren()){
			for(FqlNode c : n.getChildren()){
				b.append("\n");
				b.append(c == null ? "|-- Null" : toTree(c,indent+"  |"));
			}
		}
		return b.toString();
	}
	
	/**
	 * Get the tree representation of this filter
	 * @return - tree representation of this filter
	 */
	public String toTree() {
		return toTree(root,"");
	}
	
	@Override
	public String toString() {
		return fql;
	}
	
	@Override
	public ObjectPath getStructurePath() {
		return path;
	}

	@Override
	public String getFilterQuery() {
		return fql;
	}
}
