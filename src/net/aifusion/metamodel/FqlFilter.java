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
	/** Filter query */
	private String fql;
	/** Root of the query parse tree */
	private FqlNode root;
	
	/**
	 * Create a CimFilter based on an extension of the DMTF FQL (DSP0212)
	 * @param filterQuery - String containing filter query
	 */
	public FqlFilter(String filterQuery) {
		if(filterQuery == null) throw new ModelException(ExceptionReason.INVALID_QUERY,"Query cannot be null");
		this.fql = filterQuery;
		FqlParser p = new FqlParser(filterQuery);
		root = p.getParseTree();
		return;
	}

	/**
	 * Create a null CimFilter-- selects all elements of the given type
	 * @param objectPath path to the structure to be searched
	 */
	public FqlFilter() {
		this.fql = "";
		root = FqlOperator.EOF.getFqlNode();
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
	 * Get the tree representation of this filter
	 * @return - tree representation of this filter
	 */
	public String toTree() {
		return root.toTree("");
	}
	
	@Override
	public String toString() {
		return fql;
	}
	
	@Override
	public String getFilterQuery() {
		return fql;
	}
}
