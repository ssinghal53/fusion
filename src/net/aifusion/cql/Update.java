/**
 * Copyright 2022 Sharad Singhal. All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
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
 * Created Sep 19, 2022 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to implement an UPDATE query
 * @author Sharad Singhal
 *
 */
public class Update extends Node {

	/**
	 * Create an UPDATE node
	 */
	public Update() {
		super(Operator.UPDATE, null, null);
		return;
	}

	/**
	 * Run an UPDATE query on the repository
	 * @param repository - repository to use
	 * @return - updated instances
	 */
	public List<StructureValue> evaluate(Repository repository) {
		List<Node> children = getChildren();
		
		// locate the class to update
		ClassPath classPath = (ClassPath) children.get(0);
		List<StructureValue> updated = new Vector<StructureValue>();
		String header = classPath.getAlias() != null ? classPath.getAlias() : classPath.getName();
		
		// locate all matching elements from repository
		List<NamedElement> elements = repository.getElements("structurevalue,instance", classPath.getLocalPath(), classPath.getClassName(), true);
		if(elements.size() == 0) return updated;
		
		Node where = Operator.WHERE.equals(children.get(children.size()-1).getOperator()) ? children.get(children.size()-1) : null;
		int toAssign = where != null ? children.size()-1 : children.size();
		
		for(NamedElement e : elements) {
			// test the where clause against this element
			if(where != null) {
				where.evaluate(header, (StructureValue) e);
				if(!where.getBooleanValue()) continue;
			}
			// where matches, update instance properties
			updated.add((StructureValue)e);
			for(int i = 1; i < toAssign; i++) {
				children.get(i).evaluate(header, (StructureValue)e);
			}
		}
		// update instances in the repository
		for(StructureValue v : updated) {
			repository.put(v);
		}
		
		// return the updated instances
		return updated;
	}

	@Override
	void evaluate(BufferedCache repository, HashMap<String, List<StructureValue>> workingSet) {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate(Repository,WorkingSet) not implemented by "+toString());
	}

	@Override
	void evaluate(List<String> headers, List<StructureValue> instances) {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate(headers,instances) not implemented by "+toString());
	}

	@Override
	void evaluate(String header, StructureValue instance) {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate(header,instance) not implemented by "+toString());
	}

	@Override
	void evaluate() {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate() not implemented by "+toString());
	}
}
