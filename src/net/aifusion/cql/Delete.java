/**
 * Copyright 2021 Sharad Singhal. All Rights Reserved.
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
 * Created Oct 7, 2021 by Sharad Singhal
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
 * Node to handle Delete
 * @author Sharad Singhal
 *
 */
public class Delete extends Node {

	/**
	 * Create a DELETE node
	 */
	public Delete() {
		super(Operator.DELETE,null,null);
		return;
	}
	/**
	 * Evaluate the query against a repository
	 * @param cache - repository to use for query
	 * @return - list of values that were deleted from the repository
	 */
	List<StructureValue> evaluate(Repository cache) {
		if(debug) System.out.println(toString()+"(repository) - Enter");
		ClassPath classPath = null;
		Where where = null;
		OrderBy order = null;
		for(Node child : getChildren()){
			switch(child.getOperator()){
			case CLASS_PATH:
				classPath = (ClassPath) child;
				break;
			case WHERE:
				where = (Where) child;
				break;
			case ORDER_BY:
				order = (OrderBy) child;
				break;
			default:
				throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" does not support "+child.getOperator());
			}
		}
		if(classPath == null){
			throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" requires classPath");
		}
		
		String header = classPath.getAlias() != null ? classPath.getAlias() : classPath.getName();
		Vector<StructureValue> candidates = new Vector<StructureValue>();
		
		if(debug){
			System.out.println("Locate Instances from "+classPath.toString());
		}
		
		for(NamedElement e : cache.getElements("structurevalue,instance", classPath.getLocalPath(),
				classPath.getClassName(), true)){
			if(where != null) {
				where.evaluate(header, (StructureValue) e);
				if(where.getBooleanValue()) {
					candidates.add((StructureValue) e);
				}
			} else {
				candidates.add((StructureValue) e);
			}
		}
		if(!candidates.isEmpty()){		
			if(order != null) {
				Vector<String> headers = new Vector<String>();
				headers.add(header);
				order.evaluate(headers,candidates);
			}
			if(!candidates.isEmpty()) {
				for(StructureValue e : candidates) {
					cache.delete(e.getObjectPath());
				}
			}
		}
		if(debug) System.out.println(toString()+"(repository) - Exit "+0);
		return candidates;
	}


	@Override
	List<StructureValue> evaluate(BufferedCache cache) {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate(Repository) not implemented by "+toString());
	}

	@Override
	void evaluate(BufferedCache repository, HashMap<String, List<StructureValue>> workingSet) {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate(Repository,WorkingSet) not implemented by "+toString());
	}
	
	@Override
	void evaluate(String header, StructureValue instance) {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate(Instance) not implemented by "+toString());
	}
	
	@Override
	void evaluate(List<String> headers, List<StructureValue> instances) {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate(List<Instance>) not implemented by "+toString());
	}

	@Override
	void evaluate() {
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"Evaluate() not implemented by "+toString());
	}
}
