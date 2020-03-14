/**
 * Copyright 2016, Sharad Singhal, All Rights Reserved
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
 * Created Oct 8, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.StructureValue;

/**
 * Node to implement CLASS_LIST
 * @author Sharad Singhal
 */
class ClassList extends Node {
	/**
	 * Create a CLASS_LIST node
	 */
	ClassList() {
		super(Operator.CLASS_LIST, null, null);
		return;
	}

	@Override
	void evaluate(BufferedCache repository, HashMap<String, List<StructureValue>> workingSet) {
		if(debug) System.out.println(toString()+"(Repository, workingSet) - Enter");
		List<Node> children = getChildren();
		for(int i = 0; i < children.size(); i++){
			Node c = children.get(i);
			switch(c.getOperator()){
			case CLASS_PATH:	// have a classPath - key by classPath in working set
				String header = c.getAlias() != null ? c.getAlias() : c.getName();
				if(workingSet.containsKey(header)) throw new ModelException(ExceptionReason.INVALID_QUERY,"Class_List contains duplicate name "+header);
				ClassPath cp = (ClassPath)c;
				// locate all instances of the class with the corresponding local path from the repository
				List<StructureValue> instances = new Vector<StructureValue>();
				if(debug){
					System.out.println("Locate Instances from "+cp.toString());
				}
				for(NamedElement e : repository.getAllElements("structurevalue,instance", cp.getLocalPath(), cp.getClassName(), true)){
					
					instances.add((StructureValue)e);
				}
				workingSet.put(header, instances);
				break;
			case SELECT:	// have a sub-select statement - key by select alias in working set
				header = c.getAlias();
				if(workingSet.containsKey(header)) throw new ModelException(ExceptionReason.INVALID_QUERY,"Class_List contains duplicate name "+header);
				instances = c.evaluate(repository);
				workingSet.put(header, instances);
				break;
			/*
			case IDENTIFIER:	// have the name of a class - key by alias (if defined), else by className, else by name
				Identifier id = (Identifier)c;
				header = id.getAlias() != null ? id.getAlias() : id.getClassName() != null ? id.getClassName() : id.getName();
				if(workingSet.containsKey(header)) throw new ModelException(ExceptionReason.INVALID_QUERY,"Class_List contains duplicate name "+header);
				// locate all instances of the class from the repository, and add them to the working set
				instances = repository.getElements("instance", null, c.getName(), true);
				workingSet.put(header, instances);
				break;
			*/
			default:
				throw new ModelException("Operator "+c.getOperator()+" not yet handled in "+toString());
			}
		}
		if(debug) System.out.println(toString()+"(Repository, workingSet) - Exit "+workingSet.size());
		return;
	}
}
