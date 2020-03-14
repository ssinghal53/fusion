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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Vector;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Class to implement a SELECT Node. Evaluates against a repository.
 * @author Sharad Singhal
 */
class Select extends Node {
	/**
	 * Create a SELECT node
	 */
	Select() {
		super(Operator.SELECT, null, null);
		return;
	}
	
	/**
	 * Resolve aliases, if any, defined within this Select statement
	 */
	protected void resolve() {
		// locate the aliases, if any defined in this query
		Alias alias = null;
		for(Node child : getChildren()){
			if(Operator.ALIAS.equals(child.getOperator())){
				alias = (Alias) child;
				break;
			}
		}
		// resolve them in the parse tree
		if(alias != null){
			for(Node child : getChildren()){
				child.resolve(alias);
			}
		}
		return;
	}
	
	@Override
	protected void resolve(Alias aliases) {
		// Sub-queries do not use aliases defined in enclosing queries
		// redirect to resolve()
		resolve();
		return;
	}

	@Override
	List<StructureValue> evaluate(BufferedCache cache) {
		if(debug) System.out.println(toString()+"(repository) - Enter");
		SelectList selectList = null;
		ClassList classList = null;
		Where where = null;
		Alias alias = null;
		for(Node child : getChildren()){
			switch(child.getOperator()){
			case SELECT_LIST:
				selectList = (SelectList) child;
				break;
			case CLASS_LIST:
				classList = (ClassList) child;
				break;
			case WHERE:
				where = (Where) child;
				break;
			case ALIAS:
				alias = (Alias) child;
				break;
			default:
				throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" does not support "+child.getOperator());
			}
		}
		if(selectList == null || classList == null){
			throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+" requires both selectList and classList");
		}

		// Create the working set
		// Note that columns in the working set are keyed by:
		// 1. Alias for the class, if classList contains [class_name | classPath] [AS] Alias
		// 2. Class_Name if classList contains class_name
		// 3. ClassPath if classList contains /class/path:class_name
		// 4. Alias associated with the subQuery if subQuery is part of classList

		LinkedHashMap<String,List<StructureValue>> workingSet = new LinkedHashMap<String,List<StructureValue>>();
		// collect all candidate instances using classList
		classList.evaluate(cache, workingSet);
		
		// process the candidate instances
		Vector<String> headers = new Vector<String>();
		headers.addAll(workingSet.keySet());
		if(debug){
			System.out.println(classList.toString()+" found "+workingSet.size()+" columns");
			for(String h : headers) System.out.print(" Size of column "+h+" = "+workingSet.get(h).size());
			System.out.print("\n");
		}
		// create the Result_Set definition
		selectList.reset(cache,headers,alias, getAlias());
		
		// obtain the row-Set
		// rowSet is indexed by row number, with each row containing matching instances from working set
		// elements in the row set retain ordering defined by column headers
		LinkedHashMap<Integer,List<StructureValue>> rowSet = new LinkedHashMap<Integer,List<StructureValue>>();
		// if we have a where clause, filter the working set into the row set
		if(where != null){
			// filter using the where clause
			int row = 0;
			switch(workingSet.size()){
			case 1:	// basic query. Only one column in working set
				for(String key : workingSet.keySet()){	// we will only loop once
					List<StructureValue> elements = workingSet.get(key);	// elements of the column
					for(StructureValue e : elements){		// evaluate all rows in the column
						where.evaluate(headers.get(0), e);
						if(where.getBooleanValue()){	// save rows where there is a match
							Vector<StructureValue> r = new Vector<StructureValue>();
							r.add(e);
							rowSet.put(row++, r);
						}
					}
				}
				break;
			default:	// simple or complex join - try all possible combinations in the columns
				int index[] = new int[workingSet.size()];	// current row for each column
				int size[] = new int[workingSet.size()];	// column size for the column
				int i = 0; 
				for(String key : headers){
					index[i] = 0;							// starting row for the column
					size[i++] = workingSet.get(key).size();	// size of the column
				}
				// collect matching instances in the join
				Vector<StructureValue> instances = new Vector<StructureValue>();
				while(index[index.length-1] < size[size.length-1]){	// while we still have items to go
					// collect a combination of instances
					for(int j = 0; j < index.length; j++){
						instances.add(workingSet.get(headers.get(j)).get(index[j]));
					}
					where.evaluate(headers, instances);	// evaluate if the where criteria is satisfied for this combination
					if(true == where.getBooleanValue()){// if where-criteria matches, save the elements in a row
						Vector<StructureValue> rowValues = new Vector<StructureValue>();
						rowValues.addAll(instances);
						rowSet.put(row++, rowValues);
					}
					instances.clear();		// clear instances for the next iteration
					increment(index,size);	// increment indexes for next combination
				}
			}
		} else {
			// no where clause, Concatenate the columns into the row set. Note that in case
			// of multiple columns, each row will only have one non-null value
			int row = 0;
			switch(workingSet.size()){
			case 1:	// basic query. Only one column in working set
				for(String key : workingSet.keySet()){	// we will only loop once
					List<StructureValue> elements = workingSet.get(key);	// elements of the column
					for(StructureValue e : elements){		// evaluate all rows in the column
						Vector<StructureValue> r = new Vector<StructureValue>();
						r.add(e);
						rowSet.put(row++, r);
					}
				}
				break;
			default:	// simple or complex join - create all possible combinations of elements in the columns
				int index[] = new int[workingSet.size()];	// current row for each column
				int size[] = new int[workingSet.size()];	// column size for the column
				int i = 0; 
				for(String key : headers){
					index[i] = 0;							// starting row for the column
					size[i++] = workingSet.get(key).size();	// size of the column
				}
				// collect matching instances in the join
				Vector<StructureValue> instances = new Vector<StructureValue>();
				while(index[index.length-1] < size[size.length-1]){	// while we still have items to go
					// collect a combination of instances
					for(int j = 0; j < index.length; j++){
						instances.add(workingSet.get(headers.get(j)).get(index[j]));
					}
					Vector<StructureValue> rowValues = new Vector<StructureValue>();
					rowValues.addAll(instances);
					rowSet.put(row++, rowValues);
					instances.clear();		// clear instances for the next iteration
					increment(index,size);	// increment indexes for next combination
				}
			}
		}
		if(debug) System.out.println("Table Size ("+rowSet.size()+","+headers.size()+")");
		if(rowSet.isEmpty()){
			if(debug) System.out.println(toString()+"(repository) - Exit "+0);
			return new Vector<StructureValue>();
		}
		
		// create the result set
		Vector<StructureValue> resultSet = new Vector<StructureValue>();
		for(List<StructureValue> instances : rowSet.values()){
			selectList.evaluate(headers,instances);
			if(selectList.hasNonNullValue()){
				StructureValue e = (StructureValue) selectList.getValue().getValue();
				resultSet.add(e);
			}
		}
		if(debug) System.out.println(toString()+"(repository) - Exit "+resultSet.size());
		return resultSet;
	}

	/**
	 * Increment indexes to iterate through all possible combinations
	 * @param index - index values within columns
	 * @param size - size of columns
	 * @return - true if additional indexes are available, false if we are finished
	 */
	private boolean increment(int[] index, int[] size) {
		for(int i = 0; i < index.length; i++){
			index[i]++;
			if(index[i] < size[i]) return true;
			if(i < index.length-1) index[i] = 0;
		}
		return false;
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
