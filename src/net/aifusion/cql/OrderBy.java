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
 * Created Apr 23, 2021 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.Comparator;
import java.util.List;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.DateTime;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to manage ORDER_BY for the result set
 * @author Sharad Singhal
 */
class OrderBy extends Node {
	/** Return only the first n rows */
	int rowsToReturn = -1;
	/** Return only distinct rows */
	boolean distinct = false;

	/**
	 * Create an ORDER_BY node
	 */
	public OrderBy() {
		super(Operator.ORDER_BY, null, null);
	}
	
	public void setFirst(int n) {
		rowsToReturn = n;
		return;
	}
	
	public void setDistinct() {
		distinct = true;
		return;
	}

	@Override
	public String toString() {
		return super.toString()+"<"+rowsToReturn+","+distinct+">";
	}
	
	@Override
	void evaluate(List<String> headers, List<StructureValue> instances) {
		if(debug) System.out.println(toString()+"(headers, instances) - Enter");
		
		// sort the instances if sortSpec values are given
		if(hasChildren()) {
			instances.sort(new Comparator<StructureValue>() {
				@Override
				public int compare(StructureValue o1, StructureValue o2) {
					return compareStruct(headers,o1,o2);
				}
			});
		}

		// locate duplicate entries and delete them
		if(distinct) {
			StructureValue lastElement = instances.get(instances.size()-1);
			for(int i = instances.size()-2; i == 0; i--) {
				StructureValue currentElement = instances.get(i);
				if(currentElement.equals(lastElement)) {
					instances.remove(i+1);
				}
				lastElement = currentElement;
			}
		}
		
		// if fewer rows are needed, return them
		if(rowsToReturn > 0 && rowsToReturn < instances.size()) {
			instances.subList(rowsToReturn, instances.size()).clear();
		}
		
		if(debug) System.out.println(toString()+"(headers, instances) - Exit "+getValue());
		return;
	}
	
	/*
   |-- ORDER_BY<3,true>
   |   |-- SORT_BY<true>
   |   |   |-- ADD
   |   |   |   |-- IDENTIFIER (Property1) [path = null class = null property = Property1]
   |   |   |   |-- IDENTIFIER (Property2) [path = null class = null property = Property2]
   |   |-- SORT_BY<false>
   |   |   |-- IDENTIFIER (Property2) [path = null class = null property = Property2]
	 */
	
	private int compareStruct(List<String> headers, StructureValue s1,StructureValue s2) {
		List<Node> children = getChildren();
		for(int i = 0; i < children.size(); i++) {
			SortSpec sort = (SortSpec) children.get(i);
			sort.evaluate(headers.get(i), s1);
			DataValue v1 = sort.getValue();
			sort.evaluate(headers.get(i), s2);
			DataValue v2 = sort.getValue();
			if(v1 == null && v2 == null) continue;	// nulls are considered equal
			if(v1 == null && v2 != null) return sort.isAscending() ? 1 : -1; // null > value
			if(v1 != null && v2 == null) return sort.isAscending() ? -1 : 1;
			int comp = compareValues(v1.getType(),v1.getValue(),v2.getType(),v2.getValue());
			if(comp != 0) return sort.isAscending() ? comp : -comp;
		}
		return 0;
	}
	
	/**
	 * Compare two values and return the result of their comparison
	 * @param leftType - data type for left value
	 * @param leftValue - left value
	 * @param rightType - data type for right value
	 * @param rightValue - right value
	 * @return -1, 0, 1 depending on if leftValue <, =, or > rightValue
	 */
	private int compareValues(DataType leftType, Object leftValue, DataType rightType, Object rightValue){
		if(leftValue != null && rightValue != null){
			// neither value is null, compare
			if(leftType.isNumeric() && rightType.isNumeric()){
				if(leftType.isInteger() && rightType.isInteger()){
					// do an integer compare
					Long li = getLongValue(leftType,leftValue);
					Long ri = getLongValue(rightType,rightValue);
					return li.compareTo(ri);
				} else {
					// do a double compare
					Double lr = getDoubleValue(leftType,leftValue);
					Double rr = getDoubleValue(rightType,rightValue);
					return lr.compareTo(rr);
				}
			} else if(leftType == DataType.STRING && rightType == DataType.STRING){
				String ls = leftValue.toString();
				String rs = rightValue.toString();
				return ls.compareTo(rs);
			} else if(leftType == DataType.DATETIME && rightType == DataType.DATETIME) {
				DateTime ld = (DateTime) leftValue;
				DateTime rd = (DateTime) rightValue;
				if(debug) System.out.println(ld.toString()+" "+rd.toString());
				Integer c = ld.compareTo(rd);
				return c != null ? c : 0;
			} else if(leftType == DataType.BOOLEAN && rightType == DataType.BOOLEAN){
				Boolean lb = (Boolean) leftValue;
				Boolean rb = (Boolean) rightValue;
				if(lb == rb) return 0;
				return lb == true ? 1 : -1;	// true > false
			} else if(leftType == DataType.OBJECTPATH && rightType == DataType.OBJECTPATH) {
				ObjectPath lp = (ObjectPath) leftValue;
				ObjectPath rp = (ObjectPath) rightValue;
				if(lp.equals(rp)) return 0;
				throw new ModelException(toString()+" does not yet implement objectpath comparison "+getOperator());
			} else {
				throw new ModelException(toString()+" does not yet handle comparison for "+leftType+" CompareTo "+rightType);
			}
		} else if(leftValue == null && rightValue == null) {
			return 0;
		} else if(leftValue == null){
			return 1;
		}
		return -1;
	}

}
