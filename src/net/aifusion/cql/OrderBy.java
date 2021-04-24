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

/**
 * Class to manage ORDER_BY for the result set
 * @author Sharad Singhal
 */
public class OrderBy extends Node {
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
	
	// TODO: Incomplete. Work in progress.
	// The ORDER_BY node takes a result set, and orders it by columns
	// It has zero or more SORT_BY nodes as children, each of which defines a sort expression and an order (ascending or descending)
	// The result set is sorted by the first child, and in case the children are duplicate (equal), the second child is used to refine
	// the sort, and so on. Elements that are still equal after checks through all children are returned in some arbitrary order
	//
	// if distinct is true, only unique elements are returned
	// if rowsToReturn > 0, only the first n rows are returned (after deleting unique elements and sorting, if needed)
}
