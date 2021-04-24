package net.aifusion.cql;

import net.aifusion.metamodel.DataValue;

/**
 * Node to represent a sort order for the result set
 * @author Sharad Singhal
 */
public class SortSpec extends Node {
	/** flag to indicate if the sort is ascending */
	private boolean isAscending = true;
	/**
	 * Create a SORT_BY specification
	 * @param value - boolean value, true for ascending sort, false for descending sort
	 */
	public SortSpec(DataValue value) {
		super(Operator.SORT_BY, null, value);
		isAscending = (boolean) value.getValue();
		return;
	}
	
	// TODO: Work in progress. Incomplete
	// sortSpec expects one child which evaluates a comparison expression
	// we evaluate a list of values (column), and sort it in order using the comparison operators {<, == >}
	// the type of sort is defined by isAscending

}
