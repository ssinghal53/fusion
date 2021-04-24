package net.aifusion.cql;

import net.aifusion.metamodel.DataValue;

/**
 * Node to represent a sort order for the result set
 * @author Sharad Singhal
 */
public class SortSpec extends Node {
	boolean isAscending = true;
	/**
	 * Create a SORT_BY specification
	 * @param value - boolean value, true for ascending sort, false for descending sort
	 */
	public SortSpec(DataValue value) {
		super(Operator.SORT_BY, null, null);
		isAscending = (Boolean) value.getValue();
		return;
	}
	
	public boolean isAscending() {
		return isAscending;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		Node child = getChildren().get(0);
		setValue(child.getValue());
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
		return;
	}
	@Override
	public String toString() {
		return super.toString()+"<"+isAscending+">";
	}
	
	
}
