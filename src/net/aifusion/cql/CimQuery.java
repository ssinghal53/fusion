/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
 * Created Dec 18, 2015 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.List;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to implement a query for elements in a repository
 * @author Sharad Singhal
 */
public class CimQuery {
	/** input string */
	private String query;
	/** Root of the query parse tree */
	private Select select;
	
	/**
	 * Create a CIM query
	 * @param query query in CQL format
	 */
	public CimQuery(String query) {
		this.query = query;
		QueryParser parser = new QueryParser();
		select = (Select) parser.parse(query);
		select.resolve();
		return;
	}
	
	/**
	 * Create a tree representation of all nodes in the query parse tree
	 * @param n - current node to be represented
	 * @param indent - current level of indent
	 * @return - string containing subtree
	 */
	private String toTree(Node n, String indent){
		StringBuilder b = new StringBuilder(indent);
		if(!indent.isEmpty()) b.append("-- ");
		b.append(n.toString());
		if(n.hasChildren()){
			for(Node c : n.getChildren()){
				b.append("\n");
				b.append(c == null ? "|-- Null" : toTree(c,indent+"  |"));
			}
			
		}
		return b.toString();
	}
	
	/**
	 * Get a string representation of this query containing the query string and the parse tree
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder(query);
		b.append("\n");
		if(select != null){
			b.append(toTree(select,""));
		}
		return b.toString();
	}
	
	/**
	 * Set the value of a lazy constant in this query
	 * @param variable - name of the lazy constant
	 * @param value - value to set
	 */
	public void setVariable(String variable, DataValue value){
		select.setVariable(variable, value);
		return;
	}
	
	/**
	 * Execute this query against a repository
	 * @param repository - repository to run this query against
	 * @return - result set from the query
	 */
	public List<StructureValue> executeQuery(Repository repository){
		return select.evaluate(new BufferedCache(repository));
	}
}
