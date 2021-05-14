/**
 * <p>This package implements a query facility based on the DMTF
 * <a href="http://www.dmtf.org/sites/default/files/standards/documents/DSP0202_1.0.0.pdf">CIM Query Language (CQL)</a>. 
 * Since CQL was designed for CIM Version 2, but the Fusion metamodel is based on CIM Version 3, there are differences
 * in the query language as well.</p>
 * <p> The package exposes a single class
 * {@linkplain CimQuery} that can be used to query instances within a repository.</p>
 * <p>Currently, while the majority of the CQL specification can be parsed as a valid query, the actual query engine is only partially implemented. Thus,
 * mileage on a specific query may vary, especially for queries that involve joins. The Basic Query form defined in DSP0202 should, however, work:
 * <pre>
 * SELECT * FROM class_name WHERE &lt;selection expression&gt; 
 * </pre>
 * <p>The query will return a list of structure values that meet the desired expression. Aggregation functions (e.g., COUNT, MIN, MAX, SUM, MEAN, MEDIAN) are currently
 * not implemented. However, other result-set operations (i.e., FIRST, DISTINCT, ORDER BY) are implemented.</p>
 * <p>In addition, The construct <code>$identifier$</code> can be used in the query to define &quot;delayed constants&quot; that can be set within the query using
 * {@link CimQuery#setVariable(String, net.aifusion.metamodel.DataValue)} after it is parsed. This is useful, for example to reuse the same query with different
 * values in the selection expression without the overhead of re-constructing the query each time.</p>
 * @author Sharad Singhal
 */
package net.aifusion.cql;