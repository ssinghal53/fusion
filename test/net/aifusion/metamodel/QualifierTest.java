/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Nov 30, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test Qualifiers
 * @author Sharad Singhal
 */
public class QualifierTest {
	private static QualifierType [] qt;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Qualifier ");
		qt = new QualifierType[3];
		Vector<Scope> scopes = new Vector<Scope>();
		scopes.add(Scope.CLASS);
		qt[0] = new QualifierType("Abstract",DataType.BOOLEAN,new DataValue(false),scopes,Policy.RESTRICTED,null,Constants.defaultNameSpacePath);
		qt[1] = new QualifierType("Description",DataType.STRING,new DataValue("something"),scopes,Policy.RESTRICTED,null,Constants.defaultNameSpacePath);
		qt[2] = new QualifierType("Key",DataType.BOOLEAN,new DataValue(false),scopes,Policy.RESTRICTED,null,Constants.defaultNameSpacePath);
	}
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}
	
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		Qualifier q = new Qualifier(qt[0]);
		assertEquals("Abstract",q.toMOF());
		q = new Qualifier(qt[0],new DataValue(false));
		assertEquals("Abstract(false)",q.toMOF());
		
		q = new Qualifier(qt[1]);
		assertEquals("Description",q.toMOF());
		q = new Qualifier(qt[1],new DataValue("Some Description"));
		assertEquals("Description(\"Some Description\")",q.toMOF());
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#Qualifier(net.aifusion.metamodel.QualifierType)}.
	 */
	@Test
	public final void testQualifierQualifierType() {
		Qualifier q = new Qualifier(qt[0]);
		assertNotNull(q);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#Qualifier(net.aifusion.metamodel.QualifierType, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testQualifierQualifierTypeDataValue() {
		Qualifier q = new Qualifier(qt[1],new DataValue("String"));
		assertNotNull(q);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#getValue()}.
	 */
	@Test
	public final void testValue() {
		// a qualifier with dataValue specified during construction will return that dataValue
		Qualifier q = new Qualifier(qt[1],new DataValue("String"));
		assertEquals(new DataValue("String"),q.getValue());
		// with no dataValue specified, the dataValue() method will return the default specified in the qualifier type
		q = new Qualifier(qt[1]);
		assertEquals(new DataValue("something"),q.getValue());
		// boolean qualifiers with no value must return true
		q = new Qualifier(qt[2]);
		assertEquals(new DataValue(true),q.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#hasValue()}.
	 */
	@Test
	public final void testHasValue() {
		// a qualifier with dataValue specified during construction will return that dataValue
		Qualifier q = new Qualifier(qt[1],new DataValue("String"));
		assertTrue(q.hasValue());
		q = new Qualifier(qt[1]);
		assertFalse(q.hasValue());

	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#hasNonNullValue()}.
	 */
	@Test
	public final void testHasNonNullValue() {
		// a qualifier with dataValue specified during construction will return that dataValue
		Qualifier q = new Qualifier(qt[1],new DataValue("String"));
		assertTrue(q.hasNonNullValue());
		assertTrue(q.hasValue());
		q = new Qualifier(qt[1],new DataValue(DataType.STRING,null));
		assertTrue(q.hasValue());
		assertFalse(q.hasNonNullValue());
		q = new Qualifier(qt[1]);
		assertFalse(q.hasValue());
		assertFalse(q.hasNonNullValue());

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#getQualifierType()}.
	 */
	@Test
	public final void testType() {
		Qualifier q = new Qualifier(qt[0]);
		assertEquals(qt[0],q.getQualifierType());
		q = new Qualifier(qt[1]);
		assertEquals(qt[1],q.getQualifierType());
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#getDataType()}.
	 */
	@Test
	public final void testDataType() {
		Qualifier q = new Qualifier(qt[0]);
		assertEquals(qt[0].getDataType(),q.getDataType());
		q = new Qualifier(qt[1]);
		assertEquals(qt[1].getDataType(),q.getDataType());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#hasScope(net.aifusion.metamodel.Scope)}.
	 */
	@Test
	public final void testHasScope() {
		Qualifier q = new Qualifier(qt[1]);
		assertTrue(q.hasScope(Scope.CLASS));
		assertFalse(q.hasScope(Scope.PARAMETER));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Qualifier#appliesTo(net.aifusion.metamodel.ElementType)}.
	 */
	@Test
	public final void testAppliesTo() {
		Qualifier q = new Qualifier(qt[0]);
		assertTrue(q.appliesTo(ElementType.CLASS));
	}
	
	/**
	 * test method for {@link net.aifusion.metamodel.Qualifier#equals(Object)}.
	 */
	@Test
	public final void testEquals(){
		Qualifier q = new Qualifier(qt[0]);
		Qualifier q2 = new Qualifier(qt[0],new DataValue(false));
		// note that for these two, the hashcode will be equal
		assertFalse(q.equals(q2));
		
		q2 = new Qualifier(qt[0]);
		assertEquals(q,q2);
		
	}
	
	/**
	 * test method for {@link net.aifusion.metamodel.Qualifier#hashCode()}.
	 */
	@Test
	public final void testHashCode(){
		Qualifier q = new Qualifier(qt[0]);
		Qualifier q2 = new Qualifier(qt[0],new DataValue(false));
		assertEquals(q.hashCode(),q2.hashCode());
		q2 = new Qualifier(qt[1]);
		assertFalse(q.hashCode() == q2.hashCode());
	}
	/**
	 * Test to check enum-valued qualifiers
	 */
	@Test
	public final void testEnumValuedQualifier(){
		// create the enumeration
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		values.add(new EnumerationValue("none","Cim_AggregationKind",null,null));
		values.add(new EnumerationValue("bag","Cim_AggregationKind",null, null));
		values.add(new EnumerationValue("indexed","Cim_AggregationKind",null, null));
		CimEnumeration en = new CimEnumeration("Cim_AggregationKind",null,null,Constants.defaultNameSpacePath,DataType.STRING,values);
		assertNotNull(en);
		assertEquals("Enumeration Cim_AggregationKind : String {\n	none,\n	bag,\n	indexed\n};\n",en.toMOF());
		// create the qualifierType
		DataValue v = new DataValue(DataType.ENUMERATIONVALUE,en.getValue("none"));
		assertEquals("none",v.toString());
		Vector<Scope> scopes = new Vector<Scope>();
		scopes.add(Scope.ANY);
		QualifierType qt = new QualifierType("EnumValuedQualifier",DataType.ENUMERATIONVALUE,v,scopes,null,null,Constants.defaultNameSpacePath);
		assertEquals("Qualifier EnumValuedQualifier : Cim_AggregationKind = none Scope(Any);\n",qt.toMOF());
		// now create the qualifiers
		Qualifier q = new Qualifier(qt);
		assertEquals("EnumValuedQualifier",q.toMOF());	// default value
		q = new Qualifier(qt,new DataValue(DataType.ENUMERATIONVALUE,en.getValue("none"))); // default value
		assertEquals("EnumValuedQualifier(none)",q.toMOF());
		q = new Qualifier(qt,new DataValue(DataType.ENUMERATIONVALUE,en.getValue("indexed")));	// some other value
		assertEquals("EnumValuedQualifier(indexed)",q.toMOF());
		
		// TODO: Note that en.getValue() will return null, which is an acceptable DataValue constructor argument for any type
		// Is this behavior acceptable? QualifierType only has the default value in it, so cannot check that null is not
		// a valid argument
		q = new Qualifier(qt,new DataValue(DataType.ENUMERATIONVALUE,en.getValue("illegal")));		// illegal value
		assertEquals("EnumValuedQualifier(null)",q.toMOF());

	}
	
}
