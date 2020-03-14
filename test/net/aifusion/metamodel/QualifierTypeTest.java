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
 * Tests to check Qualifier types
 * @author Sharad Singhal
 */
public class QualifierTypeTest {
	private static String qName = "Schema_Qualifier";
	private static DataType type = DataType.STRING_ARRAY;
	private static DataValue value = new DataValue(DataType.STRING_ARRAY,new String[]{"a","b"});
	private static DataValue complexValue = new DataValue(new EnumerationValue("none","Cim_AggregationKind",null,null));
	private static DataType complexType = DataType.ENUMERATIONVALUE;
	private static Vector<Scope> scopes = new Vector<Scope>();
	private static Policy policy = Policy.DISABLEOVERRIDE;
	private static NameSpacePath path = Constants.defaultNameSpacePath;
	
	static {
		scopes.add(Scope.CLASS);
	}
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("QualifierType ");
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
	 * Test method for {@link net.aifusion.metamodel.QualifierType#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		// test normal types
		String mof = "Qualifier Schema_Qualifier : String[] = { \"a\", \"b\" } Scope(Class) Policy(DisableOverride);\n";
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		assertEquals(mof,q.toMOF());
		
		// test enum types
		mof = "Qualifier Schema_Qualifier : Cim_AggregationKind = none Scope(Class) Policy(DisableOverride);\n";
		DataValue v = new DataValue(new EnumerationValue("none","Cim_AggregationKind",null,null));
		assertNotNull(v);
		assertEquals(DataType.ENUMERATIONVALUE,v.getType());
		q = new QualifierType(qName,DataType.ENUMERATIONVALUE,v,scopes,policy,null,path);
		assertNotNull(q);
		assertEquals(mof,q.toMOF());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#QualifierType(java.lang.String, net.aifusion.metamodel.DataType, net.aifusion.metamodel.DataValue, java.util.List, net.aifusion.metamodel.Policy, java.util.List, net.aifusion.metamodel.NameSpacePath)}.
	 */
	@Test
	public final void testQualifierType() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		q = new QualifierType(qName,complexType,complexValue,scopes,policy,null,path);
		assertNotNull(q);
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#getDataType()}.
	 */
	@Test
	public final void testGetDatatype() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertEquals(type,q.getDataType());
		q = new QualifierType(qName,complexType,complexValue,scopes,policy,null,path);
		assertEquals(complexType,q.getDataType());
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#hasDefaultValue()}.
	 */
	@Test
	public final void testHasDefaultValue() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		assertTrue(q.hasDefaultValue());
		
		q = new QualifierType(qName,type,null,scopes,policy,null,path);
		assertFalse(q.hasDefaultValue());
		
		q = new QualifierType(qName,complexType,complexValue,scopes,policy,null,path);
		assertTrue(q.hasDefaultValue());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#getDefaultValue()}.
	 */
	@Test
	public final void testDefaultValue() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		assertTrue(q.hasDefaultValue());
		assertEquals(value,q.getDefaultValue());
		q = new QualifierType(qName,complexType,complexValue,scopes,policy,null,path);
		assertTrue(q.hasDefaultValue());
		assertEquals(complexValue,q.getDefaultValue());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#hasScope(net.aifusion.metamodel.Scope)}.
	 */
	@Test
	public final void testHasScope() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		assertTrue(q.hasScope(Scope.CLASS));
		assertFalse(q.hasScope(Scope.PARAMETER));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#appliesTo(net.aifusion.metamodel.ElementType)}.
	 */
	@Test
	public final void testAppliesTo() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		assertTrue(q.appliesTo(ElementType.CLASS));
		assertFalse(q.appliesTo(ElementType.STRUCTURE));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#isRestricted()}.
	 */
	@Test
	public final void testIsRestricted() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		assertFalse(q.isRestricted());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifierType#isOverridable()}.
	 */
	@Test
	public final void testIsOverridable() {
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertNotNull(q);
		assertFalse(q.isOverridable());
	}
	
	@Test
	public final void testEquals(){
		QualifierType q = new QualifierType(qName,type,value,scopes,policy,null,path);
		QualifierType q2 = new QualifierType(qName,type,value,scopes,policy,null,path);
		assertEquals(q,q2);
		
		// note that Policy.ENABLEOVERRIDE is the same as not giving it
		q = new QualifierType(qName,type,value,scopes,Policy.ENABLEOVERRIDE,null,path);
		assertEquals("Qualifier Schema_Qualifier : String[] = { \"a\", \"b\" } Scope(Class) Policy(EnableOverride);\n",q.toMOF());
		q2 = new QualifierType(qName,type,value,scopes,null,null,path);
		assertEquals("Qualifier Schema_Qualifier : String[] = { \"a\", \"b\" } Scope(Class);\n",q2.toMOF());
		assertEquals(q,q2);
		
		q = new QualifierType(qName,complexType,complexValue,scopes,null,null,path);
		q2 = new QualifierType(qName,complexType,complexValue,scopes,Policy.ENABLEOVERRIDE,null,path);
		assertEquals(q,q2);
	}

}
