/**
 * Copyright 2017 Sharad Singhal, All Rights Reserved
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
 * Created Dec 31, 2017 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import java.net.URI;
import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test NamedElement
 * @author Sharad Singhal
 *
 */
public class NamedElementTest implements CimListener {
	/**
	 * NamedElement is abstract. Create a realizable class
	 */
	private class ReElement extends NamedElement {
		protected ReElement(ElementType type, String name, NamedElement superType, List<Qualifier> qualifiers, NameSpacePath path) {
			super(type, name, superType, qualifiers, path, null, null);
		}
	}

	String eventResult = null;
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("NamedElement ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}
	
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		eventResult = null;
	}
	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#equals(java.lang.Object)}.
	 */
	@Test
	public void testEqualsObject() {
		ReElement re1 = new ReElement(ElementType.CLASS,"MyName",null,null,Constants.defaultNameSpacePath);
		assertNotNull(re1);
		ReElement re2 = new ReElement(ElementType.CLASS,"MyName",null,null,Constants.defaultNameSpacePath);
		assertNotNull(re2);
		assertEquals(re1,re2);
		// change the name
		re2 = new ReElement(ElementType.CLASS,"OtherName",null,null,Constants.defaultNameSpacePath);
		assertNotEquals(re1,re2);
		// add a superType
		re2 = new ReElement(ElementType.CLASS,"OtherName",re1,null,Constants.defaultNameSpacePath);
		assertNotEquals(re1,re2);
		// add some qualifiers
		Vector<Qualifier> quals = new Vector<Qualifier>();
		quals.add(StandardQualifierType.ABSTRACT.getQualifier());
		re2 = new ReElement(ElementType.CLASS,"OtherName",null,quals,Constants.defaultNameSpacePath);
		assertNotEquals(re1,re2);
		// change the namespace
		re2 = new ReElement(ElementType.CLASS,"MyName",null,null,new NameSpacePath("http","localhost","/root"));
		assertNotEquals(re1,re2);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#getQualifierValue(java.lang.String)}.
	 */
	@Test
	public void testGetQualifierValue() {
		Vector<Qualifier> pq = new Vector<Qualifier>();
		pq.add(StandardQualifierType.DESCRIPTION.getQualifier("Parent Description", Constants.defaultNameSpacePath));
		Vector<Qualifier> cq = new Vector<Qualifier>();
		cq.add(StandardQualifierType.DESCRIPTION.getQualifier("Child Description", Constants.defaultNameSpacePath));

		ReElement su = new ReElement(ElementType.CLASS,"parent",null,pq,Constants.defaultNameSpacePath);
		assertNotNull(su);
		ReElement ch = new ReElement(ElementType.CLASS,"child",su,cq,Constants.defaultNameSpacePath);
		assertNotNull(ch);
		
		assertEquals(new DataValue("Child Description"),ch.getQualifierValue("Description"));
		assertEquals(new DataValue(false),ch.getQualifierValue("Abstract"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#hasQualifier(java.lang.String)}.
	 */
	@Test
	public void testHasQualifier() {
		Vector<Qualifier> pq = new Vector<Qualifier>();
		pq.add(StandardQualifierType.DESCRIPTION.getQualifier("Parent Description", Constants.defaultNameSpacePath));
		Vector<Qualifier> cq = new Vector<Qualifier>();
		cq.add(StandardQualifierType.DESCRIPTION.getQualifier("Child Description", Constants.defaultNameSpacePath));

		ReElement su = new ReElement(ElementType.CLASS,"parent",null,pq,Constants.defaultNameSpacePath);
		assertNotNull(su);
		ReElement ch = new ReElement(ElementType.CLASS,"child",su,cq,Constants.defaultNameSpacePath);
		assertNotNull(ch);
		
		assertTrue(ch.hasQualifier("Description"));
		assertFalse(ch.hasQualifier("Abstract"));
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#NamedElement(net.aifusion.metamodel.ElementType, java.lang.String, net.aifusion.metamodel.NamedElement, java.util.List, net.aifusion.metamodel.NameSpacePath, java.util.Map, String)}.
	 */
	@Test
	public void testNamedElement() {
		Vector<Qualifier> pq = new Vector<Qualifier>();
		pq.add(StandardQualifierType.DESCRIPTION.getQualifier("Parent Description", Constants.defaultNameSpacePath));
		Vector<Qualifier> cq = new Vector<Qualifier>();
		cq.add(StandardQualifierType.DESCRIPTION.getQualifier("Child Description", Constants.defaultNameSpacePath));

		ReElement su = new ReElement(ElementType.CLASS,"parent",null,pq,Constants.defaultNameSpacePath);
		assertNotNull(su);
		ReElement ch = new ReElement(ElementType.CLASS,"child",su,cq,Constants.defaultNameSpacePath);
		assertNotNull(ch);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#hasListener(CimEventType, CimListener)}.
	 * Test method for {@link net.aifusion.metamodel.NamedElement#addListener(CimEventType, CimListener)}.
	 * Test method for {@link net.aifusion.metamodel.NamedElement#removeListener(CimEventType, CimListener)}.
	 * Test method for {@link net.aifusion.metamodel.NamedElement#generateEvent(CimEvent)}.
	 */
	@Test
	public void testGenerateEvent() {
		ReElement re = new ReElement(ElementType.CLASS,"MyName",null,null,Constants.defaultNameSpacePath);
		assertFalse(re.hasListener(CimEventType.UPDATED, null));
		re.addListener(CimEventType.UPDATED, this);
		assertTrue(re.hasListener(CimEventType.UPDATED, this));
		CimIndication indication = new CimIndication(CimEventType.UPDATED,re,null);
		assertNull(eventResult);
		re.generateEvent(indication);
		assertEquals("CimIndication {\n\tType = \"UPDATED\";\n};\n",eventResult);
		re.removeListener(CimEventType.UPDATED, this);
		assertFalse(re.hasListener(CimEventType.UPDATED, null));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#getObjectPath()}.
	 */
	@Test
	public void testGetObjectPath() {
		ReElement re = new ReElement(ElementType.CLASS,"MyName",null,null,Constants.defaultNameSpacePath);
		assertNotNull(re);
		ObjectPath path = new ObjectPath(ElementType.CLASS,"MyName",Constants.defaultNameSpacePath, null, null);
		assertEquals(path,re.getObjectPath());
		re = new ReElement(ElementType.CLASS,"MyName",null,null,new NameSpacePath("http","localhost","/root"));
		assertEquals("http://localhost/class/root:MyName",re.getObjectPath().toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#getNameSpacePath()}.
	 */
	@Test
	public void testGetNameSpacePath() {
		ReElement re = new ReElement(ElementType.CLASS,"MyName",null,null,Constants.defaultNameSpacePath);
		assertNotNull(re);
		assertEquals(Constants.defaultNameSpacePath,re.getNameSpacePath());
		re = new ReElement(ElementType.CLASS,"MyName",null,null,new NameSpacePath("http","localhost","/root"));
		assertEquals("http://localhost/root",re.getNameSpacePath().toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#getSuperType()}.
	 */
	@Test
	public void testGetSuperType() {
		ReElement su = new ReElement(ElementType.CLASS,"parent",null,null,Constants.defaultNameSpacePath);
		assertNotNull(su);
		ReElement ch = new ReElement(ElementType.CLASS,"child",su,null,Constants.defaultNameSpacePath);
		assertNotNull(ch);
		assertEquals(su,ch.getSuperType());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#isSubTypeOf(java.lang.String)}.
	 */
	@Test
	public void testIsSubTypeOf() {
		ReElement su = new ReElement(ElementType.CLASS,"parent",null,null,Constants.defaultNameSpacePath);
		assertNotNull(su);
		ReElement ch = new ReElement(ElementType.CLASS,"child",su,null,Constants.defaultNameSpacePath);
		assertNotNull(ch);
		assertTrue(ch.isSubTypeOf("parent"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#getFullName()}.
	 */
	@Test
	public void testGetFullName() {
		ReElement su = new ReElement(ElementType.CLASS,"parent",null,null,Constants.defaultNameSpacePath);
		assertNotNull(su);
		ReElement ch = new ReElement(ElementType.CLASS,"child",su,null,Constants.defaultNameSpacePath);
		assertNotNull(ch);
		assertEquals("parent",su.getFullName());
		assertEquals("parent.child",ch.getFullName());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NamedElement#getAllQualifiers()}.
	 */
	@Test
	public void testGetAllQualifiers() {
		Vector<Qualifier> pq = new Vector<Qualifier>();
		pq.add(StandardQualifierType.DESCRIPTION.getQualifier("Parent Description", Constants.defaultNameSpacePath));
		Vector<Qualifier> cq = new Vector<Qualifier>();
		cq.add(StandardQualifierType.DESCRIPTION.getQualifier("Child Description", Constants.defaultNameSpacePath));

		ReElement su = new ReElement(ElementType.CLASS,"parent",null,pq,Constants.defaultNameSpacePath);
		assertNotNull(su);
		ReElement ch = new ReElement(ElementType.CLASS,"child",su,cq,Constants.defaultNameSpacePath);
		assertNotNull(ch);
		
		List<Qualifier> allQualifiers = ch.getAllQualifiers();
		assertEquals(2,allQualifiers.size());
		for(Qualifier q : allQualifiers){
			assertEquals("Description",q.getName());
		}
		
	}

	@Override
	public void notify(CimEvent event) {
		eventResult = event.toString();
		return;
	}

	@Override
	public URI getURI() {
		return null;
	}

}
