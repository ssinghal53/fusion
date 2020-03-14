/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Feb 16, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests for Qualified Element
 * @author Sharad Singhal
 */
public class QualifiedElementTest {
	// the DESCRIPTION qualifier applies to all qualified elements
	private static Qualifier description = StandardQualifierType.DESCRIPTION.getQualifier("Description",Constants.defaultNameSpacePath);
	// the IN qualifier applies only to Parameters
	private static Qualifier in = StandardQualifierType.IN.getQualifier(false, Constants.defaultNameSpacePath);
	// the KEY qualifier cannot be overridden
	private static Qualifier key = StandardQualifierType.KEY.getQualifier();
	// The ABSTRACT qualifier is Restricted
	private static Qualifier abstr = StandardQualifierType.ABSTRACT.getQualifier(true, Constants.defaultNameSpacePath);
	// COUNTER is overridable, and propagates
	private static Qualifier counter = StandardQualifierType.COUNTER.getQualifier(true, Constants.defaultNameSpacePath);
	
	/**
	 * RealizedElement realizes the Qualified Element
	 * @author Sharad Singhal
	 */
	protected class RealizedElement extends QualifiedElement {
		protected RealizedElement(ElementType elementType, String name, List<Qualifier> qualifiers) {
			super(elementType, name, qualifiers);
		}
	}
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("QualifiedElement ");
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
	 * Test method for {@link net.aifusion.metamodel.QualifiedElement#toMOF(java.lang.String)}.
	 */
	@Test
	public final void testToMOFString() {
		Vector<Qualifier>sQuals = new Vector<Qualifier>();
		sQuals.add(description);
		sQuals.add(key);
		RealizedElement s = new RealizedElement(ElementType.PROPERTY,"SuperName",sQuals);
		assertEquals("\t[Description(\"Description\"), Key]",s.toMOF("\t"));
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifiedElement#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		Vector<Qualifier>sQuals = new Vector<Qualifier>();
		Vector<Qualifier>sxQuals = new Vector<Qualifier>();
		sQuals.add(description);
		sxQuals.add(description);
		
		RealizedElement s = new RealizedElement(ElementType.PROPERTY,"SuperName",sQuals);
		RealizedElement s1 = new RealizedElement(ElementType.PROPERTY,"SuperName",sxQuals);
		assertEquals(s,s1);
		RealizedElement s2 = new RealizedElement(ElementType.PROPERTY,"OtherName",sxQuals);
		assertNotEquals(s,s2);	// name is different
		sQuals.add(counter);
		s2 =  new RealizedElement(ElementType.PROPERTY,"SuperName",sxQuals);
		assertNotEquals(s,s2);	// qualifiers are different
		return;
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifiedElement#QualifiedElement(net.aifusion.metamodel.ElementType, java.lang.String, net.aifusion.metamodel.QualifiedElement, java.util.List)}.
	 */
	@Test
	public final void testQualifiedElement() {
		// validate qualifiers are only allowed on proper types
		Vector<Qualifier> quals1 = new Vector<Qualifier>();
		// DESCRIPTION is valid for all types
		quals1.add(description);
		for(ElementType t : ElementType.values()){
			RealizedElement e = new RealizedElement(t,"name",quals1);
			assertNotNull(e);
		}
		// IN is valid only for parameters
		quals1.add(in);
		for(ElementType t : ElementType.values()){
			try {
				RealizedElement e = new RealizedElement(t,"name",quals1);
				assertNotNull(e);
				assertEquals(t,ElementType.PARAMETER);
			} catch(ModelException ex){
				assertEquals(4,ex.getReason().getCode());
				assertNotEquals(t, ElementType.PARAMETER);
				
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifiedElement#getQualifierValue(java.lang.String)}.
	 */
	@Test
	public final void testGetQualifierValue() {
		Vector<Qualifier> sQuals = new Vector<Qualifier>();
		sQuals.add(abstr);
		sQuals.add(description);
		sQuals.add(StandardQualifierType.OCL.getQualifier(new String[]{ "ocl" }, Constants.defaultNameSpacePath));
		
		RealizedElement s = new RealizedElement(ElementType.CLASS,"SuperType",sQuals);
		assertNotNull(s);
		
		// defined qualifiers
		DataValue v = s.getQualifierValue("Description");
		assertNotNull(v);
		assertEquals("Description",v.getValue());
		v = s.getQualifierValue("Abstract");
		assertNotNull(v);
		assertEquals(true,v.getValue());
		
		// standard qualifiers (note that abstract does not propagate, thus this will return the standard value
		v = s.getQualifierValue("Terminal");
		assertNotNull(v);
		assertEquals(false,v.getValue());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.QualifiedElement#getQualifiers()}.
	 */
	@Test
	public final void testGetQualifiers() {
		Vector<Qualifier> sQuals = new Vector<Qualifier>();
		Vector<Qualifier> cQuals = new Vector<Qualifier>();
		sQuals.add(abstr);
		sQuals.add(description);
		cQuals.add(StandardQualifierType.DESCRIPTION.getQualifier("Description1", Constants.defaultNameSpacePath));
		sQuals.add(StandardQualifierType.OCL.getQualifier(new String[]{ "ocl" }, Constants.defaultNameSpacePath));
		
		RealizedElement s = new RealizedElement(ElementType.CLASS,"SuperType",sQuals);
		assertNotNull(s);
		RealizedElement c = new RealizedElement(ElementType.CLASS,"ChildType",cQuals);
		assertNotNull(c);
		
		assertEquals(3,s.getQualifiers().size());
		assertEquals(1,c.getQualifiers().size());
	}

}
