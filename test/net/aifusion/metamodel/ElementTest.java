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
 * Created Nov 29, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Sharad Singhal
 *
 */
public class ElementTest {
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Element ");
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
	 * The Element class is abstract, so create a realization of the class for testing
	 * @author Sharad Singhal
	 */
	
	protected class RealizedElement extends Element {

		/**
		 * @param type - element type
		 * @param name - name of the class
		 */
		protected RealizedElement(ElementType type, String name) {
			super(type, name);
		}

		/* (non-Javadoc)
		 * @see net.aifusion.metamodel.Element#toMOF()
		 */
		@Override
		protected String toMOF(String prefix) {
			return prefix+getName();
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Element#Element(net.aifusion.metamodel.ElementType, java.lang.String)}.
	 */
	@Test
	public final void testElement() {
		for(ElementType t : ElementType.values()){
			RealizedElement e = new RealizedElement(t,"Name");
			assertNotNull(e);
		}
		// a null or empty name should throw exception
		try {
			new RealizedElement(ElementType.CLASS,null);
			fail("Should not succeed");
		} catch (ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		try {
			new RealizedElement(ElementType.CLASS,"");
			fail("Should not succeed");
		} catch (ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		// a null elementType should throw exception
		try {
			new RealizedElement(null,"Name");
			fail("Should not succeed");
		} catch (ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Element#getName()}.
	 */
	@Test
	public final void testGetName() {
		for(ElementType t : ElementType.values()){
			RealizedElement e = new RealizedElement(t,"Name");
			assertNotNull(e);
			assertEquals("Name",e.getName());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Element#getLowerCaseName()}.
	 */
	@Test
	public final void testGetLowerCaseName() {
		for(ElementType t : ElementType.values()){
			RealizedElement e = new RealizedElement(t,"Name");
			assertNotNull(e);
			assertEquals("name",e.getLowerCaseName());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Element#getElementType()}.
	 */
	@Test
	public final void testElementType() {
		for(ElementType t : ElementType.values()){
			RealizedElement e = new RealizedElement(t,"Name");
			assertNotNull(e);
			assertEquals(t,e.getElementType());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Element#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		for(ElementType t : ElementType.values()){
			RealizedElement e = new RealizedElement(t,"Name");
			assertNotNull(e);
			assertEquals("Name",e.toMOF());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.Element#toMOF(String)}.
	 */
	@Test
	public final void testToMOFString() {
		for(ElementType t : ElementType.values()){
			RealizedElement e = new RealizedElement(t,"Name");
			assertNotNull(e);
			assertEquals("\tName",e.toMOF("\t"));
		}
	}
	

}
