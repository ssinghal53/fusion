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

import java.util.HashMap;
import java.util.HashSet;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Sharad Singhal
 *
 */
public class ElementTypeTest {
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("ElementType ");
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
	
	@Test
	public final void test() {
		// ensure we have the correct number of elements specified
		assertEquals(16,ElementType.values().length);
	}
	
	/**
	 * Test for method {@link net.aifusion.metamodel.ElementType#toMOF()}.
	 */
	@Test
	public final void testToMOF(){
		HashMap<ElementType,String> types = new HashMap<>();
		types.put(ElementType.CLASS, "Class");
		types.put(ElementType.STRUCTURE, "Structure");
		types.put(ElementType.ENUMERATION, "Enumeration");
		types.put(ElementType.INTERFACE, "Interface");
		types.put(ElementType.QUALIFIERTYPE, "Qualifier");
		types.put(ElementType.ASSOCIATION, "Association");
		types.put(ElementType.INDICATION, "Indication");
		for(ElementType t : ElementType.values()){
			if(types.containsKey(t))
				assertEquals(types.get(t),t.toMOF());
			else
				assertEquals("",t.toMOF());
		}
		
	}

	/**
	 * Test for method {@link net.aifusion.metamodel.ElementType#isNamedElement()}.
	 */
	@Test
	public final void testIsNamedElement(){
		HashSet<ElementType> types = new HashSet<>();
		for(ElementType t : new ElementType[]{ElementType.CLASS,ElementType.ENUMERATION,ElementType.STRUCTURE,
				ElementType.INTERFACE,ElementType.QUALIFIERTYPE,ElementType.INSTANCE,ElementType.STRUCTUREVALUE,
				ElementType.ASSOCIATION}){
			types.add(t);
		}
		
		for(ElementType t : ElementType.values()){
			if(types.contains(t) )
				assertTrue(t.isNamedElement());
			else
				assertFalse(t.isNamedElement());
		}
		
	}
}
