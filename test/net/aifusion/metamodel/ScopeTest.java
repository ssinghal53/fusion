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
public class ScopeTest {
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Scope ");
		assertEquals(16,Scope.values().length);
		assertEquals(16,ElementType.values().length);
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
	 * Test method for {@link net.aifusion.metamodel.Scope#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		for(Scope s : Scope.values()){
			assertEquals(s.toString(),s.toMOF().toUpperCase());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.Scope#appliesTo(ElementType)}
	 */
	@Test
	public final void testAppliesTo(){
		for(Scope s : Scope.values()){
			for(ElementType type : ElementType.values()){
				// System.out.println("Check scope "+s.toString()+" against "+type.toString()+" : "+s.appliesTo(type));
				if(s == Scope.ANY){
					assertTrue(s.appliesTo(type));
				} else if(s.toString().equals(type.toString())){
					assertTrue(s.appliesTo(type));	
				} else {
					assertFalse(s.appliesTo(type));
				}
			}
		}
	}

}
