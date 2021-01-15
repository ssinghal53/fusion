/**
 * Copyright 2021 Sharad Singhal. All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
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
 * Created Jan 14, 2021 by sharad
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.utils.Java2Cim;

/**
 * Class to test java bindings for different Cim Elements
 * @author Sharad Singhal
 */
public class JavaBindingTest {
	static InMemoryCache cache = new InMemoryCache();

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("JavaBindingTest ");
		for(Class<?> cls : new Class<?>[] {EnumBindingClass.class}) {
			NamedElement element = Java2Cim.getModelForClass(cls, cache);
			// System.out.println(element.getObjectPath()+"\n"+element.toMOF());
		}
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#bind()}.
	 */
	@Test
	public void testBindEnumeration() {
		CimEnumeration ce = (CimEnumeration) cache.get(new ObjectPath("/enumeration/aifusion:aifusion_enumbindingclass"));
		assertNotNull(ce);
		Class<?> javaEnum = ce.bind();
		assertEquals(EnumBindingClass.class,javaEnum);
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumerationValue#bind()}.
	 */
	@Test
	public void testBindEnumerationValue() {
		CimEnumeration ce = (CimEnumeration) cache.get(new ObjectPath("/enumeration/aifusion:aifusion_enumbindingclass"));
		assertNotNull(ce);
		Class<?> javaEnum = ce.bind();		
		for(String s : ce.getKeys()) {
			EnumerationValue v = ce.getValue(s);
			assertNotNull(v);
			try {
				Object expect = null;
				for(Object o : javaEnum.getEnumConstants()) {
					if(s.equals(o.toString())) {
						expect = o;
						break;
					}
				}
				assertEquals(expect,v.bind());
			} catch (Exception e) {
				e.printStackTrace();
				fail();
			}
		}
	}

}
