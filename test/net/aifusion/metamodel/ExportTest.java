/**
 * Copyright 2020 Sharad Singhal, All Rights Reserved
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
 * Created Jan 16, 2020 by Sharad Singhal
 * Last Modified March 13, 2020 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import java.lang.reflect.Method;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test Export
 * @author Sharad Singhal
 *
 */
public class ExportTest {
	private Class<?> cls = Export.class;
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Export ");
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
	public final void testMethods() {
		Method [] methods = cls.getDeclaredMethods();
		assertNotNull(methods);
		assertEquals(8,methods.length);
		return;
	}
	
	
	/**
	 * Test method for {@link net.aifusion.metamodel.Export#name()}.
	 */
	@Test
	public final void testName() {
		try {
			Method m = cls.getDeclaredMethod("name");
			assertNotNull(m);
			String value = (String) m.getDefaultValue();
			assertEquals("",value);
		} catch (NoSuchMethodException e) {
			fail("name() is not declared in Export");
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Export#nameSpace()}.
	 */
	@Test
	public final void testNameSpace() {
		try {
			Method m = cls.getDeclaredMethod("nameSpace");
			assertNotNull(m);
			String value = (String) m.getDefaultValue();
			assertEquals(Constants.defaultLocalPath,value);
		} catch (NoSuchMethodException e) {
			fail("nameSpace() is not declared in Export");
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Export#schema()}.
	 */
	@Test
	public final void testSchema() {
		try {
			Method m = cls.getDeclaredMethod("schema");
			assertNotNull(m);
			String value = (String) m.getDefaultValue();
			assertEquals(Constants.defaultSchema,value);
		} catch (NoSuchMethodException e) {
			fail("schema() is not declared in Export");
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Export#forceClass()}.
	 */
	@Test
	public final void testForceClass() {
		try {
			Method m = cls.getDeclaredMethod("forceClass");
			assertNotNull(m);
			Boolean value = (Boolean) m.getDefaultValue();
			assertEquals(false,value);
		} catch (NoSuchMethodException e) {
			fail("forceClass() is not declared in Export");
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Export#refClass()}.
	 */
	@Test
	public final void testRefClass() {
		try {
			Method m = cls.getDeclaredMethod("refClass");
			assertNotNull(m);
			String value = (String) m.getDefaultValue();
			assertEquals("",value);
		} catch (NoSuchMethodException e) {
			fail("refClass() is not declared in Export");
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Export#qualifiers()}.
	 */
	@Test
	public final void testQualifiers() {
		try {
			Method m = cls.getDeclaredMethod("qualifiers");
			assertNotNull(m);
			String value = (String) m.getDefaultValue();
			assertEquals("",value);
		} catch (NoSuchMethodException e) {
			fail("qualifiers() is not declared in Export");
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Export#defaultValue()}.
	 */
	@Test
	public final void testDefaultValue() {
		try {
			Method m = cls.getDeclaredMethod("defaultValue");
			assertNotNull(m);
			String value = (String) m.getDefaultValue();
			assertEquals("",value);
		} catch (NoSuchMethodException e) {
			fail("defaultValue() is not declared in Export");
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.Export#version()}.
	 */
	@Test
	public final void testVersion() {
		try {
			Method m = cls.getDeclaredMethod("version");
			assertNotNull(m);
			String value = (String) m.getDefaultValue();
			assertEquals(Constants.defaultVersion,value);
		} catch (NoSuchMethodException e) {
			fail("version() is not declared in Export");
		}
		return;
	}

}
