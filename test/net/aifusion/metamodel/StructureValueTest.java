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
 * Created Nov 27, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Unit tests for StructureValue
 * @author Sharad Singhal
 */
public class StructureValueTest {
	static CimStructure struct;
	StructureValue sv;
	Map<String,DataValue> propertyValues;
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("StructureValue ");
		CimProperty keyProperty = null;
		Vector<CimProperty> keyProperties = new Vector<CimProperty>();
		Qualifier keyQual = StandardQualifierType.KEY.getQualifier(true, Constants.defaultNameSpacePath);
		Vector<Qualifier> keyQuals = new Vector<Qualifier>();
		keyQuals.add(keyQual);
		keyProperty = new CimProperty("name","kz",DataType.BOOLEAN,new DataValue(true),keyQuals);
		keyProperties.add(keyProperty);
		struct = new CimStructure(ElementType.STRUCTURE,"name",null,null,Constants.defaultNameSpacePath,keyProperties);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		propertyValues = new HashMap<String,DataValue>();
		propertyValues.put("KZ",new DataValue(false));
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

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#toMOF(java.lang.String)}.
	 */
	@Test
	public final void testToMOFString() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, "$alias");
		assertNotNull(v);
		assertEquals(" value of name as $alias {\n \tkz = false;\n }", v.toMOF(" "));
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, "$alias");
		HashMap<String,DataValue> propertyValues2 = new HashMap<String,DataValue>();
		propertyValues2.put("KZ",new DataValue(false));
		StructureValue v2 = StructureValue.createStructureValue(struct, propertyValues2, null);
		assertEquals(v,v2);
		propertyValues2.clear();
		propertyValues2.put("KZ",new DataValue(true));
		v2 = StructureValue.createStructureValue(struct, propertyValues2, null);
		assertNotEquals(v, v2);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#createStructureValue(net.aifusion.metamodel.CimStructure, java.util.Map, java.lang.String)}.
	 */
	@Test
	public final void testCreateStructureValue() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, "$alias");
		assertNotNull(v);
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#getAlias()}.
	 */
	@Test
	public final void testGetAlias() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, "$alias");
		assertNotNull(v);
		assertEquals("$alias",v.getAlias());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#hasAlias()}.
	 */
	@Test
	public final void testHasAlias() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, "$alias");
		assertNotNull(v);
		assertTrue(v.hasAlias());
		v = StructureValue.createStructureValue(struct, propertyValues, null);
		assertNotNull(v);
		assertFalse(v.hasAlias());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#getPropertyValue(java.lang.String)}.
	 */
	@Test
	public final void testGetPropertyValue() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, null);
		assertNotNull(v);
		DataValue value = v.getPropertyValue("kz");
		assertNotNull(value);
		assertEquals(new DataValue(false),value);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#hasProperty(java.lang.String)}.
	 */
	@Test
	public final void testHasProperty() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, null);
		assertNotNull(v);
		assertTrue(v.hasProperty("kz"));
		assertFalse(v.hasProperty("ky"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#getPropertyNames()}.
	 */
	@Test
	public final void testGetPropertyNames() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, null);
		assertNotNull(v);
		Set<String> names = v.getPropertyNames();
		assertEquals(1,names.size());
		assertTrue(names.contains("kz"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StructureValue#toMOF(java.lang.String, boolean)}.
	 */
	@Test
	public final void testToMOFStringBoolean() {
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, "$alias");
		assertNotNull(v);
		assertEquals("value of name as $alias {\n\tkz = false;\n}", v.toMOF("",true));
		assertEquals("value of name {\n\tkz = false;\n}", v.toMOF("",false));
	}
	
	@Test
	public final void testObjectPath(){
		StructureValue v = StructureValue.createStructureValue(struct, propertyValues, "$alias");
		assertNotNull(v);
		ObjectPath path = new ObjectPath(ElementType.STRUCTUREVALUE,"name",Constants.defaultNameSpacePath,propertyValues,"$alias");
		assertEquals(path,v.getObjectPath());
		URI u;
		try {
			u = new URI("http","localhost","/structurevalue/aifusion/name","kz,boolean=false",null);
			assertEquals(u.toURL(),v.getObjectPath().getURL("http","localhost"));
		} catch (URISyntaxException | MalformedURLException e) {
			e.printStackTrace();	// should not happen
			fail();
		}
		// System.out.println(v.getObjectPath().getUUID());
		assertEquals("e96dd3a0-3dfc-31a0-9734-63e37b04f689",v.getObjectPath().getUUID().toString());
	}
}
