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
 * Created Dec 7, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests to check for ObjectPath
 * @author Sharad Singhal
 */
public class ObjectPathTest {
	private static ElementType elementTypes [] = {
			ElementType.CLASS, ElementType.CLASS,ElementType.INSTANCE,ElementType.INSTANCE,
			ElementType.STRUCTURE, ElementType.STRUCTUREVALUE, ElementType.ENUMERATION,
			ElementType.QUALIFIERTYPE, ElementType.INTERFACE
	};
	private static ObjectPath paths [] = null;
	private static String pathNames [] = {
		"http://user:pass@localhost:80/root:cim_class",						// class (old form)
		"http://user:pass@localhost:80/class/root:cim_class",				// class (new form)
		"http://user:pass@localhost:80/root:cim_class.k1=true",				// instance (old form)
		"http://user:pass@localhost:80/instance/root:cim_class.k1=true",	// instance (new form)
		"http://user:pass@localhost:80/structure/root:cim_class",			// structure
		"http://user:pass@localhost:80/structurevalue/root:cim_class.k1=true",	// structure value
		"http://user:pass@localhost:80/enumeration/root:cim_class",			// enumeration
		"http://user:pass@localhost:80/qualifiertype/root:cim_class",		// qualifier type
		"http://user:pass@localhost:80/interface/root:cim_class"			// interface
	};
	
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

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("ObjectPath ");
		paths = new ObjectPath[pathNames.length];
		for(int i = 0; i < pathNames.length; i++){
			paths[i] = new ObjectPath(pathNames[i]);
			assertNotNull(paths[i]);
		}
		assertEquals(elementTypes.length,paths.length);
	}
	
	/**
	 * Test to check name forms for class/instance (old forms) and others (new forms)
	 */
	@Test
	public final void testNameForm() {
		for(int i = 0; i < paths.length; i++) {
			assertEquals(elementTypes[i],paths[i].getElementType());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#hashCode()}.
	 */
	@Test
	public final void testHashCode() {
		// test different order of keys and different case on class names
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.K2=true,k1=false");
		ObjectPath p2 = new ObjectPath("http://user:pass@localhost:80/root:CIM_class.k1=false,k2=true");
		assertEquals(p.hashCode(),p2.hashCode());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#ObjectPath(ElementType, String, NameSpacePath, java.util.Map, String)}.
	 */
	@Test
	public final void testObjectPathElementTypeStringNameSpacePathMap() {
		ObjectPath p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), null, null);
		assertNotNull(p);
		HashMap<String,DataValue> keyValues = new HashMap<String,DataValue>();
		keyValues.put("k1", new DataValue(true));
		p = new ObjectPath(ElementType.INSTANCE,"cim_class",new NameSpacePath("/cimv3"), keyValues, null);
		assertNotNull(p);
		assertEquals("/instance/cimv3:cim_class.k1=true",p.toString());
		keyValues.put("k2",new DataValue(paths[2]));
		p = new ObjectPath(ElementType.INSTANCE,"cim_class",new NameSpacePath("/cimv3"), keyValues, null);
		assertNotNull(p);
		assertEquals("/instance/cimv3:cim_class.k1=true,k2=\"http://user:pass@localhost:80/instance/root:cim_class.k1=true\"",p.toString());
		try {
			p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), keyValues, null);
			fail("QualifierType with keyValues succeeded");
		} catch (ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		try {
			keyValues.put("k2", new DataValue(new Boolean []{true,false}));
			new ObjectPath(ElementType.INSTANCE,"cim_class",new NameSpacePath("/cimv3"), keyValues, null);
			fail("Instance with array keyValues succeeded");
		} catch (ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		try {
			keyValues.put("k2",new DataValue(DataType.ENUMERATIONVALUE,null));
			p = new ObjectPath(ElementType.INSTANCE,"cim_class",new NameSpacePath("/cimv3"), keyValues, null);
			fail("Instance with non-primitive/non-reference keyValues succeeded");
		} catch (ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
	}
	
	@Test
	public final void testObjectPathStringString(){
		// test an alias
		ObjectPath p = new ObjectPath(ElementType.INSTANCE,"Cim_Class", "$abc");
		assertNotNull(p);
		assertEquals("$abc",p.toString());
		assertEquals("Cim_Class",p.getName());
		assertEquals(ElementType.INSTANCE,p.getElementType());
		
		p = new ObjectPath(ElementType.STRUCTUREVALUE,"Cim_Structure", "$abc");
		assertNotNull(p);
		assertEquals("$abc",p.toString());
		assertEquals("Cim_Structure",p.getName());
		assertEquals(ElementType.STRUCTUREVALUE,p.getElementType());
		// System.out.println(p.getLocalPath());
		
		// System.out.println(p.getURL("http", "localhost"));
		// System.out.println(p.getUUID());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#ObjectPath(java.lang.String)}.
	 */
	@Test
	public final void testObjectPathString() {
		// test an complete path
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.K2=true,k1=false");
		assertNotNull(p);
		assertEquals("http://user:pass@localhost:80/instance/root:cim_class.k1=false,k2=true",p.toString());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#getElementType()}.
	 */
	@Test
	public final void testGetElementType() {
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.k1=true");
		assertEquals(ElementType.INSTANCE,p.getElementType());
		
		p = new ObjectPath("http://user:pass@localhost:80/root:cim_class");
		assertEquals(ElementType.CLASS,p.getElementType());
		
		p = new ObjectPath(ElementType.INSTANCE,"Cim_Class", "$abc");
		assertEquals(ElementType.INSTANCE,p.getElementType());
		
		p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), null, null);
		assertEquals(ElementType.QUALIFIERTYPE,p.getElementType());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#getName()}.
	 */
	@Test
	public final void testGetName() {
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.k1=true");
		
		assertEquals("cim_class",p.getName());
		p = new ObjectPath("http://user:pass@localhost:80/root:cim_class");
		assertEquals("cim_class",p.getName());
		
		p = new ObjectPath(ElementType.INSTANCE,"cim_class", "$abc");
		assertEquals("cim_class",p.getName());
		p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), null, null);
		assertEquals("Abstract",p.getName());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#getAlias()}.
	 */
	@Test
	public final void testGetAlias() {
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.k1=true");
		assertEquals(null,p.getAlias());
		
		p = new ObjectPath("http://user:pass@localhost:80/root:cim_class");
		assertEquals(null,p.getAlias());
		
		p = new ObjectPath(ElementType.INSTANCE,"cim_class", "$abc");
		assertEquals("$abc",p.getAlias());
		p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), null, null);
		assertEquals(null,p.getAlias());
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#getLowerCaseName()}.
	 */
	@Test
	public final void testGetLowerCaseName() {
		// full path
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:CIM_class.k1=true");
		assertEquals("cim_class",p.getLowerCaseName());
		// alias
		p = new ObjectPath(ElementType.INSTANCE,"Cim_Class", "$abc");
		assertEquals("cim_class",p.getLowerCaseName());
		// elementType, objectName, namespacepath
		p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), null, null);
		assertEquals("abstract",p.getLowerCaseName());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#toString()}.
	 */
	@Test
	public final void testToString() {
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:CIM_class.k2=true,k1=false");

		assertEquals("http://user:pass@localhost:80/instance/root:CIM_class.k1=false,k2=true",p.toString());

		p = new ObjectPath(ElementType.INSTANCE,"Cim_Class", "$abc");
		assertEquals("$abc",p.toString());
		
		p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), null, null);
		assertEquals("/qualifiertype/cimv3:Abstract",p.toString());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.K2=true,k1=false");
		ObjectPath p2 = new ObjectPath("http://user:pass@localhost:80/root:CIM_class.k1=false,k2=true");
		assertEquals(p,p2);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#getLocalPath()}.
	 */
	@Test
	public final void testGetLocalPath() {
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.k1=true");
		assertEquals("/root",p.getLocalPath());
	}
	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#getNameSpacePath()}.
	 */
	@Test
	public final void testGetNameSpacePath(){
		NameSpacePath p = new NameSpacePath("http://user:pass@localhost:80/root");
		ObjectPath o = new ObjectPath("http://user:pass@localhost:80/root:cim_class.k1=true");
		assertEquals(p,o.getNameSpacePath());
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#getUUID()}.
	 */
	@Test
	public final void testGetUUID(){
		// note that currently the UUID value depends on the entire namespace, not just the local path
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.K2=true,k1=false");
		assertNotNull(p);
		assertEquals("63e5c152-455e-3b42-b14b-ce5c19f69553",p.getUUID().toString());
		p = new ObjectPath("/root:cim_class.K2=true,k1=false");
		assertEquals("63e5c152-455e-3b42-b14b-ce5c19f69553",p.getUUID().toString());
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.ObjectPath#toURL()}.
	 */
	@Test
	public final void testToURL(){
		ObjectPath p = new ObjectPath("http://user:pass@localhost:80/root:cim_class.K2=true,k1=false");
		assertNotNull(p);
		URL uri = p.toURL();
		assertNotNull(uri);
		assertEquals("http://user:pass@localhost:80/instance/root/cim_class?k1,boolean=false&k2,boolean=true",uri.toString());
		
		p = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",new NameSpacePath("/cimv3"), null, null);
		assertEquals("http://localhost:8085/qualifiertype/cimv3/Abstract",p.toURL().toString());
		
	}
	
	@Test
	public final void testObjectPathURI(){
		try {
			ObjectPath p0 = new ObjectPath("http://user:pass@localhost:80/root:cim_class.K2=true,k1=false");
			ObjectPath p = new ObjectPath(new URI("http://user:pass@localhost:80/instance/root/cim_class?k1,boolean=false&k2,boolean=true"));
			assertEquals(p0,p);
			// test with string keys
			p0 = new ObjectPath("http://user:pass@localhost:80/root:cim_class.k1=\"foobar\"");
			p = new ObjectPath(new URI("http://user:pass@localhost:80/instance/root/cim_class?k1,string=%22foobar%22"));
			assertEquals(p0,p);
			
		} catch (URISyntaxException e) {
			fail("Unable to create objectPath");
		}
	}
	
	@Test
	public final void testGetKeyValue() {
		ObjectPath p0 = new ObjectPath("http://user:pass@localhost:80/root:cim_class.K2=true,k1=false");
		assertNull(p0.getKeyValue("nonExistentKey"));
		assertEquals(new DataValue(true),p0.getKeyValue("k2"));
	}

}
