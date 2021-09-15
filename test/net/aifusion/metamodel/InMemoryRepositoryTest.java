/**
 * Copyright 2016, Sharad Singhal, All Rights Reserved
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
 * Created Jan 17, 2016 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests for InMemoryRepository
 * @author Sharad Singhal
 */
public class InMemoryRepositoryTest {
	static boolean verbose = false;
	private InMemoryRepository repository = null;
	static CimStructure struct;							// Test_Struct
	static StructureValue structValue, structValue2;	// Test_Struct value
	
	static CimClass cimClass;							// Test_Struct.Test_Class
	static CimInstance instance, instance2;				// Test_Class instance
	
	static CimEnumeration enumeration;					// Enumeration with name "enum", that contains a single enumerationValue called "name"
	static EnumerationValue enumValue, enumValue2;		// enumeration value (name)
	
	private static String [] mofDefinition = {
			"Enumeration Disk_Enum : String { SSD, HD, DVD };\n",
			"Enumeration Server_Enum : String { db, app, web };\n",
			"[Version(\"1.0.0\")] Structure Test_Struct {[Write] STRING MountPoint; [Key] STRING ID; Real64 UsedCapacity; Disk_Enum type;};",
			"Class Test_Disk : Test_Struct { BOOLEAN dismount(); BOOLEAN mount();};\n",
			"Class Test_Computer {[Write] STRING HostName; [Key] STRING address; Boolean isPublic; Test_Disk ref disk;};",
			"Class App_Server : Test_Computer {Server_Enum type; String software[]; DateTime installDate;};",
			"Class Test_Application : App_Server {String appName;};",
			

			"instance of Test_Disk as $disk1 { ID = \"id1\"; MountPoint = \"sup1\"; UsedCapacity = 32;};",
			"instance of Test_Disk as $disk2 { ID = \"id2\"; UsedCapacity = 33;};",
			"instance of Test_Disk as $disk3 { ID = \"id3\";};",

			"instance of Test_Computer as $c1 { address = \"192.168.1.1\"; disk = $disk1;};",
			"instance of App_Server as $c2 { address = \"192.168.1.2\"; isPublic = true; disk = $disk1; type = \"web\"; software = {\"apache\", \"tomcat\"}; installDate = \"20120910123400.000000+740\";};",
			"instance of Test_Application as $c3 { address = \"192.168.1.3\";  appName = \"myApp\";};",

			"class test_class1 { [key] String K1; Sint64 D1; DateTime DT1; String S1; };\n",
			"class test_class2 { [key] String K2; Sint64 D2; DateTime DT2; String S2; };\n",
			"class test_class3 { [key] String K3; Sint64 D3; DateTime DT3; String S3; };\n",
			"class test_class4 : test_class1 { Boolean D4; Real64 R4; Sint32 [] I4; };\n",

			"instance of test_class1 { K1 = \"key11\"; D1 = 54; DT1 = \"20140420044028.080***+000\"; S1 = \"instance11\";  };\n",
			"instance of test_class1 { K1 = \"key12\"; D1 = 55; DT1 = \"20150420044028.080***+000\"; S1 = \"instance12\";   };\n",
			"instance of test_class2 { K2 = \"key21\"; D2 = 64; S2 = \"instance21\"; };\n",
			"instance of test_class2 { K2 = \"key22\"; D2 = 65; S2 = \"instance22\"; };\n",
			"instance of test_class3 { K3 = \"key31\"; D3 = 74; S3 = \"instance31\"; };\n",
			"instance of test_class3 { K3 = \"key32\"; D3 = 75; S3 = \"instance32\"; };\n",
			"instance of test_class4 { K1 = \"key41\"; D1 = 33; S1 = \"instance41\"; I4 = {1, 2, 3, 4, 5, 6};};\n",
			"instance of test_class4 { K1 = \"key42\"; D1 = 21; S1 = \"instance42\"; I4 = {2, 4, 6, 8, 10, 12};};\n",
	};
	

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("InMemoryRepository ");
		CimProperty keyProperty = new CimProperty("Test_Struct","kz",DataType.BOOLEAN,new DataValue(true),null);
		Vector<CimProperty> keyProperties = new Vector<CimProperty>();	// properties for Test_Struct
		keyProperties.add(keyProperty);
		struct = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,keyProperties);
		// System.out.println(struct.toMOF());

		// create a Test_Structure value
		HashMap<String,DataValue> propertyValues = new HashMap<String,DataValue>();	
		propertyValues.put("kz",new DataValue(false));
		structValue = StructureValue.createStructureValue(struct, propertyValues, "$as");
		structValue2 = StructureValue.createStructureValue(struct, propertyValues, "$as2");

		// create a CimClass "Test_Class" that inherits from "Test_Struct" with a string property with default value p2Value
		Qualifier keyQual = StandardQualifierType.KEY.getQualifier(true, Constants.defaultNameSpacePath);
		Vector<Qualifier> keyQuals = new Vector<Qualifier>();
		keyQuals.add(keyQual);
		CimProperty classProperty1 = new CimProperty("Test_Struct.Test_Class","p1",DataType.STRING,null,keyQuals);
		CimProperty classProperty = new CimProperty("Test_Struct.Test_Class","p2",DataType.STRING,new DataValue("p2Value"),null);
		Vector<CimProperty> classProperties = new Vector<CimProperty>();
		classProperties.add(classProperty1);
		classProperties.add(classProperty);
		cimClass = new CimClass(ElementType.CLASS,"Test_Class",struct,null,struct.getNameSpacePath(),classProperties);
		// System.out.println(cimClass.toMOF());

		// create a CimInstance based on Test_Class
		HashMap<String,DataValue> classPropertyValues = new HashMap<String,DataValue>();
		classPropertyValues.put("p1", new DataValue("key1Value"));
		classPropertyValues.put("p2", new DataValue("newP2Value"));
		classPropertyValues.put("KZ", new DataValue(true));
		instance = CimInstance.createInstance(cimClass, classPropertyValues, "$ac");
		classPropertyValues.put("p1", new DataValue("key2Value"));
		instance2 = CimInstance.createInstance(cimClass, classPropertyValues, "$ac2");
		// System.out.println(instance.toMOF());
		// System.out.println(instance2.toMOF());

		// create an enumeration "enum" with one string value "name" defined in it
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null,null));
		enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
		// System.out.println(enumeration.toMOF());

		// create an enumValue based on enum
		enumValue = enumeration.getValue("name");
		enumValue2 = enumeration.getValue("name");
		// System.out.println(enumValue.toMOF());
		// System.out.println(enumValue2.toMOF());
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		repository = new InMemoryRepository();
		assertNotNull(repository);
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
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#InMemoryRepository()}.
	 */
	@Test
	public final void testInMemoryRepository() {
		assertNotNull(repository);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#InMemoryRepository(net.aifusion.metamodel.NameSpacePath)}.
	 */
	@Test
	public final void testInMemoryRepositoryNameSpacePath() {
		NameSpacePath p = new NameSpacePath("http","localhost:8080","/localpath");
		InMemoryRepository r = new InMemoryRepository(p);
		assertNotNull(r);
		List<NameSpacePath> ns = r.getNameSpaces();
		assertEquals(1,ns.size());
		assertEquals(p,ns.get(0));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#put(net.aifusion.metamodel.NamedElement)}.
	 */
	@Test
	public final void testPut() {
		// test standard qualifiers
		for(StandardQualifierType q : StandardQualifierType.values()){
			QualifierType qt = q.getQualifierType();
			assertTrue(repository.put(qt));
		}
		// test other elements
		assertTrue(repository.put(enumeration));
		assertTrue(repository.put(enumeration));	// should succeed
		assertTrue(repository.put(instance));		// note we are putting in instances before classes
		assertTrue(repository.put(instance2));
		assertTrue(repository.put(cimClass));
		assertTrue(repository.put(struct));
		assertTrue(repository.put(structValue));
		assertTrue(repository.put(structValue2));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#get(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testGet() {
		testPut();
		// test standard qualifiers
		for(StandardQualifierType q : StandardQualifierType.values()){
			QualifierType expect = q.getQualifierType();
			QualifierType found = (QualifierType) repository.get(expect.getObjectPath());
			assertEquals(expect,found);
		}
		// test other elements
		assertEquals(enumeration,repository.get(enumeration.getObjectPath()));
		assertEquals(instance,repository.get(instance.getObjectPath()));
		assertEquals(instance2,repository.get(instance2.getObjectPath()));
		assertEquals(cimClass,repository.get(cimClass.getObjectPath()));
		assertEquals(struct,repository.get(struct.getObjectPath()));
		assertEquals(structValue,repository.get(structValue.getObjectPath()));
		assertEquals(structValue2,repository.get(structValue2.getObjectPath()));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#contains(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testHasElement() {
		// test standard qualifiers
		for(StandardQualifierType q : StandardQualifierType.values()){
			assertTrue(repository.contains(q.getQualifierType().getObjectPath()));
		}
		testPut();
		// test other elements
		assertTrue(repository.contains(enumeration.getObjectPath()));
		assertTrue(repository.contains(instance.getObjectPath()));		// note we are hasElementting in instances before classes
		assertTrue(repository.contains(instance2.getObjectPath()));
		assertTrue(repository.contains(cimClass.getObjectPath()));
		assertTrue(repository.contains(struct.getObjectPath()));
		assertTrue(repository.contains(structValue.getObjectPath()));
		assertTrue(repository.contains(structValue2.getObjectPath()));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#delete(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testDelete() {
		// standard qualifiers cannot be deleted
		QualifierType abstr = StandardQualifierType.ABSTRACT.getQualifierType();
		assertTrue(repository.contains(abstr.getObjectPath()));
		assertFalse(repository.delete(abstr.getObjectPath()));	// standard qualifiers cannot be deleted
		assertTrue(repository.contains(abstr.getObjectPath()));
		
		testPut();
		// test other elements
		assertTrue(repository.contains(enumeration.getObjectPath()));
		assertTrue(repository.delete(enumeration.getObjectPath()));
		assertFalse(repository.contains(enumeration.getObjectPath()));
		
		assertTrue(repository.delete(instance.getObjectPath()));
		assertFalse(repository.contains(instance.getObjectPath()));
		
		assertTrue(repository.contains(instance2.getObjectPath()));
		assertTrue(repository.delete(instance2.getObjectPath()));
		
		assertTrue(repository.delete(cimClass.getObjectPath()));
		assertFalse(repository.contains(cimClass.getObjectPath()));
		
		assertTrue(repository.delete(structValue.getObjectPath()));
		assertFalse(repository.contains(structValue.getObjectPath()));
		assertTrue(repository.delete(structValue2.getObjectPath()));
		assertFalse(repository.contains(structValue2.getObjectPath()));
		
		assertTrue(repository.delete(struct.getObjectPath()));
		assertFalse(repository.contains(struct.getObjectPath()));
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#shutdown()}.
	 */
	@Test
	public final void testShutdown() {
		testPut();
		repository.shutdown();
		// test standard qualifiers
		QualifierType abstr = StandardQualifierType.ABSTRACT.getQualifierType();
		assertTrue(repository.contains(abstr.getObjectPath()));
		// test other elements
		assertFalse(repository.contains(enumeration.getObjectPath()));
		assertFalse(repository.contains(instance.getObjectPath()));		// note we are checking instances before classes
		assertFalse(repository.contains(instance2.getObjectPath()));
		assertFalse(repository.contains(cimClass.getObjectPath()));
		assertFalse(repository.contains(struct.getObjectPath()));
		assertFalse(repository.contains(structValue2.getObjectPath()));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#getNameSpaces()}.
	 */
	@Test
	public final void testGetNameSpaces() {
		List<NameSpacePath> paths = repository.getNameSpaces();
		assertNotNull(paths);
		assertEquals(1,paths.size());
		assertEquals(Constants.defaultNameSpacePath,paths.get(0));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryRepository#getElements(java.lang.String, java.lang.String, java.lang.String, boolean)}.
	 */
	@Test
	public final void testGetElements() {
		String [] elementType = {null,"Structure","Class","Instance","Enumeration","structureValue",null,null};
		String [] localNameSpace = {null,null,null,null,null,null,"foo",null};
		String [] elementName = {null,null,null,null,null,null,null,"test_foo"};
		int [] expect = {7,1,1,2,1,2,0,0};
		
		assertEquals(elementType.length,localNameSpace.length);
		assertEquals(elementType.length,elementName.length);
		assertEquals(elementType.length,expect.length);
		testPut();
		for(int i=0; i < elementType.length; i++){
			List<NamedElement> elements = repository.getElements(elementType[i], localNameSpace[i], elementName[i], true);
			if(verbose) {
				System.out.println("** ElementType: "+elementType[i]+" Expect: "+expect[i]+" Found: "+elements.size());
				for(NamedElement e : elements) System.out.println(e.getObjectPath()+"\n"+e.toMOF());
			}
			assertEquals(expect[i],elements.size());
		}
	}
	
	
	/**
	 * Test to get elements with subtypes defined
	 */
	@Test
	public final void testGetElements3(){
		InMemoryRepository cache = new InMemoryRepository();
		MOFParser parser = new MOFParser(cache);
		for(String mof : mofDefinition){
			parser.parse(new ByteArrayInputStream(mof.getBytes()), null);
		}
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		assertEquals(25,elements.size());
		elements = cache.getElements(null, null, null, true);
		assertEquals(25,elements.size());
		elements = cache.getElements(null, null, "Test_Computer", false);
		assertEquals(2,elements.size());
		elements = cache.getElements("instance", null, "Test_Computer", false);
		assertEquals(1,elements.size());
		elements = cache.getElements("instance", null, "Test_Computer", true);
		assertEquals(3,elements.size());
		return;
	}
	
}
