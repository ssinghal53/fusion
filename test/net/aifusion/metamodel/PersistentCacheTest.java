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
 * Created Dec 13, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Unit tests for a Persistent Cache
 * @author Sharad Singhal
 */

public class PersistentCacheTest {
	private static String repositoryLocation = "testrepository";
	private static NameSpacePath nameSpacePath1 = new NameSpacePath("/root/local");
	private static NameSpacePath nameSpacePath2 = new NameSpacePath("/com/cimfusion");
	private static boolean verbose = false;
	
	// TOOD: Add enums, structs, and Instances as well
	private static String mofClass = 
			"#pragma namespace (\"/root/local\")\n"+
			"Enumeration test_enum : string { enumvalue};\n"+
			"Enumeration test_enumb : string { enumvalue};\n"+
			"Structure test_struct { test_enum v;\n string foobar; };\n"+
			"Structure test_structb { test_enumb v;\n string foobar; };\n"+
			"class test_class {\n\t[key]\n\tSint32 integerProperty;\n};\n"+
			"class test_classb {\n\t[key]\n\tSint32 integerProperty;\n};\n"+
			"instance of test_class {\n\tintegerProperty = 5;\n};\n"+
			"instance of test_classb {\n\tintegerProperty = 5;\n};\n"+
			"value of test_struct as $struct { foobar = \"value\";\n};\n"+
			"#pragma namespace (\"/com/cimfusion\")\n"+
			"class test_classc {\n\t[key]\n\tSint32 integerProperty;\n};\n";
	
	private static String [] mofDefinition = {
			"Enumeration Disk_Enum : String { SSD, HD, DVD };\n",//
			"Enumeration Server_Enum : String { db, app, web };\n",//
			"[Version(\"1.0.0\")] Structure Test_Struct {[Write] STRING MountPoint; [Key] STRING ID; Real64 UsedCapacity; Disk_Enum type;};",//
			"Class Test_Disk : Test_Struct { BOOLEAN dismount(); BOOLEAN mount();};\n", //
			"Class Test_Computer {[Write] STRING HostName; [Key] STRING address; Boolean isPublic; Test_Disk ref disk;};", //
			"Class App_Server : Test_Computer {Server_Enum type; String software[]; DateTime installDate;};", //
			"Class Test_Application : App_Server {String appName;};", //
			

			"instance of Test_Disk as $disk1 { ID = \"id1\"; MountPoint = \"sup1\"; UsedCapacity = 32;};",//
			"instance of Test_Disk as $disk2 { ID = \"id2\"; UsedCapacity = 33;};",
			"instance of Test_Disk as $disk3 { ID = \"id3\";};",

			"instance of Test_Computer as $c1 { address = \"192.168.1.1\"; disk = $disk1;};",
			"instance of App_Server as $c2 { address = \"192.168.1.2\"; isPublic = true; disk = $disk1; type = \"web\"; software = {\"apache\", \"tomcat\"}; installDate = \"20120910123400.000000+740\";};",
			"instance of Test_Application as $c3 { address = \"192.168.1.3\";  appName = \"myApp\";};",

			"class test_class1 { [key] String K1; Sint64 D1; DateTime DT1; String S1; };\n", //
			"class test_class2 { [key] String K2; Sint64 D2; DateTime DT2; String S2; };\n", //
			"class test_class3 { [key] String K3; Sint64 D3; DateTime DT3; String S3; };\n", //
			"class test_class4 : test_class1 { Boolean D4; Real64 R4; Sint32 [] I4; };\n", //

			"instance of test_class1 { K1 = \"key11\"; D1 = 54; DT1 = \"20140420044028.080***+000\"; S1 = \"instance11\";  };\n",//
			"instance of test_class1 { K1 = \"key12\"; D1 = 55; DT1 = \"20150420044028.080***+000\"; S1 = \"instance12\";   };\n",//
			"instance of test_class2 { K2 = \"key21\"; D2 = 64; S2 = \"instance21\"; };\n",//
			"instance of test_class2 { K2 = \"key22\"; D2 = 65; S2 = \"instance22\"; };\n",//
			"instance of test_class3 { K3 = \"key31\"; D3 = 74; S3 = \"instance31\"; };\n",//
			"instance of test_class3 { K3 = \"key32\"; D3 = 75; S3 = \"instance32\"; };\n",//
			"instance of test_class4 { K1 = \"key41\"; D1 = 33; S1 = \"instance41\"; I4 = {1, 2, 3, 4, 5, 6};};\n",//
			"instance of test_class4 { K1 = \"key42\"; D1 = 21; S1 = \"instance42\"; I4 = {2, 4, 6, 8, 10, 12};};\n",//

			"#pragma namespace (\"/root/local\")\n"+
					"Enumeration test_enum : string { enumvalue};\n"+ //
					"Structure test_struct { test_enum v;\n string foobar; };\n"+ //
					"class test_class1 {\n\t[key]\n\tSint32 integerProperty;\n\t[write]String stringProperty;\n\tboolean reset(String propertyName);\n};\n"+ //
					"instance of test_class1 {\n\tintegerProperty = 5;\n\tstringProperty = null;\n};\n" //
	};
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("PersistentCache ");
		assertTrue(repositoryLocation.startsWith("testr"));
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		// clean up -- comment out the next line to check the repository contents manually if tests are failing 
		deleteFiles(repositoryLocation);
		System.out.println("done.");
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		deleteFiles(repositoryLocation);
		Thread.sleep(1000L);
	}
	
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}
	
	/**
	 * @throws java.lang.Exception
	 */
	// @After
	// public void tearDown() throws Exception {
	// 	deleteFiles(repositoryLocation);
	// 	Thread.sleep(1000L);
	// }

	/**
	 * Delete a file or directory. If the input is a directory, all files and
	 * directories within that directory are deleted.
	 * @param fileOrDirectory - name of file or directory to delete
	 */
	private static void deleteFiles(String fileOrDirectory){
		File file = new File(fileOrDirectory);
		if(!file.exists()) return;
		if(file.isDirectory()){
			String [] names = file.list();
			if(names != null && names.length > 0){
				for(String n : names){
					String fName = fileOrDirectory+"/"+n;
					deleteFiles(fName);
				}
			}
			if(!file.delete()){
				System.out.println("Unable to delete Directory "+fileOrDirectory);
			}
		} else if(!file.delete()){
			System.out.println("Unable to delete file "+fileOrDirectory);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.PersistentCache#put(net.aifusion.metamodel.NamedElement)}.
	 */
	@Test
	public final void testPut() {
		PersistentCache cache = new PersistentCache(repositoryLocation);
		InMemoryCache repository = new InMemoryCache();
		MOFParser parser = new MOFParser(repository);
		// System.out.println(mofClass);
		parser.parse(new ByteArrayInputStream(mofClass.getBytes()), null);
		List<NamedElement> elements = repository.getElements(null, null, null, false);
		assertEquals(10,elements.size());
		// for(NamedElement e : elements){
		// 	System.out.println(e.toMOF());
		// }
		
		CimClass c = (CimClass) repository.get(new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath1,null, null));
		assertNotNull(c);
		
		File cf = new File(repositoryLocation+"/class"+nameSpacePath1.getLocalPath()+"/test_class.mof");
		assertFalse(cf.exists());
		assertTrue(cache.put(c));
		assertTrue(cf.exists());
		
		c = (CimClass) repository.get(new ObjectPath(ElementType.CLASS,"test_classc",nameSpacePath2,null, null));
		assertNotNull(c);
		
		cf = new File(repositoryLocation+"/class"+nameSpacePath2.getLocalPath()+"/test_classc.mof");
		assertFalse(cf.exists());
		assertTrue(cache.put(c));
		assertTrue(cf.exists());
		cache.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.PersistentCache#get(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testGet() {
		PersistentCache cache = new PersistentCache(repositoryLocation);
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mofClass.getBytes()), null);
		CimClass c = (CimClass) cache.get(new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath1,null, null));
		assertNotNull(c);
		c = (CimClass) cache.get(new ObjectPath(ElementType.CLASS,"test_classc",nameSpacePath2,null, null));
		assertNotNull(c);	
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.PersistentCache#contains(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testHasElement() {
		PersistentCache cache = new PersistentCache(repositoryLocation);
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mofClass.getBytes()), null);
		QualifierType qT = StandardQualifierType.ABSTRACT.getQualifierType(nameSpacePath1);
		assertTrue(cache.contains(qT.getObjectPath()));
		qT = StandardQualifierType.ABSTRACT.getQualifierType();
		assertTrue(cache.contains(qT.getObjectPath()));
		CimClass c = (CimClass) cache.get(new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath1,null, null));
		assertNotNull(c);
		assertTrue(cache.contains(c.getObjectPath()));
		c = (CimClass) cache.get(new ObjectPath(ElementType.CLASS,"test_classc",nameSpacePath2,null, null));
		assertNotNull(c);
		assertTrue(cache.contains(c.getObjectPath()));
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.PersistentCache#delete(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testDelete() {
		PersistentCache cache = new PersistentCache(repositoryLocation);
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mofClass.getBytes()), null);
		File cf = new File(repositoryLocation+"/class"+nameSpacePath1.getLocalPath()+"/test_class.mof");
		assertTrue(cf.exists());
		CimClass c = (CimClass) cache.get(new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath1,null, null));
		assertNotNull(c);
		assertTrue(cache.contains(c.getObjectPath()));
		assertTrue(cache.delete(c.getObjectPath()));
		assertFalse(cf.exists());
		
		cf = new File(repositoryLocation+"/class"+nameSpacePath2.getLocalPath()+"/test_classc.mof");
		assertTrue(cf.exists());
		c = (CimClass) cache.get(new ObjectPath(ElementType.CLASS,"test_classc",nameSpacePath2,null, null));
		assertNotNull(c);
		assertEquals(nameSpacePath2,c.getObjectPath().getNameSpacePath());
		assertTrue(cache.contains(c.getObjectPath()));
		assertTrue(cache.delete(c.getObjectPath()));
		assertFalse(cf.exists());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.PersistentCache#PersistentCache(String)}.
	 */
	@Test
	public final void testPersistentCache() {
		PersistentCache cache = null;
		// try failure conditions
		try {
			cache = new PersistentCache(null);
			fail("Null repository succeeded");
		} catch(ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		cache = new PersistentCache(repositoryLocation);
		assertNotNull(cache);
		File f = new File(repositoryLocation);
		assertTrue(f.exists());
		// check the presence of standard qualifiers
		ObjectPath path = new ObjectPath(ElementType.QUALIFIERTYPE,"Abstract",nameSpacePath1,null, null);
		assertTrue(cache.contains(path));
		f = new File(repositoryLocation+"/qualifiertype"+nameSpacePath1.getLocalPath()+"/Abstract.mof");
		assertFalse(f.exists());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.InMemoryCache#getNameSpaces()}.
	 */
	@Test
	public final void testGetNameSpaces() {
		String mof = "#pragma namespace (\"/root/local\")\n"+
				"class test_class {\n\t[key]\n\tSint32 integerProperty;\n};\n"+
				"class test_classb {\n\t[key]\n\tSint32 integerProperty;\n};\n"+
				"instance of test_class {\n\tintegerProperty = 5;\n};\n"+
				"instance of test_classb {\n\tintegerProperty = 5;\n};\n"+
				"#pragma namespace (\"/com/cimfusion\")\n"+
				"class test_classc {\n\t[key]\n\tSint32 integerProperty;\n};\n"+
				"#pragma namespace (\"/root/local/sublocal\")\n"+
				"class test_classd {\n\t[key]\n\tSint32 integerProperty;\n};\n";
		PersistentCache cache = new PersistentCache(repositoryLocation);
		List<NameSpacePath> paths = cache.getNameSpaces();
		if(verbose) {
			if(!paths.isEmpty()){
				for(NameSpacePath path : paths) System.out.println(path.toString());
			}
		}
		assertEquals(0,paths.size());
		// assertEquals(nameSpacePath1,paths.get(0));
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mof.getBytes()), null);
		paths = cache.getNameSpaces();
		assertEquals(3,paths.size());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.PersistentCache#getElements(String, String, String, boolean)}.
	 */
	@Test
	public final void testGetElements(){
		PersistentCache cache = new PersistentCache(repositoryLocation);
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mofClass.getBytes()), null);
		
		// get elements filtered by both known and unknown namespaces and element names
		List<NamedElement> elements = cache.getElements("class", "/root/local,/unknown", "test_class,test_structure,test_struct,test_enum", false);
		assertEquals(1,elements.size());
		elements = cache.getElements("structure", "/root/local,/unknown", "test_class,test_structure,test_struct,test_enum", false);
		assertEquals(1,elements.size());
		elements = cache.getElements("enumeration", "/root/local,/unknown", "test_class,test_structure,test_struct,test_enum", false);
		assertEquals(1,elements.size());
		elements = cache.getElements("Instance", "/root/local,/unknown", "test_class,test_structure,test_struct,test_enum", false);
		assertEquals(1,elements.size());
		elements = cache.getElements("Instance,structure", "/root/local,/unknown", "test_class,test_structure,test_struct,test_enum", false);
		assertEquals(2,elements.size());
		
		// get all classes, structures, enumerations, and instances
		elements = cache.getElements("class", null, null, false);
		assertEquals(3,elements.size());
		elements = cache.getElements("Structure", null, null, false);
		assertEquals(2,elements.size());
		elements = cache.getElements("enumeration", null, null, false);
		assertEquals(2,elements.size());
		elements = cache.getElements("instance", null, null, false);
		assertEquals(2,elements.size());
		
		// re-parse all elements into an in-memory repository
		InMemoryCache repository = new InMemoryCache();
		parser = new MOFParser(repository);
		parser.parse(new ByteArrayInputStream(mofClass.getBytes()), null);
		// get all elements from the in-memory repository, and ensure that they are in the persistent cache
		elements = repository.getElements(null, null, null, false);
		for(NamedElement element : elements){
			// System.out.println(element.getObjectPath().toURL());
			assertTrue(cache.contains(element.getObjectPath()));
			ObjectPath p = element.getObjectPath();
			assertEquals(repository.get(p),cache.get(p));
		}
		// get all elements from the persistent cache, and validate them from the in-memory repository
		elements = cache.getElements(null, null, null, false);
		for(NamedElement element : elements){
			// System.out.println(element.getObjectPath().toURL());
			assertTrue(cache.contains(element.getObjectPath()));
			ObjectPath p = element.getObjectPath();
			assertEquals(repository.get(p),cache.get(p));
		}
	}
	
	/**
	 * Test to get elements with a non-default object path from the persistent cache
	 */
	@Test
	public final void testGetElements2(){
		PersistentCache cache = new PersistentCache(repositoryLocation);
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mofClass.getBytes()), null);
		List<NamedElement> elements = cache.getElements(null, null, "test_classc", false);
		assertEquals(1,elements.size());
		assertEquals(new ObjectPath(ElementType.CLASS,"Test_ClassC",new NameSpacePath("/com/cimfusion"),null, null),elements.get(0).getObjectPath());
	}
	
	/**
	 * Test to get elements with subtypes defined
	 */
	@Test
	public final void testGetElements3(){
		PersistentCache cache = new PersistentCache(repositoryLocation);
		MOFParser parser = new MOFParser(cache);
		for(String mof : mofDefinition){
			parser.parse(new ByteArrayInputStream(mof.getBytes()), null);
		}
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		if(verbose) for(NamedElement e : elements) System.out.println(e.toMOF());
		assertEquals(29,elements.size());
		elements = cache.getElements(null, null, null, true);
		assertEquals(29,elements.size());
		elements = cache.getElements(null, null, "Test_Computer", false);
		assertEquals(2,elements.size());
		elements = cache.getElements("instance", null, "Test_Computer", false);
		assertEquals(1,elements.size());
		elements = cache.getElements("instance", null, "Test_Computer", true);
		assertEquals(3,elements.size());
		return;
	}
	
	@Test
	public final void testGolfSchema() {
		PersistentCache cache = new PersistentCache(repositoryLocation);
		// InMemoryCache cache = new InMemoryCache(nameSpacePath1);
		MOFParser p = new MOFParser(cache);
		try {
			p.parse("testcases/golf/GOLF_Schema.mof", null);
			assertEquals(3,cache.getElements("qualifierType",null,null, false).size());
			assertEquals(5,cache.getElements("enumeration",null,null, false).size());
			assertEquals(3,cache.getElements("structure",null,null, false).size());
			assertEquals(7,cache.getElements("class",null,null, false).size());
			assertEquals(1,cache.getElements("instance",null,null, false).size());
		} catch (ModelException e){
			e.printStackTrace();
			fail("Parse failed");
		}
		cache.shutdown();
		cache = new PersistentCache(repositoryLocation);
		assertEquals(3,cache.getElements("qualifierType",null,null, false).size());
		assertEquals(5,cache.getElements("enumeration",null,null, false).size());
		assertEquals(3,cache.getElements("structure",null,null, false).size());
		assertEquals(7,cache.getElements("class",null,null, false).size());
		assertEquals(1,cache.getElements("instance",null,null, false).size());
		assertEquals(2,cache.getElements("structurevalue",null,null, false).size());
		assertEquals(21,cache.getElements(null, null, null, false).size());
	}
}
