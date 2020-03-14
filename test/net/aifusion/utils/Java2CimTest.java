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
 * Created Jan 3, 2016 by Sharad Singhal
 */
package net.aifusion.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.metamodel.Repository;

import net.aifusion.utils.Java2Cim;

/**
 * Test class for Java2Cim Tool
 * @author Sharad Singhal
 */
public class Java2CimTest {
	private Repository repository, known;
	// String containing known classes
	private String mof = "#pragma namespace (\""+Constants.defaultLocalPath+"\")\n"+
			"[MappingStrings{\"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\"}]\n"+
			"Enumeration AIFusion_EmbeddedStringEnum : String {\n"+
			"NAME1 = \"xyz\",\n"+
			"Name2 = \"abc\",\n"+
			"name3 = \"def\"\n};"+

			"[MappingStrings{\"Bind.CF|net.aifusion.metamodel.EnumBindingClass\" }, Description(\"Integer Valued Enumeration\"),"+
			"PackagePath(\"newPath1::newPath2\"), Version(\"1.0.0\")]\n"+
			"Enumeration AIFusion_EnumBindingClass : SInt32 {\n"+
			"NAME1 = 0,\n"+
			"Name2 = 2,\n"+
			"name3 = 3\n"+
			"};\n"+

			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.MethodBindingSuperClass\" }]\n"+
			"Structure Cim_TestMethodsSup {\n"+
			"\tString StringProperty = \"Something\";\n};\n"+
			
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.MethodBindingClass\" }]\n"+
			"Class Cim_TestMethods : Cim_TestMethodsSup {\n"+
			"[Key] String Key;\n"+
			// "[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\" }]\n"+
			"AIFusion_EmbeddedStringEnum enumValueToEnum(AIFusion_EmbeddedStringEnum arg0);\n"+
			"String enumValueToString(AIFusion_EmbeddedStringEnum arg0);\n"+
			"String concatStringToEnumValue(AIFusion_EmbeddedStringEnum arg0, String arg1);\n"+
			"Void doSomething();\n};"+

			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.IsGetterPropertyClass\" }, "+
			"Description(\"Structure to test isGetter property bindings\")]\n"+
			"Structure Test_Class {\n"+
			"\t[Write, Description(\"write only property\"), Read(false)]\n\tBoolean P1;\n"+
			"\t[Description(\"read-only property\")]\n\tBoolean P2;\n"+
			"\t[Write, Description(\"read/write property\")]\n\tBoolean P3;\n"+
			"\t[Description(\"readonly only property with isGetter\")]\n\tBoolean P4;\n"+
			"\t[Write, Description(\"read/write only property with isGetter\")]\n\tBoolean P5;\n"+
			"\t[Description(\"readonly only property with get() method\")]\n\tBoolean P6;\n"+
			"\t[Write, Description(\"read/write property with get() method\")]\n\tBoolean P7;\n"+
			"\t[Write, Description(\"write only property\"), Read(false)]\n\tString PF1;\n"+
			"\t[Description(\"read-only property\")]\n\tString PF2;\n"+
			"\t[Write, Description(\"read/write property\")]\n\tString PF3;\n"+
			"\t[Description(\"readonly only property with isGetter\")]\n\tString isPF4;\n"+
			"\t[Write, Read(false)]\n\tString PF5;\n"+
			"\t[Description(\"read/write only property with isGetter\")]\n\tString isPF5;\n"+
			"\t[Description(\"readonly only property with get() method\")]\n\tString isPF6;\n"+
			"\tString PF6;\n"+
			"\t[Write]\n\tString PF7;\n"+
			"\t[Description(\"read/write property with get() method\")]\n\tString isPF7;\n"+
			"};\n"+

			"[MappingStrings{\"Bind.CF|net.aifusion.metamodel.PropertyBindingClass\"}, Description(\"Structure to test property bindings\")]\n"+
			"Structure Cim_Test {\n"+
			"[MappingStrings{\"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\"}]\n"+
			"Enumeration AIFusion_EmbeddedStringEnum : String {\n"+
			"NAME1 = \"xyz\",\n Name2 = \"abc\",\n name3 = \"def\"\n};\n"+
			
			"[Write] Boolean V01;\n"+
			"[Write] Boolean V02;\n"+
			"[Write] UInt8 V03;\n"+
			"[Write] UInt16 V04;\n"+
			"[Write] UInt32 V05;\n"+
			"[Write] UInt64 V06;\n"+
			"[Write] SInt8 V07;\n"+
			"[Write] SInt8 V08;\n"+
			"[Write] SInt16 V09;\n"+
			"[Write] SInt16 V10;\n"+
			"[Write] SInt32 V11;\n"+
			"[Write] SInt32 V12;\n"+
			"[Write] SInt64 V13;\n"+
			"[Write] SInt64 V14;\n"+
			"[Write] Real32 V15;\n"+
			"[Write] Real32 V16;\n"+
			"[Write] Real64 V17;\n"+
			"[Write] Real64 V18;\n"+
			"[Write] Char16 V19;\n"+
			"[Write] Char16 V20;\n"+
			"[Write] String V21;\n"+
			"[Write] Datetime V22;\n"+
			"[Write] OctetString V27;\n"+
			"[Static,Write] String V31 = \"default\";\n"+
			
			"[Write] Boolean [] Va01;\n"+
			"[Write] Boolean [] Va02;\n"+
			"[Write] UInt8 [] Va03;\n"+
			"[Write] UInt16 [] Va04;\n"+
			"[Write] UInt32 [] Va05;\n"+
			"[Write] UInt64 [] Va06;\n"+
			"[Write] SInt8 [] Va07;\n"+
			"[Write] SInt8 [] Va08;\n"+
			"[Write] SInt16 [] Va09;\n"+
			"[Write] SInt16 [] Va10;\n"+
			"[Write] SInt32 [] Va11;\n"+
			"[Write] SInt32 [] Va12;\n"+
			"[Write] SInt64 [] Va13;\n"+
			"[Write] SInt64 [] Va14;\n"+
			"[Write] Real32 [] Va15;\n"+
			"[Write] Real32 [] Va16;\n"+
			"[Write] Real64 [] Va17;\n"+
			"[Write] Real64 [] Va18;\n"+
			"[Write] Char16 [] Va19;\n"+
			"[Write] Char16 [] Va20;\n"+
			"[Write] String [] Va21;\n"+
			"[Write] Datetime [] Va22;\n"+
			"[Write] OctetString [] Va27;\n"+
			"[Static,Write] String [] Va31;\n"+
			
			"[Write] Cim_TestMethods Ref V23;\n"+
			"[Write] AIFusion_EmbeddedStringEnum V24;\n"+
			
			"[Write] Cim_TestMethodsSup V25;\n"+
			"[Write] Cim_TestMethods V26;\n"+
			"[Write] AIFusion_EnumBindingClass V28;\n"+
			"[Write] Cim_TestMethodsSup V29;\n"+	
			"[Write] Cim_TestMethods V30;\n"+
			
			"[Write] Cim_TestMethods Ref [] Va23;\n"+
			"[Write] AIFusion_EmbeddedStringEnum [] Va24;\n"+
			"[Write] Cim_TestMethodsSup [] Va25;\n"+
			"[Write] Cim_TestMethods [] Va26;\n"+
			"[Write] AIFusion_EnumBindingClass [] Va28;\n"+
			"[Write] Cim_TestMethodsSup [] Va29;\n"+
			"[Write] Cim_TestMethods [] Va30;\n"+
			
			/*

			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingClass\"}] Cim_TestMethods Ref V23;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\"}] AIFusion_EmbeddedStringEnum V24;\n"+
			
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingSuperClass\"}] Cim_TestMethodsSup V25;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingClass\"}] Cim_TestMethods V26;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.EnumBindingClass\"}] AIFusion_EnumBindingClass V28;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingSuperClass\"}] Cim_TestMethodsSup V29;\n"+	
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingClass\"}] Cim_TestMethods V30;\n"+
			
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingClass\"}] Cim_TestMethods Ref [] Va23;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\"}] AIFusion_EmbeddedStringEnum [] Va24;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingSuperClass\"}] Cim_TestMethodsSup [] Va25;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingClass\"}] Cim_TestMethods [] Va26;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.EnumBindingClass\"}] AIFusion_EnumBindingClass [] Va28;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingSuperClass\"}] Cim_TestMethodsSup [] Va29;\n"+
			"[Write,MappingStrings{\"Bind.CF|net.aifusion.metamodel.MethodBindingClass\"}] Cim_TestMethods [] Va30;\n"+
			*/
			"};"
	;				
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Java2Cim ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		deleteFiles("/test");
		repository = new PersistentCache("/test");
		known = new InMemoryCache();
		MOFParser parser = new MOFParser(known);
		parser.parse(new ByteArrayInputStream(mof.getBytes()), Constants.defaultNameSpacePath);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		repository.shutdown();
		deleteFiles("/test");
		System.out.print(".");
	}
	
	
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
		}
		if(file.exists() && !file.delete()){
			fail("Unable to delete file "+fileOrDirectory);
		}
	}

	/**
	 * Test method for {@link net.aifusion.utils.Java2Cim#getCimModelListForClass(java.lang.Class)}.
	 */
	@Test
	public final void testGetCimModelListForClass() {
		List<NamedElement> list = Java2Cim.getCimModelListForClass(net.aifusion.metamodel.PropertyBindingClass.class);
		assertNotNull(list);
		assertEquals(5,list.size());
		
	}

	/**
	 * Test method for {@link net.aifusion.utils.Java2Cim#getModelForClass(java.lang.Class, net.aifusion.metamodel.Repository)}
	 * using an enumeration
	 */
	@Test
	public final void testModelEnum() {
		NamedElement expect = known.get(new ObjectPath(ElementType.ENUMERATION,"AIFusion_EnumBindingClass",Constants.defaultNameSpacePath,null, null));
		assertNotNull(expect);
		NamedElement element = Java2Cim.getModelForClass(net.aifusion.metamodel.EnumBindingClass.class, repository);
		assertNotNull(element);
		assertEquals(expect,element);
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.Java2Cim#getModelForClass(java.lang.Class, net.aifusion.metamodel.Repository)}
	 * using a structure
	 */
	@Test
	public final void testModelStruct() {
		NamedElement expect = known.get(new ObjectPath(ElementType.STRUCTURE,"Cim_Test",Constants.defaultNameSpacePath,null, null));
		assertNotNull(expect);
		
		NamedElement element = Java2Cim.getModelForClass(net.aifusion.metamodel.PropertyBindingClass.class, repository);
		assertNotNull(element);
		// System.out.println(element.toMOF());
		assertEquals(expect,element);
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.Java2Cim#getModelForClass(java.lang.Class, net.aifusion.metamodel.Repository)}
	 * using a structure
	 */
	@Test
	public final void testModelStruct1() {
		
		NamedElement expect = known.get(new ObjectPath(ElementType.STRUCTURE,"Cim_TestMethodsSup",Constants.defaultNameSpacePath,null, null));
		assertNotNull(expect);
		
		NamedElement element = Java2Cim.getModelForClass(net.aifusion.metamodel.MethodBindingSuperClass.class, repository);
		assertNotNull(element);
		assertEquals(expect,element);
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.Java2Cim#getModelForClass(java.lang.Class, net.aifusion.metamodel.Repository)}
	 * using a class
	 */
	@Test
	public final void testModelClass() {
		
		NamedElement expect = known.get(new ObjectPath(ElementType.CLASS,"Cim_TestMethods",Constants.defaultNameSpacePath,null, null));
		assertNotNull(expect);
		
		NamedElement element = Java2Cim.getModelForClass(net.aifusion.metamodel.MethodBindingClass.class, repository);
		assertNotNull(element);
		assertEquals(expect,element);
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.Java2Cim#getModelForClass(java.lang.Class, net.aifusion.metamodel.Repository)}
	 * using a class with [get|set|is]XXX() type properties
	 */
	
	@Test
	public final void testModelIsGetter(){
		NamedElement expect = known.get(new ObjectPath(ElementType.STRUCTURE,"Test_Class",Constants.defaultNameSpacePath,null, null));
		assertNotNull(expect);

		NamedElement element = Java2Cim.getModelForClass(net.aifusion.metamodel.IsGetterPropertyClass.class, repository);
		assertNotNull(element);
		assertEquals(expect,element);
	}

}
