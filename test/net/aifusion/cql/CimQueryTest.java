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
 * Created Oct 2, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.StructureValue;

/**
 * @author Sharad Singhal
 *
 */
public class CimQueryTest {
	private static boolean verbose = false;
	private static String [] mofDefinition = {
			"Enumeration Disk_Enum : String { SSD, HD, DVD };\n",
			"Enumeration Server_Enum : String { db, app, web };\n",
			"[Version(\"1.0.0\")] Structure Test_Struct {[Write] STRING MountPoint; [Key] STRING ID; Real64 UsedCapacity; Disk_Enum type;};",
			"Class Test_Disk : Test_Struct { BOOLEAN dismount(); BOOLEAN mount();};\n",
			"Class Test_Computer {[Write] STRING HostName; [Key] STRING address; Boolean isPublic; Test_Disk ref disk;};",
			"Class App_Server : Test_Computer {Server_Enum type; String software[]; DateTime installDate;};",

			"instance of Test_Disk as $disk1 { ID = \"id1\"; MountPoint = \"sup1\"; UsedCapacity = 32;};",
			"instance of Test_Disk as $disk2 { ID = \"id2\"; UsedCapacity = 33;};",
			"instance of Test_Disk as $disk3 { ID = \"id3\";};",

			"instance of Test_Computer as $c1 { address = \"192.168.1.1\"; disk = $disk1;};",
			"instance of App_Server as $c2 { address = \"192.168.1.2\"; isPublic = true; disk = $disk1; type = \"web\"; software = {\"apache\", \"tomcat\"}; installDate = \"20120910123400.000000+740\";};",

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

			"#pragma namespace (\"/root/local\")\n"+
					"Enumeration test_enum : string { enumvalue};\n"+
					"Structure test_struct { test_enum v;\n string foobar; };\n"+
					"class test_class1 {\n\t[key]\n\tSint32 integerProperty;\n\t[write]String stringProperty;\n\tboolean reset(String propertyName);\n};\n"+
					"instance of test_class1 {\n\tintegerProperty = 5;\n\tstringProperty = null;\n};\n"
	};
	public static class TestQuery {
		String query;
		boolean isValid;
		int resultSize;
		int errorCode;
		/**
		 * Create a test query
		 * @param query - query definition
		 * @param isValid - flag to indicate if this query is a valid syntax
		 * @param resultSize - expected size of the result set
		 * @param errorCode - if query is not valid, expected ExceptionReason code
		 */
		public TestQuery(String query, boolean isValid, int resultSize, int errorCode){
			this.query = query;
			this.isValid = isValid;
			this.resultSize = resultSize;
			this.errorCode = errorCode;
		}
	}

	private static InMemoryCache cache = new InMemoryCache();

	private static TestQuery [] queries = {
			new TestQuery("select * from test_class1",true,5,0),
			new TestQuery("select c1.integerProperty from /root/local:test_class1 as c1",true,1,0),
			new TestQuery("select * from /aifusion:test_class1",true,4,0),
			new TestQuery("select * from test_class1 where s1 = 'instance11'",true,1,0),
			new TestQuery("select * from test_class1 where integerproperty = 5",true,1,0),
			new TestQuery("select * from test_class1 where d1 < 55",true,3,0),
			new TestQuery("select * from test_class1 where d1 <= 55",true,4,0),
			new TestQuery("select * from test_class1 where d1 > 54",true,1,0),
			new TestQuery("select * from test_class1 where d1 >= 55",true,1,0),
			new TestQuery("select * from test_class1 where d1 <> 55",true,3,0),
			new TestQuery("SELECT * FROM test_class4 WHERE EVERY i4[0,1..3,5..] >= 1",true,2,0),			// expr,expr...
			new TestQuery("SELECT * FROM test_class1 WHERE EVERY I4[] > 2",true,0,0),						// empty index
			new TestQuery("SELECT * FROM test_class1 WHERE EVERY I4 >= 1",true,2,0),						// array with no index
			new TestQuery("SELECT * FROM test_class1 WHERE EVERY i4[*] > 2",true,0,0),						// array with * as index
			new TestQuery("SELECT * FROM test_class4 WHERE EVERY i4[1..] > 2",true,1,0),					// expr .. 
			new TestQuery("SELECT * FROM test_class1 WHERE EVERY test_class4.i4[..3] <= 4",true,1,0), 		// .. expr
			new TestQuery("SELECT * FROM test_class1 WHERE EVERY i4[0..3] >= 2",true,1,0),					// expr .. expr
			new TestQuery("select * from test_class1 where test_class1 isa test_class4",true,2,0),
			new TestQuery("select * from test_class1 where d1 <> 55 AND DT1 = DateTime('20140420044028.080***+000')",true,1,0),
			new TestQuery("SELECT * FROM Test_Disk WHERE ID = 'id1'",true,1,0),								// string comparison (equals)
			new TestQuery("SELECT * FROM Test_Disk WHERE UsedCapacity = 32",true,1,0),						// numeric comparison (equals)
			new TestQuery("SELECT * FROM Test_Disk WHERE UsedCapacity <> 32",true,1,0),						// integer literal	(not equals)
			new TestQuery("SELECT * FROM Test_Disk WHERE UsedCapacity >= 0101b",true,2,0),					// binary literal	(greater than or equals)
			new TestQuery("Select * from Test_Disk where usedCapacity > -0x3FF",true,2,0),					// Hex literal	(greater than)
			new TestQuery("Select * from Test_Disk where UsedCapacity <= 3.14159",true,0,0), 				// decimal literal (less than or equals)
			new TestQuery("Select * from Test_Disk where UsedCapacity < 3.0E4",true,2,0),					// decimal literal (less than)
			new TestQuery("Select * from Test_Computer where isPublic = true",true,1,0),					// boolean literal (only = and <> permitted)
			new TestQuery("Select * from Test_Disk where Test_Disk.UsedCapacity = 32",true,1,0),			// className.PropertyName
			new TestQuery("Select * from Test_Computer where installDate < CurrentDateTime()",true,1,0),	// date-time function
			new TestQuery("Select * from Test_Computer where installDate = DateTime('20120910123400.000000+740')",true,1,0),	// date-time function
			new TestQuery("Select * from Test_Disk where UsedCapacity = 32 OR ID = 'id2'",true,2,0),		// OR expression
			new TestQuery("Select * from Test_Disk where UsedCapacity = 32 AND ID = 'id2'",true,0,0),		// AND expression
			new TestQuery("Select * from Test_Disk where UsedCapacity = 32 AND ID = 'id1'",true,1,0),		// AND expression
			new TestQuery("Select * from Test_Disk where (UsedCapacity = 32 OR ID = 'id2') AND (id = 'id1')",true,1,0),			// complex expressions
			new TestQuery("SELECT * FROM Test_Disk WHERE UsedCapacity IS NULL",true,1,0),					// NULL property
			new TestQuery("SELECT * FROM Test_Disk WHERE UsedCapacity IS NOT NULL",true,2,0),				// NOT NULL property
			new TestQuery("Select * from Test_Computer where isPublic = (NOT true)",true,0,0),				// NOT expression
			new TestQuery("Select * from App_Server where Software[0] = 'apache'",true,1,0),				// array property element
			new TestQuery("Select * from App_Server where Software[1] = 'apache'",true,0,0),				// array property element index
			new TestQuery("Select * from App_Server where Software[1] = 'tomcat'",true,1,0),				// array property element index
			new TestQuery("Select * from App_Server where Software IS NULL",true,0,0),						// NULL array property
			new TestQuery("Select * from App_Server where Software IS NOT NULL",true,1,0),					// not null array property
			new TestQuery("Select * from App_Server where Software = {'apache', 'tomcat'}",true,1,0),		// array property comparison (only = and <> permitted)
			new TestQuery("Select foobar from App_Server where Software = {'apache', 'tomcat'}",true,0,15),	// non-existent property in <select-list> forces exclusion
			new TestQuery("Select usedCapacity,mountpoint from Test_Disk where id = 'id1'",true,1,0),		// selection based on <select-list>
			new TestQuery("Select App_Server.* from Test_Computer",true,1,0),								// selection based on <select-list> (allows all instances of App_Server)
			new TestQuery("Select * from Test_Computer,Test_Disk",true,6,0)		
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
		System.out.print("CimQuery ");
		MOFParser parser = new MOFParser(cache);
		for(String mof : mofDefinition){
			ByteArrayInputStream input = new ByteArrayInputStream(mof.getBytes());
			parser.parse(input, Constants.defaultNameSpacePath);
			input.close();
		}
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		/*
		for(NamedElement element : elements){
			System.out.println(element.getObjectPath()+"\n"+element.toMOF());
		}
		 */
		assertEquals(27,elements.size());
	}

	/**
	 * Test method for {@link net.aifusion.cql.CimQuery#CimQuery(java.lang.String)}.
	 */
	@Test
	public final void testCimQuery() {
		for(TestQuery q : queries){
			CimQuery query = new CimQuery(q.query);
			assertNotNull(query);
			// System.out.println(query.toString());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.CimQuery#setVariable(java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testSetVariable() {
		CimQuery query = new CimQuery("Select * from test_class where k1 = $key$");
		if(verbose) System.out.println(query.toString());
		assertEquals("Select * from test_class where k1 = $key$\n"+
				"SELECT\n  |-- SELECT_LIST\n  |  |-- PROPERTY_NAME (*) [path = null class = null property = *]\n"+
				"  |-- CLASS_LIST\n  |  |-- CLASS_PATH (test_class) [localPath = null class = test_class]\n"+
				"  |-- WHERE\n  |  |-- EQUALS\n  |  |  |-- IDENTIFIER (k1) [path = null class = null property = k1]\n"+
				"  |  |  |-- VARIABLE ($key$)",
				query.toString());


		query.setVariable("$key$", new DataValue("key11"));

		// System.out.println(query.toString());
		assertEquals(
				"Select * from test_class where k1 = $key$\n"+
						"SELECT\n  |-- SELECT_LIST\n  |  |-- PROPERTY_NAME (*) [path = null class = null property = *]\n"+
						"  |-- CLASS_LIST\n  |  |-- CLASS_PATH (test_class) [localPath = null class = test_class]\n"+
						"  |-- WHERE\n  |  |-- EQUALS\n  |  |  |-- IDENTIFIER (k1) [path = null class = null property = k1]\n"+
						"  |  |  |-- VARIABLE ($key$)[STRING key11]",
						query.toString());
	}

	/**
	 * Test method for {@link net.aifusion.cql.CimQuery#executeQuery(net.aifusion.metamodel.Repository)}.
	 */
	@Test
	public final void testExecuteQuery() {
		for(TestQuery q : queries){
			CimQuery query = new CimQuery(q.query);
			assertNotNull(query);
			if(verbose) System.out.println(query.toString());
			try {
				List<StructureValue> found = query.executeQuery(cache);
				if(verbose) {
					System.out.println("**\t-------ResultSet-------");
					for(StructureValue e : found){
						System.out.println(e.getObjectPath()+"\n"+e.toMOF());
					}
				}
				
				assertEquals(q.resultSize,found.size());
				assertEquals(0,q.errorCode);
			} catch (ModelException ex){
				if(verbose) System.out.println(ex.toString());
				assertEquals(q.errorCode, ex.getReason().getCode());
			}
		}
	}
}
