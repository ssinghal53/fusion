/**
 * Copyright 2017 Sharad Singhal, All Rights Reserved
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
 * Created Sep 29, 2017 by sharad
 */
package net.aifusion.cql;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to test ClassList
 * @author Sharad Singhal
 */
public class ClassListTest {
	public static class TestQuery {
		String query;
		boolean isValid;
		String headers;
		int [] resultSize;
		/**
		 * Create a test query
		 * @param query - query definition
		 * @param headers - expected header values
		 * @param resultSize - size of the result expected
		 */
		public TestQuery(String query, String headers,int [] resultSize){
			this.query = query;
			this.headers = headers;
			this.resultSize = resultSize;
		}
	}
	private static boolean verbose = false;
	private static String [] mofDefinition = {
			"Enumeration Disk_Enum : String { SSD, HD, DVD };\n",
			"Enumeration Server_Enum : String { db, app, web };\n",
			"[Version(\"1.0.0\")] Structure Test_Struct {[Write] STRING MountPoint; [Key] STRING ID; Real64 UsedCapacity; Disk_Enum type;};",
			"Class Test_Disk : Test_Struct { BOOLEAN dismount(); BOOLEAN mount();};\n",
			"Class Test_Computer {[Write] STRING HostName; [Key] STRING address; Boolean isPublic; Test_Disk ref disk;};",
			"Class App_Server : Test_Computer {Server_Enum type; String software[]; DateTime installDate;};",

			"instance of Test_Disk as $disk1 { ID = \"id1\"; MountPoint = \"sup1\"; UsedCapacity = 32;};",
			"instance of Test_Disk as $disk2 { ID = \"id2\";};",
			"instance of Test_Computer as $c1 { address = \"192.168.1.1\"; disk = $disk1;};",
			"instance of App_Server as $c2 { address = \"192.168.1.2\"; isPublic = true; disk = $disk1; type = \"web\"; software = {\"apache\", \"tomcat\"}; installDate = \"20120910123400.000000+740\";};",

			"class test_class { [key] String K1; Sint64 D1; DateTime DT1; String S1; };\n",
			"class test_class2 { [key] String K2; Sint64 D2; DateTime DT2; String S2; };\n",
			"class test_class3 { [key] String K3; Sint64 D3; DateTime DT3; String S3; };\n",
			"class test_class4 : test_class { Boolean D4; Real64 R4; Sint32 [] I4; };\n",

			"instance of test_class { K1 = \"key11\"; D1 = 54; DT1 = \"20140420044028.080***+000\"; S1 = \"instance11\";  };\n",
			"instance of test_class { K1 = \"key12\"; D1 = 55; DT1 = \"20150420044028.080***+000\"; S1 = \"instance12\";   };\n",
			"instance of test_class2 { K2 = \"key21\"; D2 = 64; S2 = \"instance21\"; };\n",
			"instance of test_class2 { K2 = \"key22\"; D2 = 65; S2 = \"instance22\"; };\n",
			"instance of test_class3 { K3 = \"key31\"; D3 = 74; S3 = \"instance31\"; };\n",
			"instance of test_class3 { K3 = \"key32\"; D3 = 75; S3 = \"instance32\"; };\n",
			"instance of test_class4 { K1 = \"key41\"; D1 = 33; S1 = \"instance41\"; I4 = {1, 2, 3, 4, 5, 6};};\n",
			"instance of test_class4 { K1 = \"key42\"; D1 = 21; S1 = \"instance42\"; I4 = {2, 4, 6, 8, 10, 12};};\n",

			"#pragma namespace (\"/root/local\")\n"+
					"Enumeration test_enum : string { enumvalue};\n"+
					"Structure test_struct { test_enum v;\n string foobar; };\n"+
					"class test_class {\n\t[key]\n\tSint32 integerProperty;\n\t[write]String stringProperty;\n\tboolean reset(String propertyName);\n};\n"+
					"instance of test_class {\n\tintegerProperty = 5;\n\tstringProperty = null;\n};\n"
	};

	private static InMemoryCache cache = new InMemoryCache();

	private static TestQuery [] queries = {
			new TestQuery("Select * from Test_Class","Test_Class",new int[]{5}),
			new TestQuery("Select * from Test_Class C1","C1",new int[]{5}),
			new TestQuery("Select * from Test_Class C1, Test_Class2 C2","C1,C2",new int[]{5,2}),
			new TestQuery("Select * from Test_Class C1,Test_Computer C2","C1,C2",new int[]{5,2}),
			new TestQuery("Select * from /root/local:Test_Class","/root/local:Test_Class",new int[]{1}),
			new TestQuery("Select * from Test_Class, (select * from App_Server) v_v","Test_Class,v_v",new int[]{5,1}),
			// new TestQuery("Select * from Test_Computer, (select * from App_Server, (select * from Test_Disk) T_D ) A_SD","T_C,v_v",new int[]{0,0})
	};

	private static Node [] select;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("ClassList ");
		QueryParser qp = new QueryParser();
		select = new Node[queries.length];
		for(int i = 0; i < queries.length; i++){
			select[i] = qp.parse(queries[i].query);
		}
		MOFParser mp = new MOFParser(cache);
		for(String mof : mofDefinition){
			mp.parse(new ByteArrayInputStream(mof.getBytes()), Constants.defaultNameSpacePath);
		}
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		assertEquals(26,elements.size());
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		if(cache != null) cache.shutdown();
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
	 * Test method for {@link net.aifusion.cql.ClassList#evaluate(net.aifusion.metamodel.Repository, List, java.util.HashMap)}.
	 */
	@Test
	public void testEvaluateRepositoryHashMapOfStringHashSetOfNamedElement() {
		HashMap<String, List<StructureValue>> workingSet = new LinkedHashMap<String,List<StructureValue>>();
		for(int i = 0; i < select.length; i++){
			if(verbose) {
				System.out.println(queries[i].query);
				System.out.println(toTree(select[i],"")+"\n");
			}
			Node classList = select[i].getChildren().get(1);
			assertEquals(Operator.CLASS_LIST,classList.getOperator());
			classList.evaluate(new BufferedCache(cache), workingSet);

			String [] keys = queries[i].headers.split(",");
			assertEquals(queries[i].resultSize.length,keys.length);
			assertEquals(keys.length,workingSet.keySet().size());
			int k = 0;
			for(String s : workingSet.keySet()){
				if(verbose)System.out.println(keys[k]+" "+s);
				assertEquals(keys[k],s);
				if(verbose) {
					System.out.println(s+" "+workingSet.get(s).size());
					for(NamedElement e : workingSet.get(s)){
						System.out.println(e.toMOF());
					}
				}
				assertEquals(queries[i].resultSize[k],workingSet.get(s).size());
				k++;
			}

			workingSet.clear();
		}
	}

	/**
	 * Create a tree representation of all nodes in the query parse tree
	 * @param n - current node to be represented
	 * @param indent - current level of indent
	 * @return - string containing subtree
	 */
	private String toTree(Node n, String indent){
		StringBuilder b = new StringBuilder(indent);
		if(!indent.isEmpty()) b.append("-- ");
		b.append(n.toString());
		if(n.hasChildren()){
			for(Node c : n.getChildren()){
				b.append("\n");
				b.append(c == null ? "|-- Null" : toTree(c,indent+"  |"));
			}
		}
		return b.toString();
	}
}
