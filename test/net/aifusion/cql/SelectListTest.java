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
 * Created Sep 30, 2017 by sharad
 */
package net.aifusion.cql;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.Set;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to test SelectList
 * @author Sharad Singhal
 */
public class SelectListTest {
	public static class TestQuery {
		String query;
		Select select;
		int resultSize;
		int pSize;
		int exceptionNumber;
		String [] pNames;
		public TestQuery(String query, int exceptionNumber, int resultSize, int pSize, String [] props){
			this.query = query;
			this.exceptionNumber = exceptionNumber;
			this.resultSize = resultSize;
			this.pSize = pSize;
			this.pNames = props;
		}
	}
	private static InMemoryCache cache = new InMemoryCache();
	private static boolean verbose = false;
	private static String mof = "class Test_Class { [key] String p1; String [] p2;};\n"+
			"instance of Test_Class {p1 = \"key11\"; p2 = {\"s2\"}; };\n"+
			"instance of Test_Class {p1 = \"key12\"; p2 = {\"s3\"}; };\n"+
			"instance of Test_Class {p1 = \"key13\"; p2 = {\"s2\"}; };\n"+
			
			"class Test_Class2 { [key] String p2; String p3; Test_Class Ref tc; };\n"+
			"instance of Test_Class2 {p2 = \"key21\"; p3 = \"s1\"; tc = \"/cimfusion:Test_Class.p1=\\\"key11\\\"\";};\n"+
			"instance of Test_Class2 {p2 = \"key22\"; p3 = \"s2\"; };\n"+
			"instance of Test_Class2 {p2 = \"key33\"; p3 = \"s3\"; };\n"+
			
			"#pragma namespace(\"/local\")\n"+
			"Enumeration Disk_Enum : String { SSD, HD, DVD };\n"+
			"Enumeration Server_Enum : String { db, app, web };\n"+
			"[Version(\"1.0.0\")] Structure Test_Struct {[Write] STRING MountPoint; [Key] STRING ID; Real64 UsedCapacity; Disk_Enum type;};"+
			"Class Test_Disk : Test_Struct { BOOLEAN dismount(); BOOLEAN mount();};\n"+
			"Class Test_Computer {[Write] STRING HostName; [Key] STRING address; Boolean isPublic; Test_Disk ref disk;};"+
			"Class App_Server : Test_Computer {Server_Enum type; String software[]; DateTime installDate;};"+
			
			"instance of Test_Disk as $disk1 { ID = \"id1\"; MountPoint = \"sup1\"; UsedCapacity = 32;};"+
			"instance of Test_Disk as $disk2 { ID = \"id2\";};"+
			"instance of Test_Computer as $c1 { address = \"192.168.1.1\"; disk = $disk1;};"+
			"instance of App_Server as $c2 { address = \"192.168.1.2\"; isPublic = true; disk = $disk1; type = \"web\"; software = {\"apache\", \"tomcat\"}; installDate = \"20120910123400.000000+740\";};"
			;
	
	private static TestQuery [] queries = {
			// query, expected error, number of instances returned, number of properties, property names
			new TestQuery("Select * from Test_Class",0,3,2,new String [] {"p1","p2"}),
			/*
			new TestQuery("Select * from Test_Class2",0,3,3,new String [] {"p2","p3","tc"}),
			new TestQuery("Select * from Test_Disk",0,2,4,new String [] {"mountpoint","id","usedcapacity","type"}),
			new TestQuery("Select * from Test_Class as C1",0,3,2,new String [] {"p1","p2"}),
			new TestQuery("Select * from /local:Test_Disk",0,2,4,new String [] {"mountpoint","id","usedcapacity","type"}),
			new TestQuery("Select * from /local:App_Server",0,1,7,new String [] {"hostname","address","ispublic","disk","type","software","installdate"}),
			new TestQuery("Select * from Test_Class,Test_Class2",0,9,5,new String [] {"rowid","p1","p2","p3","tc"}),
			new TestQuery("Select p1 as x1 from Test_Class",0,3,2,new String [] {"rowid","x1"}),
			new TestQuery("Select p1,tc.p3 as p4 from Test_Class,Test_Class2 as tc",0,9,3,new String [] {"rowid","p1","p4"}),
			new TestQuery("Select C1.p1 as x1 from Test_Class as C1",0,3,2,new String [] {"rowid","x1"}),
			
			// sub query
			new TestQuery("select t_c.address from (select * from /local:Test_computer) t_c",0,2,2,new String [] {"rowid","address"}),
			
			// property expression
			new TestQuery("select p1, ('key' || p1) as f from Test_Class",0,3,3,new String [] {"rowid","p1","f"}),
			*/
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
		System.out.print("SelectList ");
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mof.getBytes()), Constants.defaultNameSpacePath);
		
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		// for(NamedElement e : elements) System.out.println(e.toMOF());
		assertEquals(18,elements.size());
		
		QueryParser queryParser = new QueryParser();
		for(TestQuery q : queries){
			q.select = (Select) queryParser.parse(q.query);
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
	
	@Test
	public final void testSelectList(){
		for(TestQuery q : queries){
			if(verbose) {
			System.out.println(q.query.toString());
			System.out.println(toTree(q.select,""));
			}
			try {
				List<StructureValue> output = q.select.evaluate(new BufferedCache(cache));
				for(StructureValue i : output){
					if(verbose) System.out.println(i.toMOF());
					Set<String> n = i.getPropertyNames();
					assertEquals(q.pSize,n.size());
					for(String s : q.pNames){
						assertTrue(n.contains(s));
					}
				}
				assertEquals(q.resultSize,output.size());
			} catch(ModelException e){
				assertEquals(q.exceptionNumber,e.getReason().getCode());
			}
		}
		return;
	}
	
}
