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
 * Created Nov 12, 2017 by sharad
 */
package net.aifusion.cql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test Alias
 * @author Sharad Singhal
 *
 */
public class AliasTest {
	/**
	 * Class to define a test query
	 */
	public static class TestQuery {
		String query;
		Select select;
		Alias alias;
		String [] aliases;
		/**
		 * Create a test query
		 * @param query - query definition
		 */
		public TestQuery(String query,String [] aliases){
			this.query = query;
			this.aliases = aliases;
		}
	}
	
	static TestQuery [] queries = {
			new TestQuery("select property AS p1 from cim_device",new String []{"p1"}),			// selectedEntry = expr AS Identifier
			new TestQuery("select objectpath(cim_class) AS foo FROM CIM_CLASS",new String []{"foo"}),
			new TestQuery("select Cim_SystemDevice.property AS p1 from cim_device",new String []{"p1"}),
			new TestQuery("SELECT OBJECTPATH(CIM_StorageExtent) AS Path, ElementName,   Caption  FROM CIM_StorageExtent",new String []{"Path"}),
			new TestQuery("SELECT OBJECTPATH(CIM_LogicalDevice) AS Path, CIM_LogicalDevice.OperationalStatus[*] "+     
					"FROM CIM_LogicalDevice, CIM_ComputerSystem, CIM_SystemDevice "+  
					"WHERE CIM_ComputerSystem.ElementName = 'MySystemName' "+    
					"AND CIM_SystemDevice.GroupComponent =  OBJECTPATH(CIM_ComputerSystem) "+     
					"AND CIM_SystemDevice.PartComponent =  OBJECTPATH(CIM_LogicalDevice) "+
					"AND ANY CIM_LogicalDevice.OperationalStatus[*] <> 2",new String []{"Path"}),
			new TestQuery("SELECT CIM_ComputerSystem.*, OBJECTPATH(CIM_ManagedElement)   AS   MEObjectName "+  
					"FROM CIM_ComputerSystem, CIM_ManagedElement, CIM_Dependency "+   
					"WHERE CIM_Dependency.Antecedent = OBJECTPATH(CIM_ComputerSystem) "+        
					"AND CIM_Dependency.Dependent = OBJECTPATH(CIM_ManagedElement) ",new String []{"MEObjectName"}),
			new TestQuery("SELECT OBJECTPATH(CIM_ComputerSystem) AS CSOBJECTPATH,CIM_BaseMetricValue.* FROM CIM_ComputerSystem,CIM_BaseMetricValue,CIM_MetricForME "+
					"WHERE CIM_ComputerSystem.Name = 'MySystem1' "+
					"AND CIM_BaseMetricValue.TimeStamp > DATETIME('200407101000********+300') "+
					"AND CIM_BaseMetricValue.TimeStamp < DATETIME('200407101030********+300') "+
					"AND CIM_BaseMetricValue.Duration = DATETIME('000000000005********:000') "+
					"AND CIM_MetricForME.Antecedent = OBJECTPATH(CIM_ComputerSystem) "+
					"AND CIM_MetricForME.Dependent = OBJECTPATH(CIM_BaseMetricValue)",new String []{"CSOBJECTPATH"}),
			new TestQuery("SELECT SD.* "+
					"FROM CIM_SettingData CSD, CIM_SettingData SD, CIM_ManagedSystemElement MSE, CIM_ElementSettingData ESD, CIM_ConcreteComponent CC "+
					"WHERE OBJECTPATH(MSE) = 'some desired key' AND ESD.ManagedElement = OBJECTPATH(MSE) AND ESD.SettingData = OBJECTPATH(CSD) "+
					"AND CC.GroupComponent = OBJECTPATH(CSD) AND CC.PartComponent = OBJECTPATH(SD) ",new String []{"CSD","SD","MSE","ESD","CC"}),
			new TestQuery("SELECT EventTime,AlertingManagedElement,PerceivedSeverity,ProbableCause, 'HP12345' AS FilterID "+
					"FROM CIM_AlertIndication "+
					"WHERE AlertingManagedElement = '/dev/tty0p1' "+
					"AND ProbableCause = 20 ",new String []{"FilterID"}),
			// sub queries
			new TestQuery("SELECT * FROM CIM_LogicalDevice p1, (SELECT ObjectPath(p1) p3 FROM CIM_Device) p_s "
					+ "WHERE EVERY p_s.OperationalStatus[0,1..4,6..] > 2",new String []{"p1","p3","p_s"}),	// expr,expr...
			new TestQuery("SELECT OBJECTPATH(pms) AS PrivilegeMgmtServiceInst, OBJECTPATH(hi) "
					+ "AS StorageHardwareIDInst, OBJECTPATH(p) AS AuthorizedPrivilegeInst, OBJECTPATH(v) AS StorageVolumeInst "
					+ "FROM CIM_InstModification im, CIM_StorageHardwareID hi, CIM_AuthorizedSubject s, CIM_AuthorizedPrivilege p, "
					+ "CIM_AuthorizedTarget t, CIM_StorageVolume v, CIM_SystemDevice sd, CIM_ComputerSystem cs, CIM_HostedService hs, "
					+ "CIM_PrivilegeManagementService pms "
					+ "WHERE im.SourceInstance ISA CIM_FCPort AND ANY im.SourceInstance.CIM_FCPort::OperationalStatus[*] <> #'OK' "
					+ "AND im.SourceInstance.CIM_FCPort::PermanentAddress = hi.StorageID AND s.PrivilegedElement = OBJECTPATH(hi) "
					+ "AND s.Privilege = OBJECTPATH(p) AND t.Privilege = OBJECTPATH(p) AND t.TargetElement = OBJECTPATH(v) "
					+ "AND sd.PartComponent = OBJECTPATH(v) AND sd.GroupComponent = OBJECTPATH(cs) "
					+ "AND hs.Antecedent = OBJECTPATH(cs) AND hs.Dependent = OBJECTPATH(pms)",new String []{"im"}),
			new TestQuery("SELECT "+
					"InstanceOf( "+
					"SourceInstance.CIM_SharedElement::SameElement) "+
					"AS FileShare "+
					"FROM CIM_InstCreation "+
					"WHERE SourceInstance ISA CIM_SharedElement "+
					"AND InstanceOf(SourceInstance.CIM_SharedElement::SameElement) "+
					"ISA CIM_FileShare",new String []{"FileShare"}),
			new TestQuery("SELECT OBJECTPATH(pms) AS PrivilegeMgmtServiceInst, "+
					"Oh AS StorageHardwareIDInst, Op AS AuthorizedPrivilegeInst, "+
					"Ov AS StorageVolumeInst "+
					"FROM CIM_HostedService hs, "+
					"CIM_PrivilegeManagementService pms, "+
					"( SELECT OBJECTPATH(cs) AS Oc, O.Op, O.Oh, O.Ov "+
					"FROM CIM_ComputerSystem cs, CIM_SystemDevice sd, "+
					"( SELECT OBJECTPATH(v) AS Ov, P.Op, P.Oh "+
					"FROM CIM_AuthorizedTarget t, "+
					"CIM_StorageVolume v, "+
					"( SELECT OBJECTPATH(p) AS Op, "+
					"OBJECTPATH(hi) AS Oh "+
					"FROM CIM_StorageHardwareID hi, CIM_AuthorizedPrivilege p, "+
					"CIM_AuthorizedSubject s, "+
					"( SELECT SourceInstance.CIM_FCPort ::PermanentAddress "+
					"FROM CIM_InstModification "+
					"WHERE SourceInstance ISA CIM_FCPort "+
					"AND ANY SourceInstance.CIM_FCPort::OperationalStatus[*] "+
					"<> #'OK' "+
					") f_c "+
					"WHERE fc.PermanentAddress = hi.StorageID "+
					"AND s.PrivilegedElement = OBJECTPATH(hi) "+
					"AND s.Privilege = OBJECTPATH(p) "+
					") P_S "+
					"WHERE t.Privilege = P.Op AND t.TargetElement = OBJECTPATH(v) "+
					") O_S "+
					"WHERE sd.PartComponent = Ov "+
					"AND sd.GroupComponent = OBJECTPATH(cs) "+
					") C_S "+
					"WHERE hs.Antecedent = Oc AND hs.Dependent = OBJECTPATH(pms) ",new String []{"f_c"}),
			// arith (* /)
			new TestQuery("SELECT OBJECTPATH(IM.SourceInstance) AS NeedySPPath "+
					"FROM CIM_InstModification AS IM, "+
					"CIM_PolicyRule AS PR, "+
					"CIM_PolicySetAppliesToElement AS PSATE "+
					"WHERE IM.SourceInstance ISA CIM_StoragePool "+
					"AND PR.Name = 'AllocateMoreSpace' "+
					"AND OBJECTPATH(PR) = PSATE.PolicySet "+
					"AND OBJECTPATH(IM.SourceInstance) = PSATE.ManagedElement "+
					"AND 100 * (IM.SourcInstance. CIM_StoragePool::RemainingManagedSpace / IM.SourcInstance. CIM_StoragePool::TotalManagedSpace) < 10 "+
					"AND IM.SourcInstance. CIM_StoragePool::RemainingManagedSpace <> "+
					"IM.PreviousInstance. CIM_StoragePool::RemainingManagedSpace",new String []{"PR"}),
			// satisfies
			new TestQuery("SELECT s.Name, d.Name "+
					"FROM CIM_System s, CIM_SystemDevice sd, CIM_LogicalDevice d "+
					"WHERE OBJECTPATH(s) = sd.GroupComponent "+
					"AND OBJECTPATH(d) = sd.PartComponent "+
					"AND ANY i IN s.OperationalStatus[*] SATISFIES "+
					"(i = #'Non-Recoverable Error' OR i=#'Degraded') "+
					"AND ANY j in d.OperationalStaus[*] SATISFIES (j =#'Degraded' )",new String []{"s"}),
			// x.y.z
			new TestQuery("SELECT OBJECTPATH(SCS) || '.CreateOrModifyStoragePool' "+
					"AS MethodName, "+
					"QCR.NeedySPPath AS Pool, "+
					"QCR.NeedySPPath.Size + (QCR.TotalManagedSpace / 10) AS Size, "+
					"OBJECTPATH(SP) AS InPools "+
					"FROM PR_Needy AS QCR, "+
					"CIM_ServiceAffectsElement AS SAE, "+
					"CIM_StorageConfigurationService AS SCS, "+
					"CIM_StoragePool AS SP, "+
					"CIM_AllocatedFromStoragePool AS AFSP "+
					"WHERE QCR.NeedySPPath = SAE.AffectedElement "+
					"AND OBJECTPATH(SCS) = SAE.AffectingElement "+
					"AND SP.ElementName = 'FreePool' "+
					"AND QCR.NeedySPPath = AFSP.Antecedent "+
					"AND OBJECTPATH(SP) = AFSP.Dependent",new String []{"Size"}),
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
		System.out.print("Alias ");
		QueryParser parser = new QueryParser();
		for(TestQuery q : queries){
			// System.out.println(q.query);
			q.select = (Select) parser.parse(q.query);
			// System.out.println(toTree(q.select,""));
			boolean found = false;
			for(Node child : q.select.getChildren()){
				if(child.getOperator() == Operator.ALIAS) {
					q.alias = (Alias) child;
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}
	
	/**
	 * Create a tree representation of all nodes in the query parse tree
	 * @param n - current node to be represented
	 * @param indent - current level of indent
	 * @return - string containing subtree
	 */
	private static String toTree(Node n, String indent){
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

	/**
	 * Test method for {@link net.aifusion.cql.Alias#locateAliasNode(java.lang.String)}.
	 */
	@Test
	public void testLocateAliasNode() {
		for(TestQuery q : queries){
			// System.out.println(q.query);
			// System.out.println(toTree(q.alias,""));
			for(String s : q.aliases){
				Node n = q.alias.locateAliasNode(s);
				if(n == null){
					System.out.println(q.query);
					System.out.println(toTree(q.select,""));
					fail();
				}
				// System.out.println(s+":"+n.toString()+"\n");
				assertEquals(s,n.getAlias());
			}
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.cql.Alias#getClassName(java.lang.String)}.
	 */
	@Test
	public void testGetClassName() {
		for(TestQuery q : queries){
			// System.out.println(q.query);
			// System.out.println(toTree(q.alias,""));
			for(String s : q.aliases){
				Node n = q.alias.locateAliasNode(s);
				// System.out.println(s+":"+n.toString()+"\n");
				String className = q.alias.getClassName(s);
				// System.out.println(s+":"+className+"\n");
				switch(n.getOperator()){
				case PROPERTY_NAME:
					assertEquals(className,((PropertyName)n).getClassName());
					break;
				case IDENTIFIER:
					assertEquals(className,((Identifier)n).getClassName());
					break;
				case CLASS_PATH:
					assertEquals(className,((ClassPath)n).getClassName());
					break;
				case SELECT:
					assertEquals(className,n.getAlias());
					break;
				default:
					assertEquals(null,className);
					break;
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Alias#getLocalPath(java.lang.String)}.
	 */
	@Test
	public void testGetLocalPath() {
		for(TestQuery q : queries){
			// System.out.println(q.query);
			// System.out.println(toTree(q.alias,""));
			for(String s : q.aliases){
				Node n = q.alias.locateAliasNode(s);
				// System.out.println(s+":"+n.toString()+"\n");
				String path = q.alias.getLocalPath(s);
				// System.out.println(s+": "+path+"\n");
				switch(n.getOperator()){
				case PROPERTY_NAME:
					assertEquals(path,((PropertyName)n).getLocalPath());
					break;
				case IDENTIFIER:
					assertEquals(path,((Identifier)n).getLocalPath());
					break;
				case CLASS_PATH:
					assertEquals(path,((ClassPath)n).getLocalPath());
					break;
				default:
					assertEquals(null,path);
					break;
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Alias#getPropertyName(java.lang.String)}.
	 */
	@Test
	public void testGetPropertyName() {
		for(TestQuery q : queries){
			// System.out.println(q.query);
			// System.out.println(toTree(q.alias,""));
			for(String s : q.aliases){
				Node n = q.alias.locateAliasNode(s);
				// System.out.println(s+":"+n.toString()+"\n");
				String propertyName = q.alias.getPropertyName(s);
				// System.out.println(s+": "+propertyName+"\n");
				switch(n.getOperator()){
				case PROPERTY_NAME:
					assertEquals(propertyName,((PropertyName)n).getPropertyName());
					break;
				case IDENTIFIER:
					assertEquals(propertyName,((Identifier)n).getPropertyName());
					break;
				case CLASS_PATH:
				case SELECT:
					assertEquals(null,propertyName);
					break;
				default:
					assertEquals(s,propertyName);
					break;
				}
			}
		}
	}

}
