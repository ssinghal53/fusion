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
 * Created Aug 21, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.ModelException;

/**
 * @author sharad
 *
 */
public class QueryParserTest {
	/**
	 * Class to define a test query
	 */
	public class TestQuery {
		String query;
		boolean isValid;
		int code;
		/**
		 * Create a test query
		 * @param query - query definition
		 * @param isValid - flag to indicate if this query is a valid syntax
		 * @param code - if query is not valid, expected ExceptionReason code
		 */
		public TestQuery(String query, boolean isValid, int code){
			this.query = query;
			this.isValid = isValid;
			this.code = code;
		}
	}
	
	static boolean verbose = false;
	
	TestQuery [] queries = {
			new TestQuery("select * from cim_class",true,0),						// selectedEntry = "*"
			new TestQuery("( select * from cim_class ) f_c", true, 0),				// select with named result_class
			new TestQuery("select property from cim_device",true,0),				// selectedEntry = expr
			new TestQuery("select property AS p1 from cim_device",true,0),			// selectedEntry = expr AS Identifier
			new TestQuery("select property p1 from cim_device",true,0),				// selectedEntry = expr Identifier
			new TestQuery("select cim_device.* from cim_class",true,0),				// selectedEntry = chain '.' '*'
			new TestQuery("select p1, p2 from cim_device",true,0),					// selectEntry , selectEntry
			new TestQuery("select p1#'value' from cim_device",true,0),				// identifier#literalString
			new TestQuery("select cim_SystemDevice.* from cim_device",true,0),
			new TestQuery("select ObjectPath('cim_class') from cim_device",false,1),	// Alias required for functions in SELECT
			new TestQuery("select objectpath(cim_class) AS foo FROM CIM_CLASS",true, 0),
			new TestQuery("select Cim_SystemDevice.property AS p1 from cim_device",true,0),
			new TestQuery("SELECT * FROM CIM_AlertIndication WHERE AlertType > CIM_LogicalDevice.OperationalStatus#'OK'",true,0),
			new TestQuery("SELECT OBJECTPATH(CIM_StorageExtent) AS Path, ElementName,   Caption  FROM CIM_StorageExtent",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE CIM_LogicalDevice ISA CIM_StorageExtent OR CIM_LogicalDevice ISA CIM_MediaAccessDevice",true,0),
			new TestQuery("SELECT OBJECTPATH(CIM_LogicalDevice) AS Path, CIM_LogicalDevice.OperationalStatus[*] "+     
					"FROM CIM_LogicalDevice, CIM_ComputerSystem, CIM_SystemDevice "+  
					"WHERE CIM_ComputerSystem.ElementName = 'MySystemName' "+    
					"AND CIM_SystemDevice.GroupComponent =  OBJECTPATH(CIM_ComputerSystem) "+     
					"AND CIM_SystemDevice.PartComponent =  OBJECTPATH(CIM_LogicalDevice) "+
					"AND ANY CIM_LogicalDevice.OperationalStatus[*] <> 2",true,0),
			new TestQuery("SELECT CIM_ComputerSystem.*, OBJECTPATH(CIM_ManagedElement)   AS   MEObjectName "+  
					"FROM CIM_ComputerSystem, CIM_ManagedElement, CIM_Dependency "+   
					"WHERE CIM_Dependency.Antecedent = OBJECTPATH(CIM_ComputerSystem) "+        
					"AND CIM_Dependency.Dependent = OBJECTPATH(CIM_ManagedElement) ",true,0),
			new TestQuery("SELECT OBJECTPATH(CIM_ComputerSystem) AS CSOBJECTPATH,CIM_BaseMetricValue.* "
					+ "FROM CIM_ComputerSystem,CIM_BaseMetricValue,CIM_MetricForME "+
					"WHERE CIM_ComputerSystem.Name = 'MySystem1' "+
					"AND CIM_BaseMetricValue.TimeStamp > DATETIME('200407101000********+300') "+
					"AND CIM_BaseMetricValue.TimeStamp < DATETIME('200407101030********+300') "+
					"AND CIM_BaseMetricValue.Duration = DATETIME('000000000005********:000') "+
					"AND CIM_MetricForME.Antecedent = OBJECTPATH(CIM_ComputerSystem) "+
					"AND CIM_MetricForME.Dependent = OBJECTPATH(CIM_BaseMetricValue)",true,0),
			new TestQuery("SELECT SD.* "+
					"FROM CIM_SettingData CSD, CIM_SettingData SD, CIM_ManagedSystemElement MSE, "
					+ "CIM_ElementSettingData ESD, CIM_ConcreteComponent CC "+
					"WHERE OBJECTPATH(MSE) = 'some desired key' AND ESD.ManagedElement = "
					+ "OBJECTPATH(MSE) AND ESD.SettingData = OBJECTPATH(CSD) "+
					"AND CC.GroupComponent = OBJECTPATH(CSD) AND CC.PartComponent = OBJECTPATH(SD) ",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[*] <> 2",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[*] = 2",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[*] < 2",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE ANY CIM_LogicalDevice.OperationalStatus[*] > 2",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE ANY CIM_LogicalDevice.OperationalStatus[*] <> 2",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE NOT EVERY CIM_LogicalDevice.OperationalStatus[*] = 2",true,0),
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE CIM_LogicalDevice.OperationalStatus[0] = 2",true,0),
			new TestQuery("SELECT *	FROM CIM_InstCreation WHERE SourceInstance ISA CIM_FCPort",true,0),
			new TestQuery("SELECT EventTime, AlertingManagedElement, PerceivedSeverity,	ProbableCause "+
					"FROM CIM_AlertIndication "+
					"WHERE AlertingManagedElement = '/dev/tty0p1' "+
					"AND ProbableCause = 20 ",true,0),
			new TestQuery("SELECT EventTime,AlertingManagedElement,PerceivedSeverity,ProbableCause, 'HP12345' AS FilterID "+
					"FROM CIM_AlertIndication "+
					"WHERE AlertingManagedElement = '/dev/tty0p1' "+
					"AND ProbableCause = 20 ",true,0),
			// note that expressions in SelectList require an alias
			new TestQuery("SELECT EventTime, AlertingManagedElement, 1 = PerceivedSeverity, 'Critical' = OtherSeverity, "
					+ "ProbableCause, 'HP12345' AS FilterID "+
					"FROM CIM_AlertIndication "+
					"WHERE AlertingManagedElement = '/dev/tty0p1' AND ProbableCause = 20",false,1),
			// extended index queries
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[] > 2",true,0),				// empty
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[*] > 2",true,0),				// *
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[1..] > 2",true,0),			// expr .. 
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[..3] > 2",true,0),			// .. expr
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[0..3] > 2",true,0),			// expr .. expr
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[0,1..4,6..] > 2",true,0),	// expr,expr...
			// vector compares
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[0,2..4,6] > {2,3,4,5,6}",true,0),	// expr,expr...
			// sub queries
			new TestQuery("SELECT * FROM CIM_LogicalDevice p1, (SELECT ObjectPath(p1) p3 FROM CIM_Device) p_2 "
					+ "WHERE EVERY p_2.OperationalStatus[0,1..4,6..] > 2",true,0),	// expr,expr...
			// classPath
			new TestQuery("SELECT * FROM /class/path:CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[] > 2",true,0),		// empty
			new TestQuery("SELECT * FROM /class/path:CIM_LogicalDevice WHERE EVERY CIM_LogicalDevice.OperationalStatus[] LIKE 'abc'",true,0),		// empty
			new TestQuery("select cim_device::property.* from cim_class",true,0),
			new TestQuery("SELECT OBJECTPATH(pms) AS PrivilegeMgmtServiceInst, OBJECTPATH(hi) "
					+ "AS StorageHardwareIDInst, OBJECTPATH(p) AS AuthorizedPrivilegeInst, OBJECTPATH(v) AS StorageVolumeInst "
					+ "FROM CIM_InstModification im, CIM_StorageHardwareID hi, CIM_AuthorizedSubject s, CIM_AuthorizedPrivilege p, "
					+ "CIM_AuthorizedTarget t, CIM_StorageVolume v, CIM_SystemDevice sd, CIM_ComputerSystem cs, CIM_HostedService hs, "
					+ "CIM_PrivilegeManagementService pms "
					+ "WHERE im.SourceInstance ISA CIM_FCPort AND ANY im.SourceInstance.CIM_FCPort::OperationalStatus[*] <> #'OK' "
					+ "AND im.SourceInstance.CIM_FCPort::PermanentAddress = hi.StorageID AND s.PrivilegedElement = OBJECTPATH(hi) "
					+ "AND s.Privilege = OBJECTPATH(p) AND t.Privilege = OBJECTPATH(p) AND t.TargetElement = OBJECTPATH(v) "
					+ "AND sd.PartComponent = OBJECTPATH(v) AND sd.GroupComponent = OBJECTPATH(cs) "
					+ "AND hs.Antecedent = OBJECTPATH(cs) AND hs.Dependent = OBJECTPATH(pms)",true,0),
			new TestQuery("SELECT *	FROM CIM_InstModification "+
					"WHERE SourceInstance ISA CIM_FCPort "+
					"AND PreviousInstance ISA CIM_FCPort "+
					"AND SourceInstance.CIM_FCPort::OperationalStatus[0] <> "+
					"PreviousInstance.CIM_FCPort::OperationalStatus[0] ",true,0),
			new TestQuery("SELECT SourceInstance.CIM_LogicalDevice::DeviceID, "+
					"SourceInstance.CIM_ManagedSystemElement::OperationalStatus "+           
					"FROM CIM_InstIndication "+
					"WHERE  SourceInstance ISA CIM_LogicalDevice  OR SourceInstance ISA CIM_PhysicalElement",true,0),
			new TestQuery("SELECT "+
					"InstanceOf( "+
					"SourceInstance.CIM_SharedElement::SameElement) "+
					"AS FileShare "+
					"FROM CIM_InstCreation "+
					"WHERE SourceInstance ISA CIM_SharedElement "+
					"AND InstanceOf(SourceInstance.CIM_SharedElement::SameElement) "+
					"ISA CIM_FileShare",true,0),
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
					") f_s "+
					"WHERE fc.PermanentAddress = hi.StorageID "+
					"AND s.PrivilegedElement = OBJECTPATH(hi) "+
					"AND s.Privilege = OBJECTPATH(p) "+
					") P_s "+
					"WHERE t.Privilege = P.Op AND t.TargetElement = OBJECTPATH(v) "+
					") O_s "+
					"WHERE sd.PartComponent = Ov "+
					"AND sd.GroupComponent = OBJECTPATH(cs) "+
					") C_s "+
					"WHERE hs.Antecedent = Oc AND hs.Dependent = OBJECTPATH(pms) ",true,0),
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
					"IM.PreviousInstance. CIM_StoragePool::RemainingManagedSpace",true,0),

			// satisfies
			new TestQuery("SELECT * FROM CIM_LogicalDevice WHERE ANY stat IN CIM_LogicalDevice.OperationalStatus[*] SATISFIES (stat = 3 OR stat > 5)",true,0),
			// DISTINCT
			new TestQuery("SELECT DISTINCT OBJECTPATH(sv) AS VolumePath, "+
					"(sv.BlockSize * sv.NumberOfBlocks) AS Size "+
					"FROM CIM_StorageVolume sv, "+
					"( SELECT MAX(v.BlockSize*v.NumberOfBlocks) AS Maxbytes "+
					"FROM CIM_StorageVolume v) m_v "+
					"WHERE (sv.BlockSize * sv.NumberOfBlocks) = mv.Maxbytes ",false,15),
			// satisfies
			new TestQuery("SELECT s.Name, d.Name "+
					"FROM CIM_System s, CIM_SystemDevice sd, CIM_LogicalDevice d "+
					"WHERE OBJECTPATH(s) = sd.GroupComponent "+
					"AND OBJECTPATH(d) = sd.PartComponent "+
					"AND ANY i IN s.OperationalStatus[*] SATISFIES "+
					"(i = #'Non-Recoverable Error' OR i=#'Degraded') "+
					"AND ANY j in d.OperationalStaus[*] SATISFIES (j =#'Degraded' )",true,0),
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
					"AND OBJECTPATH(SP) = AFSP.Dependent",true,0),
			// illegal className (StoragePool, AllocatedFromStorePool) (Also extended chain to method parameters - not handled)
			new TestQuery("SELECT MAR.MethodName, "+
					"MAR.MethodParameters.Pool, "+
					"MAR.MethodParameters.Size, "+
					"OBJECTPATH(SP) AS InPools "+
					"FROM PR_ModifySP MAR, "+
					"StoragePool SP, "+
					"AllocatedFromStoragePool AFSP "+
					"WHERE MAR.ResultValue <> '0' "+
					"AND SP.ElementName = 'SafetyPool' "+
					"AND MAR.MethodParameters ISA __MethodParameters "+
					"AND MAR.MethodParameters.__MethodParameters::Pool = AFSP.Antecedent "+
					"AND OBJECTPATH(SP) = AFSP.Dependent",false,13),
			new TestQuery("Select * from cim_class where property = $var$",true,0),
			new TestQuery("Select FIRST 3 DISTINCT * from cim_class where property1 > 5 Order by Property1 ASC, Property2 DESC",true,0)
	};
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("QueryParser ");
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

	/**
	 * Test method for {@link net.aifusion.cql.QueryParser#parse(java.lang.String)}.
	 */
	@Test
	public final void testParse() {
		QueryParser parser = new QueryParser();
		for(TestQuery q : queries){
			assertNotNull(q);
			if(verbose) System.out.println(q.query);
			try {
				Node select = parser.parse(q.query);
				assertNotNull(select);
				if(verbose) System.out.println(q.query+"\n"+toTree(select,""));
				assertTrue(q.isValid);
			} catch (ModelException e){
				if(verbose) System.out.println(e);
				assertFalse(q.isValid);
				assertEquals(q.code, e.getReason().getCode());
			}
			if(verbose) System.out.println("-----------------------------\n");
		}
		// TODO: Need to validate parsed trees here, not just that the query parser succeeded
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
				b.append(c == null ? "|-- Null" : toTree(c,indent+"   |"));
			}
			
		}
		return b.toString();
	}

}
