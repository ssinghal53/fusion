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
 * Created Aug 20, 2016 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import net.aifusion.utils.Java2Cim;

/**
 * @author Sharad Singhal
 */
public class CimStructureTest {
	private static InMemoryRepository repository = new InMemoryRepository();
	private static CimStructure cimStructure;
	private static HashMap<String,DataType> types = new HashMap<String,DataType>();
	static {
		types.put("v01",DataType.BOOLEAN);
		types.put("v03",DataType.UINT8);
		types.put("v02",DataType.BOOLEAN);
		types.put("v05",DataType.UINT32);
		types.put("v04",DataType.UINT16);
		types.put("v06",DataType.UINT64);
		types.put("v07",DataType.SINT8);
		types.put("v08",DataType.SINT8);
		types.put("v09",DataType.SINT16);
		types.put("v10",DataType.SINT16);
		types.put("v11",DataType.SINT32);
		types.put("v12",DataType.SINT32);
		types.put("v13",DataType.SINT64);
		types.put("v14",DataType.SINT64);
		types.put("v15",DataType.REAL32);
		types.put("v16",DataType.REAL32);
		types.put("v17",DataType.REAL64);
		types.put("v18",DataType.REAL64);
		types.put("v19",DataType.CHAR16);
		types.put("v20",DataType.CHAR16);
		types.put("v21",DataType.STRING);
		types.put("v22",DataType.DATETIME);
		types.put("v23",DataType.OBJECTPATH);
		types.put("v24",DataType.ENUMERATIONVALUE);
		types.put("v25",DataType.STRUCTUREVALUE);
		types.put("v26",DataType.INSTANCEVALUE);
		types.put("v27",DataType.OCTETSTRING);
		types.put("v28",DataType.ENUMERATIONVALUE);
		types.put("v29",DataType.STRUCTUREVALUE);
		types.put("v30",DataType.INSTANCEVALUE);
		types.put("v31",DataType.STRING);
		
		types.put("va01",DataType.BOOLEAN_ARRAY);
		types.put("va02",DataType.BOOLEAN_ARRAY);
		types.put("va03",DataType.UINT8_ARRAY);
		types.put("va04",DataType.UINT16_ARRAY);
		types.put("va05",DataType.UINT32_ARRAY);
		types.put("va06",DataType.UINT64_ARRAY);
		types.put("va07",DataType.SINT8_ARRAY);
		types.put("va08",DataType.SINT8_ARRAY);
		types.put("va09",DataType.SINT16_ARRAY);
		types.put("va10",DataType.SINT16_ARRAY);
		types.put("va11",DataType.SINT32_ARRAY);
		types.put("va12",DataType.SINT32_ARRAY);
		types.put("va13",DataType.SINT64_ARRAY);
		types.put("va14",DataType.SINT64_ARRAY);
		types.put("va15",DataType.REAL32_ARRAY);
		types.put("va16",DataType.REAL32_ARRAY);
		types.put("va17",DataType.REAL64_ARRAY);
		types.put("va18",DataType.REAL64_ARRAY);
		types.put("va19",DataType.CHAR16_ARRAY);
		types.put("va20",DataType.CHAR16_ARRAY);
		types.put("va21",DataType.STRING_ARRAY);
		types.put("va22",DataType.DATETIME_ARRAY);
		types.put("va23",DataType.OBJECTPATH_ARRAY);
		types.put("va24",DataType.ENUMERATIONVALUE_ARRAY);
		types.put("va25",DataType.STRUCTUREVALUE_ARRAY);
		types.put("va26",DataType.INSTANCEVALUE_ARRAY);
		types.put("va27",DataType.OCTETSTRING_ARRAY);
		types.put("va28",DataType.ENUMERATIONVALUE_ARRAY);
		types.put("va29",DataType.STRUCTUREVALUE_ARRAY);
		types.put("va30",DataType.INSTANCEVALUE_ARRAY);
		types.put("va31",DataType.STRING_ARRAY);
	}

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("CimStructure ");
		cimStructure = (CimStructure) Java2Cim.getModelForClass(PropertyBindingClass.class, repository);
		assertNotNull(cimStructure);
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
	 * Test method for {@link net.aifusion.metamodel.CimStructure#toMOF(java.lang.String)}.
	 */
	@Ignore
	@Test
	public final void testToMOFString() {
		// Note that the properties are in no particular order, so we need to do a structural diff, MOF varies randomely
		String mofString = "[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.PropertyBindingClass\" }, Description(\"Structure to test property bindings\")]\nStructure cim_test {\n\t[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\" }]\n\tEnumeration CimFusion_EmbeddedStringEnum : String {\n\t\tNAME1 = \"xyz\",\n\t\tName2 = \"abc\",\n\t\tname3 = \"def\"\n\t};\n\t[Write]\n\tSInt16 [] Va10;\n\t[Write]\n\tSInt32 [] Va11;\n\t[Write]\n\tSInt32 [] Va12;\n\t[Write]\n\tSInt64 [] Va13;\n\t[Write]\n\tSInt64 [] Va14;\n\t[Write]\n\tReal32 [] Va15;\n\t[Write]\n\tString V21;\n\t[Write]\n\tChar16 V20;\n\t[Write]\n\tcim_testmethods ref V23;\n\t[Write]\n\tDatetime V22;\n\t[Write]\n\tReal32 [] Va16;\n\t[Write]\n\tcim_testmethodssup V25;\n\t[Write]\n\tReal64 [] Va17;\n\t[Write]\n\tCimFusion_EmbeddedStringEnum V24;\n\t[Write]\n\tReal64 [] Va18;\n\t[Write]\n\tOctetString V27;\n\t[Write]\n\tChar16 [] Va19;\n\t[Write]\n\tcim_testmethods V26;\n\t[Write]\n\tcim_testmethodssup V29;\n\t[Write]\n\tCimFusion_EnumBindingClass V28;\n\t[Write]\n\tBoolean [] Va01;\n\t[Write]\n\tBoolean [] Va02;\n\t[Write]\n\tUInt8 [] Va03;\n\t[Write]\n\tUInt16 [] Va04;\n\t[Write]\n\tcim_testmethods V30;\n\t[Static, Write]\n\tString V31 = \"default\";\n\t[Write]\n\tUInt32 [] Va05;\n\t[Write]\n\tUInt64 [] Va06;\n\t[Write]\n\tSInt8 [] Va07;\n\t[Write]\n\tSInt8 [] Va08;\n\t[Write]\n\tSInt16 [] Va09;\n\t[Write]\n\tcim_testmethods [] Va30;\n\t[Static, Write]\n\tString [] Va31;\n\t[Write]\n\tBoolean V01;\n\t[Write]\n\tUInt8 V03;\n\t[Write]\n\tBoolean V02;\n\t[Write]\n\tUInt32 V05;\n\t[Write]\n\tUInt16 V04;\n\t[Write]\n\tSInt8 V07;\n\t[Write]\n\tUInt64 V06;\n\t[Write]\n\tSInt16 V09;\n\t[Write]\n\tSInt8 V08;\n\t[Write]\n\tChar16 [] Va20;\n\t[Write]\n\tString [] Va21;\n\t[Write]\n\tDatetime [] Va22;\n\t[Write]\n\tcim_testmethods ref [] Va23;\n\t[Write]\n\tCimFusion_EmbeddedStringEnum [] Va24;\n\t[Write]\n\tcim_testmethodssup [] Va25;\n\t[Write]\n\tcim_testmethods [] Va26;\n\t[Write]\n\tSInt16 V10;\n\t[Write]\n\tSInt32 V12;\n\t[Write]\n\tSInt32 V11;\n\t[Write]\n\tSInt64 V14;\n\t[Write]\n\tOctetString [] Va27;\n\t[Write]\n\tSInt64 V13;\n\t[Write]\n\tCimFusion_EnumBindingClass [] Va28;\n\t[Write]\n\tReal32 V16;\n\t[Write]\n\tcim_testmethodssup [] Va29;\n\t[Write]\n\tReal32 V15;\n\t[Write]\n\tReal64 V18;\n\t[Write]\n\tReal64 V17;\n\t[Write]\n\tChar16 V19;\n};\n";
		assertEquals(mofString,cimStructure.toMOF());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		CimStructure cimStructure1 = (CimStructure) Java2Cim.getModelForClass(PropertyBindingClass.class, new InMemoryRepository());
		assertNotNull(cimStructure1);
		assertEquals(cimStructure,cimStructure1);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#hasKeys()}.
	 */
	@Test
	public final void testHasKeys() {
		assertFalse(cimStructure.hasKeys());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#hasProperty(java.lang.String)}.
	 */
	@Test
	public final void testHasProperty() {
		assertTrue(cimStructure.hasProperty("Va07"));
		assertFalse(cimStructure.hasProperty("VX02"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#hasStructure(java.lang.String)}.
	 */
	@Test
	public final void testHasStructure() {
		assertFalse(cimStructure.hasStructure("foo"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#hasEnumeration(java.lang.String)}.
	 */
	@Test
	public final void testHasEnumeration() {
		assertTrue(cimStructure.hasEnumeration("AIFusion_EmbeddedStringEnum"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getPropertyValue(java.lang.String)}.
	 */
	@Test
	public final void testGetPropertyValue() {
		for(String key : cimStructure.getPropertyNames()){
			CimProperty p = cimStructure.getProperty(key);
			try {
				DataValue v = cimStructure.getPropertyValue(key);
				assertTrue(p.isStatic());
				assertTrue("v31".equalsIgnoreCase(key) || "va31".equalsIgnoreCase(key));
				assertTrue(new DataValue("default").equals(v) || v == null);
			} catch(ModelException e){
				assertFalse(p.isStatic());
			}
		}	
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getProperty(java.lang.String)}.
	 */
	@Test
	public final void testGetProperty() {
		for(String key : cimStructure.getPropertyNames()){
			CimProperty p = cimStructure.getProperty(key);
			assertNotNull(p);
		}	
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getStructure(java.lang.String)}.
	 */
	@Test
	public final void testGetStructure() {
		assertEquals(null, cimStructure.getStructure("Cim_Structure"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getEnumeration(java.lang.String)}.
	 */
	@Test
	public final void testGetEnumeration() {
		CimEnumeration e = cimStructure.getEnumeration("AIFusion_EmbeddedStringEnum");
		assertNotNull(e);
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getAllProperties()}.
	 */
	@Test
	public final void testGetAllProperties() {
		Map<String,CimProperty> props = cimStructure.getAllProperties();
		assertEquals(62,props.size());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getPropertyNames()}.
	 */
	@Test
	public final void testGetPropertyNames() {
		Set<String> pNames = cimStructure.getPropertyNames();
		assertEquals(62,pNames.size());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getPropertyType(java.lang.String)}.
	 */
	@Test
	public final void testGetPropertyType() {
		for(String key : cimStructure.getPropertyNames()){
			assertEquals(types.get(key.toLowerCase()),cimStructure.getPropertyType(key));
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#getDefaultPropertyValue(String)}.
	 */
	@Test
	@Ignore
	public final void testGetDefaultPropertyValue() {
		for(String key : cimStructure.getPropertyNames()){
			assertEquals(types.get(key),cimStructure.getPropertyType(key));
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimStructure#bind()};
	 */
	@Ignore
	@Test
	public final void testBind() {
		fail("Not yet implemented");
	}

}
