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
 * Created Jul 17, 2016 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test CimParameter
 * @author Sharad Singhal
 */
public class CimParameterTest {
	static CimStructure struct;	// Test_Struct
	static StructureValue structValue, structValue2;	// Test_Struct value
	
	static CimClass cimClass;	// Test_Struct.Test_Class
	static CimInstance instance, instance2;		// Test_Class instance
	
	static CimEnumeration enumeration;		// Enumeration with name "enum", that contains a single enumerationValue called "name"
	static EnumerationValue enumValue, enumValue2;	// enumeration value (name)
	static List<Qualifier> quals;
	
	static {
		// create some qualifiers for the parameter
		quals = new Vector<Qualifier>();
		quals.add(StandardQualifierType.DESCRIPTION.getQualifier("Qualifier", Constants.defaultNameSpacePath));
		
		// create a CimStructure "Test_Struct" with a boolean key property labeled "kz" and true default value
		Qualifier keyQual = StandardQualifierType.KEY.getQualifier(true, Constants.defaultNameSpacePath);
		Vector<Qualifier> keyQuals = new Vector<Qualifier>();
		keyQuals.add(keyQual);
		CimProperty keyProperty = new CimProperty("Test_Struct","kz",DataType.BOOLEAN,new DataValue(true),keyQuals);
		Vector<CimProperty> keyProperties = new Vector<CimProperty>();	// properties for Test_Struct
		keyProperties.add(keyProperty);
		struct = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,keyProperties);
		// System.out.println(struct.toMOF());

		// create a Test_Structure value
		HashMap<String,DataValue> propertyValues = new HashMap<String,DataValue>();	
		propertyValues.put("kz",new DataValue(false));
		structValue = StructureValue.createStructureValue(struct, propertyValues, "$as");
		structValue2 = StructureValue.createStructureValue(struct, propertyValues, "$as2");
		// System.out.println(structValue.toMOF());


		// create a CimClass "Test_Class" that inherits from "Test_Struct" with a string property with default value p2Value
		CimProperty classProperty = new CimProperty("Test_Struct.Test_Class","p2",DataType.STRING,new DataValue("p2Value"),null);
		Vector<CimProperty> classProperties = new Vector<CimProperty>();
		classProperties.add(classProperty);
		cimClass = new CimClass(ElementType.CLASS,"Test_Class",struct,null,struct.getNameSpacePath(),classProperties);
		// System.out.println(cimClass.toMOF());

		// create a CimInstance based on Test_Class
		HashMap<String,DataValue> classPropertyValues = new HashMap<String,DataValue>();
		classPropertyValues.put("p2", new DataValue("newP2Value"));
		classPropertyValues.put("KZ", new DataValue(true));
		instance = CimInstance.createInstance(cimClass, classPropertyValues, "$ac");
		instance2 = CimInstance.createInstance(cimClass, classPropertyValues, "$ac2");
		// System.out.println(instance.toMOF());

		// create an enumeration "enum" with one string value "name" defined in it
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null,null));
		enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
		// System.out.println(enumeration.toMOF());

		// create an enumValue based on enum
		enumValue = enumeration.getValue("name");
		enumValue2 = enumeration.getValue("name");
		// System.out.println(enumValue.toMOF());
	}
	
	// data objects
	private static Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
			new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
			"foobar",true,Float.valueOf((float) 45.0),(double) 500.,
			new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
			new OctetString("0x213244"),
			enumValue,
			structValue,
			instance,
			new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
			new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
			new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float) 45.0)},new double[]{(double) 500.},
			new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
			new OctetString[]{new OctetString("0x213244")},
			new EnumerationValue[]{enumValue},
			new StructureValue[]{structValue},
			new CimInstance[]{instance}
	};
	// data objects
	private static Object [] obj2 = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
			new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
			"foobar",true,Float.valueOf((float) 45.0),(double) 500.,
			new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
			new OctetString("0x213244"),
			enumValue2,
			structValue2,
			instance2,
			new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
			new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
			new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float) 45.0)},new double[]{(double) 500.},
			new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
			new OctetString[]{new OctetString("0x213244")},
			new EnumerationValue[]{enumValue2},
			new StructureValue[]{structValue2},
			new CimInstance[]{instance2}

	};
	// known data types
	private static DataType [] type = {DataType.VOID, DataType.UINT8, DataType.SINT8, DataType.UINT16, DataType.SINT16,
			DataType.UINT32, DataType.SINT32, DataType.UINT64, DataType.SINT64,
			DataType.STRING, DataType.BOOLEAN, DataType.REAL32, DataType.REAL64,
			DataType.DATETIME, DataType.CHAR16, DataType.OBJECTPATH,
			DataType.OCTETSTRING,
			DataType.ENUMERATIONVALUE,DataType.STRUCTUREVALUE,DataType.INSTANCEVALUE,
			DataType.UINT8_ARRAY, DataType.SINT8_ARRAY, DataType.UINT16_ARRAY, DataType.SINT16_ARRAY,
			DataType.UINT32_ARRAY, DataType.SINT32_ARRAY, DataType.UINT64_ARRAY, DataType.SINT64_ARRAY,
			DataType.STRING_ARRAY, DataType.BOOLEAN_ARRAY, DataType.REAL32_ARRAY, DataType.REAL64_ARRAY,
			DataType.DATETIME_ARRAY, DataType.CHAR16_ARRAY, DataType.OBJECTPATH_ARRAY,
			DataType.OCTETSTRING_ARRAY,
			DataType.ENUMERATIONVALUE_ARRAY,DataType.STRUCTUREVALUE_ARRAY,DataType.INSTANCEVALUE_ARRAY
	};
	
	// mof values
	String [] mof = {"null", 
			"[Description(\"Qualifier\")] UInt8 Name = 8",
			"[Description(\"Qualifier\")] SInt8 Name = -12",
			"[Description(\"Qualifier\")] UInt16 Name = 22",
			"[Description(\"Qualifier\")] SInt16 Name = 55",
			"[Description(\"Qualifier\")] UInt32 Name = 100",
			"[Description(\"Qualifier\")] SInt32 Name = -75",
			"[Description(\"Qualifier\")] UInt64 Name = 351",
			"[Description(\"Qualifier\")] SInt64 Name = 500",
			"[Description(\"Qualifier\")] String Name = \"foobar\"",
			"[Description(\"Qualifier\")] Boolean Name = true",
			"[Description(\"Qualifier\")] Real32 Name = 45.0",
			"[Description(\"Qualifier\")] Real64 Name = 500.0",
			"[Description(\"Qualifier\")] Datetime Name = \"20140420044028.080***+000\"",
			"[Description(\"Qualifier\")] Char16 Name = \'a\'",
			"[Description(\"Qualifier\")] Cim_Test ref Name = \"/class/cimv2:CIM_Test\"",
			"[Description(\"Qualifier\")] OctetString Name = \"0x213244\"",
			"[Description(\"Qualifier\")] enum Name = name",
			"[Description(\"Qualifier\")] Test_Struct Name = value of Test_Struct as $as {\n\tkz = false;\n}",
			"[Description(\"Qualifier\")] Test_Class Name = instance of Test_Class as $ac {\n\tp2 = \"newP2Value\";\n\tkz = true;\n}",
			"[Description(\"Qualifier\")] UInt8 [] Name = { 8 }",
			"[Description(\"Qualifier\")] SInt8 [] Name = { -12 }",
			"[Description(\"Qualifier\")] UInt16 [] Name = { 22 }", 
			"[Description(\"Qualifier\")] SInt16 [] Name = { 55 }",
			"[Description(\"Qualifier\")] UInt32 [] Name = { 100 }",
			"[Description(\"Qualifier\")] SInt32 [] Name = { -75 }",
			"[Description(\"Qualifier\")] UInt64 [] Name = { 351 }",
			"[Description(\"Qualifier\")] SInt64 [] Name = { 500 }",
			"[Description(\"Qualifier\")] String [] Name = { \"foobar\" }",
			"[Description(\"Qualifier\")] Boolean [] Name = { true }",
			"[Description(\"Qualifier\")] Real32 [] Name = { 45.0 }",
			"[Description(\"Qualifier\")] Real64 [] Name = { 500.0 }",
			"[Description(\"Qualifier\")] Datetime [] Name = { \"20140420044028.080***+000\" }",
			"[Description(\"Qualifier\")] Char16 [] Name = { \'a\' }",
			"[Description(\"Qualifier\")] Cim_Test ref [] Name = { \"/class/cimv2:CIM_Test\" }",
			"[Description(\"Qualifier\")] OctetString [] Name = { \"0x213244\" }",
			"[Description(\"Qualifier\")] enum [] Name = { name }",
			"[Description(\"Qualifier\")] Test_Struct [] Name = { value of Test_Struct as $as {\n\tkz = false;\n} }",
			"[Description(\"Qualifier\")] Test_Class [] Name = { instance of Test_Class as $ac {\n\tp2 = \"newP2Value\";\n\tkz = true;\n} }"
	};

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("CimParameter ");
		assertEquals(DataType.values().length,type.length);
		assertEquals(obj.length,type.length);
		assertEquals(type.length,obj2.length);
		assertNotNull(struct);
		assertNotNull(structValue);
		assertNotNull(cimClass);
		assertNotNull(instance);
		assertNotNull(enumeration);
		assertNotNull(enumValue);
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
	 * Test method for {@link net.aifusion.metamodel.CimParameter#CimParameter(String, java.lang.String, net.aifusion.metamodel.DataType, net.aifusion.metamodel.DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimParameterStringDataTypeDataValueListOfQualifier() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			try {
				CimParameter p = new CimParameter(null,"Name",t,v, quals);
				assertTrue(t.isPrimitive());
				assertEquals(null,p.getRefClassName());
				assertEquals(null,p.getEnum());
				assertEquals(null,p.getStruct());
			} catch (ModelException e){
				assertFalse(t.isPrimitive());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#CimParameter(String, java.lang.String, java.lang.String, boolean, net.aifusion.metamodel.DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimParameterStringStringBooleanDataValueListOfQualifier() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			try {
				CimParameter p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
				assertTrue(t.isReference());
				assertEquals(null,p.getEnum());
				assertEquals(null,p.getStruct());
			} catch (ModelException e){
				assertFalse(t.isReference());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#CimParameter(String, java.lang.String, net.aifusion.metamodel.CimEnumeration, boolean, net.aifusion.metamodel.DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimParameterStringEnumerationBooleanDataValueListOfQualifier() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			try {
				CimParameter p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
				assertTrue(t.isEnumerationValue());
				assertEquals(null,p.getRefClassName());
				assertEquals(null,p.getStruct());
			} catch (ModelException e){
				assertFalse(t.isEnumerationValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#CimParameter(String, java.lang.String, net.aifusion.metamodel.CimStructure, boolean, net.aifusion.metamodel.DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimParameterStringCimStructureBooleanDataValueListOfQualifier() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			try {
				CimStructure s = t.isStructureValue() ? struct : cimClass;
				CimParameter p = new CimParameter(null,"Name",s,t.isArray(),v, quals);
				assertTrue(t.isStructureValue() || t.isInstanceValue());
				assertEquals(null,p.getRefClassName());
				assertEquals(null,p.getEnum());
			} catch (ModelException e){
				assertFalse(t.isStructureValue() || t.isInstanceValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#getRefClassName()}.
	 */
	@Test
	public final void testGetRefClassName() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			try {
				CimParameter p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
				assertEquals("cim_test",p.getRefClassName());
			} catch (ModelException e){
				assertEquals(13,e.getReason().getCode());
				assertFalse(t.isReference());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#getEnum()}.
	 */
	@Test
	public final void testGetEnum() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			try {
				CimParameter p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
				assertTrue(t.isEnumerationValue());
				assertEquals(null,p.getRefClassName());
				assertEquals(null,p.getStruct());
				assertEquals(enumeration,p.getEnum());
			} catch (ModelException e){
				assertEquals(13,e.getReason().getCode());
				assertFalse(t.isEnumerationValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#getStruct()}.
	 */
	@Test
	public final void testGetStruct() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			try {
				CimStructure s = t.isStructureValue() ? struct : cimClass;
				CimParameter p = new CimParameter(null,"Name",s,t.isArray(),v, quals);
				assertTrue(t.isStructureValue() || t.isInstanceValue());
				assertEquals(null,p.getRefClassName());
				assertEquals(null,p.getEnum());
				assertEquals(s,p.getStruct());
			} catch (ModelException e){
				assertEquals(13,e.getReason().getCode());
				assertFalse(t.isStructureValue() || t.isInstanceValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#getDataType()}.
	 */
	@Test
	public final void testGetDataType() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertEquals(t,p.getDataType());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#hasValue()}.
	 */
	@Test
	public final void testHasValue() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertTrue(p.hasValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#hasNonNullValue()}.
	 */
	@Test
	public final void testHasNonNullValue() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertTrue(p.hasNonNullValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertEquals(v,p.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#setValue(net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testSetValue() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertEquals(v,p.getValue());
			p.setValue(new DataValue(t,null));
			assertTrue(p.hasValue());
			assertFalse(p.hasNonNullValue());
			assertEquals(new DataValue(t,null),p.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#isInput()}.
	 */
	@Test
	public final void testIsInput() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertTrue(p.isInput());
			assertFalse(p.isOutput());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#isOutput()}.
	 */
	@Test
	public final void testIsOutput() {
		List<Qualifier> quals1 = new Vector<Qualifier>();
		quals1.addAll(quals);
		quals1.add(StandardQualifierType.IN.getQualifier(false, Constants.defaultNameSpacePath));
		quals1.add(StandardQualifierType.OUT.getQualifier(true, Constants.defaultNameSpacePath));
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals1);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals1);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals1);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals1);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals1);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertTrue(p.isOutput());
			assertFalse(p.isInput());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#isReference()}.
	 */
	@Test
	public final void testIsReference() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertEquals(t.isReference(),p.isReference());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#getFullName()}.
	 */
	@Test
	public final void testGetFullName() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter("Cim_Class#Method","Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter("Cim_Class#Method","Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter("Cim_Class#Method", "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter("Cim_Class#Method","Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter("Cim_Class#Method","Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertEquals("Cim_Class#Method$Name",p.getFullName());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#isArray()}.
	 */
	@Test
	public final void testIsArray() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertEquals(t.isArray(),p.isArray());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#createInstanceParameter()}.
	 */
	@Test
	public final void testCreateInstanceParameter() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,null, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","cim_test",t.isArray(),null, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), null, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),null, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),null, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			CimParameter ip = p.createInstanceParameter();
			assertFalse(p.hasValue());
			assertFalse(ip.hasValue());
			ip.setValue(v);
			assertEquals(v,ip.getValue());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#toMOF(java.lang.String)}.
	 */
	@Test
	public final void testToMOFString() {
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			// System.out.println(t+" "+v.toMOF());
			CimParameter p = null;
			if(t.isPrimitive()){
				 p = new CimParameter(null,"Name",t,v, quals);
			} else if(t.isReference()){
				p = new CimParameter(null,"Name","Cim_Test",t.isArray(),v, quals);
			} else if(t.isEnumerationValue()){
				p = new CimParameter(null, "Name", enumeration, t.isArray(), v, quals);
			} else if(t.isStructureValue()){
				p = new CimParameter(null,"Name",struct,t.isArray(),v, quals);
			} else if(t.isInstanceValue()){
				p = new CimParameter(null,"Name",cimClass,t.isArray(),v, quals);
			} else {
				assertEquals(DataType.VOID,t);
				continue;
			}
			assertEquals(mof[i],p.toMOF());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimParameter#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		for(int j = 0; j < type.length; j++){
			DataType tj = type[j];
			DataValue vj = new DataValue(tj,obj[j]);
			CimParameter pj = null;
			if(tj.isPrimitive()){
				pj = new CimParameter(null,"Name",tj,vj, quals);
			} else if(tj.isReference()){
				pj = new CimParameter(null,"Name","Cim_Test",tj.isArray(),vj, quals);
			} else if(tj.isEnumerationValue()){
				pj = new CimParameter(null, "Name", enumeration, tj.isArray(), vj, quals);
			} else if(tj.isStructureValue()){
				pj = new CimParameter(null,"Name",struct,tj.isArray(),vj, quals);
			} else if(tj.isInstanceValue()){
				pj = new CimParameter(null,"Name",cimClass,tj.isArray(),vj, quals);
			} else {
				assertEquals(DataType.VOID,tj);
				continue;
			}
			for(int i = 0; i < type.length; i++){
				DataType ti = type[i];
				DataValue vi = new DataValue(ti,obj[i]);
				// System.out.println(t+" "+v.toMOF());
				CimParameter pi = null;
				if(ti.isPrimitive()){
					pi = new CimParameter(null,"Name",ti,vi, quals);
				} else if(ti.isReference()){
					pi = new CimParameter(null,"Name","Cim_Test",ti.isArray(),vi, quals);
				} else if(ti.isEnumerationValue()){
					pi = new CimParameter(null, "Name", enumeration, ti.isArray(), vi, quals);
				} else if(ti.isStructureValue()){
					pi = new CimParameter(null,"Name",struct,ti.isArray(),vi, quals);
				} else if(ti.isInstanceValue()){
					pi = new CimParameter(null,"Name",cimClass,ti.isArray(),vi, quals);
				} else {
					assertEquals(DataType.VOID,ti);
					continue;
				}
				if(i == j){
					assertEquals(pi,pj);
				} else {
					assertNotSame(pi,pj);
				}
			}
		}
	}


}
