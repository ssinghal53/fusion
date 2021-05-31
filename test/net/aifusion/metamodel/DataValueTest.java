/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Apr 19, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Array;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Unit tests to test CIM DataValue class
 * @author Sharad Singhal
 *
 */
public class DataValueTest {
	static boolean verbose = false;
	static CimClass cimClass;
	static CimStructure struct;
	static StructureValue sv;
	static Map<String,DataValue> propertyValues;
	static Map<String,DataValue> classPropertyValues;
	
	static {
		CimProperty keyProperty = null;
		Vector<CimProperty> keyProperties = new Vector<CimProperty>();
		Qualifier keyQual = StandardQualifierType.KEY.getQualifier(true, Constants.defaultNameSpacePath);
		Vector<Qualifier> keyQuals = new Vector<Qualifier>();
		keyQuals.add(keyQual);
		keyProperty = new CimProperty("name","kz",DataType.BOOLEAN,new DataValue(true),keyQuals);
		keyProperties.add(keyProperty);
		struct = new CimStructure(ElementType.STRUCTURE,"name",null,null,Constants.defaultNameSpacePath,keyProperties);
		
		Vector<CimProperty> classProperties = new Vector<CimProperty>();
		CimProperty classProperty = new CimProperty("name.Test_Class","p2",DataType.STRING,new DataValue("p2Value"),null);
		classProperties.add(classProperty);
		cimClass = new CimClass(ElementType.CLASS,"Test_Class",struct,null,struct.getNameSpacePath(),classProperties);
		
		propertyValues = new HashMap<String,DataValue>();
		propertyValues.put("KZ",new DataValue(false));
		classPropertyValues = new HashMap<String,DataValue>();
		classPropertyValues.put("P2", new DataValue("newP2Value"));
		classPropertyValues.put("KZ", new DataValue(true));
		
	}

	// TODO: This test contains empty enumerations, structures, and instance values. Needs to be fixed once those are done
	
	// data objects
	private static Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
		new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
		"foobar",true,Float.valueOf((float) 45.0),(double) 500.,
		new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
		new EnumerationValue("name","enum",null, null),
		StructureValue.createStructureValue(struct, propertyValues, "$alias"),
		CimInstance.createInstance(cimClass, classPropertyValues, "$alias"),
		new OctetString("0xfe10ff"),
		new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
		new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
		new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float) 45.0)},new double[]{(double) 500.},
		new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
		new EnumerationValue[]{new EnumerationValue("name","enum",null, null)},
		new StructureValue[]{StructureValue.createStructureValue(struct, propertyValues, "$alias")},
		new CimInstance[]{CimInstance.createInstance(cimClass, classPropertyValues, "$alias")},
		new OctetString[]{new OctetString("0xfe10ff")}
	};
	// data objects
	private static Object [] obj2 = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
		new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
		"foobar",true,Float.valueOf((float) 45.0),(double) 500.,
		new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
		new EnumerationValue("name","enum",null, null),
		StructureValue.createStructureValue(struct, propertyValues, "$alias"),
		CimInstance.createInstance(cimClass, classPropertyValues, "$alias"),
		new OctetString("0xfe10ff"),
		new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
		new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
		new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float) 45.0)},new double[]{(double) 500.},
		new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
		new EnumerationValue[]{new EnumerationValue("name","enum",null, null)},
		new StructureValue[]{StructureValue.createStructureValue(struct, propertyValues, "$alias")},
		new CimInstance[]{CimInstance.createInstance(cimClass, classPropertyValues, "$alias")},
		new OctetString[]{new OctetString("0xfe10ff")}
	
	};
	// known data types
	private static DataType [] type = {DataType.VOID, DataType.UINT8, DataType.SINT8, DataType.UINT16, DataType.SINT16,
		DataType.UINT32, DataType.SINT32, DataType.UINT64, DataType.SINT64,
		DataType.STRING, DataType.BOOLEAN, DataType.REAL32, DataType.REAL64,
		DataType.DATETIME, DataType.CHAR16, DataType.OBJECTPATH,
		DataType.ENUMERATIONVALUE,DataType.STRUCTUREVALUE,DataType.INSTANCEVALUE,
		DataType.OCTETSTRING,
		DataType.UINT8_ARRAY, DataType.SINT8_ARRAY, DataType.UINT16_ARRAY, DataType.SINT16_ARRAY,
		DataType.UINT32_ARRAY, DataType.SINT32_ARRAY, DataType.UINT64_ARRAY, DataType.SINT64_ARRAY,
		DataType.STRING_ARRAY, DataType.BOOLEAN_ARRAY, DataType.REAL32_ARRAY, DataType.REAL64_ARRAY,
		DataType.DATETIME_ARRAY, DataType.CHAR16_ARRAY, DataType.OBJECTPATH_ARRAY,
		DataType.ENUMERATIONVALUE_ARRAY,DataType.STRUCTUREVALUE_ARRAY,DataType.INSTANCEVALUE_ARRAY,
		DataType.OCTETSTRING_ARRAY
	};
	// mof values
	private static String [] mof = {"null", "8","-12","22","55","100","-75","351","500",
			"\"foobar\"","true","45.0","500.0","\"20140420044028.080***+000\"","\'a\'","\"/class/cimv2:CIM_Test\"",
			"name","value of name as $alias {\n\tkz = false;\n}","instance of Test_Class as $alias {\n\tp2 = \"newP2Value\";\n\tkz = true;\n}",
			"\"0xfe10ff\"",
			"{ 8 }","{ -12 }","{ 22 }",	"{ 55 }","{ 100 }","{ -75 }","{ 351 }","{ 500 }","{ \"foobar\" }",
			"{ true }","{ 45.0 }","{ 500.0 }","{ \"20140420044028.080***+000\" }","{ \'a\' }","{ \"/class/cimv2:CIM_Test\" }",
			"{ name }","{ value of name as $alias {\n\tkz = false;\n} }","{ instance of Test_Class as $alias {\n\tp2 = \"newP2Value\";\n\tkz = true;\n} }",
			"{ \"0xfe10ff\" }"
			};
	// string values 
	private static String [] str = {null, "8","-12","22","55","100","-75","351","500",
			"foobar","true","45.0","500.0","20140420044028.080***+000","a","/class/cimv2:CIM_Test",
			"name","value of name as $alias {\n\tkz = false;\n}","instance of Test_Class as $alias {\n\tp2 = \"newP2Value\";\n\tkz = true;\n}",
			"0xfe10ff",
			"{ 8 }","{ -12 }","{ 22 }",	"{ 55 }","{ 100 }","{ -75 }","{ 351 }","{ 500 }","{ \"foobar\" }",
			"{ true }","{ 45.0 }","{ 500.0 }","{ \"20140420044028.080***+000\" }","{ \'a\' }","{ \"/class/cimv2:CIM_Test\" }",
			"{ name }","{ value of name as $alias {\n\tkz = false;\n} }","{ instance of Test_Class as $alias {\n\tp2 = \"newP2Value\";\n\tkz = true;\n} }",
			"{ \"0xfe10ff\" }"
			};

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("DataValue ");
		assertEquals(type.length,obj.length);
		assertEquals(type.length,obj2.length);
		assertEquals(type.length,mof.length);
		assertEquals(DataType.values().length,type.length);
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
	 * Test method for {@link net.aifusion.metamodel.DataValue#DataValue(java.lang.Object)}.
	 */
	@Test
	public final void testDataValueObject() {
		try {
			new DataValue(obj[0]);
			fail("Null constructor should not succeed");
		} catch (ModelException ex){
			assertEquals(4,ex.getReason().getCode());	// expect INVALID_PARAMETER
		}
		for(int i = 1; i < type.length; i++){
			DataValue v = new DataValue(obj[i]);
			assertNotNull(v);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#DataValue(net.aifusion.metamodel.DataType, java.lang.Object)}.
	 */
	@Test
	public final void testDataValueDataTypeObject() {
		for(int i = 0; i < type.length; i++){
			DataValue v = new DataValue(type[i],obj[i]);
			assertNotNull(v);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#DataValue(java.lang.String, java.lang.String)}.
	 */
	@Test
	public final void testDataValueStringString() {
		for(int i = 0; i < type.length; i++){
			String typ = type[i].toString();
			String val = (obj[i] == null) ? null : obj[i].toString();
			try {
				DataValue v = new DataValue(typ,val);
				assertFalse(type[i].isArray());
				assertNotNull(v);
			} catch(ModelException e){
				// conversion should fail on array types, or complex types
				assertTrue(type[i].isArray() || type[i].isComplex());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#DataValue(java.lang.String, java.lang.String[])}.
	 */
	@Test
	public final void testDataValueStringStringArray() {
		for(int i = 0; i < type.length; i++){
			try {
				String typ = type[i].toString();
				if(type[i].isArray()){
					String [] val = new String []{Array.get(obj[i], 0).toString()};
					DataValue v = new DataValue(typ,val);
					assertNotNull(v);
				}
			} catch (ModelException e){
				// conversion should fail on complex types
				assertTrue(type[i].isComplex());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#getType()}.
	 */
	@Test
	public final void testGetType() {
		for(int i=0; i < type.length; i++){
			DataValue v = new DataValue(type[i],obj[i]);
			assertEquals(type[i],v.getType());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#getValue()}.
	 */
	@Test
	public final void testValue() {
		for(int i=0; i < type.length; i++){
			DataValue v = new DataValue(type[i],obj[i]);
			if(type[i].isArray()){
				assertEquals(Array.get(obj[i], 0),Array.get(v.getValue(), 0));
			} else {
				assertEquals(obj[i],v.getValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		for(int i=0; i < type.length; i++){
			DataValue v = new DataValue(type[i],obj[i]);
			if(verbose) System.out.println(type[i]+" ["+obj[i]+"]: "+v.toMOF());
			assertEquals(mof[i],v.toMOF());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#toString()}.
	 */
	@Test
	public final void testToString() {
		for(int i=0; i < type.length; i++){
			// System.out.println(type[i]);
			DataValue v = new DataValue(type[i],obj[i]);
			assertEquals(str[i],v.toString());
			
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataValue#equals(Object)}.
	 */
	@Test
	public final void testEquals(){
		for(int i = 0; i < type.length; i++){
			DataValue iv = new DataValue(type[i],obj[i]);
			for(int j = 0; j < type.length; j++){
				DataValue jv = new DataValue(type[j],obj2[j]);
				if(i == j){
					assertEquals(iv,jv);
				} else {
					assertNotEquals(iv, jv);
				}
			}
		}
	}
	
	/**
	 * Test method to check for defined values {@link net.aifusion.metamodel.DataType#isDefinedValue()}.
	 */
	@Test
	public final void testDefinedValues() {
		DefinedClass c = new DefinedClass();
		DataValue v = new DataValue(DataType.STRUCTUREVALUE,c);
		assertNotNull(v);
		assertEquals(DataType.STRUCTUREVALUE,v.getType());
		assertEquals(c,v.getValue());
		assertEquals("TEST_DefinedClass",JavaModelMapper.getCimClassName(c.getClass()));
		assertEquals(ElementType.STRUCTURE,JavaModelMapper.getCimElementType(c.getClass()));
		assertEquals(new ObjectPath("/structure/aifusion:test_definedclass"),JavaModelMapper.getObjectPathFromClass(c.getClass()));
		return;
	}

}
