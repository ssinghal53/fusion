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
 * Last Modified Sep 15, 2020 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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
 * Test for various CIM data types
 * @author Sharad Singhal
 */
public class DataTypeTest {
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
		Vector<CimProperty> classProperties = new Vector<CimProperty>();
		CimProperty classProperty = new CimProperty("name.Test_Class","p2",DataType.STRING,new DataValue("p2Value"),null);
		classProperties.add(classProperty);
		classProperties.add(keyProperty);
		struct = new CimStructure(ElementType.STRUCTURE,"name",null,null,Constants.defaultNameSpacePath,keyProperties);
		cimClass = new CimClass(ElementType.CLASS,"Test_Class",struct,null,struct.getNameSpacePath(),classProperties);
	}

	// known data types
	DataType [] type = {DataType.VOID, DataType.UINT8, DataType.SINT8, DataType.UINT16, DataType.SINT16,
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
			DataType.OCTETSTRING_ARRAY};
	String [] mof = {
			"Void",
			"UInt8","SInt8","UInt16","SInt16",
			"UInt32","SInt32","UInt64","SInt64",
			"String","Boolean","Real32","Real64",
			"Datetime","Char16","Ref",
			"","","",
			"OctetString",
			"UInt8","SInt8","UInt16","SInt16",
			"UInt32","SInt32","UInt64","SInt64",
			"String","Boolean","Real32","Real64",
			"Datetime","Char16","Ref",
			"","","",
			"OctetString"
	};
	Class<?>[] classes = {
			void.class,UInt8.class,Byte.class,UInt16.class,Short.class,
			UInt32.class,Integer.class,UInt64.class,Long.class,
			String.class,Boolean.class,Float.class,Double.class,
			DateTime.class,Character.class,ObjectPath.class,
			EnumerationValue.class,StructureValue.class,CimInstance.class,
			OctetString.class,
			UInt8[].class,Byte[].class, UInt16[].class,Short[].class,
			UInt32[].class,Integer[].class,UInt64[].class,Long[].class,
			String[].class,Boolean[].class,Float[].class,Double[].class,
			DateTime[].class,Character[].class,ObjectPath[].class,
			EnumerationValue[].class,StructureValue[].class,CimInstance[].class,
			OctetString[].class
	};
	DataType [] baseType = {
			DataType.VOID, DataType.UINT8, DataType.SINT8, DataType.UINT16, DataType.SINT16,
			DataType.UINT32, DataType.SINT32, DataType.UINT64, DataType.SINT64,
			DataType.STRING, DataType.BOOLEAN, DataType.REAL32, DataType.REAL64,
			DataType.DATETIME, DataType.CHAR16, DataType.OBJECTPATH,
			DataType.ENUMERATIONVALUE,DataType.STRUCTUREVALUE,DataType.INSTANCEVALUE,DataType.OCTETSTRING};
	DataType [] arrayType = {
			DataType.UINT8_ARRAY, DataType.SINT8_ARRAY, DataType.UINT16_ARRAY, DataType.SINT16_ARRAY,
			DataType.UINT32_ARRAY, DataType.SINT32_ARRAY, DataType.UINT64_ARRAY, DataType.SINT64_ARRAY,
			DataType.STRING_ARRAY, DataType.BOOLEAN_ARRAY, DataType.REAL32_ARRAY, DataType.REAL64_ARRAY,
			DataType.DATETIME_ARRAY, DataType.CHAR16_ARRAY, DataType.OBJECTPATH_ARRAY,
			DataType.ENUMERATIONVALUE_ARRAY,DataType.STRUCTUREVALUE_ARRAY,DataType.INSTANCEVALUE_ARRAY,DataType.OCTETSTRING_ARRAY};

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("DataType ");
		CimProperty keyProperty = null;
		Vector<CimProperty> keyProperties = new Vector<CimProperty>();
		Qualifier keyQual = StandardQualifierType.KEY.getQualifier(true, Constants.defaultNameSpacePath);
		Vector<Qualifier> keyQuals = new Vector<Qualifier>();
		keyQuals.add(keyQual);
		keyProperty = new CimProperty("name","kz",DataType.BOOLEAN,new DataValue(true),keyQuals);
		keyProperties.add(keyProperty);
		Vector<CimProperty> classProperties = new Vector<CimProperty>();
		CimProperty classProperty = new CimProperty("name.Test_Class","p2",DataType.STRING,new DataValue("p2Value"),null);
		classProperties.add(classProperty);
		struct = new CimStructure(ElementType.STRUCTURE,"name",null,null,Constants.defaultNameSpacePath,keyProperties);
		cimClass = new CimClass(ElementType.CLASS,"Test_Class",struct,null,struct.getNameSpacePath(),classProperties);
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		propertyValues = new HashMap<String,DataValue>();
		propertyValues.put("KZ",new DataValue(false));
		classPropertyValues = new HashMap<String,DataValue>();
		classPropertyValues.put("P2", new DataValue("newP2Value"));
		classPropertyValues.put("KZ", new DataValue(true));
		// ensure that we have all definitions
		assertEquals(39,DataType.values().length);
		assertEquals(type.length,DataType.values().length);
		assertEquals(baseType.length+arrayType.length,DataType.values().length);
		assertEquals(baseType.length,arrayType.length+1);
		// ensure that we have matching class/type pairs
		assertEquals(classes.length,type.length);
		assertEquals(type.length,mof.length);
	}
	
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}
	
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isArray()}.
	 */
	@Test
	public final void testIsArray() {
		// check for array types
		for(int i=0; i<arrayType.length; i++){
			assertTrue(arrayType[i].isArray());
		}
		// check for base types
		for(int i=0; i<baseType.length; i++)
			assertFalse(baseType[i].isArray());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#getClassForType()}.
	 */
	@Test
	public final void testGetClassForType() {
		// check that all data types match the corresponding java types
		for(int j = 0; j < type.length; j++){
			assertEquals(classes[j],type[j].getClassForType());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#getArrayType()}.
	 */
	@Test
	public final void testGetArrayType() {
		// VOID does not have an array type, and should throw exception
		try {
			DataType d = baseType[0].getArrayType();
			fail("Type "+d+" should not have arrayType");
		} catch(ModelException e){
			assertEquals(6, e.getReason().getCode());
		}
		// all remaining base types should have corresponding array types
		for(int i=1; i<baseType.length; i++){
			assertEquals(baseType[i].getArrayType(),arrayType[i-1]);
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#getComponentType()}.
	 */
	@Test
	public final void testGetComponentType() {
		// all array types should return proper base types
		for(int i=0; i<arrayType.length; i++){
			assertEquals(arrayType[i].getComponentType(),baseType[i+1]);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		// check for all MOF strings
		for(int i=0; i<type.length;i++){
			assertEquals(mof[i],type[i].toMOF());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#matches(java.lang.Object)}.
	 */
	@Test
	public final void testMatches() {
		// data objects
		Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
				new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
				"foobar",true,Float.valueOf((float) 45.0),(double) 500.,
				new DateTime(new Date()),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
				new EnumerationValue("name","enum",null, null),
				StructureValue.createStructureValue(struct, propertyValues, "$alias"),
				CimInstance.createInstance(cimClass, classPropertyValues, "$alias"),
				new OctetString("0x1234"),
				new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
				new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{(long)500},
				new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float) 45.0)},new double[]{(double) 500.},
				new DateTime[]{new DateTime(new Date())},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
				new EnumerationValue[]{new EnumerationValue("name","enum",null, null)}, 
				new StructureValue[]{StructureValue.createStructureValue(struct, propertyValues, "$alias")},
				new CimInstance[]{CimInstance.createInstance(cimClass, classPropertyValues, "$alias")},
				new OctetString[]{new OctetString("0x1234")}
		};
		// check object types and array types
		for(int j=0; j < type.length; j++){
			for(int i=0; i<obj.length; i++){
				DataType objType = DataType.getTypeForObject(obj[i]);
				// System.out.println("Checking Object "+obj[i]+" of type "+objType+" against type "+type[j]);
				if(type[j] == objType || obj[i] == null){
					assertTrue(type[j].matches(obj[i]));
				} else {
					assertFalse(type[j].matches(obj[i]));
				}
			}
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#getValueFromString(java.lang.String)}.
	 */
	@Test
	public final void testGetValueFromString() {
		String [] value = {
				"null",
				"10","-11","256","-68",
				"300","-735","471","-335",
				"jaxby","true",
				"26.0","381.0",
				"20140419165015.000000+000","x","/class/cimfusion:cim_test",
				"",
				"",
				"",
				"0x3f48"
		};
		assertEquals(baseType.length,value.length);
		for(int i = 1; i < baseType.length; i++){
			try {
				// System.out.println("Testing "+baseType[i]+" with value "+value[i]);
				Object o = baseType[i].getValueFromString(value[i]);
				assertEquals(value[i],o.toString());
				assertTrue(DataType.getTypeForObject(o) == type[i]);
				// note that complex types will throw exceptions. All others should succeed
				assertFalse(type[i].isComplex());
			} catch (ModelException e){
				assertTrue(type[i].isComplex());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#getValuesFromStrings(java.lang.String[])}.
	 */
	@Test
	public final void testGetValuesFromStrings() {
		String [][] value = {
				{"10"},{"-11"},{"256"},{"-68"},
				{"300"},{"-735"},{"471"},{"-335"},
				{"jaxby"},{"true"},
				{"26.0"},{"381.0"},
				{"20140419165015.000000+000"},{"x"},{"/class/cimfusion:cim_test"},
				{""},{""},{""},{"0x1234"}
		};
		assertEquals(arrayType.length,value.length);
		for(int i = 0; i < arrayType.length; i++){
			try {
				// System.out.println("Testing "+arrayType[i]+" with value "+value[i][0]);
				Object [] o = arrayType[i].getValuesFromStrings(value[i]);
				for(int j = 0; j<o.length; j++){
					assertEquals(value[i][j],o[j].toString());
				}
				// note that complex types will throw exceptions. All others should succeed
				assertFalse(arrayType[i].isComplex());
			} catch (ModelException e){
				assertEquals(4,e.getReason().getCode());	// invalid parameter
				assertTrue(arrayType[i].isComplex());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#getTypeForObject(java.lang.Object)}.
	 */
	@Test
	public final void testGetTypeForObject() {
		// data objects
		Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
				new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
				"foobar",true,Float.valueOf((float) 45.0),(double) 500.,
				new DateTime(new Date()),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
				new EnumerationValue("name","enum",null, null),
				StructureValue.createStructureValue(struct, propertyValues, "$alias"),
				CimInstance.createInstance(cimClass, classPropertyValues, "$alias"),
				new OctetString("0x1234"),
				new UInt8[1],new Byte[1],new UInt16[1],new short[1],
				new UInt32[1], new Integer[1],new UInt64[1],new long[1],
				new String[1],new boolean[1],new Float[1],new double[1],
				new DateTime[1],new char[1],new ObjectPath[1], new EnumerationValue[1],new StructureValue[1],new CimInstance[1],
				new OctetString[1]
		};
		// check object types and array types
		for(int j=0; j < type.length; j++){
			for(int i=0; i<obj.length; i++){
				if(i==j){
					assertEquals(type[j],DataType.getTypeForObject(obj[i]));
				} else {
					assertNotEquals(type[j],DataType.getTypeForObject(obj[i]));
				}
			}
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#getTypeForClass(java.lang.Class)}.
	 */
	@Test
	public final void testGetTypeForClass() {
		Class<?>[] classes = {
				void.class,UInt8.class,UInt8[].class,Byte.class,byte.class,Byte[].class,byte[].class,
				UInt16.class, UInt16[].class, Short.class, short.class, Short[].class, short[].class,
				UInt32.class,UInt32[].class,Integer.class,int.class,Integer[].class,int[].class,
				UInt64.class,UInt64[].class,Long.class,long.class,Long[].class,long[].class,
				String.class,String[].class,
				Boolean.class,boolean.class,Boolean[].class,boolean[].class,
				Float.class,float.class,Float[].class,float[].class,
				Double.class,double.class,Double[].class,double[].class,
				Character.class,char.class,Character[].class,char[].class,
				DateTime.class,DateTime[].class,
				ObjectPath.class,ObjectPath[].class,
				OctetString.class,OctetString[].class,
				StructureValue.class,StructureValue[].class,
				EnumerationValue.class,EnumerationValue[].class};
		DataType [] type = {
				DataType.VOID,DataType.UINT8,DataType.UINT8_ARRAY,DataType.SINT8,DataType.SINT8,DataType.SINT8_ARRAY,DataType.SINT8_ARRAY, 
				DataType.UINT16,DataType.UINT16_ARRAY,DataType.SINT16,DataType.SINT16,DataType.SINT16_ARRAY,DataType.SINT16_ARRAY,
				DataType.UINT32,DataType.UINT32_ARRAY,DataType.SINT32,DataType.SINT32,DataType.SINT32_ARRAY,DataType.SINT32_ARRAY,
				DataType.UINT64,DataType.UINT64_ARRAY,DataType.SINT64,DataType.SINT64,DataType.SINT64_ARRAY,DataType.SINT64_ARRAY,
				DataType.STRING,DataType.STRING_ARRAY,
				DataType.BOOLEAN,DataType.BOOLEAN,DataType.BOOLEAN_ARRAY,DataType.BOOLEAN_ARRAY,
				DataType.REAL32,DataType.REAL32,DataType.REAL32_ARRAY,DataType.REAL32_ARRAY,
				DataType.REAL64,DataType.REAL64,DataType.REAL64_ARRAY,DataType.REAL64_ARRAY,
				DataType.CHAR16,DataType.CHAR16,DataType.CHAR16_ARRAY,DataType.CHAR16_ARRAY,
				DataType.DATETIME,DataType.DATETIME_ARRAY,
				DataType.OBJECTPATH,DataType.OBJECTPATH_ARRAY,
				DataType.OCTETSTRING,DataType.OCTETSTRING_ARRAY,
				DataType.STRUCTUREVALUE,DataType.STRUCTUREVALUE_ARRAY,
				DataType.ENUMERATIONVALUE,DataType.ENUMERATIONVALUE_ARRAY};
		
		assertEquals(classes.length,type.length);
		
		// check known classes and types
		for(int i=0; i< classes.length; i++){
			for(int j = 0; j < type.length; j++){
				if(type[i] == type[j]){
					assertTrue(DataType.getTypeForClass(classes[i]) == type[j]);
				} else {
					assertFalse(DataType.getTypeForClass(classes[i]) == type[j]);
				}
			}
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isCharacter()}.
	 */
	@Test
	public final void testIsCharacter(){
		// data objects
		boolean [] isCharacter = {false, false, false, false, false,
				false, false, false, false,
				false,false,false,false,
				false,true,false,
				false,false,false,
				false,
				false,false,false,false,
				false, false,false,false,
				false,false,false,false,
				false,true,false,
				false,false,false,
				false
		};
		
		assertEquals(isCharacter.length,type.length);
		assertEquals(isCharacter.length,DataType.values().length);
		
		for(int i = 0; i < isCharacter.length; i++){
			// System.out.println(type[i]+" "+type[i].isCharacter()+" "+type[i].isPrimitive());
			assertEquals(isCharacter[i],type[i].isCharacter());
		}

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isString()}.
	 */
	@Test
	public final void testIsString(){
		// data objects
		boolean [] isString = {false, false, false, false, false,
				false, false, false, false,
				true,false,false,false,
				false,false,false,
				false,false,false,
				false,
				false,false,false,false,
				false, false,false,false,
				true,false,false,false,
				false,false,false,
				false,false,false,
				false
		};
		assertEquals(isString.length,type.length);
		assertEquals(isString.length,DataType.values().length);
		
		for(int i = 0; i < isString.length; i++){
			assertEquals(isString[i],type[i].isString());
		}

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isInteger()}.
	 */
	@Test
	public final void testIsInteger(){
		// data objects
		boolean [] isInteger = {false, true, true, true, true,
				true, true, true, true,
				false,false,false,false,
				false,false,false,
				false,false,false,
				false,
				true,true,true,true,
				true, true,true,true,
				false,false,false,false,
				false,false,false,
				false,false,false,false
		};
		assertEquals(isInteger.length,type.length);
		assertEquals(isInteger.length,DataType.values().length);
		
		for(int i = 0; i < isInteger.length; i++){
			assertEquals(isInteger[i],type[i].isInteger());
		}

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isReal()}.
	 */
	@Test
	public final void testIsReal(){
		// data objects
		boolean [] isReal = {false, false, false, false, false,
				false, false, false, false,
				false,false,true,true,
				false,false,false,
				false,false,false,
				false,
				false,false,false,false,
				false, false,false,false,
				false,false,true,true,
				false,false,false,
				false,false,false,
				false
		};
		assertEquals(isReal.length,type.length);
		assertEquals(isReal.length,DataType.values().length);
		
		for(int i = 0; i < isReal.length; i++){
			assertEquals(isReal[i],type[i].isReal());
		}

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isPrimitive()}.
	 */
	@Test
	public final void testIsPrimitive(){
		// data objects
		boolean [] isPrimitive = {false, true, true, true, true,
				true, true, true, true,
				true,true,true,true,
				true,true,false,
				false,false,false,
				true,
				true,true,true,true,
				true, true,true,true,
				true,true,true,true,
				true,true,false,
				false,false,false,true
		};
		assertEquals(isPrimitive.length,type.length);
		assertEquals(isPrimitive.length,DataType.values().length);
		
		for(int i = 0; i < isPrimitive.length; i++){
			assertEquals(isPrimitive[i],type[i].isPrimitive());
		}

	}
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isEnumerationValue()}.
	 */
	@Test
	public final void testIsEumeration(){
		// data objects
		boolean [] isEnumeration = {false, false, false, false, false,
				false, false, false, false,
				false,false,false,false,
				false,false,false,
				true,false,false,
				false,
				false,false,false,false,
				false, false,false,false,
				false,false,false,false,
				false,false,false,
				true,false,false,false
		};
		assertEquals(isEnumeration.length,type.length);
		assertEquals(isEnumeration.length,DataType.values().length);
		
		for(int i = 0; i < isEnumeration.length; i++){
			assertEquals(isEnumeration[i],type[i].isEnumerationValue());
		}

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isInstanceValue()}.
	 */
	@Test
	public final void testIsInstanceValue(){
		// data objects
		boolean [] isInstanceValue = {false, false, false, false, false,
				false, false, false, false,
				false,false,false,false,
				false,false,false,
				false,false,true,
				false,
				false,false,false,false,
				false, false,false,false,
				false,false,false,false,
				false,false,false,
				false,false,true,false
		};
		assertEquals(isInstanceValue.length,type.length);
		assertEquals(isInstanceValue.length,DataType.values().length);
		
		for(int i = 0; i < isInstanceValue.length; i++){
			assertEquals(isInstanceValue[i],type[i].isInstanceValue());
		}

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isStructureValue()}.
	 */
	@Test
	public final void testIsStructureValue(){
		// data objects
		boolean [] isStructureValue = {false, false, false, false, false,
				false, false, false, false,
				false,false,false,false,
				false,false,false,
				false,true,false,
				false,
				false,false,false,false,
				false, false,false,false,
				false,false,false,false,
				false,false,false,
				false,true,false,false
		};
		assertEquals(isStructureValue.length,type.length);
		assertEquals(isStructureValue.length,DataType.values().length);
		
		for(int i = 0; i < isStructureValue.length; i++){
			assertEquals(isStructureValue[i],type[i].isStructureValue());
		}

	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isNumeric()}.
	 */
	@Test
	public final void testIsNumeric(){
		// data objects
		boolean [] isNumber = {false, true, true, true, true,
				true, true, true, true,
				false,false,true,true,
				false,false,false,
				false,false,false,
				false,
				true,true,true,true,
				true, true,true,true,
				false,false,true,true,
				false,false,false,
				false,false,false,false
		};
		assertEquals(isNumber.length,type.length);
		assertEquals(isNumber.length,DataType.values().length);
		
		for(int i = 0; i < isNumber.length; i++){
			assertEquals(isNumber[i],type[i].isNumeric());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isComplex()}.
	 */
	@Test
	public final void testIsComplex(){
		// data objects
		boolean [] isComplex = {false, false, false, false, false,
				false, false, false, false,
				false,false,false,false,
				false,false,false,
				true,true,true,
				false,
				false,false,false,false,
				false, false,false,false,
				false,false,false,false,
				false,false,false,
				true,true,true,false
		};
		assertEquals(isComplex.length,type.length);
		assertEquals(isComplex.length,DataType.values().length);

		for(int i = 0; i < isComplex.length; i++){
			assertEquals(isComplex[i],type[i].isComplex());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isOctetString()}.
	 */
	@Test
	public final void testIsOctetString(){
		// data objects
		boolean [] isOctetString = {false, false, false, false, false,
				false, false, false, false,
				false,false,false,false,
				false,false,false,
				false,false,false,
				true,
				false,false,false,false,
				false, false,false,false,
				false,false,false,false,
				false,false,false,
				false,false,false,true
		};
		assertEquals(isOctetString.length,type.length);
		assertEquals(isOctetString.length,DataType.values().length);
		
		for(int i = 0; i < isOctetString.length; i++){
			assertEquals(isOctetString[i],type[i].isOctetString());
		}

	}
	
	/**
	 * Test for exported classes
	 */
	@Test
	public final void testExported(){
		// annotated classes
		assertEquals(DataType.STRUCTUREVALUE,DataType.getTypeForClass(DefinedClass.class));
		assertEquals(DataType.STRUCTUREVALUE_ARRAY,DataType.getTypeForClass(DefinedClass[].class));
		assertEquals(DataType.ENUMERATIONVALUE,DataType.getTypeForClass(EnumBindingClass.class));
		assertEquals(DataType.ENUMERATIONVALUE_ARRAY,DataType.getTypeForClass(EnumBindingClass[].class));
		assertEquals(DataType.STRUCTUREVALUE,DataType.getTypeForClass(PropertyBindingClass.class));
		assertEquals(DataType.STRUCTUREVALUE_ARRAY,DataType.getTypeForClass(PropertyBindingClass[].class));
		assertEquals(DataType.INSTANCEVALUE,DataType.getTypeForClass(MethodBindingClass.class));
		assertEquals(DataType.INSTANCEVALUE_ARRAY,DataType.getTypeForClass(MethodBindingClass[].class));
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isVoid()}.
	 */
	@Test
	public final void testIsVoid(){
		for(DataType t : type){
			if(t.isVoid()){
				assertEquals(DataType.VOID,t);
			} else {
				assertNotSame(DataType.VOID,t);
			}
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isCimType(Class)}.
	 */
	@Test
	public final void testIsCimType(){
		Class<?>[] classes = {
				void.class,UInt8.class,UInt8[].class,Byte.class,byte.class,Byte[].class,byte[].class,
				UInt16.class, UInt16[].class, Short.class, short.class, Short[].class, short[].class,
				UInt32.class,UInt32[].class,Integer.class,int.class,Integer[].class,int[].class,
				UInt64.class,UInt64[].class,Long.class,long.class,Long[].class,long[].class,
				String.class,String[].class,
				Boolean.class,boolean.class,Boolean[].class,boolean[].class,
				Float.class,float.class,Float[].class,float[].class,
				Double.class,double.class,Double[].class,double[].class,
				Character.class,char.class,Character[].class,char[].class,
				DateTime.class,DateTime[].class,
				ObjectPath.class,ObjectPath[].class,
				OctetString.class,OctetString[].class,
				StructureValue.class,StructureValue[].class,
				EnumerationValue.class,EnumerationValue[].class,
				EnumBindingClass.class,MethodBindingClass.class,PropertyBindingClass.class,
				EnumBindingClass[].class,MethodBindingClass[].class,PropertyBindingClass[].class
		};
		// check known classes
		for(int i=0; i< classes.length; i++){
			assertTrue(DataType.isCimType(classes[i]));
		}
		
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.DataType#isUnsigned()}.
	 */
	@Test
	public final void testIsUnsigned(){
		// data objects
		boolean [] isUnsigned = {false, true, false, true, false,
				true, false, true, false,
				false,false,false,false,
				false,false,
				false,
				false,false,false,
				false,
				true,false,true,false,
				true, false,true,false,
				false,false,false,false,
				false,false,false,
				false,false,false,false
		};
		assertEquals(isUnsigned.length,type.length);
		assertEquals(isUnsigned.length,DataType.values().length);
		
		for(int i = 0; i < isUnsigned.length; i++){
			assertEquals(isUnsigned[i],type[i].isUnsigned());
		}
	}
}
