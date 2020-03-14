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
 * Created Dec 24, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Date;
import java.util.Set;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test Enumerations
 * @author Sharad Singhal
 */
public class EnumerationTest {

	// data objects
	private static Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
		new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
		"foobar",true,Float.valueOf((float) 45.0),(double) 500.,
		new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
		new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
		new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
		new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float) 45.0)},new double[]{(double) 500.},
		new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)}
	};
	// known data types
	private static DataType [] type = {DataType.VOID, DataType.UINT8, DataType.SINT8, DataType.UINT16, DataType.SINT16,
		DataType.UINT32, DataType.SINT32, DataType.UINT64, DataType.SINT64,
		DataType.STRING, DataType.BOOLEAN, DataType.REAL32, DataType.REAL64,
		DataType.DATETIME, DataType.CHAR16, DataType.OBJECTPATH,
		DataType.UINT8_ARRAY, DataType.SINT8_ARRAY, DataType.UINT16_ARRAY, DataType.SINT16_ARRAY,
		DataType.UINT32_ARRAY, DataType.SINT32_ARRAY, DataType.UINT64_ARRAY, DataType.SINT64_ARRAY,
		DataType.STRING_ARRAY, DataType.BOOLEAN_ARRAY, DataType.REAL32_ARRAY, DataType.REAL64_ARRAY,
		DataType.DATETIME_ARRAY, DataType.CHAR16_ARRAY, DataType.OBJECTPATH_ARRAY};
	
	private static NameSpacePath path = Constants.defaultNameSpacePath;
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Enumeration ");
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
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#toMOF(String)}.
	 */
	@Test
	public final void testToMOFString() {
		Vector<Qualifier> quals = new Vector<Qualifier>();
		quals.add(StandardQualifierType.DESCRIPTION.getQualifier("Parent", Constants.defaultNameSpacePath));
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		values.add(new EnumerationValue("pv1","Parent",new DataValue("Parent Value1"), null));
		values.add(new EnumerationValue("pv2","Parent",new DataValue("Parent Value2"), null));
		CimEnumeration parent = new CimEnumeration("Parent",null,quals,path,DataType.STRING,values);
		assertNotNull(parent);
		assertEquals("\t[Description(\"Parent\")]\n\tEnumeration Parent : String {\n\t\tpv1 = \"Parent Value1\",\n\t\tpv2 = \"Parent Value2\"\n\t};\n",parent.toMOF("\t"));
		
		values.clear();
		values.add(new EnumerationValue("cv1","Parent.child",new DataValue("Child Value1"), null));
		values.add(new EnumerationValue("cv2","Parent.child",new DataValue("Child Value2"), null));
		CimEnumeration child = new CimEnumeration("child",parent,null,path,DataType.STRING,values);
		assertNotNull(child);
		assertEquals("\tEnumeration child : Parent {\n\t\tcv1 = \"Child Value1\",\n\t\tcv2 = \"Child Value2\"\n\t};\n",child.toMOF("\t"));
	}


	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#Enumeration(java.lang.String, net.aifusion.metamodel.DataType, java.util.List, java.util.List, net.aifusion.metamodel.NameSpacePath, net.aifusion.metamodel.CimEnumeration)}.
	 */
	@Test
	public final void testEnumeration() {
		// only string and int enums are allowed
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		CimEnumeration en = null;
		for(int i = 0; i < type.length; i++){
			values.clear();
			EnumerationValue v = new EnumerationValue("valueName","Enum",new DataValue(type[i],obj[i]), null);
			values.add(v);
			try {
				en = new CimEnumeration("Enum",null,null,path,type[i],values);
				assertNotNull(en);
				// we should be creating an integer or string enum
				assertTrue(type[i].isInteger() || type[i] == DataType.STRING);
			} catch(ModelException ex){
				// check that we have an invalid parameter exception
				assertEquals(4,ex.getReason().getCode());
				// check that type is not string or integer
				assertTrue(type[i].isArray() || !(type[i].isInteger() || type[i].isString()));
			}
		}
				
		// enum values must match the enum type
		assertEquals(DataType.UINT8,type[1]);
		assertEquals(DataType.STRING,type[9]);
		values.clear();
		values.add(new EnumerationValue("IntegerValue","Mismatched",new DataValue(type[1],obj[1]), null));	// uint8 dataValue
		values.add(new EnumerationValue("StringValue","Mismatched",new DataValue(type[9],obj[9]), null));	// string dataValue
		try {
			en = new CimEnumeration("Mismatched",null,null,path,type[1],values);
			fail("This call should not succeed -- mismatched types");
		} catch(ModelException ex){
			// check that we have an invalid parameter exception
			assertEquals(4,ex.getReason().getCode());
		}
		try {
			en = new CimEnumeration("Mismatched",null,null,path,type[9],values);
			fail("This call should not succeed -- mismatched types");
		} catch(ModelException ex){
			// check that we have an invalid parameter exception
			assertEquals(4,ex.getReason().getCode());
		}
		
		// String enums can have no values in them-- the dataValue is the name of the dataValue itself
		values.clear();
		values.add(new EnumerationValue("NullValue","NullValuedEnum",null, null));	// null string dataValue
		CimEnumeration parent = new CimEnumeration("NullValuedEnum",null,null,path,DataType.STRING,values);
		assertNotNull(parent);
		assertEquals(new DataValue("NullValue"),parent.getDataValue("NullValue"));
		
		// enums can inherit from other enums
		values.clear();
		values.add(new EnumerationValue("ChildValue","NullValuedEnum.ChildEnum",new DataValue(type[9],obj[9]), null));
		CimEnumeration child = new CimEnumeration("ChildEnum",parent,null,path,DataType.STRING,values);
		assertNotNull(child);
		// child should have two keys declared in it
		assertEquals(2,child.getKeys().size());
		
		// enum types must match in inherited enums
		values.clear();
		values.add(new EnumerationValue("IntegerValue","ChildEnum",new DataValue(type[1],obj[1]), null));	// uint8 dataValue
		try {
			child = new CimEnumeration("ChildEnum",parent,null,path,DataType.STRING,values);
			fail("This call should not succeed");
		} catch (ModelException ex){
			// check that we have an invalid parameter exception
			assertEquals(4,ex.getReason().getCode());
		}
		
		// enums must not re-declare parent enum values
		values.clear();
		values.add(new EnumerationValue("NullValue","ChildEnum",new DataValue(type[9],obj[9]), null));	// duplicate Named string dataValue
		try {
			child = new CimEnumeration("ChildEnum",parent,null,path,DataType.STRING,values);
			fail("This call should not succeed");
		} catch (ModelException ex){
			// check that we have an invalid parameter exception
			assertEquals(4,ex.getReason().getCode());
		}

	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#getDataType()}.
	 */
	@Test
	public final void testGetDataType() {
		// only string and int enums are allowed
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		for(int i = 0; i < type.length; i++){
			values.clear();
			EnumerationValue v = new EnumerationValue("valueName","Enum",new DataValue(type[i],obj[i]), null);
			values.add(v);
			try {
				CimEnumeration en = new CimEnumeration("Enum",null,null,path,type[i],values);
				assertNotNull(en);
				// we should be creating an integer or string enum
				assertTrue(type[i].isInteger() || type[i] == DataType.STRING);
				// datatype must match
				assertEquals(type[i],en.getDataType());
			} catch(ModelException ex){
				// check that we have an invalid parameter exception
				assertEquals(4,ex.getReason().getCode());
				// System.out.println(type[i]+" : "+obj[i]);
				// check that type is not an array, string or integer type
				assertTrue(type[i].isArray() || !(type[i].isInteger() || type[i].isString()) );
			}
		}

	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#getLowerCaseKeys()}.
	 */
	@Test
	public final void testGetLowerCaseKeys() {
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		values.add(new EnumerationValue("Pv1","Parent",new DataValue("Parent Value1"), null));
		values.add(new EnumerationValue("Pv2","Parent",new DataValue("Parent Value2"), null));
		CimEnumeration parent = new CimEnumeration("Parent",null,null,path,DataType.STRING,values);
		assertNotNull(parent);
		values.clear();
		values.add(new EnumerationValue("Cv1","Parent.child",new DataValue("Child Value1"), null));
		values.add(new EnumerationValue("Cv2","Parent.child",new DataValue("Child Value2"), null));
		CimEnumeration child = new CimEnumeration("child",parent,null,path,DataType.STRING,values);
		assertNotNull(child);
		Set<String> keys = parent.getKeys();
		assertEquals(2,keys.size());
		for(String k : new String[]{"Pv1","Pv2"})
			assertTrue(keys.contains(k));
		keys = child.getKeys();
		assertEquals(4,keys.size());
		for(String k : new String[]{"Pv1","Pv2","Cv1","Cv2"})
			assertTrue(keys.contains(k));
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#getKeys()}.
	 */
	@Test
	public final void testGetKeys() {
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		values.add(new EnumerationValue("Pv1","Parent",new DataValue("Parent Value1"), null));
		values.add(new EnumerationValue("Pv2","Parent",new DataValue("Parent Value2"), null));
		CimEnumeration parent = new CimEnumeration("Parent",null,null,path,DataType.STRING,values);
		assertNotNull(parent);
		values.clear();
		values.add(new EnumerationValue("Cv1","Parent.child",new DataValue("Child Value1"), null));
		values.add(new EnumerationValue("Cv2","Parent.child",new DataValue("Child Value2"), null));
		CimEnumeration child = new CimEnumeration("child",parent,null,path,DataType.STRING,values);
		assertNotNull(child);
		Set<String> keys = parent.getKeys();
		assertEquals(2,keys.size());
		for(String k : new String[]{"Pv1","Pv2"})
			assertTrue(keys.contains(k));
		keys = child.getKeys();
		assertEquals(4,keys.size());
		for(String k : new String[]{"Pv1","Pv2","Cv1","Cv2"})
			assertTrue(keys.contains(k));
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#hasKey(String)}.
	 */
	@Test
	public final void testHasKey() {
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		values.add(new EnumerationValue("pv1","Parent",new DataValue("Parent Value1"), null));
		values.add(new EnumerationValue("pv2","Parent",new DataValue("Parent Value2"), null));
		CimEnumeration parent = new CimEnumeration("Parent",null,null,path,DataType.STRING,values);
		assertNotNull(parent);
		values.clear();
		values.add(new EnumerationValue("cv1","Parent.child",new DataValue("Child Value1"), null));
		values.add(new EnumerationValue("cv2","Parent.child",new DataValue("Child Value2"), null));
		CimEnumeration child = new CimEnumeration("child",parent,null,path,DataType.STRING,values);
		assertNotNull(child);
		Set<String> keys = parent.getKeys();
		assertEquals(2,keys.size());
		for(String k : new String[]{"PV1","PV2"}){
			assertTrue(parent.hasKey(k));
			assertTrue(child.hasKey(k));
		}
		keys = child.getKeys();
		assertEquals(4,keys.size());
		for(String k : new String[]{"CV1","CV2","PV1","PV2"})
			assertTrue(child.hasKey(k));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimEnumeration#getDataValue(java.lang.String)}.
	 */
	@Test
	public final void testGetValue() {
		Vector<EnumerationValue> values = new Vector<EnumerationValue>();
		values.add(new EnumerationValue("pv1","Parent",new DataValue("Parent Value1"), null));
		values.add(new EnumerationValue("pv2","Parent",new DataValue("Parent Value2"), null));
		CimEnumeration parent = new CimEnumeration("Parent",null,null,path,DataType.STRING,values);
		assertNotNull(parent);
		assertEquals(new DataValue("Parent Value1"),parent.getDataValue("pV1"));	// note that getValue should be case insensitive
		assertEquals(new DataValue("Parent Value2"),parent.getDataValue("Pv2"));
		
		values.clear();
		values.add(new EnumerationValue("cv1","Parent.child",new DataValue("Child Value1"), null));
		values.add(new EnumerationValue("cv2","Parent.child",new DataValue("Child Value2"), null));
		CimEnumeration child = new CimEnumeration("child",parent,null,path,DataType.STRING,values);
		assertNotNull(child);
		assertEquals(new DataValue("Parent Value1"),parent.getDataValue("pV1"));
		assertEquals(new DataValue("Parent Value2"),parent.getDataValue("Pv2"));
		assertEquals(new DataValue("Child Value1"),child.getDataValue("cV1"));
		assertEquals(new DataValue("Child Value2"),child.getDataValue("cv2"));
	}
	// TODO: Add test for equals()

}
