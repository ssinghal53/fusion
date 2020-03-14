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
 * Created Dec 23, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import java.util.Date;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Sharad Singhal
 *
 */
public class EnumerationValueTest {

	// data objects
	private static Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
		new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
		"foobar",true,Float.valueOf((float)45.0),(double) 500.,
		new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
		new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
		new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
		new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float)45.0)},new double[]{(double) 500.},
		new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)}
	};
	// data objects
	private static Object [] obj2 = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
		new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
		"foobar",true,Float.valueOf((float)45.0),(double) 500.,
		new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
		new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
		new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
		new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float)45.0)},new double[]{(double) 500.},
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
	// mof values
	private static String [] mof = {"name = null", "name = 8","name = -12","name = 22","name = 55","name = 100","name = -75","name = 351","name = 500",
		"name = \"foobar\"","name = true","name = 45.0","name = 500.0","name = \"20140420044028.080***+000\"","name = \'a\'","name = \"/class/cimv2:cim_test\"",
		"name = { 8 }","name = { -12 }","name = { 22 }",	"name = { 55 }","name = { 100 }","name = { -75 }","name = { 351 }","name = { 500 }","name = { \"foobar\" }",
		"name = { true }","name = { 45.0 }","name = { 500.0 }","name = { \"20140420044028.080***+000\" }","name = { \'a\' }","name = { \"/class/cimv2:cim_test\" }"
	};

	/*
	 * ******************************
	 * NOTE: DSP0004 restricts enumeration values to be either an integer type, or a string type. However, the class EnumerationValue
	 * does not know about the containing Enumeration class. Hence, it accepts all data types. The check for invalid EnumerationValue
	 * is done in the tests for class Enumeration
	 * ******************************
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("EnumerationValue ");
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
	 * Test for {@link net.aifusion.metamodel.EnumerationValue#getFullName()}.
	 * Test for {@link net.aifusion.metamodel.EnumerationValue#toString()}.
	 */
	
	@Test
	public final void testGetFullName(){
		for(int i = 0; i < type.length; i++){
			EnumerationValue v = new EnumerationValue("name","enum",new DataValue(type[i],obj[i]), null);
			assertNotNull(v);
			assertEquals("enum.name",v.getFullName());
			assertEquals("name",v.toString());
		}
		EnumerationValue v = new EnumerationValue("name","enum",null, null);
		assertNotNull(v);
		assertEquals("enum.name",v.getFullName());
		assertEquals("name",v.toString());
		return;		
	}
	
	/**
	 * Test for {@link net.aifusion.metamodel.EnumerationValue#getEnumName()}.
	 */
	@Test
	public final void testGetEnumName(){
		for(int i = 0; i < type.length; i++){
			EnumerationValue v = new EnumerationValue("name","enum",new DataValue(type[i],obj[i]), null);
			assertNotNull(v);
			assertEquals("enum",v.getEnumName());
			assertEquals("name",v.toString());
		}
		EnumerationValue v = new EnumerationValue("name","enum",null, null);
		assertNotNull(v);
		assertEquals("enum",v.getEnumName());
		assertEquals("name",v.toString());
		return;		
	}


	/**
	 * Test method for {@link net.aifusion.metamodel.EnumerationValue#toMOF(String)}.
	 */
	@Test
	public final void testToMOFString() {
		for(int i = 0; i < type.length; i++){
			EnumerationValue v = new EnumerationValue("name","enum",new DataValue(type[i],obj[i]), null);
			assertNotNull(v);
			assertEquals(mof[i],v.toMOF(""));
		}
		EnumerationValue v = new EnumerationValue("name","enum",null, null);
		assertNotNull(v);
		assertEquals("name",v.toMOF(""));
		
		assertEquals("\tname",v.toMOF("\t"));
		
		Vector<Qualifier> quals = new Vector<Qualifier>();
		quals.add(StandardQualifierType.DESCRIPTION.getQualifier("enum value", Constants.defaultNameSpacePath));
		v = new EnumerationValue("name","enum",new DataValue("bar"),quals);
		assertEquals("\t[Description(\"enum value\")]\n\tname = \"bar\"",v.toMOF("\t"));
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.EnumerationValue#EnumerationValue(java.lang.String, String, net.aifusion.metamodel.DataValue, java.util.List)}.
	 */
	@Test
	public final void testEnumerationValue() {
		for(int i = 0; i < type.length; i++){
			EnumerationValue v = new EnumerationValue("name","enum",new DataValue(type[i],obj[i]), null);
			assertNotNull(v);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.EnumerationValue#getDataValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(int i = 0; i < type.length; i++){
			EnumerationValue v = new EnumerationValue("name","enum",new DataValue(type[i],obj[i]), null);
			assertNotNull(v);
			assertEquals(new DataValue(type[i],obj2[i]),v.getDataValue());
		}
		// check that if null values are defined, enumeration dataValue returns the name of the enumeration
		EnumerationValue v = new EnumerationValue("name","enum",null, null);
		assertNotNull(v);
		assertEquals(new DataValue("name"),v.getDataValue());


	}

	/**
	 * Test method for {@link net.aifusion.metamodel.EnumerationValue#getDataType()}.
	 */
	@Test
	public final void testGetDataType(){
		for(int i = 0; i < type.length; i++){
			EnumerationValue v = new EnumerationValue("name","enum",new DataValue(type[i],obj[i]), null);
			assertNotNull(v);
			assertEquals(type[i],v.getDataType());
		}
		// check that if null values are defined, enumeration dataValue returns the name of the enumeration
		EnumerationValue v = new EnumerationValue("name","enum",null, null);
		assertNotNull(v);
		assertEquals(DataType.STRING,v.getDataType());
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.EnumerationValue#equals(Object)}.
	 */
	@Test
	public final void testEquals(){
		for(int i=0; i < type.length; i++){
			EnumerationValue vi = new EnumerationValue("name","enum",new DataValue(type[i],obj[i]), null);
			for(int j = 0; j < type.length; j++){
				EnumerationValue vj = new EnumerationValue("name","enum",new DataValue(type[j],obj2[j]), null);
				if(i == j)
					assertEquals(vi,vj);
				else
					assertNotEquals(vi, vj);
			}
		}
	}
}
