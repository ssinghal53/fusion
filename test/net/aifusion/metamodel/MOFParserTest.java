/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Feb 28, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Array;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests for the MOF parser
 * @author Sharad Singhal
 */
public class MOFParserTest {
	static CimClass cimClass;
	static CimStructure struct;
	static CimEnumeration en;
	static StructureValue sv;
	static Map<String,DataValue> propertyValues;
	static Map<String,DataValue> classPropertyValues;
	static InMemoryRepository repository;
	static MOFParser parser;
	static boolean verbose = false;

	static {
		// create instances of a structure, class, and enumeration
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
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null, null));
		en = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
	}

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

	// data objects
	private static Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
			new UInt32(100), -75, new UInt64("351"),Long.valueOf(500L),
			"foobar",true,Float.valueOf((float)45.0),(double) 500.,
			new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
			new EnumerationValue("name","enum",null, null),
			StructureValue.createStructureValue(struct, propertyValues, "$alias"),
			CimInstance.createInstance(cimClass, classPropertyValues, "$alias"),
			new OctetString("0xfe10ff"),
			new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
			new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
			new String[]{"foobar"},new boolean[]{true},new Float[]{Float.valueOf((float)45.0)},new double[]{(double) 500.},
			new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
			new EnumerationValue[]{new EnumerationValue("name","enum",null, null)},
			new StructureValue[]{StructureValue.createStructureValue(struct, propertyValues, "$alias")},
			new CimInstance[]{CimInstance.createInstance(cimClass, classPropertyValues, "$alias")},
			new OctetString[]{new OctetString("0xfe10ff")}
	};
	
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("MOFParser ");
		assertEquals(DataType.values().length,type.length);
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		assertEquals(type.length,obj.length);
		assertEquals(DataType.values().length,type.length);
		repository = new InMemoryRepository();
		parser = new MOFParser(repository);
	}

	/**
	 * Test to check enumerations
	 * @throws Exception
	 */
	@Test
	public void testEnumerations() throws Exception {
		repository.put(en);
		repository.put(struct);
		repository.put(cimClass);
		// try creating enums with different data types
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			if(t.isArray()) continue;
			StringBuilder b = new StringBuilder("Enumeration E_");
			b.append(t);
			b.append(" : ");
			if(t.isEnumerationValue()){
				b.append("enum");
			} else if(t.isStructureValue()){
				b.append("name");
			} else if(t.isInstanceValue()){
				b.append("Test_Class");
			} else {
				b.append(t.getComponentType());
			}
			b.append(" { enumPropName = ");
			if(t.isEnumerationValue()){
				b.append("\"enumPropValue\"");
			} else {
				b.append(ModelUtilities.toMOFString(t, obj[i]));
			}
			b.append(" };");
			// System.out.println(b.toString());
			try {
				ByteArrayInputStream in = new ByteArrayInputStream(b.toString().getBytes("UTF-8"));
				parser.parse(in, null);
				in.close();
				assertTrue(t.isInteger() || t.isString() || t.isEnumerationValue());
				// System.out.println("Succeeded");
			} catch (ModelException e){
				// System.out.println(e);
				assertFalse(t.isInteger() || t.isString() || t.isEnumerationValue());
				assertEquals(6,e.getReason().getCode());
			}
		}
	}
	
	/**
	 * Test to check valid qualifiers
	 */
	@Test
	public void testQualifiers() throws Exception {
		repository.put(en);
		repository.put(struct);
		repository.put(cimClass);

		// Try all data types with values
		for(int i=0; i < type.length; i++){
			DataType t = type[i];
			StringBuilder b = new StringBuilder("Qualifier T_");
			b.append(t);
			b.append(" : ");

			if(t.isEnumerationValue()){
				b.append("enum");
			} else if(t.isStructureValue()){
				b.append("name");
			} else if(t.isInstanceValue()){
				b.append("Test_Class");
			} else {
				b.append(t.getComponentType());
			}
			b.append(" ");
			if(t.isArray()) b.append("[ ] ");
			b.append("= ");
			if(t.isArray()){
				b.append("{ ");
				b.append(ModelUtilities.toMOFString(t.getComponentType(), Array.get(obj[i], 0)));
				b.append(" }");
			} else {
				b.append(ModelUtilities.toMOFString(t, obj[i]));
			}
			b.append(" Scope(Any);");
			if(verbose) System.out.println(" Try "+b.toString());
			try {
				ByteArrayInputStream in = new ByteArrayInputStream(b.toString().getBytes("UTF-8"));
				parser.parse(in, null);
				in.close();
				assertTrue(t.isPrimitive() || t.isEnumerationValue());
				if(verbose) System.out.println("Succeeded");
			} catch (ModelException e){
				// this will assume an enumValue
				if(verbose) System.out.println(e);
				assertEquals(6,e.getReason().getCode());
			}
		}
		List<NamedElement> elements = repository.getElements(null,null,null, false);
		assertEquals(35,elements.size());
		
		// Now try all data types without values
		for(int i=0; i < type.length; i++){
			DataType t = type[i];
			StringBuilder b = new StringBuilder("Qualifier V_");
			b.append(t);
			b.append(" : ");

			if(t.isEnumerationValue()){
				b.append("enum");
			} else if(t.isStructureValue()){
				b.append("name");
			} else if(t.isInstanceValue()){
				b.append("Test_Class");
			} else {
				b.append(t.getComponentType());
			}
			b.append(" ");
			if(t.isArray()) b.append("[ ] ");
			b.append(" Scope(Any);");
			if(verbose) System.out.println(" Try "+b.toString());
			try {
				ByteArrayInputStream in = new ByteArrayInputStream(b.toString().getBytes("UTF-8"));
				parser.parse(in, null);
				in.close();
				assertTrue(t.isPrimitive() || t.isEnumerationValue());
				if(verbose) System.out.println("Succeeded");
			} catch (ModelException e){
				// this will assume an enumValue
				if(verbose) System.out.println(e);
				assertTrue(e.getReason().getCode() == 6 || e.getReason().getCode() == 1);
			}
		}
		elements = repository.getElements(null,null,null, false);
		assertEquals(65,elements.size());
	}

	@Test
	public final void testGolfSchema() {
		InMemoryRepository r = new InMemoryRepository();
		MOFParser p = new MOFParser(r);
		try {
			p.parse("testcases/golf/GOLF_Schema.mof", null);
			assertEquals(21,r.getElements(null,null,null, false).size());
		} catch (ModelException e){
			if(verbose) System.out.println(e);
			fail("Parse failed");
		}
	}

}
