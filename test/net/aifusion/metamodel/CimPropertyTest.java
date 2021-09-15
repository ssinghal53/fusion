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
 * Created Nov 1, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Date;
import java.util.HashMap;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import net.aifusion.metamodel.PropertyBindingClass.PropertyBinding;

/**
 * Unit test for CimProperty
 * @author Sharad Singhal
 *
 */
public class CimPropertyTest {
	static boolean verbose = false;
	static CimStructure struct;	// Test_Struct
	static StructureValue structValue, structValue2;	// Test_Struct value
	
	static CimClass cimClass;	// Test_Struct.Test_Class
	static CimInstance instance, instance2;		// Test_Class instance
	
	static CimEnumeration enumeration;		// Enumeration with name "enum", that contains a single enumerationValue called "name"
	static EnumerationValue enumValue, enumValue2;	// enumeration value (name)
	
	static DefinedClass defClass, defClass2;	// annotated class value
	
	static {

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
		
		// create a defined class
		defClass = new DefinedClass();
		defClass2 = new DefinedClass();


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
	String [] mof = {"null", "UInt8 propName = 8;\n","SInt8 propName = -12;\n","UInt16 propName = 22;\n",
			"SInt16 propName = 55;\n","UInt32 propName = 100;\n","SInt32 propName = -75;\n","UInt64 propName = 351;\n","SInt64 propName = 500;\n",
			"String propName = \"foobar\";\n","Boolean propName = true;\n","Real32 propName = 45.0;\n","Real64 propName = 500.0;\n",
			"Datetime propName = \"20140420044028.080***+000\";\n","Char16 propName = \'a\';\n","Cim_Test ref propName = \"/class/cimv2:CIM_Test\";\n",
			"OctetString propName = \"0x213244\";\n",
			"enum propName = name;\n","Test_Struct propName = value of Test_Struct as $as {\n\tkz = false;\n};\n",
			"Test_Class propName = instance of Test_Class as $ac {\n\tp2 = \"newP2Value\";\n\tkz = true;\n};\n",
			"UInt8 [] propName = { 8 };\n","SInt8 [] propName = { -12 };\n","UInt16 [] propName = { 22 };\n", "SInt16 [] propName = { 55 };\n",
			"UInt32 [] propName = { 100 };\n","SInt32 [] propName = { -75 };\n","UInt64 [] propName = { 351 };\n","SInt64 [] propName = { 500 };\n",
			"String [] propName = { \"foobar\" };\n",
			"Boolean [] propName = { true };\n","Real32 [] propName = { 45.0 };\n","Real64 [] propName = { 500.0 };\n",
			"Datetime [] propName = { \"20140420044028.080***+000\" };\n","Char16 [] propName = { \'a\' };\n",
			"Cim_Test ref [] propName = { \"/class/cimv2:CIM_Test\" };\n",
			"OctetString [] propName = { \"0x213244\" };\n",
			"enum [] propName = { name };\n","Test_Struct [] propName = { value of Test_Struct as $as {\n\tkz = false;\n} };\n",
			"Test_Class [] propName = { instance of Test_Class as $ac {\n\tp2 = \"newP2Value\";\n\tkz = true;\n} };\n"
	};

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("CimProperty ");
		assertEquals(DataType.values().length,type.length);
		assertEquals(obj.length,type.length);
		assertEquals(type.length,obj2.length);
		assertNotNull(struct);
		assertNotNull(structValue);
		assertNotNull(cimClass);
		assertNotNull(instance);
		assertNotNull(enumeration);
		assertNotNull(enumValue);
		assertNotNull(defClass);
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
	 * Test method for {@link net.aifusion.metamodel.CimProperty#toMOF(java.lang.String)}.
	 */
	@Test
	public final void testToMOFString() {
		assertEquals(type.length,mof.length);
		
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			if(verbose) System.out.println("Trying "+propType);
			// The property in class My_Class with name propName, and refers to others
			CimProperty p = null;
			if(propType.isPrimitive()){
				p = new CimProperty("My_Class","propName",propType,defaultValue,null);
			} else if(propType.isReference()){
				p = new CimProperty("My_Class","propName","Cim_Test",isArray,defaultValue,null);
			} else if(propType.isEnumerationValue()){
				p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
			} else if(propType.isStructureValue()){
				p = new CimProperty("My_Class","propName",struct,isArray,defaultValue,null);
			} else if(propType.isInstanceValue()){
				p = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue,null);				
			}
			assertNotNull(p);
			if(verbose) System.out.println(p.toMOF());
			assertEquals(mof[i],p.toMOF());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null,null));
		CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);

		Vector<QualifiedElement> sv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,sv);

		Vector<QualifiedElement> cv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimClass cimClass = new CimClass(ElementType.CLASS,"Test_Struct",null,null,Constants.defaultNameSpacePath,cv);

		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue1 = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			CimProperty p1 = null;
			if(propType.isPrimitive()){
				p1 = new CimProperty("Cim_Test","propName",propType,defaultValue1,null);
			} else if(propType.isReference()){
				p1 = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue1,null);
			} else if(propType.isEnumerationValue()){
				p1 = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue1,null);
			} else if(propType.isStructureValue()){
				p1 = new CimProperty("My_Class","propName",structure,isArray,defaultValue1,null);
			} else if(propType.isInstanceValue()){
				p1 = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue1,null);				
			}
			for(int j = 1; j < type.length; j++){
				DataType propType2 = type[j];
				DataValue defaultValue2 = new DataValue(propType2,obj2[j]);
				isArray = propType2.isArray();
				CimProperty p2 = null;
				if(propType2.isPrimitive()){
					p2 = new CimProperty("Cim_Test","propName",propType2,defaultValue2,null);
				} else if(propType2.isReference()){
					p2 = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue2,null);
				} else if(propType2.isEnumerationValue()){
					p2 = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue2,null);
				} else if(propType2.isStructureValue()){
					p2 = new CimProperty("My_Class","propName",structure,isArray,defaultValue2,null);
				} else if(propType2.isInstanceValue()){
					p2 = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue2,null);				
				}
				if(i == j){
					assertEquals(p1,p2);
				} else {
					assertNotEquals(p1, p2);
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getFullName()}.
	 */
	@Test
	public final void testGetFullName() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			if(!propType.isPrimitive()) continue;
			DataValue defaultValue = new DataValue(propType,obj[i]);
			// System.out.println("Trying "+propType);
			CimProperty p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			assertNotNull(p);
			// System.out.println(p.toMOF());
			assertEquals("Cim_Test#propName",p.getFullName());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#CimProperty(String, String, DataType, DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimPropertyPrimitiveConstructor() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			// System.out.println("Trying "+propType);
			try {
				CimProperty p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
				assertTrue(propType.isPrimitive());
				assertNotNull(p);
				// System.out.println(p.toMOF());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
				// assertEquals(4,e.getReason().getCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#CimProperty(String, String, CimStructure, boolean, DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimPropertyReferenceConstructor() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			boolean isArray = propType.isArray();
			DataValue defaultValue = new DataValue(propType,obj[i]);
			// System.out.println("Trying "+propType);
			try {
				CimProperty p = new CimProperty("My_Class","propName","Cim_Test",isArray,defaultValue,null);
				assertTrue(propType.isReference());
				assertNotNull(p);
				// System.out.println(p.toMOF());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
				// assertEquals(4,e.getReason().getCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#CimProperty(String, String, CimEnumeration, boolean, DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimPropertyEnumerationConstructor() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			boolean isArray = propType.isArray();
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<EnumerationValue> v = new Vector<EnumerationValue>();
			v.add(new EnumerationValue("name","enum",null,null));
			CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,v);
			// System.out.println("Trying "+propType);
			try {
				CimProperty p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
				assertTrue(propType.isEnumerationValue());
				assertNotNull(p);
				// System.out.println(p.toMOF());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
				// assertEquals(4,e.getReason().getCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#CimProperty(String, String, CimStructure, boolean, DataValue, java.util.List)}.
	 */
	@Test
	public final void testCimPropertyStructureConstructor() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			boolean isArray = propType.isArray();
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<QualifiedElement> v = new Vector<QualifiedElement>();
			v.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
			CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,v);
			// System.out.println("Trying "+propType);
			try {
				CimProperty p = new CimProperty("My_Class","propName",structure,isArray,defaultValue,null);
				assertTrue(propType.isStructureValue());
				assertNotNull(p);
				// System.out.println(p.toMOF());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getOriginClass()}.
	 */
	@Test
	public final void testGetOriginClass() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			// System.out.println("Trying "+propType);
			try {
				CimProperty p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
				assertTrue(propType.isPrimitive());
				assertNotNull(p);
				assertEquals("Cim_Test",p.getOriginClass());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
				// assertEquals(4,e.getReason().getCode());
			}
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getRefClassName()}.
	 */
	@Test
	public final void testGetRefClassName() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			boolean isArray = propType.isArray();
			DataValue defaultValue = new DataValue(propType,obj[i]);
			// System.out.println("Trying "+propType);
			try {
				CimProperty p = new CimProperty("My_Class","propName","Cim_Test",isArray,defaultValue,null);
				assertTrue(propType.isReference());
				assertNotNull(p);
				assertEquals("Cim_Test",p.getRefClassName());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getEnum()}.
	 */
	@Test
	public final void testGetEnum() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			boolean isArray = propType.isArray();
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<EnumerationValue> v = new Vector<EnumerationValue>();
			v.add(new EnumerationValue("name","enum",null,null));
			CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,v);
			try {
				CimProperty p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
				assertTrue(propType.isEnumerationValue());
				assertNotNull(p);
				assertEquals(enumeration, p.getEnum());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
				// assertEquals(4,e.getReason().getCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getStruct()}.
	 */
	@Test
	public final void testGetStruct() {
		for(int i = 0; i < type.length; i++){
			DataType propType = type[i];
			boolean isArray = propType.isArray();
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<QualifiedElement> v = new Vector<QualifiedElement>();
			v.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
			CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,v);
			// System.out.println("Trying "+propType);
			try {
				CimProperty p = new CimProperty("My_Class","propName",structure,isArray,defaultValue,null);
				assertTrue(propType.isStructureValue());
				assertNotNull(p);
				assertEquals(structure,p.getStruct());
			} catch (ModelException e){
				int code = e.getReason().getCode();
				assertEquals(13,code);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getDataType()}.
	 */
	@Test
	public final void testGetDataType() {
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null,null));
		CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
		
		Vector<QualifiedElement> sv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,sv);
		
		Vector<QualifiedElement> cv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimClass cimClass = new CimClass(ElementType.CLASS,"Test_Struct",null,null,Constants.defaultNameSpacePath,cv);
		
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			if(verbose) System.out.println("Trying "+propType);
			
			CimProperty p = null;
			if(propType.isPrimitive()){
				p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			} else if(propType.isReference()){
				p = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue,null);
			} else if(propType.isEnumerationValue()){
				p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
			} else if(propType.isStructureValue()){
				p = new CimProperty("My_Class","propName",structure,isArray,defaultValue,null);
			} else if(propType.isInstanceValue()){
				p = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue,null);				
			}
			assertNotNull(p);
			assertEquals(type[i],p.getDataType());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#hasDefaultValue()}.
	 */
	@Test
	public final void testHasDefaultValue() {
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null,null));
		CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
		
		Vector<QualifiedElement> sv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("name","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,sv);
		
		Vector<QualifiedElement> cv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimClass cimClass = new CimClass(ElementType.CLASS,"Test_Struct",null,null,Constants.defaultNameSpacePath,cv);
		
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			if(verbose) System.out.println("Trying "+propType);
			
			CimProperty p = null;
			if(propType.isPrimitive()){
				p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			} else if(propType.isReference()){
				p = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue,null);
			} else if(propType.isEnumerationValue()){
				p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
			} else if(propType.isStructureValue()){
				p = new CimProperty("My_Class","propName",structure,isArray,defaultValue,null);
			} else if(propType.isInstanceValue()){
				p = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue,null);				
			}
			assertNotNull(p);
			assertTrue(p.hasDefaultValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getDefaultValue()}.
	 */
	@Test
	public final void testGetDefaultValue() {
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null,null));
		CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
		
		Vector<QualifiedElement> sv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,sv);
		
		Vector<QualifiedElement> cv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimClass cimClass = new CimClass(ElementType.CLASS,"Test_Struct",null,null,Constants.defaultNameSpacePath,cv);
		
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			if(verbose) System.out.println("Trying "+propType);
			
			CimProperty p = null;
			if(propType.isPrimitive()){
				p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			} else if(propType.isReference()){
				p = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue,null);
			} else if(propType.isEnumerationValue()){
				p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
			} else if(propType.isStructureValue()){
				p = new CimProperty("My_Class","propName",structure,isArray,defaultValue,null);
			} else if(propType.isInstanceValue()){
				p = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue,null);				
			}
			assertNotNull(p);
			assertEquals(defaultValue,p.getDefaultValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#hasValue()}.
	 */
	@Test
	public final void testHasValue() {
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("Test_Struct","enum",null,null));
		CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
		
		Vector<QualifiedElement> sv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,sv);
		
		Vector<QualifiedElement> cv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimClass cimClass = new CimClass(ElementType.CLASS,"Test_Struct",null,null,Constants.defaultNameSpacePath,cv);
		
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			// System.out.println("Trying "+propType);
			
			CimProperty p = null;
			if(propType.isPrimitive()){
				p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			} else if(propType.isReference()){
				p = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue,null);
			} else if(propType.isEnumerationValue()){
				p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
			} else if(propType.isStructureValue()){
				p = new CimProperty("My_Class","propName",structure,isArray,defaultValue,null);
			} else if(propType.isInstanceValue()){
				p = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue,null);				
			}
			assertNotNull(p);
			assertFalse(p.hasValue());
			p.writeValue(new DataValue(propType,null));
			assertTrue(p.hasValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#hasNonNullValue()}.
	 */
	@Test
	public final void testHasNonNullValue() {
		Vector<EnumerationValue> ev = new Vector<EnumerationValue>();
		ev.add(new EnumerationValue("name","enum",null,null));
		CimEnumeration enumeration = new CimEnumeration("enum",null,null,Constants.defaultNameSpacePath,DataType.STRING,ev);
		
		Vector<QualifiedElement> sv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("name","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimStructure structure = new CimStructure(ElementType.STRUCTURE,"Test_Struct",null,null,Constants.defaultNameSpacePath,sv);
		
		Vector<QualifiedElement> cv = new Vector<QualifiedElement>();
		sv.add(new CimProperty("Test_Struct","innerProp",DataType.BOOLEAN,new DataValue(true),null));
		CimClass cimClass = new CimClass(ElementType.CLASS,"Test_Struct",null,null,Constants.defaultNameSpacePath,cv);
		
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			// System.out.println("Trying "+propType);
			
			CimProperty p = null;
			if(propType.isPrimitive()){
				p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			} else if(propType.isReference()){
				p = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue,null);
			} else if(propType.isEnumerationValue()){
				p = new CimProperty("My_Class","propName",enumeration,isArray,defaultValue,null);
			} else if(propType.isStructureValue()){
				p = new CimProperty("My_Class","propName",structure,isArray,defaultValue,null);
			} else if(propType.isInstanceValue()){
				p = new CimProperty("My_Class","propName",cimClass,isArray,defaultValue,null);				
			}
			assertNotNull(p);
			assertFalse(p.hasValue());
			p.writeValue(new DataValue(propType,null));
			assertTrue(p.hasValue());
			assertFalse(p.hasNonNullValue());
			p.writeValue(defaultValue);
			assertTrue(p.hasNonNullValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#isReadable()}.
	 */
	@Test
	public final void testIsReadable() {
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			if(!propType.isPrimitive()) continue;
			
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<Qualifier> quals = new Vector<Qualifier>();
			quals.add(StandardQualifierType.READ.getQualifier(false, Constants.defaultNameSpacePath));
			
			CimProperty	p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			assertNotNull(p);
			assertTrue(p.isReadable());
			p = new CimProperty("Cim_Test","propName",propType,defaultValue,quals);
			assertFalse(p.isReadable());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#isWritable()}.
	 */
	@Test
	public final void testIsWritable() {
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			if(!propType.isPrimitive()) continue;
			
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<Qualifier> quals = new Vector<Qualifier>();
			quals.add(StandardQualifierType.WRITE.getQualifier(true, Constants.defaultNameSpacePath));
			
			CimProperty	p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			assertNotNull(p);
			assertFalse(p.isWritable());
			p = new CimProperty("Cim_Test","propName",propType,defaultValue,quals);
			assertTrue(p.isWritable());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#isKey()}.
	 */
	@Test
	public final void testIsKey() {
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			if(!propType.isPrimitive()) continue;	
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<Qualifier> quals = new Vector<Qualifier>();
			quals.add(StandardQualifierType.KEY.getQualifier(true, Constants.defaultNameSpacePath));
			try {
				CimProperty	p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
				assertNotNull(p);
				assertFalse(p.isKey());
				p = new CimProperty("Cim_Test","propName",propType,defaultValue,quals);
				assertTrue(p.isKey());
			} catch (ModelException e){
				assertEquals(4,e.getReason().getCode());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			if(!propType.isPrimitive()) continue;
			
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<Qualifier> quals = new Vector<Qualifier>();
			quals.add(StandardQualifierType.READ.getQualifier(false, Constants.defaultNameSpacePath));
			
			CimProperty	p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			assertNotNull(p);
			DataValue v = p.getValue();
			assertEquals(defaultValue,v);
			p = new CimProperty("Cim_Test","propName",propType,defaultValue,quals);
			try {
				v = p.getValue();
				fail("value returned on non-readable property");
			} catch (ModelException e){
				assertEquals(2,e.getReason().getCode());	// access denied
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#setValue(net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testSetValue() {
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			if(!propType.isPrimitive()) continue;
			
			DataValue defaultValue = new DataValue(propType,obj[i]);
			Vector<Qualifier> quals = new Vector<Qualifier>();
			quals.add(StandardQualifierType.WRITE.getQualifier(true, Constants.defaultNameSpacePath));
			
			CimProperty	p = new CimProperty("Cim_Test","propName",propType,defaultValue,null);
			assertNotNull(p);
			try {
				p.setValue(defaultValue);
				fail("value accepted on non-writable property");
			} catch (ModelException e){
				assertEquals(2,e.getReason().getCode());	// access denied
			}
			p = new CimProperty("Cim_Test","propName",propType,defaultValue,quals);
			p.setValue(defaultValue);
			assertEquals(defaultValue,p.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#bind(java.lang.reflect.Method, java.lang.reflect.Method, java.lang.Object)}.
	 */
	@Ignore
	@Test
	public final void testBind() {
		// we will create a writable property
		Vector<Qualifier> quals = new Vector<Qualifier>();
		quals.add(StandardQualifierType.WRITE.getQualifier(true, Constants.defaultNameSpacePath));
		
		PropertyBindingClass boundClass = new PropertyBindingClass();
		HashMap<Class<?>, PropertyBinding> bindings = boundClass.getMethods();
		// System.out.println(JavaModelMapper.getCimClassName(boundClass.getClass()));
		
		for(int i = 1; i < type.length; i++){
			DataType propType = type[i];
			DataValue defaultValue = new DataValue(propType,obj[i]);
			boolean isArray = propType.isArray();
			// try all possible bindings
			for(Class<?> cls : propType.getAcceptedClasses()){
				CimProperty p = null;	
				if(propType.isPrimitive()){
					p = new CimProperty("Cim_Test","propName",propType,defaultValue,quals);
				} else if(propType.isReference()){
					p = new CimProperty("Cim_Test","propName","Cim_Test",isArray,defaultValue,quals);
				} else if(propType.isEnumerationValue()){
					// note that the property can either take a Enumeration class or an annotated Java enum
					p = new CimProperty("Cim_Test","propName",enumeration,isArray,defaultValue,quals);
				} else if(propType.isStructureValue()){
					// note that the property can either take a StructureValue class or an annotated Java class
					p = new CimProperty("Cim_Test","propName",struct,isArray,defaultValue,quals);
				} else if(propType.isInstanceValue()){
					// note that the property can either take a CimInstance class or an annotated Java class
					p = new CimProperty("Cim_Test","propName",cimClass,isArray,defaultValue,quals);				
				}
				assertNotNull(p);
				if(verbose) System.out.println("Testing binding for "+propType+" with "+cls);
				PropertyBinding b = bindings.get(cls);
				assertNotNull(b);
				assertFalse(p.hasValue());					// we should start with no value
				assertEquals(defaultValue,p.getValue());	// getValue returns default value
				p.bind(b.getter, b.setter, boundClass);
				// read the value
				DataValue value = p.getValue();
				assertTrue(p.hasValue());
				// System.out.println(" value "+value);
				p.setValue(defaultValue);
				value = p.getValue();
				assertEquals(value,defaultValue);
				p.unbind();
				bindings.remove(cls);
			}
		}
		if(verbose) {
			System.out.println("Left Over Bindings : "+bindings.size());
			for(Class<?> cls : bindings.keySet()){
				System.out.println(bindings.get(cls));
			}
		}
		
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#unbind()}.
	 */
	@Test
	@Ignore
	public final void testUnbind() {
		fail("Not implemented -- see TestBind");
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#readValue()}.
	 */
	@Test
	@Ignore
	public final void testReadValue() {
		fail("Not yet implemented -- see TestBind");
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimProperty#writeValue(net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	@Ignore
	public final void testWriteValue() {
		fail("Not yet implemented -- see TestBind");
	}
	
}
