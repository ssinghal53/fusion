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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Method;
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

import net.aifusion.utils.Java2Cim;

/**
 * Class to test CimMethod
 * @author Sharad Singhal
 */
public class CimMethodTest {
	private static boolean verbose = false;
	
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
			new UInt32(100), -75, new UInt64("351"),Long.valueOf(500),
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
			new UInt32(100), -75, new UInt64("351"),Long.valueOf(500),
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
	String [] mof = {"[Description(\"Qualifier\")]\nVoid Method();\n", 
			"[Description(\"Qualifier\")]\nUInt8 Method(UInt8 Name = 8);\n",
			"[Description(\"Qualifier\")]\nSInt8 Method(SInt8 Name = -12);\n",
			"[Description(\"Qualifier\")]\nUInt16 Method(UInt16 Name = 22);\n",
			"[Description(\"Qualifier\")]\nSInt16 Method(SInt16 Name = 55);\n",
			"[Description(\"Qualifier\")]\nUInt32 Method(UInt32 Name = 100);\n",
			"[Description(\"Qualifier\")]\nSInt32 Method(SInt32 Name = -75);\n",
			"[Description(\"Qualifier\")]\nUInt64 Method(UInt64 Name = 351);\n",
			"[Description(\"Qualifier\")]\nSInt64 Method(SInt64 Name = 500);\n",
			"[Description(\"Qualifier\")]\nString Method(String Name = \"foobar\");\n",
			"[Description(\"Qualifier\")]\nBoolean Method(Boolean Name = true);\n",
			"[Description(\"Qualifier\")]\nReal32 Method(Real32 Name = 45.0);\n",
			"[Description(\"Qualifier\")]\nReal64 Method(Real64 Name = 500.0);\n",
			"[Description(\"Qualifier\")]\nDatetime Method(Datetime Name = \"20140420044028.080***+000\");\n",
			"[Description(\"Qualifier\")]\nChar16 Method(Char16 Name = \'a\');\n",
			"[Description(\"Qualifier\")]\nCim_Test ref Method(Cim_Test ref Name = \"/class/cimv2:CIM_Test\");\n",
			"[Description(\"Qualifier\")]\nOctetString Method(OctetString Name = \"0x213244\");\n",
			"[Description(\"Qualifier\")]\nenum Method(enum Name = name);\n",
			"[Description(\"Qualifier\")]\nTest_Struct Method(Test_Struct Name = value of Test_Struct as $as {\n\tkz = false;\n});\n",
			"[Description(\"Qualifier\")]\nTest_Class Method(Test_Class Name = instance of Test_Class as $ac {\n\tp2 = \"newP2Value\";\n\tkz = true;\n});\n",
			"[Description(\"Qualifier\")]\nUInt8 [] Method(UInt8 [] Name = { 8 });\n",
			"[Description(\"Qualifier\")]\nSInt8 [] Method(SInt8 [] Name = { -12 });\n",
			"[Description(\"Qualifier\")]\nUInt16 [] Method(UInt16 [] Name = { 22 });\n", 
			"[Description(\"Qualifier\")]\nSInt16 [] Method(SInt16 [] Name = { 55 });\n",
			"[Description(\"Qualifier\")]\nUInt32 [] Method(UInt32 [] Name = { 100 });\n",
			"[Description(\"Qualifier\")]\nSInt32 [] Method(SInt32 [] Name = { -75 });\n",
			"[Description(\"Qualifier\")]\nUInt64 [] Method(UInt64 [] Name = { 351 });\n",
			"[Description(\"Qualifier\")]\nSInt64 [] Method(SInt64 [] Name = { 500 });\n",
			"[Description(\"Qualifier\")]\nString [] Method(String [] Name = { \"foobar\" });\n",
			"[Description(\"Qualifier\")]\nBoolean [] Method(Boolean [] Name = { true });\n",
			"[Description(\"Qualifier\")]\nReal32 [] Method(Real32 [] Name = { 45.0 });\n",
			"[Description(\"Qualifier\")]\nReal64 [] Method(Real64 [] Name = { 500.0 });\n",
			"[Description(\"Qualifier\")]\nDatetime [] Method(Datetime [] Name = { \"20140420044028.080***+000\" });\n",
			"[Description(\"Qualifier\")]\nChar16 [] Method(Char16 [] Name = { \'a\' });\n",
			"[Description(\"Qualifier\")]\nCim_Test ref [] Method(Cim_Test ref [] Name = { \"/class/cimv2:CIM_Test\" });\n",
			"[Description(\"Qualifier\")]\nOctetString [] Method(OctetString [] Name = { \"0x213244\" });\n",
			"[Description(\"Qualifier\")]\nenum [] Method(enum [] Name = { name });\n",
			"[Description(\"Qualifier\")]\nTest_Struct [] Method(Test_Struct [] Name = { value of Test_Struct as $as {\n\tkz = false;\n} });\n",
			"[Description(\"Qualifier\")]\nTest_Class [] Method(Test_Class [] Name = { instance of Test_Class as $ac {\n\tp2 = \"newP2Value\";\n\tkz = true;\n} });\n"
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
		System.out.print("CimMethod ");
		assertEquals(DataType.values().length,type.length);
		assertEquals(obj.length,type.length);
		assertEquals(type.length,obj2.length);
		assertNotNull(struct);
		assertNotNull(structValue);
		assertNotNull(cimClass);
		assertNotNull(instance);
		assertNotNull(enumeration);
		assertNotNull(enumValue);
		/*
		repository = new InMemoryRepository();
		mClass = (CimClass) Java2Cim.getModelForClass(CimMethodTestClass.class, repository.getDefaultNameSpace(), repository);
		assertNotNull(mClass);
		System.out.println(mClass.toMOF());	
		*/
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#CimMethod(String, java.lang.String, net.aifusion.metamodel.DataType, java.util.List, java.util.List)}.
	 * Test method for {@link net.aifusion.metamodel.CimMethod#CimMethod(String, java.lang.String, java.lang.String, boolean, java.util.List, java.util.List)}.
	 * Test method for {@link net.aifusion.metamodel.CimMethod#CimMethod(String, java.lang.String, net.aifusion.metamodel.CimEnumeration, boolean, java.util.List, java.util.List)}.
	 * Test method for {@link net.aifusion.metamodel.CimMethod#CimMethod(String, java.lang.String, net.aifusion.metamodel.CimStructure, boolean, java.util.List, java.util.List)}.
	 */
	@Test
	public final void testCimMethod() {
		Vector<CimParameter> params = new Vector<CimParameter>();
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			params.clear();
			CimMethod m = null;
			// System.out.println(t+" "+v.toMOF());
			if(t.isVoid()){
				m = new CimMethod("Cim_Class","Method",t,quals, null);
				assertNotNull(m);
			} else if(t.isPrimitive()){
				 params.add(new CimParameter(null,"Name",t,v, null));
				 m = new CimMethod("Cim_Class","Method",t,quals, params); 
			} else if(t.isReference()){
				params.add(new CimParameter(null,"Name","Cim_Test",t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method","Cim_Test",t.isArray(),quals, params);
			} else if(t.isEnumerationValue()){
				params.add(new CimParameter(null, "Name", enumeration, t.isArray(), v, null));
				m = new CimMethod("Cim_Class","Method",enumeration,t.isArray(),quals, params);
			} else if(t.isStructureValue()){
				params.add(new CimParameter(null,"Name",struct,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",struct,t.isArray(),quals, params);
			} else if(t.isInstanceValue()){
				params.add(new CimParameter(null,"Name",cimClass,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",cimClass,t.isArray(),quals, params);
			} else {
				fail("Unknown data type "+t);
			}
			assertNotNull(m);
		}
	}


	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#getRefClassName()}.
	 * Test method for {@link net.aifusion.metamodel.CimMethod#getEnum()}.
	 * Test method for {@link net.aifusion.metamodel.CimMethod#getReturnedType()}.
	 * Test method for {@link net.aifusion.metamodel.CimMethod#getStruct()}.
	 */
	@Test
	public final void testGetMethods() {
		Vector<CimParameter> params = new Vector<CimParameter>();
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			params.clear();
			CimMethod m = null;
			// System.out.println(t+" "+v.toMOF());
			if(t.isVoid()){
				m = new CimMethod("Cim_Class","Method",t,quals, null);
				assertNotNull(m);
			} else if(t.isPrimitive()){
				 params.add(new CimParameter(null,"Name",t,v, null));
				 m = new CimMethod("Cim_Class","Method",t,quals, params); 
			} else if(t.isReference()){
				params.add(new CimParameter(null,"Name","Cim_Test",t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method","Cim_Test",t.isArray(),quals, params);
			} else if(t.isEnumerationValue()){
				params.add(new CimParameter(null, "Name", enumeration, t.isArray(), v, null));
				m = new CimMethod("Cim_Class","Method",enumeration,t.isArray(),quals, params);
			} else if(t.isStructureValue()){
				params.add(new CimParameter(null,"Name",struct,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",struct,t.isArray(),quals, params);
			} else if(t.isInstanceValue()){
				params.add(new CimParameter(null,"Name",cimClass,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",cimClass,t.isArray(),quals, params);
			} else {
				fail("Unknown data type "+t);
			}
			assertNotNull(m);
			assertEquals(t,m.getReturnedType());
			if(t.isReference()){
				assertEquals("Cim_Test",m.getRefClassName());
			} else {
				assertEquals(null,m.getRefClassName());
			}
			if(t.isEnumerationValue()){
				assertEquals(enumeration,m.getEnum());
			} else {
				assertEquals(null,m.getEnum());
			}
			if(t.isInstanceValue()){
				assertEquals(cimClass,m.getStruct());
			} else {
				if(t.isStructureValue()){
					assertEquals(struct,m.getStruct());
				} else {
					assertEquals(null,m.getStruct());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#getParameters()}.
	 */
	@Test
	public final void testGetParameters() {
		Vector<CimParameter> params = new Vector<CimParameter>();
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			params.clear();
			CimMethod m = null;
			// System.out.println(t+" "+v.toMOF());
			if(t.isVoid()){
				m = new CimMethod("Cim_Class","Method",t,quals, null);
				assertNotNull(m);
			} else if(t.isPrimitive()){
				 params.add(new CimParameter(null,"Name",t,v, null));
				 m = new CimMethod("Cim_Class","Method",t,quals, params); 
			} else if(t.isReference()){
				params.add(new CimParameter(null,"Name","Cim_Test",t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method","Cim_Test",t.isArray(),quals, params);
			} else if(t.isEnumerationValue()){
				params.add(new CimParameter(null, "Name", enumeration, t.isArray(), v, null));
				m = new CimMethod("Cim_Class","Method",enumeration,t.isArray(),quals, params);
			} else if(t.isStructureValue()){
				params.add(new CimParameter(null,"Name",struct,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",struct,t.isArray(),quals, params);
			} else if(t.isInstanceValue()){
				params.add(new CimParameter(null,"Name",cimClass,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",cimClass,t.isArray(),quals, params);
			} else {
				fail("Unknown data type "+t);
			}
			assertNotNull(m);
			List<CimParameter> l = m.getParameters();
			assertEquals(params.size(),l.size());
			for(int j = 0; j < params.size(); j++){
				assertEquals(params.get(j),l.get(j));
			}
		}
	}
	
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#bind(java.lang.reflect.Method, java.lang.Object)}.
	 * Test method for {@link net.aifusion.metamodel.CimMethod#invoke(java.util.List)}.
	 */
	@Test
	public final void testBindAndInvoke() {
		Object [] param = new Object[]{
				null,new UInt8((short)4),Byte.valueOf((byte)6),(byte)7,new UInt16(30000),	// 0-4
				Short.valueOf((short)22),(short)15,new UInt32(54000L),Integer.valueOf(22000),45,new UInt64("530000"),Long.valueOf(540L),(long)50,	// 5-12
				"String arg",Boolean.valueOf(true),false,Float.valueOf((float) 54000D),(float)45.4,Double.valueOf(6400D),(double)4000,new DateTime(),	// 13-20
				Character.valueOf('x'),'x',	new OctetString("0x3f"), // 21-22
				JavaModelMapper.getObjectPathFromClass(CimMethodTestClass.class),	// 23
				new MethodBindingClass(),	// 24
				new UInt8[]{new UInt8((short)4)},new Byte[]{Byte.valueOf((byte)6)},new byte[]{(byte)7},new UInt16[]{new UInt16(30000)},	// 25-28
				new Short[]{Short.valueOf((short)22)},new short[]{(short)15},new UInt32[]{new UInt32(54000L)},	// 29-31
				new Integer[]{Integer.valueOf(22000)},new int[]{45},new UInt64[]{new UInt64("530000")},new Long[]{Long.valueOf(540L)},new long[]{(long)50},	// 32-36
				new String[]{"String arg"},new Boolean[]{Boolean.valueOf(true)},new boolean[]{false},new Float[]{Float.valueOf((float) 54000D)},new float[]{(float)45.4},	// 37-41
				new Double[]{Double.valueOf(6400D)},new double[]{(double)4000},new DateTime[]{new DateTime()},	// 42-44
				new Character[]{Character.valueOf('x')},new char[]{'x'}, new OctetString[]{new OctetString("0x3f")},// 45-46
				new ObjectPath[]{JavaModelMapper.getObjectPathFromClass(CimMethodTestClass.class)}, // 47
				new MethodBindingClass[]{new MethodBindingClass()},	// 48
				EnumBindingClass.NAME1,		// 50
				new EnumBindingClass[]{EnumBindingClass.NAME1}, // 51
				new MethodBindingSuperClass(),					// 52
				new MethodBindingSuperClass[]{new MethodBindingSuperClass()}	// 53
		};
		
		CimMethodTestClass javaClass = new CimMethodTestClass();
		Method [] javaMethods = CimMethodTestClass.class.getDeclaredMethods();
		Repository r = new InMemoryRepository();
		CimClass cimClass = (CimClass) Java2Cim.getModelForClass(CimMethodTestClass.class, r);
		assertNotNull(cimClass);
		CimEnumeration cimEnum = (CimEnumeration) r.get(new ObjectPath(ElementType.ENUMERATION,"AIFusion_EnumBindingClass",Constants.defaultNameSpacePath, null, null));
		assertNotNull(cimEnum);
		CimStructure cimStruct = (CimStructure) r.get(new ObjectPath(ElementType.STRUCTURE,"cim_testmethodssup",Constants.defaultNameSpacePath,null, null));
		assertNotNull(cimStruct);
		CimClass cimClass1 = (CimClass) r.get(new ObjectPath(ElementType.CLASS,"Cim_TestMethods",Constants.defaultNameSpacePath,null, null));
		assertNotNull(cimClass1);
		if(verbose) {
			for(NamedElement e : r.getElements(null,null,null, false)){
				System.out.println(e.toMOF());
			}
		}
		Map<String,CimMethod> cimMethods = cimClass.getAllMethods();
		// for each CIM method declared
		for(CimMethod cimMethod : cimMethods.values()){
			if(verbose) System.out.println("Trying CIM Method "+cimMethod.toMOF());
			// obtain the method parameters
			List<CimParameter> cimParameters = cimMethod.getParameters();
			// try all java methods in the java class
			for(Method javaMethod : javaMethods){
				// ignore any non-exported methods
				if(!javaMethod.isAnnotationPresent(Export.class)) continue;
				// try to bind the Cim method to the java method
				try {
					cimMethod.bind(javaMethod, javaClass);
					if(verbose) System.out.println("\t\t\tBind Succeeded for "+javaMethod.toString());
					if(cimMethod.getReturnedType() == DataType.VOID){
						// Void method in class does not take any parameters
						assertEquals(0,cimParameters.size());
						try {
							DataValue returned = cimMethod.invoke(cimParameters);
							assertNull(returned);
						} catch(ModelException ei){
							// we will fail here if invoke() fails on void method
							fail(ei.toString());
						}
					} else {
						// all others take one parameter
						assertEquals(1,cimParameters.size());
						// clone the parameter
						CimParameter cimParameter = cimParameters.get(0).createInstanceParameter();
						assertNotNull(cimParameter);
						if(verbose) System.out.println("\tParameter "+cimParameter.toMOF());
						// try every parameter value
						for(Object javaParam : param){
							DataType t = DataType.getTypeForObject(javaParam);
							DataValue v = new DataValue(t,javaParam);
							assertNotNull(v);
							try {
								cimParameter.setValue(v);
								if(verbose) System.out.println("\t Matched value "+v.toMOF());
								cimParameters.clear();
								cimParameters.add(cimParameter);
								try {
									DataValue rv = cimMethod.invoke(cimParameters);
									if(verbose) System.out.println("\t Returned value "+rv.toMOF());
									assertEquals(v,rv);
								} catch (ModelException ez){
									// we fail here if cimMethod#invoke() fails
									if(verbose) System.out.println("\t Invocation failed");
									assertEquals(1,ez.getReason().getCode());
									continue;
								}
							} catch (ModelException ep){
								// cimParameter#setValue() should throw exceptions for mismatched parameter values 
								assertEquals(4, ep.getReason().getCode());
								continue;
							}
						}
					}					
				} catch (ModelException ex){
					// we will fail due to type mismatch during CimMethod#bind()
					if(ex.getReason().getCode() != 13){
						if(verbose) System.out.println(ex.toString());
						fail();
					}
					continue;
				}
				
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#createInstanceMethod()}.
	 */
	@Test
	public final void testCreateInstanceMethod() {
		String mof = "class Test_Class { [static] String foo(sint32 p);\n String bar(String q); };";
		MOFParser p = new MOFParser();
		p.parse(new ByteArrayInputStream(mof.getBytes()), Constants.defaultNameSpacePath);
		Repository r = p.getRepository();
		CimClass e = (CimClass) r.get(new ObjectPath(ElementType.CLASS,"Test_Class",Constants.defaultNameSpacePath,null, null));
		assertNotNull(e);
		// System.out.println(e.toMOF());
		Map<String,CimMethod> m = e.getAllMethods();
		assertEquals(2,m.size());
		
		// non static methods are cloned
		CimMethod meth = m.get("bar");
		assertFalse(meth.isStatic());
		CimMethod instanceMethod = meth.createInstanceMethod();
		assertEquals(meth,instanceMethod);
		assertFalse(meth == instanceMethod);
		
		// static methods are shared
		meth = m.get("foo");
		assertTrue(meth.isStatic());
		instanceMethod = meth.createInstanceMethod();
		assertEquals(meth,instanceMethod);
		assertTrue(meth == instanceMethod);
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#isStatic()}.
	 */
	@Test
	public final void testIsStatic() {
		String mof = "class Test_Class { [static] String foo(sint32 p);\n String bar(String q); };";
		MOFParser p = new MOFParser();
		p.parse(new ByteArrayInputStream(mof.getBytes()), Constants.defaultNameSpacePath);
		Repository r = p.getRepository();
		CimClass e = (CimClass) r.get(new ObjectPath(ElementType.CLASS,"Test_Class",Constants.defaultNameSpacePath,null, null));
		assertNotNull(e);
		// System.out.println(e.toMOF());
		Map<String,CimMethod> m = e.getAllMethods();
		assertEquals(2,m.size());
		assertFalse(m.get("bar").isStatic());
		assertTrue(m.get("foo").isStatic());

	}

	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#toMOF(java.lang.String)}.
	 */
	@Test
	public final void testToMOFString() {
		Vector<CimParameter> params = new Vector<CimParameter>();
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			params.clear();
			CimMethod m = null;
			// System.out.println(t+" "+v.toMOF());
			if(t.isVoid()){
				m = new CimMethod(null,"Method",t,quals, null);
				assertNotNull(m);
			} else if(t.isPrimitive()){
				 params.add(new CimParameter("Cim_Class#Method","Name",t,v, null));
				 m = new CimMethod(null,"Method",t,quals, params); 
			} else if(t.isReference()){
				params.add(new CimParameter("Cim_Class#Method","Name","Cim_Test",t.isArray(),v, null));
				m = new CimMethod(null,"Method","Cim_Test",t.isArray(),quals, params);
			} else if(t.isEnumerationValue()){
				params.add(new CimParameter("Cim_Class#Method", "Name", enumeration, t.isArray(), v, null));
				m = new CimMethod(null,"Method",enumeration,t.isArray(),quals, params);
			} else if(t.isStructureValue()){
				params.add(new CimParameter("Cim_Class#Method","Name",struct,t.isArray(),v, null));
				m = new CimMethod(null,"Method",struct,t.isArray(),quals, params);
			} else if(t.isInstanceValue()){
				params.add(new CimParameter("Cim_Class#Method","Name",cimClass,t.isArray(),v, null));
				m = new CimMethod("Cim_Class#Method","Method",cimClass,t.isArray(),quals, params);
			} else {
				fail("Unknown data type "+t);
			}
			assertNotNull(m);
			assertEquals(mof[i],m.toMOF());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.CimMethod#getFullName()}.
	 */
	@Test
	public final void testGetFullName() {
		Vector<CimParameter> params = new Vector<CimParameter>();
		for(int i = 0; i < type.length; i++){
			DataType t = type[i];
			DataValue v = new DataValue(t,obj[i]);
			params.clear();
			CimMethod m = null;
			// System.out.println(t+" "+v.toMOF());
			if(t.isVoid()){
				m = new CimMethod("Cim_Class","Method",t,quals, null);
				assertNotNull(m);
			} else if(t.isPrimitive()){
				 params.add(new CimParameter("Cim_Class#Method","Name",t,v, null));
				 m = new CimMethod("Cim_Class","Method",t,quals, params); 
			} else if(t.isReference()){
				params.add(new CimParameter("Cim_Class#Method","Name","Cim_Test",t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method","Cim_Test",t.isArray(),quals, params);
			} else if(t.isEnumerationValue()){
				params.add(new CimParameter("Cim_Class#Method", "Name", enumeration, t.isArray(), v, null));
				m = new CimMethod("Cim_Class","Method",enumeration,t.isArray(),quals, params);
			} else if(t.isStructureValue()){
				params.add(new CimParameter("Cim_Class#Method","Name",struct,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",struct,t.isArray(),quals, params);
			} else if(t.isInstanceValue()){
				params.add(new CimParameter("Cim_Class#Method","Name",cimClass,t.isArray(),v, null));
				m = new CimMethod("Cim_Class","Method",cimClass,t.isArray(),quals, params);
			} else {
				fail("Unknown data type "+t);
			}
			assertNotNull(m);
			assertEquals("Cim_Class#Method",m.getFullName());
		}
	}
	
	

}
