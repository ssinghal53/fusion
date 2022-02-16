/**
 * Copyright 2021 Sharad Singhal. All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
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
 * Created Dec 14, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
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
 * Class to test FqlNode
 * @author Sharad Singhal
 */
public class FqlNodeTest {
	@Export(schema="S")
	static enum ExportedEnum {
		V1,V2
	}
	@Export(schema="S")
	static class ExportedStructure {
		@Export(qualifiers = "Key")
		public int getPName() {
			return 0;
		}
	}
	@Export(schema="S")
	static class ExportedClass {
		@Export(qualifiers = "Key")
		public int getKeyProp() {
			return 0;
		}
		@Export
		public int methodName(int foo) {
			return 0;
		}
	}
	static CimClass cimClass;
	static CimStructure struct;
	static StructureValue sv;
	static CimInstance iv;
	static EnumerationValue ev;
	static Map<String,DataValue> propertyValues = new HashMap<String,DataValue>();
	static {
		CimProperty keyProperty = null;
		Vector<CimProperty> keyProperties = new Vector<CimProperty>();
		Qualifier keyQual = StandardQualifierType.KEY.getQualifier(true, Constants.defaultNameSpacePath);
		Vector<Qualifier> keyQuals = new Vector<Qualifier>();
		keyQuals.add(keyQual);
		keyProperty = new CimProperty("Schema_Name","kz",DataType.BOOLEAN,new DataValue(true),keyQuals);
		keyProperties.add(keyProperty);
		Vector<CimProperty> classProperties = new Vector<CimProperty>();
		CimProperty classProperty = new CimProperty("Test_Class","p2",DataType.STRING,new DataValue("p2Value"),null);
		classProperties.add(classProperty);
		classProperties.add(keyProperty);
		struct = new CimStructure(ElementType.STRUCTURE,"Schema_Name",null,null,Constants.defaultNameSpacePath,keyProperties);
		cimClass = new CimClass(ElementType.CLASS,"Test_Class",struct,null,struct.getNameSpacePath(),classProperties);
		propertyValues.put("kz", new DataValue(true));
		ev = new EnumerationValue("name","enum",null, null);
		sv = StructureValue.createStructureValue(struct, propertyValues, null);
		iv = CimInstance.createInstance(cimClass, propertyValues, null);
	}
	static DataType [] types = {DataType.VOID, DataType.UINT8, DataType.SINT8, DataType.UINT16, DataType.SINT16,
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
	
	// data objects
	static Object [][] obj = {{null}, {new UInt8((short)8)}, {Byte.valueOf("3"),(byte) -12 },{new UInt16(22)}, 
			{Short.valueOf("25"),(short)55},{new UInt32(100)},{Integer.valueOf(70),-75},{new UInt64("351")},{Long.valueOf(500L),500L},
			{"foobar"},{Boolean.valueOf(false),true},{Float.valueOf((float) 45.0),(float)45.0},{Double.valueOf(500.0D),(double) 500.},
			{new DateTime(new Date(1397968828080L))},{Character.valueOf('c'), (char)'a'},{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
			{ev},
			{sv},
			{iv},
			{new OctetString("0x1234")},
			{new UInt8[] {new UInt8("8")}},
			{new Byte[] {Byte.valueOf("10")},new byte[] {(byte)10}},
			{new UInt16[] {new UInt16(32)}},
			{new Short[] {Short.valueOf((short)20)},new short[] {20}},
			{new UInt32[] {new UInt32(45)}},
			{new Integer[] {Integer.valueOf(50)},new int[] {55}},
			{new UInt64[] {new UInt64("55")}},
			{new Long[] {Long.valueOf(35)},new long[] {35}},
			{new String[] {"foobar"}},
			{new Boolean[] {Boolean.valueOf(false)},new boolean[] {true}},
			{new Float[] {Float.valueOf(500)},new float[] {500F}},
			{new Double[] {Double.valueOf(108.0)},new double[] {108.5}},
			{new DateTime[] {new DateTime(new Date(1397968828080L))}},
			{new Character[] {Character.valueOf('c')},new char[] {'a'}},
			{new ObjectPath[] {new ObjectPath("/class/cimv2:CIM_Test")}},
			{new EnumerationValue[] {ev}},
			{new StructureValue[] {sv}},
			{new CimInstance[] {iv}},
			{new OctetString[] {new OctetString("0x1234")}}
	};
	// string values
	static String [][] str = {{null}, {"8"}, {"3","-12" },{"22"}, 
			{"25","55"},{"100"},{"70","-75"},{"351"},{"500","500"},
			{"foobar"},{"false","true"},{"45.0","45.0"},{"500.0","500.0"},
			{"20140420044028.080***+000"},{"c","a"},{"/class/cimv2:CIM_Test"},
			{"name"},
			{"value of Schema_Name {\n"
					+ "	kz = true;\n"
					+ "}"},
			{"instance of Test_Class {\n"
					+ "	kz = true;\n"
					+ "	p2 = \"p2Value\";\n"
					+ "};"},
			{"0x1234"},
			{"[8]"},
			{"[10]","[10]"},
			{"[32]"},
			{"[20]","[20]"},
			{"[45]"},
			{"[50]","[55]"},
			{"[55]"},
			{"[35]","[35]"},
			{"[foobar]"},
			{"[false]","[true]"},
			{"[500.0]","[500.0]"},
			{"[108.0]","[108.5]"},
			{"[20140420044028.080***+000]"},
			{"[c]","[a]"},
			{"[/class/cimv2:CIM_Test]"},
			{"[name]"},
			{"[value of Schema_Name {\n"
					+ "	kz = true;\n"
					+ "}]"},
			{"[instance of Test_Class {\n"
					+ "	kz = true;\n"
					+ "	p2 = \"p2Value\";\n"
					+ "};]"},
			{"[0x1234]"}
	};
	private static InMemoryCache cache = new InMemoryCache();
	private static String [] svals = new String [] {
			"Structure class_struct { [key] SInt32 id; SInt32 iv; Real32 rv; Boolean bv; String [] s1; String [] s2; };",
			"value of class_struct {id = 4; iv = 6; rv = 54.0; bv = true; s1 = {\"s1\"}; s2 = {\"s21\",\"s22\"}; };",
			"value of class_struct {id = 5; iv = 6; rv = 54.0; bv = true; s1 = {\"s1\"}; s2 = {\"s21\",\"s22\"}; };",
			"value of class_struct {id = 6; iv = 6; rv = 54.0; bv = true; s1 = {\"s1\"}; s2 = {\"s21\",\"s22\"}; };",
	};

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("FqlNodeTest");
		assertEquals(types.length,obj.length);
		assertEquals(DataType.values().length,types.length);
		MOFParser parser = new MOFParser(cache);
		for(String s : svals) {
			parser.parse(new ByteArrayInputStream(s.getBytes()),new NameSpacePath("/path"));
		}
		List<NamedElement> elem = cache.getElements(null, null, null, false);
		for(NamedElement e : elem) {
			System.out.println(e.getObjectPath()+" "+e.toMOF());
		}
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#FqlNode(net.aifusion.metamodel.FqlOperator)}.
	 */
	@Test
	public void testFqlNodeFqlOperator() {
		for(FqlOperator o : FqlOperator.values()) {
			FqlNode n = new FqlNode(o);
			assertNotNull(n);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#FqlNode(net.aifusion.metamodel.FqlOperator, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public void testFqlNodeFqlOperatorDataValue() {
		for(FqlOperator o : FqlOperator.values()) {
			FqlNode n = new FqlNode(o,new DataValue("something"));
			assertNotNull(n);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#FqlNode(net.aifusion.metamodel.FqlOperator, java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public void testFqlNodeFqlOperatorStringDataValue() {
		for(FqlOperator o : FqlOperator.values()) {
			FqlNode n = new FqlNode(o,"nodename",new DataValue("something"));
			assertNotNull(n);
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getOperator()}.
	 */
	@Test
	public void testGetOperator() {
		for(FqlOperator o : FqlOperator.values()) {
			FqlNode n = new FqlNode(o,"nodename",new DataValue("something"));
			assertNotNull(n);
			assertEquals(o,n.getOperator());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getValue()}.
	 */
	@Test
	public void testGetValue() {
		for(FqlOperator o : FqlOperator.values()) {
			FqlNode n = new FqlNode(o,"nodename",new DataValue("something"));
			assertNotNull(n);
			assertEquals(new DataValue("something"),n.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#setValue(net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public void testSetValue() {
		FqlNode n = new FqlNode(FqlOperator.VARIABLE,"$v$",null);
		assertNotNull(n);
		assertNull(n.getValue());
		n.setValue(new DataValue("something"));
		assertEquals(new DataValue("something"),n.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getName()}.
	 */
	@Test
	public void testGetName() {
		FqlNode n = new FqlNode(FqlOperator.ADD,"nodename",new DataValue("something"));
		assertNotNull(n);
		assertEquals("nodename",n.getName());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#addChild(net.aifusion.metamodel.FqlNode)}.<br>
	 * Test method for {@link net.aifusion.metamodel.FqlNode#hasChildren()}.<br>
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getChildren()}.
	 */
	@Test
	public void testChildren() {
		FqlNode n = new FqlNode(FqlOperator.ADD,"nodename",new DataValue("something"));
		assertNotNull(n);
		assertFalse(n.hasChildren());
		FqlNode c = new FqlNode(FqlOperator.CONSTANT,null,new DataValue(3));
		n.addChild(c);
		assertTrue(n.hasChildren());
		assertEquals(c,n.getChildren().get(0));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#hasNonNullValue()}.
	 * Test method for {@link net.aifusion.metamodel.FqlNode#hasNullValue()}.
	 */
	@Test
	public void testHasNonNullValue() {
		FqlNode n = new FqlNode(FqlOperator.ADD,"nodename",null);
		assertNotNull(n);
		assertTrue(n.hasNullValue());
		assertFalse(n.hasNonNullValue());
		n.setValue(new DataValue(DataType.STRING,null));
		assertTrue(n.hasNullValue());
		assertFalse(n.hasNonNullValue());
		n.setValue(new DataValue("something"));
		assertTrue(n.hasNonNullValue());
		assertFalse(n.hasNullValue());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getLongValue()}.
	 */
	@Test
	public void testGetLongValue() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				FqlNode n = new FqlNode(FqlOperator.IDENTIFIER,"n",new DataValue(t,obj[i][j]));
				Long b = n.getLongValue();
				if(b != null) {
					assertTrue(t.isNumeric() || t.isDateTime());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getIntegerValue()}.
	 */
	@Test
	public void testGetIntegerValue() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				FqlNode n = new FqlNode(FqlOperator.IDENTIFIER,"n",new DataValue(t,obj[i][j]));
				Integer b = n.getIntegerValue();
				if(b != null) {
					assertTrue(t.isInteger() || t.isReal());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getBooleanValue()}.
	 */
	@Test
	public void testGetBooleanValue() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				FqlNode n = new FqlNode(FqlOperator.IDENTIFIER,"n",new DataValue(t,obj[i][j]));
				Boolean b = n.getBooleanValue();
				if(b != null) {
					assertTrue(t.isBoolean());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getBooleanArrayValue()}.
	 */
	@Test
	public void testGetBooleanArrayValue() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				FqlNode n = new FqlNode(FqlOperator.IDENTIFIER,"n",new DataValue(t,obj[i][j]));
				Boolean [] b = n.getBooleanArrayValue();
				if(b != null) {
					assertTrue(t.isBoolean());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getDoubleValue()}.
	 */
	@Test
	public void testGetDoubleValue() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				FqlNode n = new FqlNode(FqlOperator.IDENTIFIER,"n",new DataValue(t,obj[i][j]));
				Double d = n.getDoubleValue();
				if(d != null) {
					assertTrue(t.isNumeric() || t.isDateTime());
				}
			}
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getStringValue()}.
	 */
	@Test
	public void testGetStringValue() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
			for(int j = 0; j < obj[i].length; j++) {
//				System.out.println(t+" "+i+" "+j);
				FqlNode n = new FqlNode(FqlOperator.IDENTIFIER,"p",new DataValue(t,obj[i][j]));
				assertEquals(str[i][j],n.getStringValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getLongValue(net.aifusion.metamodel.DataType, java.lang.Object)}.
	 */
	@Test
	public void testGetLongValueDataTypeObject() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				Long d = FqlNode.getLongValue(t, obj[i][j]);
				if(d != null) {
					assertTrue(t.isNumeric() || t.isDateTime());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getIntegerValue(net.aifusion.metamodel.DataType, java.lang.Object)}.
	 */
	@Test
	public void testGetIntegerValueDataTypeObject() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				Integer d = FqlNode.getIntegerValue(t, obj[i][j]);
				if(d != null) {
					assertTrue(t.isInteger() || t.isReal());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getDoubleValue(net.aifusion.metamodel.DataType, java.lang.Object)}.
	 */
	@Test
	public void testGetDoubleValueDataTypeObject() {
		for(int i = 0; i < types.length; i++){
			DataType t = types[i];
//			System.out.println(t);
			for(int j = 0; j < obj[i].length; j++) {
				Double d = FqlNode.getDoubleValue(t, obj[i][j]);
				if(d != null) {
					assertTrue(t.isNumeric() || t.isDateTime());
				}
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#getType()}.
	 */
	@Test
	public void testGetType() {
		FqlNode n = new FqlNode(FqlOperator.IDENTIFIER,"p1",null);
		assertEquals(DataType.VOID,n.getType());
		for(int i = 1; i < types.length; i++){
			for(int j = 0; j < obj[i].length; j++) {
				DataValue v = new DataValue(obj[i][j]);
				assertNotNull(v);
				n = new FqlNode(FqlOperator.IDENTIFIER,"p1",v);
				assertEquals(types[i],n.getType());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#setVariable(java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public void testSetVariable() {
		FqlNode n = new FqlNode(FqlOperator.VARIABLE,"$v$",null);
		assertEquals(null,n.getValue());
		n.setVariable("$v$", new DataValue(3));
		assertEquals(new DataValue(3),n.getValue());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#toTree(java.lang.String)}.
	 */
	@Test
	public void testToTree() {
		FqlNode n = new FqlNode(FqlOperator.ADD);
		n.addChild(new FqlNode(FqlOperator.CONSTANT,"a",new DataValue(3)));
		n.addChild(new FqlNode(FqlOperator.CONSTANT,"b",new DataValue(4)));
		assertEquals("ADD\n"
				+ "  |-- CONSTANT (a)[SINT32 3]\n"
				+ "  |-- CONSTANT (b)[SINT32 4]",n.toTree(""));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#toString()}.
	 */
	@Test
	public void testToString() {
		FqlNode n = new FqlNode(FqlOperator.ADD,"nodename",new DataValue("something"));
		assertNotNull(n);
		assertEquals("ADD (nodename)[STRING something]",n.toString());
	}
	
	class TestCase {
		String fql;
		FqlNode root;
		DataValue dv;
		int errorCode;
		TestCase(String fql, DataValue dv,int errorCode){
			this.fql = fql;
			this.dv = dv;
			this.errorCode = errorCode;
			FqlParser p = new FqlParser(fql);
			root = p.getParseTree();
			return;
		}
		public boolean matches(StructureValue sv) {
			root.evaluate(sv, cache);
			DataValue v = root.getValue();
			if(v == null || v.getValue() == null) {
				return dv == null || dv.getValue() == null;
			}
			return v.equals(dv);
		}
	}
	
	TestCase [] cases = {
			new TestCase("'123'",new DataValue("123"),0),	// constant
			new TestCase("$abc$",null,15),					// variable
			new TestCase("iv",new DataValue(6),0),			// simple property
			new TestCase("s2",new DataValue(new String[] {"s21","s22"}),0),	// array property
			new TestCase("s3",null,0),	// missing property
			new TestCase("'value = ' || rv",new DataValue("value = 54.0"),0),	// concat simple values
			new TestCase("s1 || s2",new DataValue("[s1][s21, s22]"),0),			// concat array values
			new TestCase(" 3 + s1 * 20 / s2",null,15),							// arithmetic (invalid query)
			new TestCase(" 3 + iv * 108 / rv",new DataValue(15D),0),			// arithmetic
			
	};
	
	/**
	 * Test method for {@link net.aifusion.metamodel.FqlNode#evaluate(net.aifusion.metamodel.StructureValue, net.aifusion.metamodel.Repository)}.
	 */
	@Test
	public void testEvaluate() {
		StructureValue sv1 = (StructureValue) cache.get(new ObjectPath("/structurevalue/path:class_struct.id=4"));
		assertNotNull(sv1);
//		System.out.println(sv1.toMOF());
		for(TestCase t : cases) {
			try {
			System.out.println(t.root.toTree(""));
			assertTrue(t.matches(sv1));
			} catch (ModelException e) {
				assertEquals(t.errorCode,e.getReason().getCode());
			}
		}
	}
	


}
