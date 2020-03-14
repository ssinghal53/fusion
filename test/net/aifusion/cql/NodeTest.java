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
 * Created Oct 2, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.util.Date;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.DateTime;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.OctetString;
import net.aifusion.metamodel.UInt16;
import net.aifusion.metamodel.UInt32;
import net.aifusion.metamodel.UInt64;
import net.aifusion.metamodel.UInt8;

/**
 * Class to test Query Node
 * @author Sharad Singhal
 */
public class NodeTest {

	// data objects
		private static Object [] obj = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
			new UInt32(100), -75, new UInt64("351"),new Long(500),
			"foobar",true,new Float(45.0),(double) 500.,
			new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
			new OctetString("0xfe10ff"),
			new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
			new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
			new String[]{"foobar"},new boolean[]{true},new Float[]{new Float(45.0)},new double[]{(double) 500.},
			new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
			new OctetString[]{new OctetString("0xfe10ff")}
		};
		// data objects
		private static Object [] obj2 = {null, new UInt8((short)8), (byte) -12, new UInt16(22), (short)55,
			new UInt32(100), -75, new UInt64("351"),new Long(500),
			"foobar",true,new Float(45.0),(double) 500.,
			new DateTime(new Date(1397968828080L)),(char)'a',new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null),
			new OctetString("0xfe10ff"),
			new UInt8[]{new UInt8((short)8)},new Byte[]{(byte) -12},new UInt16[]{new UInt16(22)},new short[]{(short)55},
			new UInt32[]{new UInt32(100)}, new Integer[]{-75},new UInt64[]{new UInt64("351")},new long[]{500L},
			new String[]{"foobar"},new boolean[]{true},new Float[]{new Float(45.0)},new double[]{(double) 500.},
			new DateTime[]{new DateTime(new Date(1397968828080L))},new char[]{(char)'a'},new ObjectPath[]{new ObjectPath(ElementType.CLASS,"CIM_Test",new NameSpacePath("/cimv2"), null, null)},
			new OctetString[]{new OctetString("0xfe10ff")}
		
		};
		// known data types
		private static DataType [] type = {DataType.VOID, DataType.UINT8, DataType.SINT8, DataType.UINT16, DataType.SINT16,
			DataType.UINT32, DataType.SINT32, DataType.UINT64, DataType.SINT64,
			DataType.STRING, DataType.BOOLEAN, DataType.REAL32, DataType.REAL64,
			DataType.DATETIME, DataType.CHAR16, DataType.OBJECTPATH,
			DataType.OCTETSTRING,
			DataType.UINT8_ARRAY, DataType.SINT8_ARRAY, DataType.UINT16_ARRAY, DataType.SINT16_ARRAY,
			DataType.UINT32_ARRAY, DataType.SINT32_ARRAY, DataType.UINT64_ARRAY, DataType.SINT64_ARRAY,
			DataType.STRING_ARRAY, DataType.BOOLEAN_ARRAY, DataType.REAL32_ARRAY, DataType.REAL64_ARRAY,
			DataType.DATETIME_ARRAY, DataType.CHAR16_ARRAY, DataType.OBJECTPATH_ARRAY,
			DataType.OCTETSTRING_ARRAY
		};
		
		private static InMemoryCache cache = new InMemoryCache();
		private static String mof = "class test_class1 {  [key] String K1; Sint64 D1; DateTime DT1; String S1; };\n"
				+ "class test_class2 { [key] String K2; Sint64 D2; DateTime DT2; String S2; };\n"
				+ "class test_class3 { [key] String K3; Sint64 D3; DateTime DT3; String S3; };\n"
				+ "class test_class4 : test_class1 { Boolean D4; Real64 R4;};\n"
				+ "instance of test_class1 { K1 = \"key11\"; D1 = 54; DT1 = \"20140420044028.080***+000\"; S1 = \"instance11\";  };\n"
				+ "instance of test_class1 { K1 = \"key12\"; D1 = 55; DT1 = \"20150420044028.080***+000\"; S1 = \"instance12\";   };\n"
				+ "instance of test_class2 { K2 = \"key21\"; D2 = 64; S2 = \"instance21\";  };\n"
				+ "instance of test_class2 { K2 = \"key22\"; D2 = 65; S2 = \"instance22\";  };\n"
				+ "instance of test_class3 { K3 = \"key31\"; D3 = 74; S3 = \"instance31\";  };\n"
				+ "instance of test_class3 { K3 = \"key32\"; D3 = 75; S3 = \"instance32\"; };\n"
				+ "instance of test_class4 { K1 = \"key41\"; D1 = 54; DT1 = \"20140420044028.080***+000\"; S1 = \"instance41\"; D4 = false; R4 = 3.14;  };\n"
				;
		
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Node ");
		assertEquals(obj.length,obj2.length);
		assertEquals(obj.length,type.length);
		MOFParser parser = new MOFParser(cache);
		ByteArrayInputStream input = new ByteArrayInputStream(mof.getBytes());
		parser.parse(input, null);
		input.close();
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		// for(NamedElement e : elements) System.out.println(e.toMOF());
		assertEquals(11,elements.size());
		
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
	 * Test method for {@link net.aifusion.cql.Node#Node(net.aifusion.cql.Operator, java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testNode() {
		for(Operator o : Operator.values()){
			Node n = new Node(o,"foo",new DataValue(54));
			assertNotNull(n);
			assertEquals(o,n.getOperator());
			assertEquals("foo",n.getName());
			assertEquals(new DataValue(54),n.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#addChild(net.aifusion.cql.Node)}.
	 * Test method for {@link net.aifusion.cql.Node#hasChildren()}.
	 * Test method for {@link net.aifusion.cql.Node#getChildren()}.
	 */
	@Test
	public final void testAddChild() {
		Node n = new Node(Operator.INDEX,null,null);
		assertFalse(n.hasChildren());
		n.addChild(n);
		assertTrue(n.hasChildren());
		assertEquals(1,n.getChildren().size());
	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#getValue()}.
	 */
	@Test
	public final void testGetValue() {
		for(int i = 0; i < obj.length; i++){
			Node n = new Node(Operator.CONSTANT,null,new DataValue(type[i],obj[i]));
			assertEquals(new DataValue(type[i],obj[i]),n.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#getLongValue()}.
	 */
	@Test
	public final void testGetLongValue() {
		for(int i = 0; i < obj.length; i++){
			// System.out.println(type[i]+" "+obj[i]);
			Node n = new Node(Operator.CONSTANT,null,new DataValue(type[i],obj[i]));
			Long longValue = n.getLongValue();
			if(longValue != null) {
				assertTrue(type[i].isNumeric() || type[i] == DataType.DATETIME);
				
			}
		}

	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#getBooleanValue()}.
	 */
	@Test
	public final void testGetBooleanValue() {
		for(int i = 0; i < obj.length; i++){
			// System.out.println(type[i]+" "+obj[i]);
			Node n = new Node(Operator.CONSTANT,null,new DataValue(type[i],obj[i]));
			Boolean value = n.getBooleanValue();
			if(value != null) {
				assertTrue(type[i] == DataType.BOOLEAN);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#getDoubleValue()}.
	 */
	@Test
	public final void testGetDoubleValue() {
		for(int i = 0; i < obj.length; i++){
			// System.out.println(type[i]+" "+obj[i]);
			Node n = new Node(Operator.CONSTANT,null,new DataValue(type[i],obj[i]));
			Double value = n.getDoubleValue();
			if(value != null) {
				assertTrue(type[i].isNumeric() || type[i] == DataType.DATETIME);
				
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#getStringValue()}.
	 */
	@Test
	public final void testGetStringValue() {
		for(int i = 0; i < obj.length; i++){
			// System.out.println(type[i]+" "+obj[i]);
			Node n = new Node(Operator.CONSTANT,null,new DataValue(type[i],obj[i]));
			String value = n.getStringValue();
			// System.out.println(type[i]+" "+obj[i]+" "+value);
			if(value == null) {
				assertEquals(DataType.VOID,type[i]);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#getType()}.
	 */
	@Test
	public final void testGetType() {
		for(int i = 0; i < obj.length; i++){
			Node n = new Node(Operator.CONSTANT,null,new DataValue(type[i],obj[i]));
			DataType t = n.getType();
			assertEquals(type[i],t);
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.cql.Node#setVariable(java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testSetVariable() {
		for(Operator o : Operator.values()){
			if(Operator.FUNCTION.equals(o)) continue;
			Node n = o.getNode("$foo$");
			assertEquals(null,n.getValue());
			n.setVariable("$foo$", new DataValue(54));
			DataValue v = n.getValue();
			if(v != null){
				assertEquals(Operator.VARIABLE,o);
				assertEquals(new DataValue(54),v);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Node#setAlias(java.lang.String)}.
	 * Test method for {@link net.aifusion.cql.Node#getAlias()}.
	 */
	@Test
	public final void testSetAlias() {
		Node n = new Node(Operator.INDEX,null,null);
		assertNull(n.getAlias());
		n.setAlias("alias");
		assertEquals("alias",n.getAlias());
		// reset should fail
		try {
			n.setAlias("alias1");
			fail("Should not succeed");
		} catch(ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
	}
	
	// Tests for Arithmetic (Add, Subtract, Multiply, Divide, Concat, Sign)
	
	@Test
	public final void testAdd(){
		Node addNode = Operator.ADD.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		addNode.addChild(leftChild);
		addNode.addChild(rightChild);
		// System.out.println(addNode.toString());
		for(int i = 0; i < obj.length; i++){
			DataValue v1 = new DataValue(type[i],obj[i]);
			addNode.setVariable("$left$",v1);
			for(int j = 0; j < obj2.length; j++){
				DataValue v2 = new DataValue(type[j],obj2[j]);
				addNode.setVariable("$right$", v2);
				try {
					addNode.evaluate();
					if(!(leftChild.hasNonNullValue() && rightChild.hasNonNullValue())){
						assertFalse(addNode.hasNonNullValue());
					} else {
						// System.out.println("Add - "+leftChild.toString()+" + "+rightChild.toString()+" = "+addNode.toString());
						Double expected = leftChild.getDoubleValue()+rightChild.getDoubleValue();
						assertEquals(expected,addNode.getDoubleValue());
					}
				} catch (ModelException e){
					if(type[i].isArray() || type[j].isArray()){
						assertEquals(15,e.getReason().getCode());	// array values
					} else if(type[i].isDateTime() && type[j].isDateTime()){
						assertEquals(7,e.getReason().getCode());	// datetime computation error
					} else if(!type[i].isNumeric() || !type[j].isNumeric()){
						// System.out.println(e);
						assertEquals(15,e.getReason().getCode());	// non-numeric value
					} else {
						System.out.println(e);
						fail("Condition not handled");	// test error
					}
				}
				
			}
		}
	}
	
	@Test
	public final void testSubtract(){
		Node subtractNode = Operator.SUBTRACT.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		subtractNode.addChild(leftChild);
		subtractNode.addChild(rightChild);
		// System.out.println(addNode.toString());
		for(int i = 0; i < obj.length; i++){
			DataValue v1 = new DataValue(type[i],obj[i]);
			subtractNode.setVariable("$left$",v1);
			for(int j = 0; j < obj2.length; j++){
				DataValue v2 = new DataValue(type[j],obj2[j]);
				subtractNode.setVariable("$right$", v2);
				try {
					subtractNode.evaluate();
					if(!(leftChild.hasNonNullValue() && rightChild.hasNonNullValue())){
						assertFalse(subtractNode.hasNonNullValue());
					} else {
						// System.out.println("Add - "+leftChild.toString()+" + "+rightChild.toString()+" = "+subtractNode.toString());
						Double expected = leftChild.getDoubleValue()-rightChild.getDoubleValue();
						assertEquals(expected,subtractNode.getDoubleValue());
					}
				} catch (ModelException e){
					if(type[i].isArray() || type[j].isArray()){
						assertEquals(15,e.getReason().getCode());	// array values
					} else if(type[i].isDateTime() && type[j].isDateTime()){
						// System.out.println(e);
						assertEquals(4,e.getReason().getCode());	// datetime computation error (interval is [-999,999]
					} else if(!type[i].isNumeric() || !type[j].isNumeric()){
						assertEquals(15,e.getReason().getCode());	// non-numeric value
					} else {
						System.out.println(e);
						fail("Condition not handled");	// test error
					}
				}
			}
		}
	}
	
	@Test
	public final void testMultiply(){
		Node multiplyNode = Operator.MULTIPLY.getNode();
		assertTrue(multiplyNode.getClass() == Arithmetic.class);
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		multiplyNode.addChild(leftChild);
		multiplyNode.addChild(rightChild);
		// System.out.println(addNode.toString());
		for(int i = 0; i < obj.length; i++){
			DataValue v1 = new DataValue(type[i],obj[i]);
			multiplyNode.setVariable("$left$",v1);
			for(int j = 0; j < obj2.length; j++){
				DataValue v2 = new DataValue(type[j],obj2[j]);
				multiplyNode.setVariable("$right$", v2);
				try {
					multiplyNode.evaluate();
					if(leftChild.hasNullValue() || rightChild.hasNullValue()){
						assertTrue(multiplyNode.hasNullValue());
					} else {
						// System.out.println("Multiply - "+leftChild.toString()+" + "+rightChild.toString()+" = "+multiplyNode.toString());
						Double expected = leftChild.getDoubleValue()*rightChild.getDoubleValue();
						assertEquals(expected,multiplyNode.getDoubleValue());
					}
				} catch (ModelException e){
					if(type[i].isArray() || type[j].isArray()){
						assertEquals(15,e.getReason().getCode());	// array values
					} else if(type[i].isDateTime() && type[j].isDateTime()){
						// System.out.println(e);
						assertEquals(15,e.getReason().getCode());	// datetime computation error (interval is [-999,999]
					} else if(!type[i].isNumeric() || !type[j].isNumeric()){
						assertEquals(15,e.getReason().getCode());	// non-numeric value
					} else {
						System.out.println(e);
						fail("Condition not handled");	// test error
					}
				}
			}
		}
	}
	
	@Test
	public final void testDivide(){
		Node divideNode = Operator.DIVIDE.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		divideNode.addChild(leftChild);
		divideNode.addChild(rightChild);
		// System.out.println(addNode.toString());
		for(int i = 0; i < obj.length; i++){
			DataValue v1 = new DataValue(type[i],obj[i]);
			divideNode.setVariable("$left$",v1);
			for(int j = 0; j < obj2.length; j++){
				DataValue v2 = new DataValue(type[j],obj2[j]);
				divideNode.setVariable("$right$", v2);
				try {
					divideNode.evaluate();
					if(!(leftChild.hasNonNullValue() && rightChild.hasNonNullValue())){
						assertFalse(divideNode.hasNonNullValue());
					} else {
						// System.out.println("Divide - "+leftChild.toString()+" + "+rightChild.toString()+" = "+divideNode.toString());
						if(type[i].isReal() || type[j].isReal()){
							Double expected = leftChild.getDoubleValue()/rightChild.getDoubleValue();
							assertEquals(expected,divideNode.getDoubleValue());
						} else {
							Long expected = leftChild.getLongValue()/rightChild.getLongValue();
							assertEquals(expected,divideNode.getLongValue());
						}
					}
				} catch (ModelException e){
					if(type[i].isArray() || type[j].isArray()){
						assertEquals(15,e.getReason().getCode());	// array values
					} else if(type[i].isDateTime() && type[j].isDateTime()){
						// System.out.println(e);
						assertEquals(15,e.getReason().getCode());	// datetime computation error (interval is [-999,999]
					} else if(!type[i].isNumeric() || !type[j].isNumeric()){
						assertEquals(15,e.getReason().getCode());	// non-numeric value
					} else {
						System.out.println(e);
						fail("Condition not handled");	// test error
					}
				}
			}
		}
	}
	
	@Test
	public final void testSign(){
		Node signNode = Operator.SIGN.getNode("-");
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		signNode.addChild(leftChild);
		for(int i = 0; i < obj.length; i++){
			if(type[i].isArray() || !(type[i].isNumeric() || type[i].isVoid())) continue;
			DataValue v1 = new DataValue(type[i],obj[i]);
			signNode.setVariable("$left$",v1);
			// System.out.println(leftChild.toString());
			try {
				signNode.evaluate();
				if(!(leftChild.hasNonNullValue())){
					assertFalse(signNode.hasNonNullValue());
				} else {
					// System.out.println("Sign - "+leftChild.toString()+" = "+signNode.toString());
					if(type[i].isReal()){
						Double expected = -leftChild.getDoubleValue();
						assertEquals(expected,signNode.getDoubleValue());
					} else {
						Long expected = -leftChild.getLongValue();
						assertEquals(expected,signNode.getLongValue());
					}
				}
			} catch (ModelException e){
				// System.out.println(e);
				assertEquals(15,e.getReason().getCode());
			}
		}

	}
	
	@Test
	public final void testConcat(){
		Node concatNode = Operator.CONCAT.getNode();
		Node c1 = Operator.VARIABLE.getNode("$1$");
		Node c2 = Operator.VARIABLE.getNode("$2$");
		Node c3 = Operator.VARIABLE.getNode("$3$");
		concatNode.addChild(c1);
		concatNode.addChild(c2);
		concatNode.addChild(c3);
		concatNode.setVariable("$1$", new DataValue("abc"));
		concatNode.setVariable("$2$", new DataValue(54L));
		concatNode.setVariable("$3$", new DataValue("55"));
		concatNode.evaluate();
		assertEquals(DataType.STRING,concatNode.getType());
		assertEquals(new DataValue("abc5455"),concatNode.getValue());
	}
	
	
	// Boolean comparisons (and/or/not/every/any)

	@Test
	public final void testAnd(){
		Boolean[] values = {new Boolean(true),new Boolean(true),new Boolean(true),
				new Boolean(true),new Boolean(false),new Boolean(false),
				new Boolean(true),null,null,
				new Boolean(false),new Boolean(true),new Boolean(false),
				new Boolean(false),new Boolean(false),new Boolean(false),
				new Boolean(false),null,new Boolean(false),
				null,new Boolean(true),null,
				null,new Boolean(false),new Boolean(false),
				null,null,null
		};

		Node andNode = Operator.AND.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		andNode.addChild(leftChild);
		andNode.addChild(rightChild);
		for(int i = 0; i < values.length; i+=3){
			andNode.setVariable("$left$", new DataValue(DataType.BOOLEAN,values[i]));
			andNode.setVariable("$right$", new DataValue(DataType.BOOLEAN,values[i+1]));
			andNode.evaluate();
			assertEquals(values[i+2],andNode.getBooleanValue());
		}
	}
	
	@Test
	public final void testOr(){
		Boolean[] values = {new Boolean(true),new Boolean(true),new Boolean(true),
				new Boolean(true),new Boolean(false),new Boolean(true),
				new Boolean(true),null,new Boolean(true),
				new Boolean(false),new Boolean(true),new Boolean(true),
				new Boolean(false),new Boolean(false),new Boolean(false),
				new Boolean(false),null,null,
				null,new Boolean(true),new Boolean(true),
				null,new Boolean(false),null,
				null,null,null
		};

		Node orNode = Operator.OR.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		orNode.addChild(leftChild);
		orNode.addChild(rightChild);
		for(int i = 0; i < values.length; i+=3){
			orNode.setVariable("$left$", new DataValue(DataType.BOOLEAN,values[i]));
			orNode.setVariable("$right$", new DataValue(DataType.BOOLEAN,values[i+1]));
			orNode.evaluate();
			// System.out.println(leftChild.toString()+" OR "+rightChild.toString()+" = "+orNode.toString());
			assertEquals(values[i+2],orNode.getBooleanValue());
		}
	}
	
	@Test
	public final void testNot(){
		Boolean[] values = {new Boolean(true),new Boolean(false),
				new Boolean(false),new Boolean(true),
				null,null
		};

		Node notNode = Operator.NOT.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		notNode.addChild(leftChild);
		for(int i = 0; i < values.length; i+=2){
			notNode.setVariable("$left$", new DataValue(DataType.BOOLEAN,values[i]));
			notNode.evaluate();
			// System.out.println(leftChild.toString()+" OR "+rightChild.toString()+" = "+orNode.toString());
			assertEquals(values[i+1],notNode.getBooleanValue());
		}
	}
	
	@Test
	public final void testAny(){
		Boolean [] left = {null,new Boolean(false),new Boolean(true)};
		Node orNode = Operator.ANY.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		orNode.addChild(leftChild);
		orNode.addChild(rightChild);
		orNode.setVariable("$left$", new DataValue(DataType.BOOLEAN_ARRAY,left));
		orNode.setVariable("$right$", new DataValue(DataType.BOOLEAN,false));
		orNode.evaluate();
		assertTrue(orNode.getBooleanValue());
	}
	
	@Test
	public final void testEvery(){
		Boolean [] left = {null,new Boolean(false),new Boolean(true)};
		Node orNode = Operator.EVERY.getNode();
		Node leftChild = Operator.VARIABLE.getNode("$left$");
		Node rightChild = Operator.VARIABLE.getNode("$right$");
		orNode.addChild(leftChild);
		orNode.addChild(rightChild);
		orNode.setVariable("$left$", new DataValue(DataType.BOOLEAN_ARRAY,left));
		orNode.setVariable("$right$", new DataValue(DataType.BOOLEAN,false));
		orNode.evaluate();
		assertFalse(orNode.getBooleanValue());
	}
	
	
}
