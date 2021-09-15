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
 * Created Jan 2, 2016 by Sharad Singhal
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
import java.lang.reflect.Parameter;
import java.util.HashMap;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import net.aifusion.metamodel.PropertyBindingClass.EmbeddedStringEnum;
import net.aifusion.utils.GetSetClass;
import net.aifusion.utils.Java2Cim;

/**
 * Tests to check Java to CIM mappings
 * @author Sharad Singhal
 */
public class JavaModelMapperTest {

	private InMemoryCache cache;
	private MOFParser parser;

	private String[] mofClasses = {	};

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("JavaModelMapper ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		cache = new InMemoryCache();
		parser = new MOFParser(cache);
		for(String s : mofClasses){
			ByteArrayInputStream input = new ByteArrayInputStream(s.getBytes());
			parser.parse(input, null);
		}
		List<NamedElement> parsedElements = cache.getElements(null, null, null, false);
		assertEquals(0,parsedElements.size());
	}

	@After
	public void tearDown() throws Exception {
		cache.shutdown();
		System.out.print(".");
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getPackagePath(java.lang.Class)}.
	 */
	@Test
	public final void testGetPackagePath() {
		assertEquals("net::aifusion::metamodel",JavaModelMapper.getPackagePath(PropertyBindingClass.class));	// java package
		assertEquals("newPath1::newPath2",JavaModelMapper.getPackagePath(EnumBindingClass.class));				// package path qualifier
		assertEquals("net::aifusion::metamodel",JavaModelMapper.getPackagePath(MethodBindingClass.class));
		assertEquals("net::aifusion::utils",JavaModelMapper.getPackagePath(GetSetClass.class));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getCimElementType(Class)}.
	 */
	@Test
	public final void testGetCimElementType(){
		assertEquals(ElementType.ENUMERATION,JavaModelMapper.getCimElementType(EnumBindingClass.class));
		assertEquals(ElementType.INTERFACE,JavaModelMapper.getCimElementType(InterfaceBindingClass.class));
		assertEquals(ElementType.STRUCTURE,JavaModelMapper.getCimElementType(PropertyBindingClass.class));
		assertEquals(ElementType.CLASS,JavaModelMapper.getCimElementType(MethodBindingClass.class));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#isStructure(java.lang.Class)}.
	 */
	@Test
	public final void testIsStructure() {
		// check enumeration
		assertFalse(JavaModelMapper.isStructure(EnumBindingClass.class));
		// check annotated structure
		assertTrue(JavaModelMapper.isStructure(PropertyBindingClass.class));
		// check a class with cim methods in it
		assertFalse(JavaModelMapper.isStructure(MethodBindingClass.class));
		// an un-annotated structure
		assertTrue(JavaModelMapper.isStructure(MethodBindingSuperClass.class));
		// another un-annotated structure
		assertFalse(JavaModelMapper.isStructure(GetSetClass.class));
		// class with cim methods in it
		assertFalse(JavaModelMapper.isStructure(CimMethodTestClass.class));
		// interface class
		assertFalse(JavaModelMapper.isStructure(InterfaceBindingClass.class));
	}


	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getCimClassName(String)}.
	 */
	@Test
	public final void testGetCimClassNameString(){
		assertEquals("Cim_Test",JavaModelMapper.getCimClassName(PropertyBindingClass.class.getName()));
		assertEquals("AIFusion_EnumBindingClass",JavaModelMapper.getCimClassName(EnumBindingClass.class.getName()));
		assertEquals("Cim_TestMethods",JavaModelMapper.getCimClassName(MethodBindingClass.class.getName()));
		assertEquals("CIM_TestClass",JavaModelMapper.getCimClassName(GetSetClass.class.getName()));
		assertEquals("AIFusion_EmbeddedStringEnum",JavaModelMapper.getCimClassName(PropertyBindingClass.EmbeddedStringEnum.class.getName()));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getCimClassName(java.lang.Class)}.
	 */
	@Test
	public final void testGetCimClassNameClass() {
		assertEquals("Cim_Test",JavaModelMapper.getCimClassName(PropertyBindingClass.class));
		assertEquals("AIFusion_EnumBindingClass",JavaModelMapper.getCimClassName(EnumBindingClass.class));
		assertEquals("Cim_TestMethods",JavaModelMapper.getCimClassName(MethodBindingClass.class));
		assertEquals("CIM_TestClass",JavaModelMapper.getCimClassName(GetSetClass.class));
		assertEquals("AIFusion_EmbeddedStringEnum",JavaModelMapper.getCimClassName(PropertyBindingClass.EmbeddedStringEnum.class));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getCimSuperClassName(Class)}.
	 */
	@Test
	public final void testGetCimSuperClassName(){
		assertEquals(null,JavaModelMapper.getCimSuperClassName(PropertyBindingClass.class));
		assertEquals(null,JavaModelMapper.getCimSuperClassName(EnumBindingClass.class));
		assertEquals("Cim_TestMethodsSup",JavaModelMapper.getCimSuperClassName(MethodBindingClass.class));
		assertEquals(null,JavaModelMapper.getCimSuperClassName(GetSetClass.class));
		assertEquals(null,JavaModelMapper.getCimSuperClassName(PropertyBindingClass.EmbeddedStringEnum.class));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getFeatureName(Method)}.
	 */
	@Test
	public final void testGetFeatureName(){
		HashMap<String,String> names = new HashMap<String,String>();
		names.put("getKey","Key");
		names.put("enumValueToString","enumValueToString");
		names.put("concatStringToEnumValue","concatStringToEnumValue");
		names.put("enumValueToEnum","enumValueToEnum");
		names.put("doSomething","doSomething");

		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method [] methods = c.getClass().getDeclaredMethods();
		assertNotNull(methods);
		assertEquals(names.size(),methods.length);
		for(int i = 0; i < methods.length; i++){
			// System.out.println(methods[i].toGenericString());
			assertEquals(names.get(methods[i].getName()),JavaModelMapper.getFeatureName(methods[i]));
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getFeatureType(Method)}.
	 */
	@Test
	public final void testGetFeatureType(){
		HashMap<String,Class<?>> types = new HashMap<String,Class<?>>();
		types.put("getKey",String.class);
		types.put("enumValueToString",String.class);
		types.put("concatStringToEnumValue",String.class);
		types.put("enumValueToEnum",EnumerationValue.class);
		types.put("doSomething",void.class);

		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method [] methods = c.getClass().getDeclaredMethods();
		assertNotNull(methods);
		assertEquals(types.size(),methods.length);
		for(int i = 0; i < methods.length; i++){
			// System.out.println(methods[i].toGenericString());
			assertEquals(types.get(methods[i].getName()),JavaModelMapper.getFeatureType(methods[i]));
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getRefCimClass(Method)}. 
	 */
	@Test
	public final void testGetRefClassMethod(){
		HashMap<String,String> quals = new HashMap<String,String>();
		quals.put("Key","");
		quals.put("enumValueToString","");
		quals.put("concatStringToEnumValue","");
		quals.put("enumValueToEnum","AIFusion_EmbeddedStringEnum");
		quals.put("doSomething","");

		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method [] methods = c.getClass().getDeclaredMethods();
		assertNotNull(methods);
		assertEquals(quals.size(),methods.length);
		for(int i = 0; i < methods.length; i++){
			// System.out.println(methods[i].toGenericString());
			assertEquals(quals.get(JavaModelMapper.getFeatureName(methods[i])),JavaModelMapper.getRefCimClass(methods[i]));
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getRefCimClass(Parameter)}. 
	 */
	@Test
	public final void testGetRefClassParameter(){
		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method m;
		try {
			m = c.getClass().getDeclaredMethod("enumValueToString",EmbeddedStringEnum.class);
			assertNotNull(m);
			assertEquals(1,m.getParameterCount());
			assertEquals("AIFusion_EmbeddedStringEnum",JavaModelMapper.getRefCimClass(m.getParameters()[0]));
		} catch (NoSuchMethodException | SecurityException e) {
			fail(e.toString());
		}
	}


	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getMappedJavaClassName(Method)}. 
	 */
	@Test
	public final void testGetMappedJavaClassMethod(){
		HashMap<String,String> quals = new HashMap<String,String>();
		quals.put("Key","");
		quals.put("enumValueToString","");
		quals.put("concatStringToEnumValue","");
		quals.put("enumValueToEnum","net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum");
		quals.put("doSomething","");

		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method [] methods = c.getClass().getDeclaredMethods();
		assertNotNull(methods);
		assertEquals(quals.size(),methods.length);
		for(int i = 0; i < methods.length; i++){
			// System.out.println(methods[i].toGenericString());
			assertEquals(quals.get(JavaModelMapper.getFeatureName(methods[i])),JavaModelMapper.getMappedJavaClassName(methods[i]));
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getMappedJavaClassName(Parameter)}. 
	 */
	@Test
	public final void testGetMappedJavaClassParameter(){
		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method m;
		try {
			m = c.getClass().getDeclaredMethod("enumValueToString",EmbeddedStringEnum.class);
			assertNotNull(m);
			assertEquals(1,m.getParameterCount());
			assertEquals("net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum",
					JavaModelMapper.getMappedJavaClassName(m.getParameters()[0]));
		} catch (NoSuchMethodException | SecurityException e) {
			fail(e.toString());
		}
	}


	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getMappingString(Method)}. 
	 */
	@Test
	public final void testGetMappingStringsMethod(){
		HashMap<String,String> quals = new HashMap<String,String>();
		quals.put("Key","");
		quals.put("enumValueToString","");
		quals.put("concatStringToEnumValue","");
		quals.put("enumValueToEnum","MappingStrings{\"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\"}");
		quals.put("doSomething","");

		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method [] methods = c.getClass().getDeclaredMethods();
		assertNotNull(methods);
		assertEquals(quals.size(),methods.length);
		for(int i = 0; i < methods.length; i++){
			// System.out.println(methods[i].toGenericString());
			assertEquals(quals.get(JavaModelMapper.getFeatureName(methods[i])),JavaModelMapper.getMappingString(methods[i]));
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getMappingString(Parameter)}.
	 */
	@Test
	public final void testGetMappingStringsParameter(){
		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		Method m;
		try {
			m = c.getClass().getDeclaredMethod("enumValueToString",EmbeddedStringEnum.class);
			assertNotNull(m);
			assertEquals(1,m.getParameterCount());
			assertEquals("MappingStrings{\"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\"}",
					JavaModelMapper.getMappingString(m.getParameters()[0]));
		} catch (NoSuchMethodException | SecurityException e) {
			fail(e.toString());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getClassVersion(Class)}.
	 */
	@Test
	public final void testGetClassVersion(){
		assertEquals("0.0.1",JavaModelMapper.getClassVersion(PropertyBindingClass.class));	// nothing defined
		assertEquals("6.0.0",JavaModelMapper.getClassVersion(MethodBindingClass.class));	// version annotation
		assertEquals("1.0.0",JavaModelMapper.getClassVersion(EnumBindingClass.class));		// version qualifier

	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#isPropertyMethod(Method)}.
	 */
	@Test
	public final void testIsPropertyMethod(){
		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		for(Method m : c.getClass().getMethods()){
			if(m.getAnnotation(Export.class) == null) continue;
			// System.out.println(m.toGenericString());
			String name = m.getName();
			if(name.startsWith("set") || name.startsWith("get")){
				assertTrue(JavaModelMapper.isPropertyMethod(m));
			} else {
				assertFalse(JavaModelMapper.isPropertyMethod(m));
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#isGetter(Method)}.
	 */
	@Test
	public final void testIsGetter(){
		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		for(Method m : c.getClass().getMethods()){
			if(m.getAnnotation(Export.class) == null) continue;
			// System.out.println(m.toGenericString());
			String name = m.getName();
			if(name.startsWith("get")){
				assertTrue(JavaModelMapper.isGetter(m));
			} else {
				assertFalse(JavaModelMapper.isGetter(m));
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#isSetter(Method)}.
	 */
	@Test
	public final void testIsSetter(){
		MethodBindingClass c = new MethodBindingClass();
		assertNotNull(c);
		for(Method m : c.getClass().getMethods()){
			if(m.getAnnotation(Export.class) == null) continue;
			// System.out.println(m.toGenericString());
			String name = m.getName();
			if(name.startsWith("set")){
				assertTrue(JavaModelMapper.isSetter(m));
			} else {
				assertFalse(JavaModelMapper.isSetter(m));
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getObjectPathFromClass(java.lang.Class)}.
	 */
	@Test
	public final void testGetObjectPathFromClass() {
		NameSpacePath p = new NameSpacePath(Constants.defaultLocalPath);
		assertEquals(new ObjectPath(ElementType.STRUCTURE,"CIM_TEST",p,null, null),
				JavaModelMapper.getObjectPathFromClass(PropertyBindingClass.class));
		assertEquals(new ObjectPath(ElementType.ENUMERATION,"AIFusion_EnumbindingClass",p,null, null),
				JavaModelMapper.getObjectPathFromClass(EnumBindingClass.class));
		assertEquals(new ObjectPath(ElementType.CLASS,"Cim_TestMethods",p,null, null),
				JavaModelMapper.getObjectPathFromClass(MethodBindingClass.class));
	}



	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#javaEnumMatchesCimEnum(java.lang.Class, net.aifusion.metamodel.CimEnumeration)}.
	 */
	@Test
	public final void testJavaEnumMatches() {
		String mof = "Enumeration CimFusion_EnumBindingClass : SInt32 {\nNAME1 = 0,\nName2 = 2,\nname3 = 3\n};\n";
		MOFParser parser = new MOFParser();
		parser.parse(new ByteArrayInputStream(mof.getBytes()), Constants.defaultNameSpacePath);
		Repository r = parser.getRepository();
		CimEnumeration cimEnumType = (CimEnumeration) r.get(new ObjectPath(ElementType.ENUMERATION,"CimFusion_EnumBindingClass",Constants.defaultNameSpacePath,null, null));
		assertNotNull(cimEnumType);
		assertTrue(JavaModelMapper.javaEnumMatchesCimEnum(EnumBindingClass.class, cimEnumType));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#JavaEnumValueMatches(Enum, EnumerationValue)}.
	 */
	@Test
	public final void testJavaEnumValueMatches() {
		String mof = "Enumeration CimFusion_EnumBindingClass : SInt32 {\nNAME1 = 0,\nName2 = 2,\nname3 = 3\n};\n";
		MOFParser parser = new MOFParser();
		parser.parse(new ByteArrayInputStream(mof.getBytes()), Constants.defaultNameSpacePath);
		Repository r = parser.getRepository();
		CimEnumeration cimEnumType = (CimEnumeration) r.get(new ObjectPath(ElementType.ENUMERATION,"CimFusion_EnumBindingClass",Constants.defaultNameSpacePath,null, null));
		assertNotNull(cimEnumType);
		EnumerationValue v = cimEnumType.getValue("Name1");
		assertNotNull(v);
		assertTrue(JavaModelMapper.JavaEnumValueMatches(EnumBindingClass.NAME1, v));
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#createJavaObjectForCim(StructureValue, Class)}.
	 */
	@Test
	public final void testcreateJavaObjectForCim() {
		// create a structure value by introspection of the test java class
		InMemoryCache c = new InMemoryCache();
		NamedElement e = Java2Cim.getModelForClass(StructureValueClass.class, c);
		assertEquals(ElementType.STRUCTURE,e.getElementType());
		CimStructure s = (CimStructure)e;
		HashMap<String,DataValue> props = new HashMap<String,DataValue>();
		props.put("Id", new DataValue(DataType.STRING,"myId"));
		StructureValue sv = StructureValue.createStructureValue(s, props, null);

		// create a java object from the structure value
		Object javaObject = JavaModelMapper.createJavaObjectForCim(sv, StructureValueClass.class);
		assertNotNull(javaObject);

		// validate that the java object contains the proper properties
		assertTrue(javaObject instanceof StructureValueClass);
		StructureValueClass cl = (StructureValueClass) javaObject;
		assertEquals("DIGEST",cl.getP1());
		assertEquals("myId",cl.getId());
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#validatePropertyBinding(CimProperty, Object)}.
	 */
	@Test
	public final void testvalidatePropertyBinding() {
		// create a structure value by introspection of the test java class
		InMemoryCache c = new InMemoryCache();
		NamedElement e = Java2Cim.getModelForClass(StructureValueClass.class, c);
		assertEquals(ElementType.STRUCTURE,e.getElementType());
		CimStructure s = (CimStructure)e;
		HashMap<String,DataValue> props = new HashMap<String,DataValue>();
		props.put("Id", new DataValue(DataType.STRING,"myId"));
		StructureValue sv = StructureValue.createStructureValue(s, props, null);

		// create a java object from the structure value
		Object javaObject = JavaModelMapper.createJavaObjectForCim(sv, StructureValueClass.class);
		assertNotNull(javaObject);

		for(CimProperty p : s.getAllProperties().values()) {
			Method [] accessors = JavaModelMapper.validatePropertyBinding(p, javaObject);
			assertEquals(2,accessors.length);
			switch(p.getName()) {
			case "Id":	// readable property
				assertNotNull(accessors[0]);
				assertNull(accessors[1]);
				break;
			case "P1":	// writable property
				assertNotNull(accessors[0]);
				assertNotNull(accessors[1]);
				break;
			default:
				fail("Property "+p.getName()+" not yet handled in the test case");
			}
		}
		
		// TODO - these tests should go into StructureValueTest.java (#testBind) once we are done with JavaModelMapper 
		Object o2 = sv.bind();
		assertNotNull(o2);
		StructureValueClass cl = (StructureValueClass) o2;
		assertEquals("myId DIGEST", cl.toString());
		sv.setPropertyValue("P1", new DataValue(DataType.STRING,"SOME"));
		assertEquals("myId SOME", cl.toString());
		return;
	}

	// TODO: ------- We are here --------
	
	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#createCimValueFromJavaObject(CimStructure, Object)}.
	 */
	@Ignore
	@Test
	public final void testCreateCimValueForJavaObject() {
		fail("Not yet implemented"); // TODO
	}

	@Ignore
	@Test
	public final void testStructureValueMatches(){
		fail("Not yet implemented"); // TODO
	}


	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#getInvocationParameter(java.lang.Class, java.lang.Object)}.
	 */
	@Ignore
	@Test
	public final void testGetInvocationParameter() {
		fail("Not yet implemented"); // TODO
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#invokeMethod(net.aifusion.metamodel.CimMethod, java.lang.Object, java.lang.reflect.Method, java.util.List)}.
	 */
	@Ignore
	@Test
	public final void testInvokeMethod() {
		fail("Not yet implemented"); // TODO
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#validateMethodBinding(net.aifusion.metamodel.CimMethod, java.lang.reflect.Method, java.lang.Object)}.
	 */
	@Ignore
	@Test
	public final void testValidateMethodBinding() {
		fail("Not yet implemented"); // TODO
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#validatePropertyBinding(net.aifusion.metamodel.CimProperty, java.lang.reflect.Method, java.lang.reflect.Method, java.lang.Object)}.
	 */
	@Ignore
	@Test
	public final void testValidatePropertyBinding() {
		fail("Not yet implemented"); // TODO
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#readPropertyValue(net.aifusion.metamodel.CimProperty, java.lang.reflect.Method, java.lang.Object)}.
	 */
	@Ignore
	@Test
	public final void testReadPropertyValue() {
		fail("Not yet implemented"); // TODO
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#writePropertyValue(net.aifusion.metamodel.CimProperty, java.lang.reflect.Method, java.lang.Object, net.aifusion.metamodel.DataValue)}.
	 */
	@Ignore
	@Test
	public final void testWritePropertyValue() {
		fail("Not yet implemented"); // TODO
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#invokeMethod(CimMethod, Object, Method, List)}.
	 */
	@Ignore
	@Test
	public final void testInvokeOperation() {
		fail("Not yet implemented"); // TODO
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#validateStaticMethodBinding(CimMethod, Class)}.
	 */
	@Ignore
	@Test
	public final void testValidateStaticMethodBinding() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#validateMethodBinding(CimMethod, Object)}.
	 */
	@Ignore
	@Test
	public final void testvalidateMethodBinding() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.JavaModelMapper#validateStaticPropertyBinding(CimProperty, Class)}.
	 */
	@Ignore
	@Test
	public final void testvalidateStaticPropertyBinding() {
		fail("Not yet implemented");
	}




}
