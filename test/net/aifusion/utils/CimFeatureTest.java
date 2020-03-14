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
 * Created Jun 25, 2016 by Sharad Singhal
 */
package net.aifusion.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.MethodBindingClass;
import net.aifusion.metamodel.PropertyBindingClass;

/**
 * Test methods for CimFeature
 * @author Sharad Singhal
 */
public class CimFeatureTest {
	private static Class<PropertyBindingClass> testClass = net.aifusion.metamodel.PropertyBindingClass.class;
	private static Vector<Method> methods = new Vector<Method>();
	private static HashMap<String,String> mofs = new HashMap<String,String>();
	private static HashMap<String,String> refs = new HashMap<String,String>();
	private static String propertyMof[] = {
			"V01", "Boolean V01","Va01", "Boolean [] Va01","V02", "Boolean V02","Va02", "Boolean [] Va02","V03", "UInt8 V03","Va03", "UInt8 [] Va03",
			"V04", "UInt16 V04","Va04", "UInt16 [] Va04","V05", "UInt32 V05","Va05", "UInt32 [] Va05","V06", "UInt64 V06","Va06", "UInt64 [] Va06",
			"V07", "SInt8 V07","Va07", "SInt8 [] Va07","V08", "SInt8 V08","Va08", "SInt8 [] Va08","V09", "SInt16 V09","Va09", "SInt16 [] Va09",
			"V10", "SInt16 V10","Va10", "SInt16 [] Va10","V11", "SInt32 V11","Va11", "SInt32 [] Va11","V12", "SInt32 V12","Va12", "SInt32 [] Va12",
			"V13", "SInt64 V13","Va13", "SInt64 [] Va13","V14", "SInt64 V14","Va14", "SInt64 [] Va14","V15", "Real32 V15","Va15", "Real32 [] Va15",
			"V16", "Real32 V16","Va16", "Real32 [] Va16","V17", "Real64 V17","Va17", "Real64 [] Va17","V18", "Real64 V18","Va18", "Real64 [] Va18",
			"V19", "Char16 V19","Va19", "Char16 [] Va19","V20", "Char16 V20","Va20", "Char16 [] Va20","V21", "String V21","Va21", "String [] Va21",
			"V22", "Datetime V22","Va22", "Datetime [] Va22","V23", "Cim_TestMethods Ref V23","Va23", "Cim_TestMethods Ref [] Va23",
			"V24", "AIFusion_EmbeddedStringEnum V24","Va24", "AIFusion_EmbeddedStringEnum [] Va24",
			"V25", "Cim_TestMethodsSup V25","Va25", "Cim_TestMethodsSup [] Va25","V26", "Cim_TestMethods V26","Va26", "Cim_TestMethods [] Va26",
			"V27", "OctetString V27","Va27", "OctetString [] Va27",
			"V28", "AIFusion_EnumBindingClass V28","Va28", "AIFusion_EnumBindingClass [] Va28",
			"V29", "Cim_TestMethodsSup V29","Va29", "Cim_TestMethodsSup [] Va29","V30", "Cim_TestMethods V30","Va30", "Cim_TestMethods [] Va30",
			"V31", "String V31","Va31", "String [] Va31"
	};
	
	private static String [] refClasses = {
			"V01","","Va01","","V02","","Va02","","V03","","Va03","","V04","","Va04","","V05","","Va05","","V06","","Va06","",
			"V07","","Va07","","V08","","Va08","","V09","","Va09","","V10","","Va10","","V11","","Va11","","V12","","Va12","",
			"V13","","Va13","","V14","","Va14","","V15","","Va15","","V16","","Va16","","V17","","Va17","","V18","","Va18","",
			"V19","","Va19","","V20","","Va20","","V21","","Va21","","V22","","Va22","",
			"V23","Cim_TestMethods","Va23","Cim_TestMethods","V24","AIFusion_EmbeddedStringEnum","Va24","AIFusion_EmbeddedStringEnum",
			"V25","Cim_TestMethodsSup","Va25","Cim_TestMethodsSup","V26","Cim_TestMethods","Va26","Cim_TestMethods",
			"V27","","Va27","","V28","AIFusion_EnumBindingClass","Va28","AIFusion_EnumBindingClass","V29","Cim_TestMethodsSup",
			"Va29","Cim_TestMethodsSup","V30","Cim_TestMethods","Va30","Cim_TestMethods","V31","","Va31",""
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
		System.out.print("CimFeature ");
		for(Method m : testClass.getDeclaredMethods()){
			if(!Modifier.isPublic(m.getModifiers())) continue;	// not a public method
			if(m.getAnnotation(Export.class) == null) continue;	// not exported
			methods.add(m);
		}
		for(int i = 0; i < propertyMof.length; i += 2){
			mofs.put(propertyMof[i], propertyMof[i+1]);
		}
		for(int i = 0; i < refClasses.length; i += 2){
			refs.put(refClasses[i], refClasses[i+1]);
		}
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#CimFeature(java.lang.reflect.Method)}.
	 */
	@Test
	public final void testCimFeature() {
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#isGetter()}.
	 */
	@Test
	public final void testIsGetter() {
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			// System.out.println(f.isGetter()+" : "+m.toGenericString());
			if(m.getName().startsWith("get")){
				assertTrue(f.isGetter());
			} else {
				assertFalse(f.isGetter());
			}
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#isSetter()}.
	 */
	@Test
	public final void testIsSetter() {
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			// System.out.println(f.isSetter()+" : "+m.toGenericString());
			if(m.getName().startsWith("set")){
				assertTrue(f.isSetter());
			} else {
				assertFalse(f.isSetter());
			}
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#isIsGetter()}.
	 */
	@Test
	public final void testIsIsGetter() {
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			// System.out.println(f.isSetter()+" : "+m.toGenericString());
			if(m.getName().startsWith("is")){
				assertTrue(f.isIsGetter());
			} else {
				assertFalse(f.isIsGetter());
			}
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getName()}.
	 */
	@Test
	public final void testGetName() {
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			String name = m.getName();
			name = f.isIsGetter() ? name.substring(2) : f.isProperty() ? name.substring(3) : name;
			assertEquals(name,f.getName());
		}
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#isProperty()}.
	 */
	@Test
	public final void testIsProperty() {
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			assertTrue(f.isProperty());
		}
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getQualifiers()}.
	 */
	@Test
	public final void testGetQualifiers() {
		// we do not have any qualifiers (defined using qualifiers="" annotation) in our test class
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			String qualifiers = f.getQualifiers();
			assertEquals("",qualifiers);
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getCimType()}.
	 */
	@Test
	public final void testGetCimType() {
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			DataType dt = f.getCimType();
			if(f.isSetter()) {
				assertEquals(DataType.getTypeForClass(m.getParameters()[0].getType()),dt);
			} else {
				assertEquals(DataType.getTypeForClass(m.getReturnType()),dt);
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getCimParameters()}.
	 */
	@Test
	public final void testGetCimParameters() {
		// TODO: This needs to be finished once CimFeature implements method features
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			assertEquals("",f.getCimParameters());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#toMOF()}.
	 */
	@Test
	public final void testToMOF(){		
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			String mof = f.toMOF();
			String expect = mofs.get(f.getName());
			assertEquals(expect,mof);
		}
	}
	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getJavaType()}.
	 */
	@Test
	public final void testGetJavaType(){
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			DataType dt = f.getCimType();
			Class<?> javaType = f.getJavaType();
			assertEquals(dt,DataType.getTypeForClass(javaType));
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getDefaultValue()}.
	 */
	@Test
	public final void testGetDefaultValue(){
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			if(f.getName().equals("V31") && f.isGetter()){
				assertEquals("\"default\"",f.getDefaultValue());
			} else {
				assertEquals("",f.getDefaultValue());
			}
			
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#isStatic()}.
	 */
	@Test
	public final void testIsStatic(){
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			if(f.getName().equals("V31") || f.getName().equals("Va31")){
				assertTrue(f.isStatic());
			} else {
				assertFalse(f.isStatic());
			}
			
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getRefClass()}.
	 */
	@Test
	public final void testGetRefClass(){
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			// System.out.println(f.getName()+" : "+m.toGenericString());
			String refClass = refs.get(f.getName());
			assertEquals(refClass,f.getRefClass());
		}
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getJavaParameters()}.
	 */
	@Test
	public final void testGetJavaParameters(){
		HashMap<String,Integer> count = new HashMap<String,Integer>();
		count.put("enumValueToEnum", 1);
		count.put("doSomething", 0);
		count.put("concatStringToEnumValue", 2);
		count.put("enumValueToString", 1);
		for(Method m : MethodBindingClass.class.getMethods()){
			if(!Modifier.isPublic(m.getModifiers())) continue;
			if(m.getAnnotation(Export.class) == null) continue;
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			List<Class<?>> params = f.getJavaParameters();
			if(f.isProperty()) { 
				assertTrue(params.isEmpty());
			} else if(f.isMethod()){
				assertEquals(count.get(f.getName()).intValue(),params.size());
			}
		}
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.CimFeature#getRefJavaClass()}.
	 */
	@Test
	public final void testGetRefJavaClass(){
		for(Method m : MethodBindingClass.class.getMethods()){
			if(!Modifier.isPublic(m.getModifiers())) continue;
			if(m.getAnnotation(Export.class) == null) continue;
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			Class<?> javaClass = f.getRefJavaClass();
			if(f.getCimType().isPrimitive() || f.getCimType() == DataType.VOID){
				assertNull(javaClass);
			} else {
				assertNotNull(javaClass);
			}
		}
		return;
	}
	
	
	

}
