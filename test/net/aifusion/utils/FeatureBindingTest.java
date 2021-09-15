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
 * Created Jun 26, 2016 by Sharad Singhal
 */
package net.aifusion.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Vector;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.MethodBindingClass;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.PropertyBindingClass;

import net.aifusion.utils.CimFeature;
import net.aifusion.utils.FeatureBinding;

/**
 * @author Sharad Singhal
 *
 */
public class FeatureBindingTest {
	
	private static Class<PropertyBindingClass> testClass = net.aifusion.metamodel.PropertyBindingClass.class;
	private static Vector<Method> methods = new Vector<Method>();
	private static HashMap<String,String> mofs = new HashMap<String,String>();
	
	private static String [] featureMofs = {
			"Va10","[Write]\nSInt16 [] Va10;\n","Va11","[Write]\nSInt32 [] Va11;\n","Va12","[Write]\nSInt32 [] Va12;\n",
			"Va13","[Write]\nSInt64 [] Va13;\n","Va14","[Write]\nSInt64 [] Va14;\n","Va15","[Write]\nReal32 [] Va15;\n",
			"V21","[Write]\nString V21;\n","V20","[Write]\nChar16 V20;\n",
			"V23","[Write]\nCim_TestMethods Ref V23;\n",
			"V22","[Write]\nDatetime V22;\n","Va16","[Write]\nReal32 [] Va16;\n",
			"V25","[Write]\nCim_TestMethodsSup V25;\n",
			"Va17","[Write]\nReal64 [] Va17;\n",
			"V24","[Write]\nAIFusion_EmbeddedStringEnum V24;\n",
			"Va18","[Write]\nReal64 [] Va18;\n","V27","[Write]\nOctetString V27;\n",
			"Va19","[Write]\nChar16 [] Va19;\n",
			"V26","[Write]\nCim_TestMethods V26;\n",
			"V29","[Write]\nCim_TestMethodsSup V29;\n",
			"V28","[Write]\nAIFusion_EnumBindingClass V28;\n",
			"Va01","[Write]\nBoolean [] Va01;\n","Va02","[Write]\nBoolean [] Va02;\n","Va03","[Write]\nUInt8 [] Va03;\n",
			"Va04","[Write]\nUInt16 [] Va04;\n",
			"V30","[Write]\nCim_TestMethods V30;\n",
			"V31","[Static,Write]\nString V31 = \"default\";\n",
			"Va05","[Write]\nUInt32 [] Va05;\n","Va06","[Write]\nUInt64 [] Va06;\n",
			"Va07","[Write]\nSInt8 [] Va07;\n","Va08","[Write]\nSInt8 [] Va08;\n",
			"Va09","[Write]\nSInt16 [] Va09;\n","Va30","[Write]\nCim_TestMethods [] Va30;\n",
			"Va31","[Static,Write]\nString [] Va31;\n","V01","[Write]\nBoolean V01;\n","V03","[Write]\nUInt8 V03;\n","V02","[Write]\nBoolean V02;\n",
			"V05","[Write]\nUInt32 V05;\n","V04","[Write]\nUInt16 V04;\n","V07","[Write]\nSInt8 V07;\n","V06","[Write]\nUInt64 V06;\n",
			"V09","[Write]\nSInt16 V09;\n","V08","[Write]\nSInt8 V08;\n","Va20","[Write]\nChar16 [] Va20;\n","Va21","[Write]\nString [] Va21;\n",
			"Va22","[Write]\nDatetime [] Va22;\n",
			"Va23","[Write]\nCim_TestMethods Ref [] Va23;\n",
			"Va24","[Write]\nAIFusion_EmbeddedStringEnum [] Va24;\n",
			"Va25","[Write]\nCim_TestMethodsSup [] Va25;\n",
			"Va26","[Write]\nCim_TestMethods [] Va26;\n",
			"V10","[Write]\nSInt16 V10;\n","V12","[Write]\nSInt32 V12;\n","V11","[Write]\nSInt32 V11;\n","V14","[Write]\nSInt64 V14;\n",
			"Va27","[Write]\nOctetString [] Va27;\n",
			"V13","[Write]\nSInt64 V13;\n",
			"Va28","[Write]\nAIFusion_EnumBindingClass [] Va28;\n",
			"V16","[Write]\nReal32 V16;\n",
			"Va29","[Write]\nCim_TestMethodsSup [] Va29;\n",
			"V15","[Write]\nReal32 V15;\n","V18","[Write]\nReal64 V18;\n","V17","[Write]\nReal64 V17;\n","V19","[Write]\nChar16 V19;\n"
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
		System.out.print("FeatureBinding ");
		for(Method m : testClass.getDeclaredMethods()){
			if(!Modifier.isPublic(m.getModifiers())) continue;	// not a public method
			if(m.getAnnotation(Export.class) == null) continue;	// not exported
			methods.add(m);
		}
		for(int i = 0; i < featureMofs.length; i += 2){
			mofs.put(featureMofs[i], featureMofs[i+1]);
		}
	}

	/**
	 * Test method for {@link net.aifusion.utils.FeatureBinding#FeatureBinding(java.lang.String)}.
	 */
	@Test
	public final void testFeatureBinding() {
		FeatureBinding b = new FeatureBinding("someName");
		assertNotNull(b);
		return;
	}

	/**
	 * Test method for {@link net.aifusion.utils.FeatureBinding#getName()}.
	 */
	@Test
	public final void testGetName() {
		FeatureBinding b = new FeatureBinding("someName");
		assertNotNull(b);
		assertEquals("someName",b.getName());
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.FeatureBinding#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		HashMap<String,FeatureBinding> bindings = new HashMap<String,FeatureBinding>();
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			if(!bindings.containsKey(f.getName())) bindings.put(f.getName(), new FeatureBinding(f.getName()));
			FeatureBinding b = bindings.get(f.getName());
			b.addFeature(f);
		}
		assertEquals(62,bindings.size());
		for(FeatureBinding b : bindings.values()){
			// System.out.println("\""+b.getName()+"\",\""+ModelUtilities.escape(b.toMOF())+"\",");
			assertEquals(mofs.get(b.getName()),b.toMOF());
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.utils.FeatureBinding#addFeature(net.aifusion.utils.CimFeature)}.
	 */
	@Test
	public final void testAddFeature() {
		HashMap<String,FeatureBinding> bindings = new HashMap<String,FeatureBinding>();
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			if(!bindings.containsKey(f.getName())) bindings.put(f.getName(), new FeatureBinding(f.getName()));
			FeatureBinding b = bindings.get(f.getName());
			b.addFeature(f);
		}
		assertEquals(62,bindings.size());
		for(Method m : methods){
			CimFeature f = new CimFeature(m);
			assertNotNull(f);
			FeatureBinding b = bindings.get(f.getName());
			assertNotNull(b);
			try {
				b.addFeature(f);
			} catch (ModelException ex){
				assertEquals(11,ex.getReason().getCode()); // ExceptionReason.ALREADY_EXISTS
			}
		}
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.FeatureBinding#toMOF()}.
	 */
	@Test
	public final void testBinding(){
		HashMap<String,FeatureBinding> bindings = new HashMap<String,FeatureBinding>();
		HashMap<String,String> mofs = new HashMap<String,String>();
		mofs.put("IsGetGetSetValue","[Write,Description(\"Case 7 - get/IsGet/Set only\")]\nBoolean IsGetGetSetValue;\n");
		mofs.put("WriteOnlyValue","[Write,Description(\"Case 2 - setter only\"),Read(false)]\nString WriteOnlyValue;\n");
		mofs.put("GetSetValue","[Write,Description(\"Case 6 - get/set only\")]\nString GetSetValue;\n");
		mofs.put("echo","[Description(\"Case 0 - method\")]\nString echo(String arg0);\n");
		mofs.put("ID","[Key,Description(\"Case 4 - getter only\")]\nString ID;\n");
		mofs.put("IsGetOnlyValue","[Description(\"Case 1 - isGet only\")]\nBoolean IsGetOnlyValue;\n");
		mofs.put("IsGetSetValue","[Write,Description(\"Case 3 - isGet/set only\")]\nBoolean IsGetSetValue;\n");
		mofs.put("IsGetGetValue","[Description(\"Case 5 - get/IsGet only\")]\nBoolean IsGetGetValue;\n");
		try {
			Class<?> javaClass = Class.forName("net.aifusion.utils.GetSetClass");
			for(Method m : javaClass.getDeclaredMethods()){
				if(!Modifier.isPublic(m.getModifiers())) continue;
				if(!m.isAnnotationPresent(Export.class)) continue;
				CimFeature f = new CimFeature(m);
				if(!bindings.containsKey(f.getName())) bindings.put(f.getName(), new FeatureBinding(f.getName()));
				FeatureBinding b = bindings.get(f.getName());
				b.addFeature(f);
			}
		} catch (ClassNotFoundException e) {
			fail(e.toString());
		}
		for(FeatureBinding b : bindings.values()){
			assertEquals(mofs.get(b.getName()),b.toMOF());
		}
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.FeatureBinding#getReferencedTypes()}.
	 */
	@Test
	public final void testGetReferencedTypes(){
		HashMap<String,FeatureBinding> bindings = new HashMap<String,FeatureBinding>();
		HashMap<String,Integer> count = new HashMap<String,Integer>();
		count.put("enumValueToEnum", 1);
		count.put("enumValueToString", 1);
		count.put("concatStringToEnumValue", 1);
		count.put("doSomething", 0);
		count.put("Key", 0);
		Class<?> javaClass = MethodBindingClass.class;
		for(Method m : javaClass.getDeclaredMethods()){
			if(!Modifier.isPublic(m.getModifiers())) continue;
			if(!m.isAnnotationPresent(Export.class)) continue;
			CimFeature f = new CimFeature(m);
			if(!bindings.containsKey(f.getName())) bindings.put(f.getName(), new FeatureBinding(f.getName()));
			FeatureBinding b = bindings.get(f.getName());
			b.addFeature(f);
		}
		for(FeatureBinding b : bindings.values()){
			/*
			Map<String,Class<?>> types = b.getReferencedTypes();
			System.out.println(b.getName()+" "+types.size());
			for(String k : types.keySet()){
				System.out.println(k+" : "+types.get(k).toGenericString());
			}
			*/
			assertEquals(count.get(b.getName()).intValue(),b.getReferencedTypes().size());
		}
		return;
	}
	
	/**
	 * Test method for {@link net.aifusion.utils.FeatureBinding#toMOF()}.
	 */
	@Test
	public final void testMethodBinding(){
		HashMap<String,FeatureBinding> bindings = new HashMap<String,FeatureBinding>();
		HashMap<String,String> mofs = new HashMap<String,String>();
		mofs.put("enumValueToEnum", "AIFusion_EmbeddedStringEnum enumValueToEnum(AIFusion_EmbeddedStringEnum arg0);\n");
		mofs.put("enumValueToString", "String enumValueToString(AIFusion_EmbeddedStringEnum arg0);\n");
		mofs.put("concatStringToEnumValue", "String concatStringToEnumValue(AIFusion_EmbeddedStringEnum arg0,String arg1);\n");
		mofs.put("doSomething", "Void doSomething();\n");
		mofs.put("Key", "[Key]\nString Key;\n");
		try {
			Class<?> javaClass = Class.forName("net.aifusion.metamodel.MethodBindingClass");
			for(Method m : javaClass.getDeclaredMethods()){
				if(!Modifier.isPublic(m.getModifiers())) continue;
				if(!m.isAnnotationPresent(Export.class)) continue;
				CimFeature f = new CimFeature(m);
				if(!bindings.containsKey(f.getName())) bindings.put(f.getName(), new FeatureBinding(f.getName()));
				FeatureBinding b = bindings.get(f.getName());
				b.addFeature(f);
			}
		} catch (ClassNotFoundException e) {
			fail(e.toString());
		}
		for(FeatureBinding b : bindings.values()){
			assertEquals(mofs.get(b.getName()),b.toMOF());
		}
		return;
		
		
	}
	
	

}
