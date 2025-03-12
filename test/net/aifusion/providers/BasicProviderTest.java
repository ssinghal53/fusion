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
 * Created Feb 7, 2016 by Sharad Singhal
 */
package net.aifusion.providers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.CimEvent;
import net.aifusion.metamodel.CimEventType;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimListener;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to test BasicProvider
 * @author Sharad Singhal
 */
public class BasicProviderTest implements CimListener {
	private static boolean verbose = false;
	private static int nProviders = 3;
	private static int parent = 0, child1 = 1, child2 = 2;
	private static String nameBase = "/root/ns";
	private static String mofClass[] = {
			"Enumeration test_enum : string { enumvalue};\n"+
			"Structure test_struct { test_enum v;\n string foobar; };\n"+
			"class test_class {\n\t[key]\n\tSint32 integerProperty;\nvoid voidMethod();\n};\n"+
			"instance of test_class {\n\tintegerProperty = 5;\n};\n",
			
			"Enumeration test_enumb : string { enumvalue};\n"+
			"Structure test_structb { test_enumb v;\n string foobar; };\n"+
			"class test_classb {\n\t[key]\n\tSint32 integerProperty;\n\t[write]\n\tString foobar;\n};\n"+
			"instance of test_classb {\n\tintegerProperty = 5;\n};\n",
			
			"Enumeration test_enumc : string { enumvalue};\n"+
			"Structure test_structc { test_enumc v;\n string foobar; };\n"+
			"class test_classc {\n\t[key]\n\tSint32 integerProperty;\n\tString loopBack(String input);\n};\n"+
			"instance of test_classc {\n\tintegerProperty = 5;\n};\n"
	};
	
	private NameSpacePath nameSpacePath[] = new NameSpacePath[nProviders];
	private Repository repository[] = new Repository[nProviders];
	private Provider provider[] = new Provider[nProviders];
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("BasicProvider ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		assertEquals(nProviders,mofClass.length);
		for(int i = 0; i < nProviders; i++){
			nameSpacePath[i] = new NameSpacePath("http","localhost:"+(8080+i),nameBase+i);
			repository[i] = new InMemoryCache();
			MOFParser parser = new MOFParser(repository[i]);
			parser.parse(new ByteArrayInputStream(mofClass[i].getBytes()), nameSpacePath[i]);
			provider[i] = new BasicProvider(repository[i]);
		}	
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#BasicProvider(net.aifusion.metamodel.Repository)}.
	 */
	@Test
	public final void testBasicProvider() {
		for(Provider p : provider){
			assertNotNull(p);
		}		
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#executeQuery(java.lang.String)}.
	 */
	@Test
	public final void testExecuteQuery() {
		BasicProvider p = (BasicProvider) provider[parent];
		assertNotNull(p);
		try {
			List<StructureValue> resultSet = p.executeQuery("select * from test_class where integerProperty = 5");
			assertEquals(1,resultSet.size());
		} catch (ModelException ex){
			assertEquals(7,ex.getReason().getCode());	// NOT_SUPPORTED
		}
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#registerChildProvider(net.aifusion.providers.Provider)}.
	 */
	@Test
	public final void testRegisterChildProvider() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		try {
			provider[parent].registerChildProvider(provider[child1]);
			fail("Should not succeed");
		} catch (ModelException ex){
			assertEquals(11,ex.getReason().getCode());	// ALREADY_EXISTS
		}
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#unregisterChildProvider(net.aifusion.providers.Provider)}.
	 */
	@Test
	public final void testUnregisterChildProvider() {
		provider[parent].registerChildProvider(provider[child1]);
		try {
			provider[parent].registerChildProvider(provider[child1]);
			fail("Should not succeed");
		} catch (ModelException ex){
			assertEquals(11,ex.getReason().getCode());	// ALREADY_EXISTS
		}
		provider[parent].unregisterChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child1]);
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getPropertyNames(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testGetPropertyNames() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];

		// get names of a class property in parent
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath[parent],null, null);
		List<String> names = p.getPropertyNames(path);
		assertNotNull(names);
		assertEquals(1,names.size());
		assertEquals("integerProperty",names.get(0));

		// get names of a structure in parent
		path = new ObjectPath(ElementType.STRUCTURE,"test_struct",nameSpacePath[parent],null, null);
		names = p.getPropertyNames(path);
		assertNotNull(names);
		assertEquals(2,names.size());
		assertTrue(names.contains("v"));
		assertTrue(names.contains("foobar"));
		
		// get names of an enum in parent
		path = new ObjectPath(ElementType.ENUMERATION,"test_enum",nameSpacePath[parent],null, null);
		names = p.getPropertyNames(path);
		assertNotNull(names);
		assertEquals(1,names.size());
		assertTrue(names.contains("enumvalue"));
		
		// get names of an instance in parent
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(5));
		path = new ObjectPath(ElementType.INSTANCE,"test_class",nameSpacePath[parent],keys, null);
		names = p.getPropertyNames(path);
		assertNotNull(names);
		assertEquals(1,names.size());
		assertTrue(names.contains("integerProperty"));
		
		// locate some class in a child provider
		path = new ObjectPath(ElementType.CLASS,"test_classb",nameSpacePath[child1],null, null);
		names = p.getPropertyNames(path);
		assertNotNull(names);
		assertEquals(2,names.size());
		assertTrue(names.contains("integerProperty"));
		assertTrue(names.contains("foobar"));
		
		// get names of a structure in child
		path = new ObjectPath(ElementType.STRUCTURE,"test_structc",nameSpacePath[child2],null, null);
		names = p.getPropertyNames(path);
		assertNotNull(names);
		assertEquals(2,names.size());
		assertTrue(names.contains("v"));
		assertTrue(names.contains("foobar"));
		
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getPropertyType(net.aifusion.metamodel.ObjectPath, java.lang.String)}.
	 */
	@Test
	public final void testGetPropertyType() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];

		// get names of a class property in parent
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath[parent],null, null);
		DataType dt = p.getPropertyType(path, "IntegerProperty");
		assertEquals(DataType.SINT32,dt);

		// get names of a structure in parent
		path = new ObjectPath(ElementType.STRUCTURE,"test_struct",nameSpacePath[parent],null, null);
		dt = p.getPropertyType(path, "v");
		assertEquals(DataType.ENUMERATIONVALUE,dt);
		
		// locate some items in a child provider
		path = new ObjectPath(ElementType.ENUMERATION,"test_enumb",nameSpacePath[child1],null, null);
		dt = p.getPropertyType(path, "enumValue");
		assertEquals(DataType.STRING,dt);
		
		// locate items in a child provider with non-matching namespace (same localPath)
		NameSpacePath diffPath = new NameSpacePath(null,null,nameSpacePath[child1].getLocalPath());
		path = new ObjectPath(ElementType.ENUMERATION,"test_enumb",diffPath,null, null);
		dt = p.getPropertyType(path, "enumValue");
		assertEquals(DataType.STRING,dt);

		
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getPropertyValue(net.aifusion.metamodel.ObjectPath, java.lang.String)}.
	 */
	@Test
	public final void testGetPropertyValue() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];

		// get names of a class property in parent
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath[parent],null, null);
		try {
			p.getPropertyValue(path, "IntegerProperty");
			fail("should not succeed");
		} catch(ModelException ex){
			assertEquals(2,ex.getReason().getCode());	// ACCESS_DENIED
		}
		
		// get value of an instance in parent
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(5));
		path = new ObjectPath(ElementType.INSTANCE,"test_class",nameSpacePath[parent],keys, null);
		DataValue v = p.getPropertyValue(path, "IntegerProperty");
		assertEquals(new DataValue(5),v);
		
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#setPropertyValue(net.aifusion.metamodel.ObjectPath, java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testSetPropertyValue() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		
		// get value of an instance in parent
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(5));
		ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_classb",nameSpacePath[child1],keys, null);
		DataValue v = p.getPropertyValue(path, "foobar");
		assertNull(v);
		p.setPropertyValue(path, "foobar", new DataValue("something"));
		v = p.getPropertyValue(path, "foobar");
		assertEquals(new DataValue("something"),v);
	}
	
	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getMethodNames(ObjectPath)}.
	 */
	@Test
	public final void testGetMethodNames(){
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];

		for(NamedElement element : provider[parent].getElements(null, null, null, false)){
			ObjectPath path = element.getObjectPath();
			try {
				List<String> names = p.getMethodNames(path);
				switch(element.getElementType()){
				case CLASS:
				case INSTANCE:
					switch(element.getLowerCaseName()){
					case "test_class":
						assertEquals(1,names.size());
						assertEquals("voidMethod",names.get(0));
						break;
					case "test_classc":
						assertEquals(1,names.size());
						assertEquals("loopBack",names.get(0));
						break;
					case "test_classb":
					default:
						assertEquals(0,names.size());
						break;
					}
					break;
				default:
					fail("Should not succeed");
					break;
				}
			} catch(ModelException ex){
				assertEquals(7,ex.getReason().getCode());	// NOT_SUPPORTED
			}
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getMethodReturnType(ObjectPath, String)}.
	 */
	@Test
	public final void testGetMethodReturnType(){
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		List<NamedElement> elements = provider[parent].getElements(null, null, null, false);
		
		for(NamedElement element : elements){
			ObjectPath path = element.getObjectPath();
			for(String methodName : new String[]{"unknown","loopback","voidmethod"}){
				try {
					DataType dt = p.getMethodReturnType(path, methodName);
					// System.out.println("Testing "+element.getName()+" for "+methodName+" "+dt);
					switch(element.getElementType()){
					case CLASS:
					case INSTANCE:
					case INTERFACE:
						switch(methodName){
						case "voidmethod":
							if("test_class".equals(element.getLowerCaseName()))
								assertEquals(DataType.VOID,dt);
							else 
								assertNull(dt);
							break;
						case "loopback":
							if("test_classc".equals(element.getLowerCaseName()))
								assertEquals(DataType.STRING,dt);
							else 
								assertNull(dt);
							break;
						case "unknown":
						default:
							assertNull(dt);
							break;
						}
						break;
					default:
						fail("Should not succeed");
						break;
					}
				} catch(ModelException ex){
					// System.out.println(ex);
					// NOT_SUPPORTED || METHOD_NOT_FOUND
					assertTrue(ex.getReason().getCode() == 7 || ex.getReason().getCode() == 17);	
				}
			}
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getMethodParameters(ObjectPath, String)}.
	 */
	@Test
	public final void testGetMethodParameters(){
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];

		for(NamedElement element : provider[parent].getElements(null, null, null, false)){
			ObjectPath path = element.getObjectPath();
			for(String methodName : new String[]{"unknown","loopback","voidmethod"}){
				try {
					List<CimParameter> params = p.getMethodParameters(path, methodName);
					// System.out.println(element.getName()+" "+methodName+" "+params);
					switch(element.getElementType()){
					case CLASS:
					case INSTANCE:
						switch(methodName){
						case "voidmethod":
							if("test_class".equals(element.getLowerCaseName()))
								assertEquals(0,params.size());
							else
								assertNull(params);
							break;
						case "loopback":
							if("test_classc".equals(element.getLowerCaseName())){
								assertEquals(1,params.size());
								assertEquals("input",params.get(0).getLowerCaseName());
								assertEquals(DataType.STRING,params.get(0).getDataType());
							} else {
								assertNull(params);
							}
							break;
						case "unknown":
							assertNull(params);
							break;
						default:
							fail("Unknown should not succed");
							break;
						}
						break;
					default:
						fail("Should not succeed");
						break;
					}
				} catch(ModelException ex){
					// System.out.println(ex);
					// NOT_SUPPORTED || METHOD_NOT_FOUND
					assertTrue(ex.getReason().getCode() == 7 || ex.getReason().getCode() == 17);	
				}
			}
		}
	}
	

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#invokeMethod(net.aifusion.metamodel.ObjectPath, java.lang.String, java.util.List)}.
	 */
	@Test
	public final void testInvokeMethod() {
		// TODO: Currently, methods will fail, because they are not yet bound. Fix this once java bindings to methods are fixed
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		try {
			HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
			keys.put("integerProperty", new DataValue(5));
			ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_classc",nameSpacePath[child2],keys, null);
			p.invokeMethod(path, "loopback", null);
			fail("Should not succeed");
		} catch (ModelException ex){
			assertEquals(16,ex.getReason().getCode()); // NOT_AVAILABLE
		}
		
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getRepository()}.
	 */
	@Test
	public final void testGetRepository() {
		Repository r = provider[parent].getRepository();
		assertEquals(repository[parent],r);
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#put(net.aifusion.metamodel.NamedElement)}.
	 */
	@Test
	public final void testPut() {
		String mof = "Enumeration test_enumd : string { enumvalue};\n";
		
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		
		// create a new element with a path to the child repository
		BufferedCache cache = new BufferedCache(repository[child2]);
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mof.getBytes()),nameSpacePath[child2]);
		List<NamedElement> elements = cache.getElements("enumeration",null,"test_enumd", false);
		assertEquals(1,elements.size());
		// System.out.println(elements.get(0).getObjectPath());
		// System.out.println(elements.get(0).toMOF());
		// insert using parent
		assertTrue(p.put(elements.get(0)));
		// ensure that the element was put into child2
		assertFalse(repository[parent].contains(elements.get(0).getObjectPath()));
		assertTrue(repository[child2].contains(elements.get(0).getObjectPath()));
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#get(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testGet() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];

		// get class in parent
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_class",nameSpacePath[parent],null, null);
		NamedElement element = p.get(path);
		assertNotNull(element);
		assertEquals(ElementType.CLASS,element.getElementType());

		// get a structure in child
		path = new ObjectPath(ElementType.STRUCTURE,"test_structc",nameSpacePath[child2],null, null);
		element = p.get(path);
		assertNotNull(element);
		assertEquals(ElementType.STRUCTURE,element.getElementType());
		assertEquals(path,element.getObjectPath());
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#contains(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testHasElement() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		// check an instance in parent
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(5));
		ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_class",nameSpacePath[parent],keys, null);
		assertTrue(p.contains(path));
		// check an instance in a child
		path = new ObjectPath(ElementType.INSTANCE,"test_classb",nameSpacePath[child1],keys, null);
		assertTrue(p.contains(path));
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#delete(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public final void testDelete() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		List<NamedElement> elements = p.getElements(null, null, null, false);
		assertEquals(12,elements.size());
		// delete an instance in parent
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(5));
		ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_class",nameSpacePath[parent],keys, null);
		assertTrue(p.delete(path));
		assertFalse(p.contains(path));
		// delete an instance in a child
		path = new ObjectPath(ElementType.INSTANCE,"test_classb",nameSpacePath[child1],keys, null);
		assertTrue(p.delete(path));
		assertFalse(p.contains(path));
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#shutdown()}.
	 */
	@Test
	public final void testShutdown() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		p.shutdown();
		List<NamedElement> elements = p.getElements(null, null, null, false);
		assertEquals(0,elements.size());
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getNameSpaces()}.
	 */
	@Test
	public final void testGetNameSpaces() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		List<NameSpacePath> paths = p.getNameSpaces();
		assertEquals(3,paths.size());
	}

	/**
	 * Test method for {@link net.aifusion.providers.BasicProvider#getElements(java.lang.String, java.lang.String, java.lang.String, boolean)}.
	 */
	@Test
	public final void testGetElements() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		List<NamedElement> elements = p.getElements(null, null, null, false);
		assertEquals(12,elements.size());
	}
	
	/**
	 * Test method for {@linkplain net.aifusion.providers.BasicProvider#getURI()}
	 */
	@Test
	public final void testGetURI() {
		try {
			Provider p = new BasicProvider(new InMemoryCache(),new URI("http://localhost:8080/endpoint"));
			assertNotNull(p);
			assertEquals(new URI("http://localhost:8080/endpoint"),p.getURI());
		} catch (URISyntaxException e) {
			fail("should not happen");
		}
	}
	
	/**
	 * Test to check if MOFParser places instances in the correct Provider
	 * Note that even though the parser uses the parent, all created instances reside in the provider
	 * containing the correct namespace
	 */
	@Test
	public final void testParser() {
		provider[parent].registerChildProvider(provider[child1]);
		provider[parent].registerChildProvider(provider[child2]);
		Provider p = provider[parent];
		
		String mofClass[] = {
				"#pragma namespace(\""+nameBase+"0"+"\")\n"+
				"instance of test_class {\n\tintegerProperty = 6;\n};\n",
				"#pragma namespace(\""+nameBase+"1"+"\")\n"+
				"instance of test_classb {\n\tintegerProperty = 7;\n\tfoobar = \"newString\";\n};\n",
				"#pragma namespace(\""+nameBase+"2"+"\")\n"+
				"instance of test_classc {\n\tintegerProperty = 8;\n};\n"
		};
		
		MOFParser parser = new MOFParser(p);
		for(String input : mofClass){
			parser.parse(new ByteArrayInputStream(input.getBytes()), null);
		}
		List<NamedElement> elements = p.getElements(null, null, null, false);
		assertEquals(15,elements.size());
		for(int i = 0; i < nProviders; i++){
			elements = repository[i].getElements("instance", null, null, false);
			assertEquals(2,elements.size());
			// for(NamedElement e : elements){
			//	System.out.println("["+i+"]: "+((CimInstance)e).getObjectPath());
			//	System.out.println(e.toMOF());
			//}
		}
		
	}
	

	private String eventResult = null;
	
	@Test
	public final void testEvents(){
		Provider p = provider[0];
		assertFalse(p.hasListener(CimEventType.ADDED, null));
		assertFalse(p.hasListener(CimEventType.REMOVED, null));
		p.addListener(CimEventType.ADDED, this);
		p.addListener(CimEventType.REMOVED, this);
		assertNull(eventResult);
		ObjectPath path = new ObjectPath(ElementType.ENUMERATION,"test_enum",new NameSpacePath("http","localhost:8080",nameBase+"0"), null, null);
		assertTrue(p.contains(path));
		NamedElement element = p.get(path);
		assertTrue(p.delete(path));
		assertEquals("CimIndication {\n\tType = \"REMOVED\";\n\tDescription = \"http://localhost:8080/enumeration/root/ns0:test_enum\";\n};\n",eventResult);
		eventResult = null;
		p.put(element);
		assertEquals("CimIndication {\n\tType = \"ADDED\";\n\tDescription = \"Enumeration test_enum : String {\\n\\tenumvalue\\n};\\n\";\n};\n",eventResult);
		p.removeListener(CimEventType.ADDED, this);
		p.removeListener(CimEventType.REMOVED, this);
		assertFalse(p.hasListener(CimEventType.ADDED, null));
		assertFalse(p.hasListener(CimEventType.REMOVED, null));
	}
	
	@Override
	public void notify(CimEvent event) {
		eventResult = event.toString();
		if(verbose) System.out.println(eventResult);
		return;
	}

	@Override
	public URL getURL() {
		// TODO Auto-generated method stub
		return null;
	}

}
