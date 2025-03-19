/**
 * Copyright 2017 Sharad Singhal, All Rights Reserved
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
 * Created Jul 16, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimEventType;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimStructure;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelUtilities;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.providers.BasicProvider;
import net.aifusion.providers.Provider;
import net.aifusion.utils.Java2Cim;

/**
 * Class to test CimHandler
 * @author Sharad Singhal
 */
public class CimHandlerTest {
	private static boolean verbose = false;
	private static String CRLF = "\r\n";
	private static String mof = "#pragma namespace (\"/root/local\")\n"+
			"Enumeration test_enum : string { enumvalue};\n"+
			"Enumeration test_enumb : string { enumvalue};\n"+
			"Structure test_struct { test_enum v;\n string foobar; };\n"+
			"Structure test_structb { test_enumb v;\n string foobar; };\n"+
			"class test_class {\n\t[key]\n\tSint32 integerProperty;\n\tString stringMethod(Sint32 intParam);\n};\n"+
			"class test_classb {\n\t[key]\n\tSint32 integerProperty;\n\t[write]String stringProperty;\n};\n"+
			"instance of test_class {\n\tintegerProperty = 5;\n};\n"+
			"instance of test_classb {\n\tintegerProperty = 5;\n\tstringProperty = \"foo\";\n};\n"+
			
			"#pragma namespace (\"/net/aifusion\")\n"+
			"class test_classc {\n\t[key]\n\tSint32 integerProperty;\n};\n";

	private static InMemoryCache configCache = new InMemoryCache();
	private static String testRepository = "resources/test";
	private static ObjectPath serverConfigPath = new ObjectPath("http://localhost:8085/structurevalue/aifusion:aifusion_httpconfiguration.Id=\"serverConfig\"");
	private static ObjectPath clientConfigPath = new ObjectPath("http://localhost:8080/structurevalue/aifusion:aifusion_httpconfiguration.Id=\"clientConfig\"");
	private static String serverConfigMof = "value of aifusion_httpconfiguration {\n"+
			"Id = \"serverConfig\";\n"+
			"ServerPort = 8085;"+
			"Repository = \""+testRepository+"/server\";\n"+
			"};\n"+
			"value of aifusion_httpconfiguration {\n"+
			"Id = \"clientConfig\";\n"+
			"ServerPort = 8080;"+
			"Repository = \""+testRepository+"/client\";\n"+
			"};\n";
	
	// name space for test_classc in the server repository
	private static NameSpacePath ns1 = new NameSpacePath("http://localhost:8085/net/aifusion");
	// name space for other server-side elements in the server repository
	private static NameSpacePath ns2 = new NameSpacePath("http://localhost:8085/root/local");
	private static URI serverURI, clientURI;
	
	// client side server to handle event  listeners
	private static CimServer clientServer;
	
	
	private TestHandler serverHandler;
	private Provider serverProvider,clientProvider;
	private BufferedReader header;
	private BufferedInputStream input;
	private ByteArrayOutputStream output;
	private String bodyString;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("CimHandler ");
		deleteFiles(testRepository);
		// Create server configuration for the handler
		CimStructure configClass = (CimStructure) Java2Cim.getModelForClass(HttpConfiguration.class, configCache);
		assertNotNull(configClass);
		assertTrue(configCache.contains(configClass.getObjectPath()));
		if(verbose) System.out.println(configClass.getObjectPath().toURL()+"\n"+configClass.toMOF());
		MOFParser parser = new MOFParser(configCache);
		ByteArrayInputStream in = new ByteArrayInputStream(serverConfigMof.getBytes());
		parser.parse(in, Constants.defaultNameSpacePath);
		in.close();
		assertTrue(configCache.contains(serverConfigPath));
		assertTrue(configCache.contains(clientConfigPath));
		if(verbose) {
			System.out.println(configCache.get(serverConfigPath).toMOF());
			System.out.println(configCache.get(clientConfigPath).toMOF());
		}
		
		HttpConfiguration clientConfig = new HttpConfiguration((StructureValue) configCache.get(clientConfigPath));
		clientServer = new CimServer(clientConfig);
		clientServer.startServer();
		return;
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		if(clientServer != null) clientServer.stopServer();
		deleteFiles(testRepository);
		System.out.println("done.");
	}
	
	/**
	 * Delete a file or directory. If the input is a directory, all files and
	 * directories within that directory are deleted.
	 * @param fileOrDirectory - name of file or directory to delete
	 */
	private static void deleteFiles(String fileOrDirectory){
		File file = new File(fileOrDirectory);
		if(!file.exists()) return;
		if(file.isDirectory()){
			String [] names = file.list();
			if(names != null && names.length > 0){
				for(String n : names){
					String fName = fileOrDirectory+"/"+n;
					deleteFiles(fName);
				}
			}
		}
		if(file.exists() && !file.delete()){
			fail("Unable to delete file "+fileOrDirectory);
		}
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");

		// create server-side configuration
		StructureValue sv = (StructureValue) configCache.get(serverConfigPath);
		assertNotNull(sv);
		HttpConfiguration config = new HttpConfiguration(sv);
		assertNotNull(config);
		
		// clean out the server-side cache, and reinsert all definitions
		PersistentCache serverCache = new PersistentCache(config.getRepository());
		for(NamedElement e : serverCache.getElements(null, null, null, false)) {
			serverCache.delete(e.getObjectPath());
		}
		MOFParser parser = new MOFParser(serverCache);
		parser.parse(new ByteArrayInputStream(mof.getBytes()), null);
		List<NamedElement> elements = serverCache.getElements(null, null, null, false);
		assertEquals(9,elements.size());
		
		// create the handler with the provider pointing to the server cache
		// we can use the serverProvider to check server-side data
		serverHandler = new TestHandler(config);		
		assertNotNull(serverHandler);
		serverProvider = serverHandler.getProvider("/");
		
		
		// client side provider is a basic provider
		clientURI = new URI("http://localhost:8080/");
		clientProvider = new BasicProvider(new InMemoryCache(),clientURI);
		assertNotNull(clientProvider);
		output = new ByteArrayOutputStream(16*1024);
		assertNotNull(output);
		return;
	}

	@After
	public void tearDown() throws Exception {
		if(serverHandler != null) serverHandler.shutdown();
		if(clientProvider != null) clientProvider.shutdown();
		if(header != null) header.close();
		if(input != null) input.close();
		if(output != null) output.close();
		System.out.print(".");
		return;
	}

	/**
	 * Test for getNameSpaces()
	 */
	@Test
	public void testGetNameSpaces() {
		CimHeader h = CimHeader.GET_NAMESPACES;
		HttpRequest request = getRequest("/", h, null, null, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("http://127.0.0.1:8085/root/local\r\n"
				+ "http://127.0.0.1:8085/net/aifusion\r\n",bodyString);
	}

	@Test
	public void testGetElement() {
		// get an existing element
		CimHeader h = CimHeader.GET_ELEMENT;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_classc",ns1,null, null);
		HttpRequest request = getRequest("/", h, path, null, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("#pragma namespace(\"/net/aifusion\")\nClass test_classc {\n	[Key]\n	SInt32 integerProperty;\n};\n",bodyString);

		// get a non-existent element
		path = new ObjectPath(ElementType.CLASS,"test_classx",ns1,null, null);
		request = getRequest("/", h, path, null, null);
		assertNotNull(request);
		response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 404");
		assertEquals("",bodyString);
		return;	
	}

	@Test
	public void testHasElement() {
		// check for an existing element
		CimHeader h = CimHeader.HAS_ELEMENT;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_classc",ns1,null, null);
		HttpRequest request = getRequest("/", h, path, null, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("",bodyString);

		// check for a non-existent element
		path = new ObjectPath(ElementType.CLASS,"test_classx",ns1,null, null);
		request = getRequest("/", h, path, null, null);
		assertNotNull(request);
		response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 404");
		assertEquals("",bodyString);
		return;
	}

	@Test
	public void testInvokeMethod() {
		// TODO: Add a bound class to test_class so we can get a successful return as well.
		CimHeader h = CimHeader.INVOKE_METHOD;
		ObjectPath path = new ObjectPath("http://localhost/root/local:test_class.integerproperty=5");
		HttpRequest request = getRequest("/", h, path, new String[]{"stringMethod"}, "10");
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 404");	// method not available...
		assertEquals("16 METHOD_NOT_AVAILABLE [The extrinsic method cannot be invoked because it is not available]: stringMethod is not available because it is not bound\r\n",bodyString);
	}

	@Test
	public void testUnregisterProvider() {
		CimHeader h = CimHeader.UNREGISTER_PROVIDER;
		HttpRequest request = getRequest("/", h, null, new String[]{clientURI.toString()}, null);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
//		validate(response,"HTTP/1.1 501");
		// CimHandler will return success as long as it can construct a cim client to talk back to us
		validate(response,null);
	}

	
	@Test
	public void testRegisterProvider() {
		CimHeader h = CimHeader.REGISTER_PROVIDER;
		HttpRequest request = getRequest("/",h,null,new String[]{clientURI.toString()}, null);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
	}

	@Test
	public void testRemoveListener() {
		CimHeader h = CimHeader.REMOVE_LISTENER;
		HttpRequest request = getRequest("/",h,null,new String[]{CimEventType.CREATED.toString(),clientURI.toString()}, null);
		HttpResponse response = serverHandler.serve(request);
		validate(response,null);
	}

	@Test
	public void testHasListener() {
		CimHeader h = CimHeader.HAS_LISTENER;
		HttpRequest request = getRequest("/", h, null, new String[]{CimEventType.CREATED.toString(),clientURI.toString()}, null);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,"");
	}

	@Test
	public void testAddListener() {
		assertFalse(serverProvider.hasListener(CimEventType.CREATED, null));
		CimHeader h = CimHeader.ADD_LISTENER;
		HttpRequest request = getRequest("/",h,null,new String[]{CimEventType.CREATED.toString(),clientURI.toString()}, null);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		// assertTrue(provider.hasListener(CimEventType.CREATED, null));
	}

	@Test
	public void testGetElements() {
		CimHeader h = CimHeader.GET_ELEMENTS;
		HttpRequest request = getRequest("/", h, null, new String[]{"null","null","null","false"}, null);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		// TODO: Check returned values from the query
	}

	@Test
	public void testExecuteQuery() {
		CimHeader h = CimHeader.EXECUTE_QUERY;
		String queryString = "Select * from test_class";
		HttpRequest request = getRequest("/", h, null, null, queryString);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("#pragma namespace(\"/root/local\")\ninstance of test_class {\n\tintegerProperty = 5;\n};\r\n",bodyString);
	}

	/**
	 * Test for delete()
	 */
	@Test
	public void testDeleteElement() {
		CimHeader h = CimHeader.DELETE_ELEMENT;
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(Integer.valueOf(5)));
		ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_classb",ns2,keys, null);
		HttpRequest request = getRequest("/",h,path,null, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertFalse(serverProvider.contains(path));
	}

	/**
	 * Test for put()
	 */
	@Test
	public void testPutElement() {
		CimHeader h = CimHeader.PUT_ELEMENT;
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(new Integer(7)));
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_classb",ns2,null, null);
		CimClass template = (CimClass) serverProvider.get(path);
		assertNotNull(template);
		keys.put("stringProperty",new DataValue("zap"));
		CimInstance instance = CimInstance.createInstance(template, keys, null);
		assertNotNull(instance);

		HttpRequest request = getRequest("/",h,instance.getObjectPath(),new String[]{instance.getObjectPath().getLocalPath()}, instance.toMOF());
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);

		validate(response,"HTTP/1.1 201");
		NamedElement element = serverProvider.get(instance.getObjectPath());
		assertNotNull(element);
		assertEquals(instance,element);
	}

	/**
	 * Test for setPropertyValue()
	 */
	@Test
	public void testSetPropertyValue() {
		CimHeader h = CimHeader.SET_PROPERTY_VALUE;
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(new Integer(5)));	// key value
		ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_classb",ns2,keys, null);
		if(verbose) System.out.println(path.toURL());
		HttpRequest request = getRequest("/", h, path, new String[]{"stringProperty","STRING"}, ModelUtilities.quote("bar"));
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		DataValue v = serverProvider.getPropertyValue(path, "stringProperty");
		assertNotNull(v);
		assertEquals("\"bar\"",v.toMOF());

		// TODO: test complex properties and array properties here

		return;
	}

	/**
	 * Test for getMethodParameters()
	 */
	@Test
	public void testGetMethodParameters() {
		CimHeader h = CimHeader.GET_METHOD_PARAMETERS;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_class",ns2,null, null);
		HttpRequest request = getRequest("/",h,path,new String[]{"stringMethod"}, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("SInt32 intParam\r\n\r\n",bodyString);
		return;
	}

	/**
	 * Test for GetMethodType()
	 */
	@Test
	public void testGetMethodType() {
		CimHeader h = CimHeader.GET_METHOD_TYPE;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_class",ns2,null, null);
		HttpRequest request = getRequest("/",h,path,new String[]{"stringMethod"}, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("STRING\r\n",bodyString);
	}

	/**
	 * Test for getMethodNames()
	 */
	@Test
	public void testGetMethodNames() {
		CimHeader h = CimHeader.GET_METHOD_NAMES;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_class",ns2,null, null);
		HttpRequest request = getRequest("/",h,path,null, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("stringMethod\r\n",bodyString);
	}

	/**
	 * Test for getPropertyValue()
	 */
	@Test
	public void testGetPropertyValue() {
		CimHeader h = CimHeader.GET_PROPERTY_VALUE;
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("integerProperty", new DataValue(new Integer(5)));
		ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_classb",ns2,keys, null);
		HttpRequest request = getRequest("/",h,path,new String[]{"stringProperty"}, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("\"foo\"",bodyString);

		// TODO: Check complex property value
	}

	/**
	 * Test for getPropertyType()
	 */
	@Test
	public void testGetPropertyType() {
		CimHeader h = CimHeader.GET_PROPERTY_TYPE;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_classc",ns1,null, null);
		HttpRequest request = getRequest("/",h,path,new String[]{"integerProperty"}, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("SINT32\r\n",bodyString);
	}

	/**
	 * Test for getProperyNames()
	 */
	@Test
	public void testGetPropertyNames() {
		CimHeader h = CimHeader.GET_PROPERTY_NAMES;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_classc",ns1,null, null);
		HttpRequest request = getRequest("/",h,path,null, null);
		assertNotNull(request);
		HttpResponse response = serverHandler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("integerProperty\r\n",bodyString);
	}



	/**
	 * Validate the response
	 * @param response - HTTP response to validate
	 * @param expect - expected HTTP Status value. An "HTTP/1.1 200 OK" is used if expect is null
	 */
	private void validate(HttpResponse response,String expect) {
		if(expect == null) expect = "HTTP/1.1 200";
		output.reset();
		response.send(output);
		String outputString = output.toString();
		System.out.println(outputString);
		assertTrue(outputString.startsWith(expect));
		bodyString = outputString.substring(outputString.indexOf("\r\n\r\n")+4);
		return;
	}

	/**
	 * Construct a request with an HttpHeader and input body
	 * @param endpoint - server-side  endpoint to use
	 * @param h - CimHeader to use
	 * @param path - object path (if any)
	 * @param extensions - extended header values (in order defined in CimHeader)
	 * @param inputBody - input body (if any)
	 * @return CimRequest
	 */
	private HttpRequest getRequest(String endpoint, CimHeader h, ObjectPath path, String [] extensions, String inputBody){
		String host = ns1.getAuthority();
		// get the query, if any to add to the endpoint
		// TODO: Check if we need the query on the request line-- it will be part of the object_path extension header in any case
		if(path != null){
			URL url = path.toURL();
			host = url.getAuthority();
			String query = url.getQuery();
			if(query != null && !query.isEmpty()) endpoint = endpoint+"?"+query;
		}
		StringBuilder sb = new StringBuilder();
		sb.append(h.getHttpMethod()).append(" ").append(endpoint).append(" ");
		sb.append("HTTP/1.1").append(CRLF);
		sb.append(HttpHeader.HOST).append(": ").append(host).append(CRLF);
		sb.append(HttpHeader.ACCEPT).append(": ").append(MimeType.MOF.getType()).append(", ").append(MimeType.PLAINTEXT.getType()).append(CRLF);
		sb.append(HttpHeader.CONNECTION).append(": close").append(CRLF);
		sb.append(CimXHeader.INTRINSIC).append(": ").append(h).append(CRLF);
		// User-Agent should be of form Name '/' version
		sb.append(HttpHeader.USER_AGENT).append(": ").append("AIFusion/1.0").append(CRLF);
		List<CimXHeader> xheaders = CimXHeader.getXHeaders(h);
		if(xheaders.contains(CimXHeader.OBJECT_PATH)) {
			sb.append(CimXHeader.OBJECT_PATH.toString()).append(": ").append(path.toString()).append(CRLF);
			assertTrue(xheaders.remove(CimXHeader.OBJECT_PATH));
		}
		if(xheaders != null && xheaders.size() > 0){
			assertNotNull(extensions);
			if(verbose) {
				if(xheaders.size() != extensions.length){
					for(CimXHeader x : xheaders){
						System.out.println(x);
					}
					for(String v : extensions){
						System.out.println(v);
					}
				}
			}
			assertEquals(xheaders.size(),extensions.length);
			for(int i = 0; i < xheaders.size(); i++){
				if(extensions[i] != null) sb.append(xheaders.get(i)).append(": ").append(extensions[i]).append(CRLF);
			}
		}
		byte [] buffer = inputBody != null ? inputBody.getBytes() : new byte[0];
		if(inputBody != null){
			assertTrue(h.getHttpMethod().equals(HttpMethod.PUT) || h.getHttpMethod().equals(HttpMethod.POST));
			sb.append(HttpHeader.CONTENT_LENGTH).append(": ").append(buffer.length).append(CRLF);
		}
		sb.append(CRLF);
//		if(verbose) 
			System.out.println(sb);
		header = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(sb.toString().getBytes())));
		input = new BufferedInputStream(new ByteArrayInputStream(buffer));
		return new HttpRequest(header,input);
	}
}
