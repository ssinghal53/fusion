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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
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
import net.aifusion.metamodel.StructureValue;
import net.aifusion.providers.BasicProvider;
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
	private static CimServer server;
	private static String serverMof = "value of aifusion_httpconfiguration {\n"+
			"MaxSessions = 0;\n"+
			"ServerTimeout = 5000;\n"+
			"RequestHandler = \"net.aifusion.cimserver.TestHandler\";\n"+
			"Secure = false;\n"+
			"Id = \"serverConfig\";\n"+
			"HostName = \"localhost\";\n"+
			"ServerPort = 8085;\n"+
			"};\n";
	private static InMemoryCache serverCache = new InMemoryCache();
	private static NameSpacePath ns1 = new NameSpacePath("http://localhost:8085/net/aifusion");
	private static NameSpacePath ns2 = new NameSpacePath("http://localhost:8085/root/local");
	private CimHandler handler;
	private BasicProvider provider,clientProvider;
	private BufferedReader header;
	private BufferedInputStream input;
	private ByteArrayOutputStream output;
	private String bodyString;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("CimHandler ");
		// create a server-side cache, and server configuration
		CimStructure configClass = (CimStructure) Java2Cim.getModelForClass(HttpConfiguration.class, serverCache);
		assertNotNull(configClass);
		assertTrue(serverCache.contains(configClass.getObjectPath()));
		// System.out.println(configClass.getObjectPath().toURL()+"\n"+configClass.toMOF());
		MOFParser parser = new MOFParser(serverCache);
		ByteArrayInputStream in = new ByteArrayInputStream(serverMof.getBytes());
		parser.parse(in, Constants.defaultNameSpacePath);
		in.close();
		StructureValue conf = (StructureValue) serverCache.get(new ObjectPath("http://localhost:8085/structurevalue/aifusion:aifusion_httpconfiguration.Id=\"serverConfig\""));
		assertNotNull(conf);
		// System.out.println(conf.toMOF());
		server = new CimServer(new HttpConfiguration(conf));
		server.startServer();
		return;
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		if(server != null) server.stopServer();
		System.out.println("done.");
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		InMemoryCache cache = new InMemoryCache();
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mof.getBytes()), null);
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		assertEquals(9,elements.size());
		provider = new BasicProvider(cache);
		assertNotNull(provider);
		handler = new CimHandler(provider);		
		assertNotNull(handler);
		clientProvider = new BasicProvider(new InMemoryCache());
		assertNotNull(clientProvider);
		output = new ByteArrayOutputStream(16*1024);
		assertNotNull(output);
		return;
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		if(handler != null) handler.shutdown();
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
		HttpRequest request = getRequest(h, null, null, null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("/root/local\r\n/net/aifusion\r\n",bodyString);
	}

	@Test
	public void testGetElement() {
		// get an existing element
		CimHeader h = CimHeader.GET_ELEMENT;
		ObjectPath path = new ObjectPath(ElementType.CLASS,"test_classc",ns1,null, null);
		HttpRequest request = getRequest(h, path, null, null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("#pragma namespace(\"/net/aifusion\")\nClass test_classc {\n	[Key]\n	SInt32 integerProperty;\n};\n",bodyString);

		// get a non-existent element
		path = new ObjectPath(ElementType.CLASS,"test_classx",ns1,null, null);
		request = getRequest(h, path, null, null);
		assertNotNull(request);
		response = handler.serve(request);
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
		HttpRequest request = getRequest(h, path, null, null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertEquals("",bodyString);

		// check for a non-existent element
		path = new ObjectPath(ElementType.CLASS,"test_classx",ns1,null, null);
		request = getRequest(h, path, null, null);
		assertNotNull(request);
		response = handler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 404");
		assertEquals("",bodyString);
		return;
	}

	@Test
	public void testInvokeMethod() {
		CimHeader h = CimHeader.INVOKE_METHOD;
		ObjectPath path = new ObjectPath("http://localhost/root/local:test_class.integerproperty=5");
		HttpRequest request = getRequest(h, path, new String[]{"stringMethod"}, "10");
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 404");	// method not available...
		assertEquals("16 METHOD_NOT_AVAILABLE [The extrinsic method cannot be invoked because it is not available]: stringMethod is not available because it is not bound\r\n",bodyString);
	}

	@Test
	public void testUnregisterProvider() {
		CimHeader h = CimHeader.UNREGISTER_PROVIDER;
		HttpRequest request = getRequest(h, null, new String[]{"http://localhost:8085/"}, null);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 501");	// TestHandler does not implement unregisterProvider
	}

	@Test
	public void testRegisterProvider() {
		CimHeader h = CimHeader.REGISTER_PROVIDER;
		HttpRequest request = getRequest(h,null,new String[]{"http://localhost:8085/"},null);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 501");
	}

	@Test
	public void testRemoveListener() {
		CimHeader h = CimHeader.REMOVE_LISTENER;
		HttpRequest request = getRequest(h,null,new String[]{CimEventType.CREATED.toString(),"http://localhost:8080/"},null);
		HttpResponse response = handler.serve(request);
		validate(response,"HTTP/1.1 501");
	}

	@Test
	public void testHasListener() {
		CimHeader h = CimHeader.HAS_LISTENER;
		HttpRequest request = getRequest(h, null, new String[]{CimEventType.CREATED.toString(),"http://localhost:8080/"}, null);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 501");
	}

	@Test
	public void testAddListener() {
		assertFalse(provider.hasListener(CimEventType.CREATED, null));
		CimHeader h = CimHeader.ADD_LISTENER;
		HttpRequest request = getRequest(h,null,new String[]{CimEventType.CREATED.toString(),"http://localhost:8080/"},null);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,"HTTP/1.1 501");
		// assertTrue(provider.hasListener(CimEventType.CREATED, null));
	}

	@Test
	public void testGetElements() {
		CimHeader h = CimHeader.GET_ELEMENTS;
		HttpRequest request = getRequest(h, null, new String[]{"null","null","null","false"}, null);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,null);
		// TODO: Check returned values from the query
	}

	@Test
	public void testExecuteQuery() {
		CimHeader h = CimHeader.EXECUTE_QUERY;
		String queryString = "Select * from test_class";
		HttpRequest request = getRequest(h, null, null, queryString);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
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
		keys.put("integerProperty", new DataValue(new Integer(5)));
		ObjectPath path = new ObjectPath(ElementType.INSTANCE,"test_classb",ns2,keys, null);
		HttpRequest request = getRequest(h,path,null,null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,null);
		assertFalse(provider.contains(path));
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
		CimClass template = (CimClass) provider.get(path);
		assertNotNull(template);
		keys.put("stringProperty",new DataValue("zap"));
		CimInstance instance = CimInstance.createInstance(template, keys, null);
		assertNotNull(instance);

		HttpRequest request = getRequest(h,instance.getObjectPath(),new String[]{instance.getObjectPath().getLocalPath()},instance.toMOF());
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);

		validate(response,"HTTP/1.1 201");
		NamedElement element = provider.get(instance.getObjectPath());
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
		HttpRequest request = getRequest(h, path, new String[]{"stringProperty","STRING"}, ModelUtilities.quote("bar"));
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
		assertNotNull(response);
		validate(response,null);
		DataValue v = provider.getPropertyValue(path, "stringProperty");
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
		HttpRequest request = getRequest(h,path,new String[]{"stringMethod"},null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
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
		HttpRequest request = getRequest(h,path,new String[]{"stringMethod"},null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
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
		HttpRequest request = getRequest(h,path,null,null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
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
		HttpRequest request = getRequest(h,path,new String[]{"stringProperty"},null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
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
		HttpRequest request = getRequest(h,path,new String[]{"integerProperty"},null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
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
		HttpRequest request = getRequest(h,path,null,null);
		assertNotNull(request);
		HttpResponse response = handler.serve(request);
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
		if(verbose) System.out.println(outputString);
		assertTrue(outputString.startsWith(expect));
		bodyString = outputString.substring(outputString.indexOf("\r\n\r\n")+4);
		return;
	}

	/**
	 * Construct a request with an HttpHeader and input body
	 * @param h - CimHeader to use
	 * @param path - object path (if any)
	 * @param extensions - extended header values (in order required by CimHeader)
	 * @param inputBody - input body (if any)
	 * @return CimRequest
	 */
	private HttpRequest getRequest(CimHeader h, ObjectPath path, String [] extensions, String inputBody){
		String localPath = "/";
		String host = ns1.getAuthority();
		if(path != null){
			URL url = path.toURL();
			localPath = url.getPath();
			host = url.getAuthority();
			String query = url.getQuery();
			if(query != null && !query.isEmpty()) localPath = localPath+"?"+query;
		}
		StringBuilder sb = new StringBuilder();
		sb.append(h.getHttpMethod()).append(" ").append(localPath).append(" ");
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
		if(verbose) System.out.println(sb);
		header = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(sb.toString().getBytes())));
		input = new BufferedInputStream(new ByteArrayInputStream(buffer));
		return new HttpRequest(header,input);
	}
}
