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
 * Created Aug 25, 2017 by sharad
 */
package net.aifusion.cimserver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.cql.CimQuery;
import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimEventType;
import net.aifusion.metamodel.CimIndication;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.utils.Java2Cim;

/**
 * Class to test CimServer and CimClient
 * @author Sharad Singhal
 */
public class CimClientAndServerTest {
	private static boolean verbose = false;
	private static String repositoryLocation = "testrepository";
	private static CimServer server;
	private static CimServer server2;

	private static String serverMof = "instance of aifusion_httpconfiguration {\n"+
			"KeyStorePassword = \"serverpass\";\n"+
			"MaxSessions = 0;\n"+
			"TrustStore = \"testrepository/serverTrustStore.jks\";\n"+
			"KeyStore = \"testrepository/serverKeyStore.jks\";\n"+
			"ServerTimeout = 5000;\n"+
			"Repository = \"testrepository\";\n"+
			"RequestHandler = \"CimHandler\";\n"+
			"Secure = false;\n"+
			"X500Principal = \"CN=localhost, OU=cimfusion.com, O=cimfusion, C=US, L=Belmont, ST=California\";\n"+
			"Id = \"serverConfig\";\n"+
			"TrustStorePassword = \"serverpass\";\n"+
			"HostName = \"localhost\";\n"+
			"ServerPort = 8085;\n"+
			"};\n"+

			"instance of aifusion_httpconfiguration {\n"+
			// "TrustStore = \"testrepository/clientTrustStore.jks\";\n"+
			// "TrustStorePassword = \"clientpass\";\n"+
			// "KeyStore = \"testrepository/clientKeyStore.jks\";\n"+
			// "KeyStorePassword = \"clientpass\";\n"+
			"Secure = false;\n"+
			"X500Principal = \"CN=localclient, OU=cimfusion.com, O=cimfusion, C=US, L=Belmont, ST=California\";\n"+
			"Id = \"clientConfig\";\n"+
			"RequestHandler = \"net.aifusion.cimserver.TestHandler\";\n"+
			"HostName = \"localhost\";\n"+
			"ServerPort = 8089;\n"+
			"};\n"+

			"#pragma namespace (\"/root/local\")\n"+
			"Enumeration test_enum : string { enumvalue};\n"+
			"Structure test_struct { test_enum v;\n string foobar; };\n"+
			"class test_class {\n\t[key]\n\tSint32 integerProperty;\n\t[write]String stringProperty;\n\tboolean reset(String propertyName);\n};\n"+
			"instance of test_class {\n\tintegerProperty = 5;\n\tstringProperty = null;\n};\n"
			;
	private static MOFParser parser;
	private static URL serverURL, server2URL;
	private static HttpConfiguration clientConfig;
	
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("CimClientAndServer ");
		// create the server-side cache, and server configuration
		deleteFiles(repositoryLocation);	// clean up from prior tests
		PersistentCache cache = new PersistentCache(repositoryLocation);	// create a new cache

		CimClass configClass = (CimClass) Java2Cim.getModelForClass(HttpConfiguration.class, cache);
		assertNotNull(configClass);
		assertTrue(cache.contains(configClass.getObjectPath()));

		// create instances at the server.
		parser = new MOFParser(cache);
		ByteArrayInputStream in = new ByteArrayInputStream(serverMof.getBytes());
		parser.parse(in, Constants.defaultNameSpacePath);
		in.close();
		List<NamedElement> elements = cache.getElements(null, null, null, false);
		assertEquals(7,elements.size());
		cache.shutdown();

		// create the server configuration, and start the server
		HttpConfiguration serverConfig = HttpConfiguration.getConfiguration("serverConfig", null, repositoryLocation);
		assertFalse(serverConfig.isSecure());
		assertEquals("localhost",serverConfig.getHostName());
		assertEquals(8085,serverConfig.getServerPort());
		serverURL = new URL("http://localhost:8085/");
		server = new CimServer(serverConfig);
		server.startServer();

		// get the client configuration, and the server proxy for the client (needed for [un]RegisterProvider)
		clientConfig = HttpConfiguration.getConfiguration("clientConfig", null, repositoryLocation);
		assertNotNull(clientConfig);
		assertFalse(clientConfig.isSecure());
		assertEquals("localhost",clientConfig.getHostName());
		assertEquals(8089,clientConfig.getServerPort());
		server2URL = new URL("http://localhost:8089/");
		server2 = new CimServer(clientConfig);
		server2.startServer();
		return;

	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
		if(server != null) server.stopServer();
		if(server2 != null) server2.stopServer();
		// comment the following line to retain content of server-side repository after the tests complete if needed for debugging
		deleteFiles(repositoryLocation);
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		PersistentCache cache = new PersistentCache(repositoryLocation);
		ObjectPath path = new ObjectPath("/local:new_class");
		if(cache.contains(path)){
			assertTrue(cache.delete(path));
		}
		cache.shutdown();
	}

	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
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

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#CimClient(java.net.URL, net.aifusion.cimserver.HttpConfiguration)}.
	 */
	@Test
	public void testCimClient() {
		CimClient client1 = new CimClient(serverURL,clientConfig);
		assertNotNull(client1);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#hashCode()}.
	 * Test method for {@link net.aifusion.cimserver.CimClient#equals(java.lang.Object)}.
	 */
	@Test
	public void testEquals() {
		CimClient client1 = new CimClient(serverURL,clientConfig);
		CimClient client2 = new CimClient(serverURL,clientConfig);
		assertEquals(client1.hashCode(),client2.hashCode());
		assertEquals(client1,client2);
		try {
			client2 = new CimClient(new URL("http://localhost:8086/"), clientConfig);
			assertNotEquals(client1,client2);
			assertNotEquals(client1.hashCode(),client2.hashCode());
		} catch(MalformedURLException e){
			fail(e.toString());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getURL()}.
	 */
	@Test
	public void testGetProviderURL(){
		CimClient client = new CimClient(serverURL,clientConfig);
		assertEquals("http://localhost:8089/",client.getURL().toString());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getNameSpaces()}.
	 */
	@Test
	public void testGetNameSpaces() {
		CimClient client = new CimClient(serverURL,clientConfig);
		List<NameSpacePath> paths = client.getNameSpaces();
		assertNotNull(paths);
		assertEquals(2,paths.size());
		assertTrue(paths.contains(new NameSpacePath("http://localhost:8085/root/local")));
		assertTrue(paths.contains(new NameSpacePath("http://localhost:8085/aifusion")));
		client.shutdown();
		client = new CimClient(server2URL,clientConfig);	// test server 2 name space
		paths = client.getNameSpaces();
		assertNotNull(paths);
		assertEquals(1,paths.size());
		// note that "/localPath" is hardcoded in TestHandler.java
		assertEquals("http://localhost:8089/localpath",paths.get(0).toString());	
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#get(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public void testGet() {
		// check for an existing element without host name
		CimClient client = new CimClient(serverURL,clientConfig);
		ObjectPath path = new ObjectPath("/aifusion:aifusion_httpconfiguration.id=\"clientConfig\"");
		PersistentCache cache = new PersistentCache(repositoryLocation);
		NamedElement expected = cache.get(path);
		assertNotNull(expected);
		NamedElement retrieved = client.get(path);
		assertNotNull(retrieved);
		// System.out.println("Expect "+expected.toMOF());
		// System.out.println("Retrieved "+retrieved.toMOF());
		assertEquals(expected.toMOF(),retrieved.toMOF());
		// check for a non-existent element
		path = new ObjectPath("/aifusion:aifusion_httpconfiguration.id=\"otherConfig\"");
		assertFalse(cache.contains(path));
		retrieved = client.get(path);
		assertNull(retrieved);
		cache.shutdown();
		client.shutdown();

	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#contains(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public void testContains() {
		// check for an existing element without host name
		CimClient client = new CimClient(serverURL,clientConfig);
		assertNotNull(client);
		ObjectPath path = new ObjectPath("/aifusion:aifusion_httpconfiguration.id=\"clientConfig\"");
		assertTrue(client.contains(path));
		// check for an existing element with the host name
		path = new ObjectPath("http://localhost:8085/aifusion:aifusion_httpconfiguration.id=\"clientConfig\"");
		assertTrue(client.contains(path));
		// check for a non-existent element
		path = new ObjectPath("/aifusion:aifusion_httpconfiguration.id=\"otherConfig\"");
		assertFalse(client.contains(path));
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#put(net.aifusion.metamodel.NamedElement)}.
	 */
	@Test
	public void testPut() {
		// create an element to put
		InMemoryCache localCache = new InMemoryCache();
		MOFParser parser = new MOFParser(localCache);
		BufferedReader reader = new BufferedReader(new StringReader("Class new_class {[key]String foobar;};"));
		parser.parse(reader, new NameSpacePath("/local"));
		ObjectPath p = new ObjectPath("/local:new_class");
		NamedElement e = localCache.get(p);
		localCache.shutdown();
		assertNotNull(e);
		// System.out.println(e.getObjectPath()+"\n"+e.getNameSpacePath()+"\n"+e.toMOF());
		// put it in the server
		CimClient client = new CimClient(serverURL,clientConfig);
		assertTrue(client.put(e));
		// validate that it exists at the server
		PersistentCache cache = new PersistentCache(repositoryLocation);
		assertTrue(cache.contains(p));
		cache.shutdown();
		// delete it from the server
		assertTrue(client.delete(p));
		client.shutdown();
		// validate that it no longer exists at the server
		cache = new PersistentCache(repositoryLocation);
		assertFalse(cache.contains(p));
		cache.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#delete(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public void testDelete() {
		// note that we have to shut down the persistent cache each time because it caches classes in-memory
		// add a new class to the server repository 
		PersistentCache cache = new PersistentCache(repositoryLocation);
		MOFParser parser = new MOFParser(cache);
		BufferedReader reader = new BufferedReader(new StringReader("Class new_class {[key]String foobar;};"));
		ObjectPath p = new ObjectPath("/local:new_class");
		parser.parse(reader, p.getNameSpacePath());
		assertTrue(cache.contains(p));
		cache.shutdown();

		// ask the server to delete it
		CimClient client = new CimClient(serverURL,clientConfig);
		assertTrue(client.delete(p));
		// test that it has been deleted
		cache = new PersistentCache(repositoryLocation);
		assertFalse(cache.contains(p));
		cache.shutdown();

		// try it again -- should fail
		assertFalse(client.delete(p));
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getElements(java.lang.String, java.lang.String, java.lang.String, boolean)}.
	 */
	@Test
	public void testGetElements() {
		CimClient client = new CimClient(serverURL,clientConfig);
		assertNotNull(client);
		List<NamedElement> elements = client.getElements(null, null, null, false);
		if(verbose) for(NamedElement e : elements) System.out.println(e.toMOF());
		assertEquals(7,elements.size());
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#shutdown()}.
	 */
	@Test
	public void testShutdown() {
		CimClient client = new CimClient(serverURL,clientConfig);
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#addListener(net.aifusion.metamodel.CimEventType, net.aifusion.metamodel.CimListener)}.
	 */
	@Test
	public void testAddListener() {
		CimClient client = new CimClient(serverURL, clientConfig);
		boolean b = client.addListener(CimEventType.ADDED, client);
		client.shutdown();
		assertTrue(b);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#removeListener(net.aifusion.metamodel.CimEventType, net.aifusion.metamodel.CimListener)}.
	 */
	@Test
	public void testRemoveListener() {
		CimClient client = new CimClient(serverURL, clientConfig);
		client.removeListener(CimEventType.ADDED, client);
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#hasListener(net.aifusion.metamodel.CimEventType, net.aifusion.metamodel.CimListener)}.
	 */
	@Test
	public void testHasListener() {
		CimClient client = new CimClient(serverURL, clientConfig);
		boolean b = client.addListener(CimEventType.ADDED, client);
		client.shutdown();
		assertTrue(b);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#notify(net.aifusion.metamodel.CimEvent)}.
	 */
	@Test
	public void testNotifyCimEvent() {
		CimIndication indication = new CimIndication(CimEventType.ADDED,null,"some description");
		CimClient client = new CimClient(serverURL,clientConfig);
		try {
			client.notify(indication);
			fail("Should not succeed");
		} catch(ModelException e){
			assertEquals(ExceptionReason.NOT_SUPPORTED,e.getReason());	// not supported
		}
		client.shutdown();
	}


	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#registerChildProvider(net.aifusion.providers.Provider)}.
	 */
	@Test
	public void testRegisterChildProvider() {
		CimClient client = new CimClient(serverURL,clientConfig);
		assertEquals(server2URL,client.getURL());
		String error = null;
		try {
			client.registerChildProvider(client);
		} catch (ModelException e){
			error = e.toString();
		} finally {
			// clean up
			client.unregisterChildProvider(client);
			client.shutdown();
		}
		assertNull(error);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#unregisterChildProvider(net.aifusion.providers.Provider)}.
	 */
	@Test
	public void testUnregisterChildProvider() {
		CimClient client = new CimClient(serverURL,clientConfig);
		assertEquals(server2URL,client.getURL());
		String error = null;
		try {
			client.unregisterChildProvider(client);
		} catch (ModelException e){
			error = e.toString();
		} finally {
			client.shutdown();
		}
		assertNull(error);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getPropertyNames(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public void testGetPropertyNames() {
		CimClient client = new CimClient(serverURL,clientConfig);
		// use instance path
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		List<String> propertyNames = client.getPropertyNames(path);
		// for(String s : propertyNames) System.out.println(s);
		assertEquals(2,propertyNames.size());
		// use class path
		path = new ObjectPath("/root/local:test_class");
		propertyNames = client.getPropertyNames(path);
		assertEquals(2,propertyNames.size());
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getPropertyType(net.aifusion.metamodel.ObjectPath, java.lang.String)}.
	 */
	@Test
	public void testGetPropertyType() {
		CimClient client = new CimClient(serverURL,clientConfig);
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		DataType type = client.getPropertyType(path, "stringproperty");
		client.shutdown();
		assertEquals(DataType.STRING,type);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getPropertyValue(net.aifusion.metamodel.ObjectPath, java.lang.String)}.
	 */
	@Test
	public void testGetPropertyValue() {
		CimClient client = new CimClient(serverURL,clientConfig);
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		DataValue value = client.getPropertyValue(path, "stringproperty");
		if(verbose) System.out.println(value);
		assertEquals(new DataValue(DataType.STRING,null),value);
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#setPropertyValue(net.aifusion.metamodel.ObjectPath, java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public void testSetPropertyValue() {
		CimClient client = new CimClient(serverURL,clientConfig);
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		// get a property value
		DataValue value = client.getPropertyValue(path, "stringproperty");
		assertEquals(new DataValue(DataType.STRING,null),value);
		// set the property value, and check that it is set
		client.setPropertyValue(path, "stringproperty", new DataValue("foo"));
		assertEquals(new DataValue("\"foo\""),client.getPropertyValue(path, "stringproperty"));
		// re-set the property to the original value, and check it has been reset
		client.setPropertyValue(path, "stringproperty", new DataValue(DataType.STRING,null));
		assertEquals(new DataValue(DataType.STRING,null),client.getPropertyValue(path, "stringproperty"));
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getMethodNames(net.aifusion.metamodel.ObjectPath)}.
	 */
	@Test
	public void testGetMethodNames() {
		CimClient client = new CimClient(serverURL,clientConfig);
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		List<String> names = client.getMethodNames(path);
		client.shutdown();
		assertEquals(1,names.size());
		assertEquals("reset",names.get(0));
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getMethodReturnType(net.aifusion.metamodel.ObjectPath, java.lang.String)}.
	 */
	@Test
	public void testGetMethodReturnType() {
		CimClient client = new CimClient(serverURL,clientConfig);
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		DataType type = client.getMethodReturnType(path, "reset");
		client.shutdown();
		assertEquals(DataType.BOOLEAN,type);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getMethodParameters(net.aifusion.metamodel.ObjectPath, java.lang.String)}.
	 */
	@Test
	public void testGetMethodParameters() {
		CimClient client = new CimClient(serverURL,clientConfig);
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		List<CimParameter> parameters = client.getMethodParameters(path, "reset");
		client.shutdown();
		assertNotNull(parameters);
		assertEquals(1,parameters.size());
		assertEquals("String propertyName",parameters.get(0).toMOF());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#invokeMethod(net.aifusion.metamodel.ObjectPath, java.lang.String, java.util.List)}.
	 */
	@Test
	public void testInvokeMethod() {
		ObjectPath path = new ObjectPath("/root/local:test_class.integerproperty=5");
		PersistentCache cache = new PersistentCache(repositoryLocation);
		CimInstance testInstance = (CimInstance) cache.get(path);
		cache.shutdown();

		assertNotNull(testInstance);
		List<CimParameter> methodParameters = testInstance.getMethodParameters("reset");
		assertNotNull(methodParameters);
		assertEquals(1,methodParameters.size());
		if(verbose) System.out.println(methodParameters.get(0).toMOF());
		methodParameters.get(0).setValue(new DataValue("stringProperty"));
		CimClient client = new CimClient(serverURL,clientConfig);
		try {
			DataValue v = client.invokeMethod(path, "reset", methodParameters);
			if(verbose) System.out.println(v);
		} catch(ModelException e){
			assertEquals(ExceptionReason.METHOD_NOT_AVAILABLE,e.getReason());
		}
		client.shutdown();
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#executeQuery(java.lang.String)}.
	 */
	@Test
	public void testExecuteQuery() {
		CimQuery q = new CimQuery("select * from test_class");
		if(verbose) System.out.println(q.toString());
		PersistentCache cache = new PersistentCache(repositoryLocation);

		List<NamedElement> el = cache.getElements("instance", null, "test_class", true);
		if(verbose) {
			for(NamedElement ex : el){
				System.out.println(ex.getObjectPath()+"\n"+ex.toMOF());
			}
		}
		List<StructureValue> elements = q.executeQuery(cache);
		cache.shutdown();
		assertEquals(1,elements.size());

		CimClient client = new CimClient(serverURL,clientConfig);
		elements = client.executeQuery("select * from test_class");
		client.shutdown();
		assertNotNull(elements);
		assertEquals(1,elements.size());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimClient#getRepository()}.
	 */
	@Test
	public void testGetRepository() {
		CimClient client = new CimClient(serverURL,clientConfig);
		try {
			client.getRepository();
			fail("Should not succeed");
		} catch(ModelException e){
			assertEquals(ExceptionReason.NOT_SUPPORTED,e.getReason());
		} finally {
			client.shutdown();
		}
	}
}
