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
 * Created Aug 19, 2017 by sharad
 */
package net.aifusion.cimserver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.HashMap;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.cimserver.HttpConfiguration;
import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.utils.Java2Cim;

/**
 * Class to test server configuration
 * @author Sharad Singhal
 *
 */
public class HttpConfigurationTest {
	private static String testRepository = "testRepository";
	private static PersistentCache cache;
	
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
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("HttpConfiguration ");
		// get a default configuration
		HashMap<String,DataValue> propertyValues = new HashMap<String,DataValue>();
		propertyValues.put("Id", new DataValue(DataType.STRING,"confKey"));
		propertyValues.put("KeyStorePassword", new DataValue(DataType.STRING,"keyStorePass"));
		propertyValues.put("TrustStorePassword", new DataValue(DataType.STRING,"trustStorePass"));
		propertyValues.put("MaxSessions", new DataValue(DataType.SINT32,0));
		propertyValues.put("ServerTimeout", new DataValue(DataType.SINT32,5000));
		propertyValues.put("ServerPort", new DataValue(DataType.SINT32,8080));
		propertyValues.put("TrustStore", new DataValue(DataType.STRING,testRepository+"/trustStore.jks"));
		propertyValues.put("KeyStore", new DataValue(DataType.STRING,testRepository+"/keyStore.jks"));
		propertyValues.put("Repository", new DataValue(DataType.STRING,testRepository));
		propertyValues.put("RequestHandler", new DataValue(DataType.STRING,"CimHandler"));
		propertyValues.put("Secure", new DataValue(DataType.BOOLEAN,true));
		propertyValues.put("X500Principal", new DataValue(DataType.STRING,"CN=localhost,OU=cimfusion.com,O=cimfusion,C=US"));
		propertyValues.put("HostName", new DataValue(DataType.STRING,"localhost"));
		deleteFiles(testRepository);
		cache = new PersistentCache(testRepository);
		CimClass configClass = (CimClass) Java2Cim.getModelForClass(HttpConfiguration.class, cache);
		assertNotNull(configClass);
		assertTrue(cache.contains(configClass.getObjectPath()));
		CimInstance configInstance = CimInstance.createInstance(configClass, propertyValues, null);
		assertNotNull(configInstance);
		ObjectPath configPath = configInstance.getObjectPath();
		cache.put(configInstance);
		assertTrue(cache.contains(configPath));
		cache.shutdown();
		return;
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		// comment the following line if tests are failing to check repository content
		deleteFiles(testRepository);
		System.out.println("done.");
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
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#ServerConfiguration()}.
	 */
	@Test
	public void testServerConfiguration() {
		HttpConfiguration config = new HttpConfiguration();
		assertNotNull(config);
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getId()}.
	 */
	@Test
	public void testGetId() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals("defaultConfig",conf.getId());
		
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getHostName()}.
	 */
	@Test
	public void testGetHostName() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals("localhost",conf.getHostName());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getRepository()}.
	 */
	@Test
	public void testGetRepository() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertNull(conf.getRepository());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getServerPort()}.
	 */
	@Test
	public void testGetServerPort() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals(8085,conf.getServerPort());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getServerTimeout()}.
	 */
	@Test
	public void testGetServerTimeout() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals(5000,conf.getServerTimeout());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getMaxSessions()}.
	 */
	@Test
	public void testGetMaxSessions() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals(0,conf.getMaxSessions());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#isSecure()}.
	 */
	@Test
	public void testIsSecure() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals(false,conf.isSecure());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getKeyStore()}.
	 */
	@Test
	public void testGetKeyStore() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals("resources/keyStore.jks",conf.getKeyStore());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getKeyStorePassword()}.
	 */
	@Test
	public void testGetKeyStorePassword() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertNull(conf.getKeyStorePassword());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getCookieStore()}.
	 */
	@Test
	public void testGetCookieStore() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals(null,conf.getCookieStore());
	}
	
	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getTrustStore()}.
	 */
	@Test
	public void testGetTrustStore() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals("resources/trustStore.jks",conf.getTrustStore());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getTrustStorePassword()}.
	 */
	@Test
	public void testGetTrustStorePassword() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertNull(conf.getTrustStorePassword());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getProxyHost()}.
	 */
	@Test
	public void testGetProxyHost() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals("localhost",conf.getProxyHost());
	}
	
	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getProxyPort()}.
	 */
	@Test
	public void testGetProxyPort() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals(0,conf.getProxyPort());
	}

	
	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getX500Principal()}.
	 */
	@Test
	public void testGetX500Principal() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals("CN=localhost, OU=cimfusion.com, O=cimfusion, C=US, L=Belmont, ST=California",conf.getX500Principal());
	}


	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getRequestHandler()}.
	 */
	@Test
	public void testGetRequestHandler() {
		HttpConfiguration conf = new HttpConfiguration();
		assertNotNull(conf);
		assertEquals("CimHandler",conf.getRequestHandler());
	}

	/**
	 * Test method for {@link com.aifusion.cimserver.HttpConfiguration#getConfiguration(java.lang.String, java.lang.String, java.lang.String)}.
	 */
	@Test
	public void testGetConfiguration() {
		HttpConfiguration conf = HttpConfiguration.getConfiguration("confKey", null, testRepository);
		assertNotNull(conf);
	}

}
