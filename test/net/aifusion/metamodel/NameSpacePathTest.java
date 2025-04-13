/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Nov 22, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.net.URISyntaxException;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Class to test NameSpace Paths
 * @author Sharad Singhal
 */
public class NameSpacePathTest {
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("NameSpacePath ");
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
	 * Test method for {@link net.aifusion.metamodel.NameSpacePath#NameSpacePath(java.lang.String)}.
	 */
	@Test
	public final void testNameSpacePath() {
		NameSpacePath p = new NameSpacePath("/root/cimv2");
		assertTrue(p.toString().equals("/root/cimv2"));
		try {
			p = new NameSpacePath(null);
			fail("Should throw an exception");
		} catch (NullPointerException e){
			// this is OK
		}
		try {
			p = new NameSpacePath("/root/../..");
			fail("Should throw an exception");
		} catch(ModelException e){
			// this is OK
			assertEquals(4,e.getReason().getCode());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.NameSpacePath#NameSpacePath(String, String, String)}.
	 */
	@Test
	public final void testNameSpacePathStringStringString(){
		String scheme = "http+Cim-xml";
		String authority = "user:password@localhost:5050";
		String path = "/root/cimv2";
		try {
			new NameSpacePath(scheme,authority,null);
			fail("Should throw an exception");
		} catch (ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		try {
			new NameSpacePath(scheme,authority,"/root/../..");
			fail("Should throw an exception");
		} catch(ModelException e){
			assertEquals(4,e.getReason().getCode());
		}
		// try other combinations of null scheme and authority
		for(int i = 0; i<4; i++){
			try {
				URI uri = new URI((i&0x1)!= 0 ? scheme : null,(i&0x2)!=0 ? authority : null,path,null,null);
				NameSpacePath p = new NameSpacePath((i&0x1)!= 0 ? scheme : null,(i&0x2)!=0 ? authority : null,path);
				assertTrue(uri.toString().equals(p.toString()));
			} catch (URISyntaxException e) {
				fail(e.toString());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NameSpacePath#getScheme()}.
	 */
	@Test
	public final void testGetScheme() {
		NameSpacePath p = new NameSpacePath("http+Cim-xml","user:password@localhost:5050","/root/cimv2");
		assertTrue(p.getScheme().equals("http+Cim-xml"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NameSpacePath#getAuthority()}.
	 */
	@Test
	public final void testGetAuthority() {
		NameSpacePath p = new NameSpacePath("http+Cim-xml","user:password@localhost:5050","/root/cimv2");
		assertNotNull(p);
		assertTrue(p.getAuthority().equals("user:password@localhost:5050"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NameSpacePath#getLocalPath()}.
	 */
	@Test
	public final void testGetLocalPath() {
		NameSpacePath p = new NameSpacePath("http","user:Info@host:8000","/root/cimv2");
		assertTrue(p.getLocalPath().equals("/root/cimv2"));
		assertEquals("http://user:Info@host:8000/root/cimv2",p.toString());
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.NameSpacePath#getResourcePath()}.
	 */
	@Test
	public final void testGetResourcePath() {
		NameSpacePath p = new NameSpacePath("http","user:Info@host:8000","/root/+/cimv2");
		assertTrue(p.getLocalPath().equals("/cimv2"));
		assertTrue(p.getResourcePath().equals("/root/"));
		assertEquals("http://user:Info@host:8000/root/+/cimv2",p.toString());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.NameSpacePath#toString()}.
	 */
	@Test
	public final void testToString() {
		String scheme = "http+Cim-xml";
		String authority = "user:password@localhost:5050";
		String path = "/root/+/cimv2";
		String [] expect = {
				"/root/+/cimv2",
				"http+Cim-xml:/root/+/cimv2",
				"//user:password@localhost:5050/root/+/cimv2",
				"http+Cim-xml://user:password@localhost:5050/root/+/cimv2"
		};
			
		for(int i = 0; i<4; i++){
			NameSpacePath p = new NameSpacePath((i&0x1)!= 0 ? scheme : null,(i&0x2)!=0 ? authority : null,path);
			assertEquals(expect[i],p.toString());
		}
	}
	
	@Test
	public final void testCombinations() {
		NameSpacePath p = new NameSpacePath("http://hostname/resource/+/path");
		assertEquals("http",p.getScheme());
		assertEquals("hostname",p.getAuthority());
		assertEquals("/resource/",p.getResourcePath());
		assertEquals("/path",p.getLocalPath());
		p = new NameSpacePath("/resource/+/path");
		assertEquals(null,p.getScheme());
		assertEquals(null,p.getAuthority());
		assertEquals("/resource/",p.getResourcePath());
		assertEquals("/path",p.getLocalPath());
		p = new NameSpacePath("/resource//path");
		assertEquals(null,p.getScheme());
		assertEquals(null,p.getAuthority());
		assertEquals("/",p.getResourcePath());
		assertEquals("/resource/path",p.getLocalPath());
		
		return;
	}

}
