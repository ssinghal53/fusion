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
 * Created Jul 8, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import static org.junit.Assert.*;

import java.net.URI;
import java.util.Date;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.cimserver.HttpCookie;
import net.aifusion.cimserver.HttpException;
import net.aifusion.cimserver.HttpStatus;

/**
 * Class to test an HTTP Cookie
 * @author Sharad Singhal
 *
 */
public class HttpCookieTest {
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("HttpCookie ");
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
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#HttpCookie(java.lang.String, java.lang.String, java.lang.String, java.lang.String, long, java.util.Date, boolean, boolean)}.
	 */
	@Test
	public void testHttpCookie() {
		HttpCookie c = new HttpCookie("SID","3-56978-41457","/","myDomain.ibm.com",3600,null,false,false);
		assertNotNull(c);
		assertEquals("SID=3-56978-41457; Path=/; Max-Age=3600",c.toString());
		// test a cookie with TLD values
		try {
			c = new HttpCookie("SID","345","/","ibm.com",0,null,false,false);
			fail("ibm.com should not succeed");
		} catch(HttpException e){
			assertEquals(HttpStatus.NOT_ACCEPTABLE,e.getStatus());
		}
		// test a cookie with public suffix
		try {
			c = new HttpCookie("SID","345","/","j.bg",0,null,false,false);
			fail("j.bg should not succeed");
		} catch(HttpException e){
			assertEquals(HttpStatus.NOT_ACCEPTABLE,e.getStatus());
		}
		// test a cookie with public suffix wildcard
		try {
			c = new HttpCookie("SID","345","/","myDomain.bd",0,null,false,false);
			fail("*.bd should not succeed");
		} catch(HttpException e){
			assertEquals(HttpStatus.NOT_ACCEPTABLE,e.getStatus());
		}
		// test a cookie with public suffix exception
		try {
			c = new HttpCookie("SID","345","/","mydomain.ck",0,null,false,false);
			fail("*.ck should not succeed");
		} catch(HttpException e){
			assertEquals(HttpStatus.NOT_ACCEPTABLE,e.getStatus());
		}
		// test a cookie with public suffix exception
		try {
			c = new HttpCookie("SID","345","/","www.ck",0,null,false,false);
		} catch(HttpException e){
			fail("www.ck should succeed");
		}
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#parseCookieValues(java.lang.String)}.
	 */
	@Test
	public void testParseCookieValues() {
		HttpCookie[] cookies = HttpCookie.parseCookieValues("SID=3-56978-41457; Lang=US-en");
		assertEquals(2,cookies.length);
		assertEquals("SID=3-56978-41457",cookies[0].toString());
		assertEquals("Lang=US-en",cookies[1].toString());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#parseSetCookieValue(java.net.URI, java.lang.String)}.
	 */
	@Test
	public void testParseSetCookieValue() {
		URI originURI = URI.create("http://myDomain.ibm.com/abc");
		HttpCookie c = HttpCookie.parseSetCookieValue(originURI, "SID=3-56978-41457; Max-Age=3600; Path=/; ");
		assertNotNull(c);
		assertEquals("SID=3-56978-41457; Path=/; Max-Age=3600",c.toString());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#isExpired()}.
	 */
	@Test
	public void testIsExpired() {
		HttpCookie c = new HttpCookie("SID","34AC","/","mydomain.com",1,null,false,false);
		assertNotNull(c);
		assertFalse(c.isExpired());
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			// nothing to do
		}
		assertTrue(c.isExpired());
		c = new HttpCookie("SID","34AC","/","mydomain.com",0,new Date(99,1,1),false,false);
		assertNotNull(c);
		assertTrue(c.isExpired());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#isPersistent()}.
	 */
	@Test
	public void testIsPersistent() {
		HttpCookie c = new HttpCookie("SID","34AC","/","mydomain.com",1,null,false,false);
		assertNotNull(c);
		assertTrue(c.isPersistent());
		c = new HttpCookie("SID","34AC","/","mydomain.com",0,null,false,false);
		assertNotNull(c);
		assertFalse(c.isPersistent());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#isSecure()}.
	 */
	@Test
	public void testIsSecure() {
		HttpCookie c = new HttpCookie("SID","34AC","/","mydomain.com",1,null,false,false);
		assertNotNull(c);
		assertFalse(c.isSecure());
		c = new HttpCookie("SID","34AC","/","mydomain.com",1,null,true,false);
		assertTrue(c.isSecure());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#isHttpOnly()}.
	 */
	@Test
	public void testIsHttpOnly() {
		HttpCookie c = new HttpCookie("SID","34AC","/","mydomain.com",1,null,false,false);
		assertNotNull(c);
		assertFalse(c.isHttpOnly());
		c = new HttpCookie("SID","34AC","/","mydomain.com",1,null,false,true);
		assertTrue(c.isHttpOnly());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#matchesPath(java.lang.String)}.
	 */
	@Test
	public void testMatchesPath() {
		HttpCookie c = new HttpCookie("SID","34AC","/abc","mydomain.com",1,null,false,false);
		assertNotNull(c);
		assertFalse(c.matchesPath("/"));
		assertTrue(c.matchesPath("/abc"));
		assertTrue(c.matchesPath("/abc/def"));
		
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#matchesDomain(java.lang.String)}.
	 */
	@Test
	public void testMatchesDomain() {
		HttpCookie c = new HttpCookie("SID","34AC","/abc","z.mydomain.com",1,null,false,false);
		assertNotNull(c);
		assertFalse(c.matchesDomain("myDomain.com"));
		assertTrue(c.matchesDomain("z.mydomain.com"));
		assertTrue(c.matchesDomain("a.z.mydomain.com"));
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#clientValue()}.
	 */
	@Test
	public void testClientValue() {
		HttpCookie c = new HttpCookie("SID","34AC","/abc","z.mydomain.com",1,null,false,false);
		assertNotNull(c);
		assertEquals("SID=34AC",c.clientValue());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#matches(net.aifusion.cimserver.HttpCookie)}.
	 */
	@Test
	public void testMatches() {
		// note that matches() ONLY matches name=value in the cookies
		HttpCookie c = new HttpCookie("SID","34AC","/abc","z.mydomain.com",1,null,false,false);
		assertNotNull(c);
		HttpCookie c1 = new HttpCookie("SID","34AC","/","abc.mydomain.com",1,null,true,true);
		assertTrue(c.matches(c1));
		c1 = new HttpCookie("SID","34AB","/","abc.mydomain.com",1,null,true,true);
		assertFalse(c.matches(c1));
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpCookie#toString()}.
	 */
	@Test
	public void testToString() {
		HttpCookie c = new HttpCookie("SID","34AC","/abc","mydomain.com",1,new Date(99,1,1),true,true);
		assertNotNull(c);
		assertEquals("SID=34AC; Path=/abc; Max-Age=1; Expires=Mon, 1 Feb 1999 08:00:00 GMT; HttpOnly; Secure",c.toString());
	}

}
