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
 * Created Jul 8, 2017 by sharad
 */
package net.aifusion.cimserver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.cimserver.CimHeader;
import net.aifusion.cimserver.HttpHeader;
import net.aifusion.cimserver.HttpMethod;
import net.aifusion.cimserver.HttpRequest;
import net.aifusion.cimserver.CimXHeader;
import net.aifusion.cimserver.MimeType;

/**
 * @author Sharad Singhal
 *
 */
public class HttpRequestTest {
	private static String CRLF = "\r\n";
	private static String [] inputs = {
			"Some input"+CRLF	
	};
	private static String [] requests = {
			"GET /data?a=b HTTP/1.1"+CRLF+
			"Host: abc.com"+CRLF+
			"Content-Length: "+inputs[0].length()+CRLF+
			"Content-Type: text/plain;charset=utf-8"+CRLF+
			CimXHeader.INTRINSIC+": "+CimHeader.GET_ELEMENT+CRLF+
			"Cookie:ID=xyzzy;Lang=US-en"	
	};

	private BufferedReader header;
	private BufferedInputStream input;
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("HttpRequest ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		header = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(requests[0].getBytes())));
		input = new BufferedInputStream(new ByteArrayInputStream(inputs[0].getBytes()));
	}
	@After
	public void tearDown() throws Exception {
		header.close();
		input.close();
		System.out.print(".");
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#HttpRequest(java.io.BufferedReader, java.io.BufferedInputStream)}.
	 */
	@Test
	public void testHttpRequest() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getContentLength()}.
	 */
	@Test
	public void testGetContentLength() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals(inputs[0].length(),r.getContentLength());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getProtocolVersion()}.
	 */
	@Test
	public void testGetProtocolVersion() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals("HTTP/1.1",r.getProtocolVersion());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getHttpMethod()}.
	 */
	@Test
	public void testGetHttpMethod() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals(HttpMethod.GET,r.getHttpMethod());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getInputStream()}.
	 */
	@Test
	public void testGetInputStream() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		BufferedInputStream in = r.getInputStream();
		byte [] buffer = new byte[256];
		try {
			int b = in.read(buffer);
			assertEquals(r.getContentLength(),b);
			assertEquals("Some input\r\n",new String(Arrays.copyOf(buffer, b)));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getURI()}.
	 */
	@Test
	public void testGetURI() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals("/data",r.getURI());
	}
	
	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getRequestTarget()}.
	 */
	@Test
	public void getRequestTarget(){
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals("/data?a=b",r.getRequestTarget());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getHeader(net.aifusion.cimserver.HttpHeader)}.
	 */
	@Test
	public void testGetHeader() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals("abc.com",r.getHeader(HttpHeader.HOST));
		assertNull(r.getHeader(HttpHeader.ACCEPT));
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getXHeader(java.lang.String)}.
	 */
	@Test
	public void testGetXheader() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals(CimHeader.GET_ELEMENT.toString(),r.getXHeader(CimXHeader.INTRINSIC.toString()));
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getHeaders()}.
	 */
	@Test
	public void testGetHeaders() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals(3,r.getHeaders().size());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getXHeaders()}.
	 */
	@Test
	public void testGetXheaders() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals(1,r.getXHeaders().size());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getUriParameter(java.lang.String)}.
	 */
	@Test
	public void testGetUriParameter() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals("b",r.getUriParameter("a"));
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getUriParameters()}.
	 */
	@Test
	public void testGetUriParameters() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals(1,r.getUriParameters().size());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#isLastRequest()}.
	 */
	@Test
	public void testIsLastRequest() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertFalse(r.isLastRequest());
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getCookieValue(java.lang.String)}.
	 */
	@Test
	public void testGetCookieValue() {
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals("xyzzy",r.getCookieValue("ID"));
		assertEquals("US-en",r.getCookieValue("Lang"));
	}
	
	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getContentType()}.
	 */
	@Test
	public void testGetContentType(){
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals(MimeType.PLAINTEXT,r.getContentType());
	}
	
	/**
	 * Test method for {@link net.aifusion.cimserver.HttpRequest#getCharset()}.
	 */
	@Test
	public void testGetCharSet(){
		HttpRequest r = new HttpRequest(header,input);
		assertNotNull(r);
		assertEquals("utf-8",r.getCharset());
		
	}

}
