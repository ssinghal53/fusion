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
 * Created Jul 9, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.zip.GZIPInputStream;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.ModelException;

/**
 * Class to test HttpResponse
 * @author Sharad Singhal
 *
 */
public class HttpResponseTest {
	private final static String CRLF = "\r\n";
	private final static String bodyText = "Some input for the body"+CRLF;	
	private InputStream input;
	private ByteArrayOutputStream output;
	private String received;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("HttpResponse ");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}
	@Before
	public void setUp() throws Exception {
		System.out.print("-");
		byte[] buffer = bodyText.getBytes(Constants.byteEncoding);
		assertEquals(buffer.length,bodyText.length());
		input = new ByteArrayInputStream(bodyText.getBytes(Constants.byteEncoding));
		output = new ByteArrayOutputStream(8192);
	}
	@After
	public void tearDown() throws Exception {
		if(input != null) input.close();
		if(output!= null) output.close();
		System.out.print(".");
	}
	
	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#HttpResponse(HttpMethod,HttpStatus)}.
	 */
	@Test
	public void testHttpResponseHttpStatus(){
		for(HttpMethod m : HttpMethod.values()){
			for(HttpStatus s : HttpStatus.values()){
				try {
					HttpResponse r = new HttpResponse(m,s);
					assertNotNull(r);
					sendToReceiveString(r);
					if(!received.contains("Content-Length:")){
						// System.out.println(m+"\n"+received);
						String code = s.code();
						assertTrue(
								m == HttpMethod.HEAD || m == HttpMethod.GET && code.equals("304") || code.startsWith("1") ||
								code.equals("204") || m == HttpMethod.CONNECT && code.startsWith("2")
								);
					}
				} catch(ModelException e){
					fail("failed with ("+m+","+s+")");
				}
			}
		}
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#HttpResponse(HttpMethod, net.aifusion.cimserver.HttpStatus, net.aifusion.cimserver.MimeType, java.io.InputStream)}.
	 */
	@Test
	public void testHttpResponseHttpStatusMimeTypeInputStream() {
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT,input);
		assertNotNull(r);
		sendToReceiveString(r);
		// System.out.println(received);
		assertTrue(received.contains("Transfer-Encoding: chunked"));
		assertTrue(received.endsWith("19\r\nSome input for the body\r\n\r\n0\r\n\r\n"));
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#HttpResponse(HttpMethod, net.aifusion.cimserver.HttpStatus, net.aifusion.cimserver.MimeType, java.lang.String)}.
	 */
	@Test
	public void testHttpResponseHttpStatusMimeTypeString() {
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT, bodyText);
		assertNotNull(r);
		sendToReceiveString(r);
		// System.out.println(received);
		assertTrue(received.contains("Content-Length"));
		assertTrue(received.endsWith(bodyText));
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#send(java.io.OutputStream)}.
	 */
	@Test
	public void testSend() {
		// use the string input constructor
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT, bodyText);
		assertNotNull(r);
		sendToReceiveString(r);
		// System.out.println(received);
		assertEquals(165,received.length());

		// use the stream input constructor
		r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT,input);
		assertNotNull(r);
		sendToReceiveString(r);
		// System.out.println(received);
		assertEquals(184,received.length());
		return;
	}

	private void sendToReceiveString(HttpResponse r) {
		r.send(output);
		try {
			received = output.toString(Constants.byteEncoding.toString());
		} catch(UnsupportedEncodingException e){
			fail("Unsupported Encoding "+e.toString());
		}
		output.reset();
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#addHeader(net.aifusion.cimserver.HttpHeader, java.lang.String)}.
	 */
	@Test
	public void testAddHeader() {
		// use the string input constructor
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT, bodyText);
		assertNotNull(r);
		r.addHeader(HttpHeader.HOST, "myDomain.com");
		sendToReceiveString(r);
		// System.out.println(received);
		assertEquals(185,received.length());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#addXHeader(java.lang.String, java.lang.String)}.
	 */
	@Test
	public void testAddXHeader() {
		// use the string input constructor
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT, bodyText);
		assertNotNull(r);
		r.addXHeader(CimXHeader.INTRINSIC.toString(), CimHeader.GET_ELEMENT.toString());
		sendToReceiveString(r);
		System.out.println(received);
		assertEquals(193,received.length());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#addCookie(net.aifusion.cimserver.HttpCookie)}.
	 */
	@Test
	public void testAddCookie() {
		// use the string input constructor
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT, bodyText);
		assertNotNull(r);
		r.addCookie(new HttpCookie("id","3f25",null,null,0,null,false,true));
		sendToReceiveString(r);
		// System.out.println(received);
		assertEquals(196,received.length());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.HttpResponse#isLastResponse()}.
	 */
	@Test
	public void testIsLastResponse() {
		// use the string input constructor
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT, bodyText);
		assertNotNull(r);
		assertFalse(r.isLastResponse());
		r.addHeader(HttpHeader.CONNECTION, "close");
		assertTrue(r.isLastResponse());
		sendToReceiveString(r);
		// System.out.println(received);
		assertTrue(received.contains("Connection: close"));
	}
	/**
	 * Test method for Response#send() when gzip encoding is desired
	 */
	@Test
	public void testGzipEncodedResponse() {
		HttpResponse r = new HttpResponse(HttpMethod.GET,HttpStatus.OK,MimeType.PLAINTEXT, bodyText);
		assertNotNull(r);
		r.addHeader(HttpHeader.TRANSFER_ENCODING, "gzip");
		r.send(output);
		try {
			byte[] buf = output.toByteArray();
			output.reset();
			// System.out.println(new String(buf));
			// skip past the header
			int cursor = 0;
			for(int headerEnd = 3; headerEnd < buf.length; headerEnd++){
				// System.out.println(inputBuffer[headerEnd-3]+" "+inputBuffer[headerEnd-2]+" "+inputBuffer[headerEnd-1]+" "+inputBuffer[headerEnd]);
				if(buf[headerEnd-3] == '\r' && buf[headerEnd -2] == '\n' && 
						buf[headerEnd -1] == '\r' && buf[headerEnd] == '\n') {
					cursor = headerEnd + 1;
					break;
				}
			}
			if(cursor == 0) fail("Cursor is zero");
			int outCursor = 0;
			int len = 0;
			do {
				// locate next chunk
				len = 0;
				for(int i = cursor; i < buf.length; i++){
					// System.out.println(i+" "+buf[i]+" ["+buf[i+1]+","+buf[i+2]+"]");
					len = len * 16 + hexToInt(buf[i]);
					if(buf[i+1] == '\r' && buf[i+2] == '\n'){
						cursor = i+3;
						break;
					}
				}
				// System.out.println("Next Chunk "+len+" at "+cursor);
				for(int i=cursor; i < cursor+len; i++){
					// System.out.println("buf["+outCursor+"] <- buf["+i+"] ("+(char)buf[i] +")");
					buf[outCursor++] = buf[i];
				}
				cursor += len;
				if(buf[cursor] == '\r' && buf[cursor+1] == '\n') cursor += 2;
			} while (len > 0 && cursor <= buf.length-2);
			GZIPInputStream inp = new GZIPInputStream(new ByteArrayInputStream(Arrays.copyOf(buf, outCursor)));
			BufferedReader reader = new BufferedReader(new InputStreamReader(inp,Constants.byteEncoding));
			String line;
			while((line = reader.readLine()) != null){
				assertEquals("Some input for the body",line);
			}
		} catch (IOException e) {
			fail(e.toString());
		}
	}
	
	private int hexToInt(byte b){
		return (b >= 48 && b <= 57) ? b-48 : (b >= 97 && b <= 102) ? b-97+10 : b;
	}
}
