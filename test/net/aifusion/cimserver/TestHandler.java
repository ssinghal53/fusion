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
 * Created Sep 6, 2017 by sharad
 */
package net.aifusion.cimserver;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Arrays;

import net.aifusion.cimserver.CimHeader;
import net.aifusion.cimserver.HttpConfiguration;
import net.aifusion.cimserver.HttpHeader;
import net.aifusion.cimserver.HttpMethod;
import net.aifusion.cimserver.HttpRequest;
import net.aifusion.cimserver.HttpRequestHandler;
import net.aifusion.cimserver.HttpResponse;
import net.aifusion.cimserver.HttpStatus;
import net.aifusion.cimserver.HttpXHeader;
import net.aifusion.cimserver.MimeType;

/**
 * @author Sharad Singhal
 *
 */
public class TestHandler implements HttpRequestHandler {

	/**
	 * 
	 */
	public TestHandler(HttpConfiguration config) {
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.httpserver.HttpRequestHandler#serve(net.aifusion.cimserver.HttpRequest)
	 */
	@Override
	public HttpResponse serve(HttpRequest request) {
		HttpMethod method = request.getHttpMethod();
		CimHeader cimRequest = CimHeader.lookup(request.getXHeader(HttpXHeader.INTRINSIC.toString()));
		HttpResponse response = null;
		String body = null;
		if(CimHeader.GET_NAMESPACES.equals(cimRequest)){
			String authority = request.getHeader(HttpHeader.HOST);
			response = new HttpResponse(request.getHttpMethod(),HttpStatus.OK,MimeType.PLAINTEXT,"http://"+authority+"/localPath");
		} else {
			switch(method){
			case DELETE:
				break;
			case GET:
			case POST:
				body = getBody(request);	// note that GET does not send a body, but POST does
				if(body == null || body.isEmpty()) body = "Server Created Body";
				break;
			case HEAD:
				break;
			case OPTIONS:
				break;
			case PUT:
				String b = getBody(request);
				break;
			default:
				response = new HttpResponse(method,HttpStatus.BAD_REQUEST,MimeType.PLAINTEXT,"Method "+method+" not implemented");
				break;
			}
		}
		if(response == null){
			response = (body == null) ? new HttpResponse(request.getHttpMethod(),HttpStatus.OK) :
				new HttpResponse(request.getHttpMethod(),HttpStatus.OK,MimeType.PLAINTEXT,body);
		}
		return response;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.httpserver.HttpRequestHandler#shutdown()
	 */
	@Override
	public void shutdown() {
		return;
	}
	
	String getBody(HttpRequest request){
		long l = request.getContentLength();
		if(l == 0) return null;
		byte [] b = new byte[1024];
		BufferedInputStream in = request.getInputStream();
		long read = 0;
		while(read < l){
			try {
				int found = in.read(b, (int) read, (int)(l-read));
				read += found;
			} catch (IOException e) {
				e.printStackTrace();
			}	
		}
		return new String(Arrays.copyOf(b, (int) l),Charset.forName(request.getCharset()));
	}

}
