/**
 * Copyright 2017, Sharad Singhal, All Rights Reserved
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
 * Created Mar 12, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Map;

/**
 * Default request handler for HttpServer. It simply echoes what the client sent it.
 * @author Sharad Singhal
 */
class DefaultHandler implements HttpRequestHandler {
	
	/**
	 * Create a new request handler
	 * @param config - configuration to use in the handler
	 */
	public DefaultHandler(HttpConfiguration config){
		return;
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.services.http.HttpRequestHandler#serve(net.aifusion.services.http.HttpRequest)
	 */
	@Override
	public HttpResponse serve(HttpRequest request){
        StringBuilder sb = new StringBuilder();
        sb.append("<html>");
        sb.append("<head><title>CimFusion Debugging Server</title></head>");
        sb.append("<body>");
        sb.append("<h1>CimFusion Debugging Server</h1>");

        sb.append("<p><blockquote><b>URI</b> = ").append(request.getURI()).append("<br/>");

        sb.append("<b>Method</b> = ").append(request.getHttpMethod()).append("</blockquote></p>");

        sb.append("<h3>Headers</h3><p><blockquote>");
        Map<HttpHeader,String> headers = request.getHeaders();
        for(HttpHeader h : headers.keySet()){
        	sb.append(h.toString()).append(":").append(headers.get(h)).append("<br/>");
        }
        Map<String,String> xHeaders = request.getXHeaders();
        for(String h : xHeaders.keySet()){
        	sb.append(h).append(":").append(xHeaders.get(h)).append("<br/>");
        }
        sb.append("</blockquote></p>");

        sb.append("<h3>Parms</h3><p><blockquote>");
        Map<String,String> params = request.getUriParameters();
        for(String p : params.keySet()){
        	sb.append(p).append(" = ").append(params.get(p)).append("<br/>");
        }
        sb.append("</blockquote></p>");
        
        sb.append("<h3>Cookies</h3><p><blockquote>");
        Map<String,HttpCookie> cookies = request.getCookies();
        for(HttpCookie cookie : cookies.values()){
        	sb.append(cookie.getName()).append(" = ").append(cookie.getValue()).append("<br/>");
        }
        sb.append("</blockquote></p>");
        
        sb.append("<h3>Content</h3><p><blockquote>");
        BufferedInputStream input = request.getInputStream();
        int contentLength = (int) request.getContentLength();
        if(contentLength > 0){
        	byte [] buffer = new byte[contentLength];
        	int bytesRead = 0;
        	while(bytesRead < contentLength){
        		try {
					int r = input.read(buffer, 0, (int) Math.min(8192, contentLength-bytesRead));
					if(r < 0) break;
					bytesRead += r;
				} catch (IOException e) {
					sb.append(e.toString());
				}
        	}
			try {
				String body = new String(buffer,"UTF-8");
				sb.append(body);
			} catch (UnsupportedEncodingException e) {
				sb.append(e.toString());
			}
        }
        sb.append("</blockquote></p>");
        sb.append("</body>");
        sb.append("</html>");
        return new HttpResponse(request.getHttpMethod(),HttpStatus.OK,MimeType.HTML, sb.toString());
	}

	@Override
	public void shutdown() {
		// This handler does not maintain any state.  Nothing to do here. 
		return;
	}

}
