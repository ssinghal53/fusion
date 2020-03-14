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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import net.aifusion.metamodel.ModelException;

/**
 * Interface to handle an incoming Http Request
 * @author Sharad Singhal
 */
public interface HttpRequestHandler {
	/**
	 * Get a request handler
	 * @param config - Server Configuration class
	 * @return - request handler
	 */
	public static HttpRequestHandler getHandler(HttpConfiguration config) {
		String handlerName = config.getRequestHandler();
		if(handlerName == null) return new DefaultHandler(config);
		switch(handlerName){
		case "CimHandler":
			return new CimHandler(config);
		case "HttpHandler":
			return new HttpHandler(config);
		case "DefaultHandler":
			return new DefaultHandler(config);
		default:
			try {
				ClassLoader loader = HttpRequestHandler.class.getClassLoader();
				Class<?> handler = loader.loadClass(handlerName);
				if(HttpRequestHandler.class.isAssignableFrom(handler)){
					try {
						Constructor<?> constructor = handler.getConstructor(HttpConfiguration.class);
						return (HttpRequestHandler) constructor.newInstance(config);
					} catch (NoSuchMethodException | SecurityException | IllegalArgumentException | InvocationTargetException e) {
						return (HttpRequestHandler) handler.newInstance();
					}
				} else {
					throw new ModelException(handlerName+" is not a HttpRequestHandler");
				}
			} catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
				throw new ModelException("HttpRequestHandler#getHandler(): Unable to load class "+handlerName, e);
			}
		}
	}
	
	/**
	 * Serve a request, and return a response
	 * @param request - incoming request
	 * @return - HttpResponse for this request (must not be null)
	 */
	public HttpResponse serve(HttpRequest request);
	
	/**
	 * Shut down the handler, and free any resources. The handler
	 * is not expected to serve any requests after this
	 */
	public void shutdown();
}
