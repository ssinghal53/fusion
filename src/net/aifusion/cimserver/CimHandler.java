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
 * Created Jul 16, 2017 by sharad
 */
package net.aifusion.cimserver;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Logger;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimEventType;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.providers.BasicProvider;
import net.aifusion.providers.Provider;

/**
 * Class to Handle Cim Requests directed at an underlying provider. This handler provides server-side processing for the CIM server
 * @author Sharad Singhal
 */
class CimHandler implements HttpRequestHandler {
	/** Logger for this class */
	private static Logger logger = Logger.getLogger(CimHandler.class.getName());
	/** Underlying Cim Provider */
	private Provider provider;
	/** Configuration, if any */
	private HttpConfiguration config = null;
	/** Line terminator */
	private static final String CRLF = "\r\n";
	/** flag to indicate logging */
	private boolean logEnabled = false;

	/**
	 * Create a CimHandler with an underlying Cim Provider
	 * @param provider - provider used in the handler
	 */
	public CimHandler(Provider provider){
		if(provider == null) throw new ModelException("CimHandler - Provider cannot be null");
		this.provider = provider;
		return;
	}

	/**
	 * Create a CimHandler from a server configuration
	 * @param config server configuration to use
	 */
	public CimHandler(HttpConfiguration config) {
		this.config = config;
		String r = config.getRepository();
		String p = config.getProvider();
		Repository repo = (r == null) ? new InMemoryCache() 
				: new PersistentCache(r);
		if(logEnabled) logger.info("Using Repository "+r+" Provider "+p);
		if(p != null) {
			try {
				ClassLoader loader = CimHandler.class.getClassLoader();
				Class<?> providerClass = loader.loadClass(p);
				if(Provider.class.isAssignableFrom(providerClass)){
					try {
						Constructor<?> constructor = providerClass.getConstructor(Repository.class);
						this.provider = (Provider) constructor.newInstance(repo);
					} catch (NoSuchMethodException | SecurityException | IllegalArgumentException | InvocationTargetException e) {
						try {
							this.provider =  (Provider) providerClass.getDeclaredConstructor().newInstance();
						} catch (IllegalArgumentException | InvocationTargetException | SecurityException e1) {
							throw new ModelException("Unable to locate a provider for "+p,e1);
						}
					}
				} else {
					throw new ModelException(p+" is not a Provider");
				}
			} catch (InstantiationException | IllegalAccessException | ClassNotFoundException | NoSuchMethodException e) {
				throw new ModelException("CimHandler: Unable to load class "+p, e);
			}
		} else {
			this.provider = new BasicProvider(repo);
		}
		return;
	}
	
	/**
	 * Enable logging on handler
	 * @param enable - true to enable logging, false to disable logging
	 */
	public void enableLog(boolean enable){
		logEnabled = enable;
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.httpserver.HttpRequestHandler#serve(net.aifusion.httpserver.HttpRequest)
	 */
	@Override
	public HttpResponse serve(HttpRequest request) {
		HttpMethod requestMethod = request.getHttpMethod();
		// validate that the request accepts PLAINTEXT and MOF
		if(!(request.accepts(MimeType.MOF) && request.accepts(MimeType.PLAINTEXT))){
			return new HttpResponse(requestMethod,HttpStatus.UNSUPPORTED_MEDIA_TYPE,MimeType.PLAINTEXT,
					"Only plainText and MOF Media types supported");
		}
		CimHeader cimRequest = CimHeader.lookup(request.getXHeader(CimXHeader.INTRINSIC.toString()));
		// validate the intrinsic request
		if(cimRequest == null || !cimRequest.getHttpMethod().equals(requestMethod)){
			// Either the request header is not there, or the httpMethod required by it does not match
			return new HttpResponse(requestMethod,HttpStatus.BAD_REQUEST,MimeType.PLAINTEXT,
					"HttpMethod "+requestMethod+" does not match Cim-Intrinsic "+cimRequest);
		}
		// validate that all required extension headers are present
		for(CimXHeader x : CimXHeader.getXHeaders(cimRequest)){
			if(!request.hasXHeader(x.toString())) 
				return new HttpResponse(requestMethod,HttpStatus.BAD_REQUEST,MimeType.PLAINTEXT,
						cimRequest+" requires header "+x);
		}
		// null response implies that HttpStatus.NOT_FOUND will be sent to client
		HttpResponse response = null;
		try {
			String httpBody = getBody(request);
			switch(cimRequest){
			case GET_NAMESPACES:	// all name spaces, one per line (OK)
				List<NameSpacePath> names = provider.getNameSpaces();
				StringBuilder b = new StringBuilder();
				for(NameSpacePath name : names){
					if(config != null && config.getServerPort() > 0){
						b.append(new NameSpacePath(config.isSecure() ? "https" : "http",
								config.getHostName()+":"+config.getServerPort(),name.getLocalPath()));
					} else {
						b.append(name);
					}
					b.append(CRLF);
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, b.toString());
				break;
			case GET_ELEMENT:	// return the mof value of the named element
				ObjectPath path = getObjectPath(request);
				NamedElement element = provider.get(path);
				if(element != null){
					b = new StringBuilder();
					b.append("#pragma namespace(\"").append(element.getNameSpacePath().getLocalPath()).append("\")\n");
					b.append(element.toMOF());
					switch(element.getElementType()) {
					case STRUCTUREVALUE:
						b.append(";");
					case INSTANCE:
						b.append(CRLF);
						break;
					default:
						break;
					}
					response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, b.toString());
				}
				break;
			case HAS_ELEMENT:	// return OK/NOT found (OK)
				path = getObjectPath(request);
				boolean bv = provider.contains(path);
				response = bv ? new HttpResponse(requestMethod,HttpStatus.OK) :
					new HttpResponse(requestMethod,HttpStatus.NOT_FOUND);
				break;
			case GET_PROPERTY_NAMES:	// all property names, one per line
				path = getObjectPath(request);
				List<String> pNames = provider.getPropertyNames(path);
				b = new StringBuilder();
				for(String name : pNames){
					b.append(name).append(CRLF);
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, b.toString());
				break;
			case GET_PROPERTY_TYPE:	// type of a given property
				String propertyName = request.getXHeader(CimXHeader.PROPERTY_NAME.toString());
				path = getObjectPath(request);
				DataType type = provider.getPropertyType(path, propertyName);
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, type.toString()+CRLF);
				break;
			case GET_PROPERTY_VALUE:	// mof value of a given property
				propertyName = request.getXHeader(CimXHeader.PROPERTY_NAME.toString());
				path = getObjectPath(request);
				DataValue v = provider.getPropertyValue(path, propertyName);
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, v.toMOF());
				response.addXHeader(CimXHeader.PROPERTY_TYPE.toString(),v.getType().toMOF());
				break;
			case GET_METHOD_NAMES:	// names of methods, if any
				path = getObjectPath(request);
				pNames = provider.getMethodNames(path);
				b = new StringBuilder();
				for(String name : pNames){
					b.append(name).append(CRLF);
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, b.toString());
				break;
			case GET_METHOD_TYPE:	// return type of the method
				propertyName = request.getXHeader(CimXHeader.METHOD_NAME.toString());
				path = getObjectPath(request);
				type = provider.getMethodReturnType(path, propertyName);
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, type.toString()+CRLF);
				break;
			case GET_METHOD_PARAMETERS:	// method parameters, one per line
				propertyName = request.getXHeader(CimXHeader.METHOD_NAME.toString());
				path = getObjectPath(request);
				List<CimParameter> pm = provider.getMethodParameters(path, propertyName);
				b = new StringBuilder();
				for(CimParameter p : pm){
					b.append(p.toMOF()).append(CRLF);
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, b.toString()+CRLF);
				break;
			case SET_PROPERTY_VALUE:	// set property value
				path = getObjectPath(request);
				propertyName = request.getXHeader(CimXHeader.PROPERTY_NAME.toString());
				NamedElement target = provider.get(path);
				if(target == null || propertyName == null){
					response = new HttpResponse(requestMethod,HttpStatus.NOT_FOUND,MimeType.PLAINTEXT,path+" ["+propertyName+"]");
					break;
				}
				
				MOFParser parser = new MOFParser(provider);
				DataValue propertyValue = parser.parsePropertyValue(path,propertyName,new ByteArrayInputStream(httpBody.getBytes()));
				provider.setPropertyValue(path, propertyName, propertyValue);
				response = new HttpResponse(requestMethod,HttpStatus.OK);
				break;
			case PUT_ELEMENT:
				NameSpacePath ns = new NameSpacePath(request.getXHeader(CimXHeader.NAMESPACE_PATH.toString()));
				InMemoryCache cache = new InMemoryCache();
				// BufferedCache cache = new BufferedCache(provider);
				MOFParser bp = new MOFParser(cache,provider);
				if(logEnabled) {
					System.out.println("--- Put Received ---");
					System.out.println(ns.toString());
					System.out.println(httpBody);
					System.out.println("--- End Received ---");
				}
				bp.parse(new ByteArrayInputStream(httpBody.getBytes()),ns);
				List<NamedElement> elements = cache.getElements(null,null,null,false);
				if(logEnabled) {
					// TODO: The parser seems to insert the Structure definition in the buffered cache
					// in addition to a singleton StructureValue received from the client even though the
					// definition is present in the underlying repository. This is a bug. Need to chase it down.
					System.out.println("--- Parsed Elements ---");
					for(NamedElement el : elements) System.out.println(el.getObjectPath()+"\n"+el.toMOF());
					System.out.println("--- End Parsed ---");
				}
				if(elements != null && !elements.isEmpty()) {
					boolean exists = true;
					for(NamedElement el : elements) {
						exists &= provider.contains(el.getObjectPath());
						provider.put(el);
					}
					response = new HttpResponse(requestMethod,exists ? HttpStatus.OK : HttpStatus.CREATED);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NO_CONTENT);
				}
				cache.shutdown();
				break;
			case DELETE_ELEMENT:
				path = getObjectPath(request);
				// System.out.println("Server Deleting "+path);
				boolean result = provider.delete(path);
				response = result ? new HttpResponse(HttpMethod.GET,HttpStatus.OK) :
					new HttpResponse(requestMethod,HttpStatus.NOT_MODIFIED);
				break;
			case GET_ELEMENTS:
				// note that path is ignored, and is normally "/"
				String elementTypes = request.getXHeader(CimXHeader.ELEMENT_TYPES.toString());
				if("null".equalsIgnoreCase(elementTypes)) elementTypes = null;
				String localNameSpaces = request.getXHeader(CimXHeader.NAME_SPACES.toString());
				if("null".equalsIgnoreCase(localNameSpaces)) localNameSpaces = null;
				String elementNames = request.getXHeader(CimXHeader.ELEMENT_NAMES.toString());
				if("null".equalsIgnoreCase(elementNames)) elementNames = null;
				String locateTypes = request.getXHeader(CimXHeader.LOCATE_SUBCLASS.toString());
				boolean locateSubTypes = "true".equalsIgnoreCase(locateTypes) ? true : false;
				elements = provider.getElements(elementTypes, localNameSpaces, elementNames, locateSubTypes);
				// TODO: This list can be long, so we may need to send it as a chunked response
				b = new StringBuilder();
				NameSpacePath ns0 = null;
				for(NamedElement e : elements){
					NameSpacePath n = e.getNameSpacePath();
					if(!n.equals(ns0)) {
						b.append("#pragma namespace(\"").append(n.getLocalPath()).append("\")\n");
						ns0 = n;
					}
					b.append(e.toMOF());
					switch(e.getElementType()) {
					case STRUCTUREVALUE:
						b.append(";");
					case INSTANCE:
						b.append(CRLF);
						break;
					default:
						break;
					}
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, b.toString());
				break;
			case EXECUTE_QUERY:
				// note that path is ignored, and is normally "/"
				List<StructureValue> resultset = provider.executeQuery(httpBody.trim());
				b = new StringBuilder("");
				ns0 = null;
				for(NamedElement e : resultset){
					NameSpacePath n = e.getNameSpacePath();
					if(!n.equals(ns0)) {
						b.append("#pragma namespace(\"").append(n.getLocalPath()).append("\")\n");
						ns0 = n;
					}
					b.append(e.toMOF());
					switch(e.getElementType()) {
					case STRUCTUREVALUE:
						b.append(";");
					case INSTANCE:
						b.append(CRLF);
						break;
					default:
						break;
					}
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, b.toString());
				break;
			case HAS_LISTENER:
				URL clientURL = new URL(request.getXHeader(CimXHeader.CIM_URL.toString()));
				CimClient client = getClient(clientURL);
				if(client != null){
					CimEventType t = CimEventType.valueOf(request.getXHeader(CimXHeader.EVENT_TYPE.toString()));
					response = provider.hasListener(t, client) ? new HttpResponse(requestMethod,HttpStatus.OK) :
						new HttpResponse(requestMethod,HttpStatus.NOT_FOUND);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case ADD_LISTENER:
				clientURL = new URL(request.getXHeader(CimXHeader.CIM_URL.toString()));
				client = getClient(clientURL);
				if(client != null){
					CimEventType t = CimEventType.valueOf(request.getXHeader(CimXHeader.EVENT_TYPE.toString()));
					response = provider.addListener(t, client) ? new HttpResponse(requestMethod,HttpStatus.OK) :
						new HttpResponse(requestMethod,HttpStatus.NOT_MODIFIED);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case REMOVE_LISTENER:
				clientURL = new URL(request.getXHeader(CimXHeader.CIM_URL.toString()));
				client = getClient(clientURL);
				if(client != null){
					CimEventType t = CimEventType.valueOf(request.getXHeader(CimXHeader.EVENT_TYPE.toString()));
					provider.removeListener(t, client);
					response = new HttpResponse(requestMethod,HttpStatus.OK);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case REGISTER_PROVIDER:
				clientURL = new URL(request.getXHeader(CimXHeader.CIM_URL.toString()));
				client = getClient(clientURL);
				if(client != null){
					provider.registerChildProvider(client);
					response = new HttpResponse(requestMethod,HttpStatus.OK);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case UNREGISTER_PROVIDER:
				clientURL = new URL(request.getXHeader(CimXHeader.CIM_URL.toString()));
				client = getClient(clientURL);
				if(client != null){
					provider.unregisterChildProvider(client);
					response = new HttpResponse(requestMethod,HttpStatus.OK);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case INVOKE_METHOD:
				path = getObjectPath(request);
				String methodName = request.getXHeader(CimXHeader.METHOD_NAME.toString());
				target = provider.get(path);
				if(target == null || methodName == null || methodName.isEmpty()){
					break;	// Object not found
				}
				parser = new MOFParser(provider);				
				switch(target.getElementType()){
				case CLASS:
					CimClass cls = (CimClass) target;
					if(!cls.hasMethod(methodName)){
						response = new HttpResponse(requestMethod,HttpStatus.NOT_FOUND);
						break;
					}
					List<CimParameter> cimParameters = cls.getMethodParameters(methodName);
					parser.parseParameters(cimParameters, new ByteArrayInputStream(httpBody.getBytes()));
					// for(CimParameter p : cimParameters) System.out.println(p.getName()+" = "+p.getValue());
					DataValue returnedValue = cls.invokeMethod(methodName, cimParameters);
					response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, returnedValue.toMOF());
					break;
				case INSTANCE:
					CimInstance ins = (CimInstance) target;
					cimParameters = ins.getMethodParameters(methodName);
					parser.parseParameters(cimParameters, new ByteArrayInputStream(httpBody.getBytes()));
					// for(CimParameter p : cimParameters) System.out.println(p.getName()+" = "+p.getValue());
					returnedValue = ins.invokeMethod(methodName, cimParameters);
					response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, returnedValue.toMOF());
					break;
				default:
					response = new HttpResponse(requestMethod,HttpStatus.NOT_FOUND);
					for(Entry<String, String> e : request.getXHeaders().entrySet()){
						response.addXHeader(e.getKey(), e.getValue());
					}					
					break;
				}
			case SHUT_DOWN:
				response = new HttpResponse(requestMethod,HttpStatus.OK);
				// response.addHeader(HttpHeader.ALLOW, "DELETE,GET,HEAD,OPTIONS,POST,PUT");
				break;
			case SEND_EVENT:
			default:
				response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				response.addXHeader(CimXHeader.INTRINSIC.toString(), cimRequest.toString());
				break;
			}
		} catch (Exception ex){
			if(logEnabled) ex.printStackTrace();
			response = exceptionResponse(requestMethod,cimRequest,ex instanceof ModelException ? 
					(ModelException) ex : new ModelException(ex));
		}
		// not found
		if(response == null) response = new HttpResponse(requestMethod,HttpStatus.NOT_FOUND);
		response.addXHeader(CimXHeader.INTRINSIC.toString(), cimRequest.toString());
		if(logEnabled) logger.info(response.toString());
		return response;
	}

	/**
	 * Get a cimClient that can talk back to a client side receiver
	 * @param clientURL - url for the client
	 * @return cimClient, or Null if the client could not be constructed
	 */
	private CimClient getClient(URL clientURL) {
		if(config != null){
			try {
				String ourHost = config.getHostName();
				int ourPort = config.getServerPort();
				URL ourURL = new URL(config.isSecure() ? "https" : "http",ourHost+":"+ourPort,"/");
				return new CimClient(clientURL,ourURL,config.getProxyHost(),config.getProxyPort());
			} catch (MalformedURLException e) {
				logger.warning("CimHandler - unable to construct server URL "+e.toString());
			}
		} else if(provider.getURL() != null){
			return new CimClient(clientURL,provider.getURL(),null,0);
		}
		return null;
	}

	/**
	 * Get the body string from the request
	 * @param request - input request
	 * @return - body string, if any. Else an empty string is returned
	 * @throws IOException
	 */
	private String getBody(HttpRequest request) throws IOException {
		long contentLength = request.getContentLength();
		// System.out.println(contentLength);
		if(contentLength > 0){
			BufferedInputStream in = request.getInputStream();
			byte [] buf = new byte[(int) contentLength];
			int bytesRead = 0;
			while(bytesRead < contentLength){
				int read = in.read(buf, bytesRead, (int) (contentLength-bytesRead));
				bytesRead += read;
			}
			return new String(buf,0,bytesRead,request.getCharset());
		}
		return "";
	}

	/**
	 * Get the Cim Object path associated with this request
	 * @param request - incoming request
	 * @return - object path for the request
	 */
	private ObjectPath getObjectPath(HttpRequest request){
		ObjectPath path;
		String host = request.getHeader(HttpHeader.HOST);
		String target = request.getRequestTarget();
		if(request.hasXHeader(CimXHeader.OBJECT_PATH.toString())) {
			path = new ObjectPath(request.getXHeader(CimXHeader.OBJECT_PATH.toString()));
		} else {
			path = new ObjectPath(URI.create("http://"+host+target));
		}
		// System.out.println(path.toString());
		return path;
	}
	/**
	 * Get a response mapped from a ModelException
	 * @param method - Http Method in the request
	 * @param header - Cim Header in the request
	 * @param e - ModelException thrown by the underlying provider
	 * @return - response appropriate to the exception
	 */
	private HttpResponse exceptionResponse(HttpMethod method, CimHeader header, ModelException e){
		HttpStatus status = HttpStatus.INTERNAL_ERROR;
		switch(e.getReason()){
		case ACCESS_DENIED:
			status = HttpStatus.FORBIDDEN;
			break;
		case NO_SUCH_PROPERTY:
		case NOT_FOUND:
		case METHOD_NOT_FOUND:
		case METHOD_NOT_AVAILABLE:
			status = HttpStatus.NOT_FOUND;
			break;
		default:
			break;
		}
		return new HttpResponse(method,status,MimeType.PLAINTEXT, e.toString()+CRLF);
	}

	@Override
	public void shutdown() {
		if(provider != null) provider.shutdown();
		return;
	}
}
