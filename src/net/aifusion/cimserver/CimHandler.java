/**
 * Copyright 2017,2025 Sharad Singhal, All Rights Reserved
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
 * Created Jul 16, 2017 by Sharad Singhal
 * Last Modified March 10, 2025 by Sharad Singhal
 */
package net.aifusion.cimserver;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Logger;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimEventType;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.FqlFilter;
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
 * Class to Handle CIM Requests directed at an underlying provider. This handler provides server-side processing for the CIM server
 * @author Sharad Singhal
 */
class CimHandler implements HttpRequestHandler {
	/** Logger for this class */
	private static Logger logger = Logger.getLogger(CimHandler.class.getName());
	// provider information can be accessed by subclasses-- currently only for testing (see TestHandler}
	/** Underlying default Cim Provider */
	protected Provider defaultProvider;
	/** Providers based on end points */
	protected HashMap<String,Provider> providers = new HashMap<String,Provider>();


	/** Configuration, if any */
	private HttpConfiguration config = null;
	/** Line terminator */
	private static final String CRLF = "\r\n";
	/** flag to indicate logging */
	private boolean logEnabled = false;

	/**
	 * Create a CimHandler from a server configuration
	 * @param config server configuration to use
	 */
	public CimHandler(HttpConfiguration config) {
		try {
			this.config = config;
			// load default provider for this handler
			String r = config.getRepository();
			Repository repo = (r == null) ? new InMemoryCache() : new PersistentCache(r);
			URI uri = new URI("http://127.0.0.1/");
			System.out.println(uri.toString());
			defaultProvider = config.getProvider() != null ? loadProvider(config.getProvider(),uri,repo) : new BasicProvider(repo,uri);
			// any additional providers if defined
			if(config.getProviderNames() != null) {
				for(String p : config.getProviderNames()) {
					// serverEndpoint|providerClassName[|repositoryName]
					String [] v = p.split("|");
					String endpoint = v[0];
					String providerClassName = v.length > 1 ? v[1] : BasicProvider.class.getName();
					URI u = new URI(config.isSecure()?"https":"http",null,config.getHostName(),config.getServerPort(),endpoint,null,null);
					Repository rep = v.length > 2 ? new PersistentCache(v[2]) : new InMemoryCache();
					Provider prov = loadProvider(providerClassName, u, rep);
					providers.put(endpoint, prov);
				}
			}
		} catch (URISyntaxException e) {
			throw new ModelException("Unable to instantiate CimHandler because of invalid URI",e);
		}
		return;
	}

	/**
	 * Load a provider
	 * @param provider - name of the class to be loaded. Must implement Provider interface
	 * @param uri - server uri at which this provider is accessible
	 * @param repo - repository to use for the provider
	 * @return - the provider instance
	 */
	private Provider loadProvider(String provider,URI uri, Repository repo) {
		try {
			ClassLoader loader = CimHandler.class.getClassLoader();
			Class<?> providerClass = loader.loadClass(provider);
			if(Provider.class.isAssignableFrom(providerClass)){
				try {
					Constructor<?> constructor = providerClass.getConstructor(Repository.class, URI.class);
					return (Provider) constructor.newInstance(repo,uri);
				} catch (NoSuchMethodException | SecurityException | IllegalArgumentException | InvocationTargetException e) {
				//	try {
				//		return (Provider) providerClass.getDeclaredConstructor().newInstance();
				//	} catch (IllegalArgumentException | InvocationTargetException | SecurityException e1) {
						throw new ModelException("Unable to locate a provider for "+provider,e);
				//	}
				}
			} else {
				throw new ModelException(provider+" is not a Provider");
			}
		} catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
			throw new ModelException(ExceptionReason.INVALID_CLASS,"CimHandler: Unable to load class "+provider, e);
		}
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
		// locate the provider to use
		Provider requestProvider = defaultProvider;
		try {
			String endpoint = new URI(request.getURI()).getPath();
			if(providers.containsKey(endpoint)) {
				requestProvider = providers.get(endpoint);
			}
		} catch (URISyntaxException e) {
			return new HttpResponse(requestMethod,HttpStatus.BAD_REQUEST,MimeType.PLAINTEXT,
					cimRequest+" has bad target "+request.getURI());
		}		
		// null response implies that HttpStatus.NOT_FOUND will be sent to client
		HttpResponse response = null;
		try {
			String httpBody = getBody(request);
			switch(cimRequest){
			case GET_NAMESPACES:	// all name spaces, one per line
				List<NameSpacePath> names = requestProvider.getNameSpaces();
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
				NamedElement element = requestProvider.get(path);
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
			case HAS_ELEMENT:	// return OK/NOT found
				path = getObjectPath(request);
				boolean bv = requestProvider.contains(path);
				response = bv ? new HttpResponse(requestMethod,HttpStatus.OK) :
					new HttpResponse(requestMethod,HttpStatus.NOT_FOUND);
				break;
			case GET_PROPERTY_NAMES:	// all property names, one per line
				path = getObjectPath(request);
				List<String> pNames = requestProvider.getPropertyNames(path);
				b = new StringBuilder();
				for(String name : pNames){
					b.append(name).append(CRLF);
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, b.toString());
				break;
			case GET_PROPERTY_TYPE:	// type of a given property
				String propertyName = request.getXHeader(CimXHeader.PROPERTY_NAME.toString());
				path = getObjectPath(request);
				DataType type = requestProvider.getPropertyType(path, propertyName);
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, type.toString()+CRLF);
				break;
			case GET_PROPERTY_VALUE:	// mof value of a given property
				propertyName = request.getXHeader(CimXHeader.PROPERTY_NAME.toString());
				path = getObjectPath(request);
				DataValue v = requestProvider.getPropertyValue(path, propertyName);
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, v.toMOF());
				response.addXHeader(CimXHeader.PROPERTY_TYPE.toString(),v.getType().toString());
				break;
			case GET_METHOD_NAMES:	// names of methods, if any
				path = getObjectPath(request);
				pNames = requestProvider.getMethodNames(path);
				b = new StringBuilder();
				for(String name : pNames){
					b.append(name).append(CRLF);
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, b.toString());
				break;
			case GET_METHOD_TYPE:	// return type of the method
				propertyName = request.getXHeader(CimXHeader.METHOD_NAME.toString());
				path = getObjectPath(request);
				type = requestProvider.getMethodReturnType(path, propertyName);
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.PLAINTEXT, type.toString()+CRLF);
				break;
			case GET_METHOD_PARAMETERS:	// method parameters, one per line
				propertyName = request.getXHeader(CimXHeader.METHOD_NAME.toString());
				path = getObjectPath(request);
				List<CimParameter> pm = requestProvider.getMethodParameters(path, propertyName);
				b = new StringBuilder();
				for(CimParameter p : pm){
					b.append(p.toMOF()).append(CRLF);
				}
				response = new HttpResponse(requestMethod,HttpStatus.OK,MimeType.MOF, b.toString()+CRLF);
				break;
			case SET_PROPERTY_VALUE:	// set property value
				path = getObjectPath(request);
				propertyName = request.getXHeader(CimXHeader.PROPERTY_NAME.toString());
				NamedElement target = requestProvider.get(path);
				if(target == null || propertyName == null){
					response = new HttpResponse(requestMethod,HttpStatus.NOT_FOUND,MimeType.PLAINTEXT,path+" ["+propertyName+"]");
					break;
				}

				MOFParser parser = new MOFParser(requestProvider);
				DataValue propertyValue = parser.parsePropertyValue(path,propertyName,new ByteArrayInputStream(httpBody.getBytes()));
				requestProvider.setPropertyValue(path, propertyName, propertyValue);
				response = new HttpResponse(requestMethod,HttpStatus.OK);
				break;
			case PUT_ELEMENT:
				NameSpacePath ns = new NameSpacePath(request.getXHeader(CimXHeader.NAMESPACE_PATH.toString()));
				InMemoryCache cache = new InMemoryCache();
				MOFParser bp = new MOFParser(cache,requestProvider);
				bp.parse(new ByteArrayInputStream(httpBody.getBytes()),ns);
				List<NamedElement> elements = cache.getElements(null,null,null,false);
				// NOTE: the cache will automatically add structure/class definitions
				// into elements if only StructurValues/Instances are received
				if(elements != null && !elements.isEmpty()) {
					boolean exists = true;
					for(NamedElement el : elements) {
						exists &= requestProvider.contains(el.getObjectPath());
						requestProvider.put(el);
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
				boolean result = requestProvider.delete(path);
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
				elements = requestProvider.getElements(elementTypes, localNameSpaces, elementNames, locateSubTypes);
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
				List<StructureValue> resultset = requestProvider.executeQuery(httpBody.trim());
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
				URL clientURL = new URI(request.getXHeader(CimXHeader.CIM_URL.toString())).toURL();
				CimClient serverSideclient = getServerSideClient(clientURL,requestProvider.getroviderEndpoint().getPath());
				if(serverSideclient != null){
					CimEventType t = CimEventType.valueOf(request.getXHeader(CimXHeader.EVENT_TYPE.toString()));
					response = requestProvider.hasListener(t, serverSideclient) ? new HttpResponse(requestMethod,HttpStatus.OK) :
						new HttpResponse(requestMethod,HttpStatus.NOT_FOUND);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case ADD_LISTENER:
				clientURL = new URI(request.getXHeader(CimXHeader.CIM_URL.toString())).toURL();
				serverSideclient = getServerSideClient(clientURL,requestProvider.getroviderEndpoint().getPath());
				if(serverSideclient != null){
					CimEventType t = CimEventType.valueOf(request.getXHeader(CimXHeader.EVENT_TYPE.toString()));
					response = requestProvider.addListener(t, serverSideclient) ? new HttpResponse(requestMethod,HttpStatus.OK) :
						new HttpResponse(requestMethod,HttpStatus.NOT_MODIFIED);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case REMOVE_LISTENER:
				clientURL = new URI(request.getXHeader(CimXHeader.CIM_URL.toString())).toURL();
				serverSideclient = getServerSideClient(clientURL,requestProvider.getroviderEndpoint().getPath());
				if(serverSideclient != null){
					CimEventType t = CimEventType.valueOf(request.getXHeader(CimXHeader.EVENT_TYPE.toString()));
					requestProvider.removeListener(t, serverSideclient);
					response = new HttpResponse(requestMethod,HttpStatus.OK);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case REGISTER_PROVIDER:
				clientURL = new URI(request.getXHeader(CimXHeader.CIM_URL.toString())).toURL();
				serverSideclient = getServerSideClient(clientURL,requestProvider.getroviderEndpoint().getPath());
				if(serverSideclient != null){
					requestProvider.registerChildProvider(serverSideclient);
					response = new HttpResponse(requestMethod,HttpStatus.OK);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case UNREGISTER_PROVIDER:
				clientURL = new URI(request.getXHeader(CimXHeader.CIM_URL.toString())).toURL();
				serverSideclient = getServerSideClient(clientURL,requestProvider.getroviderEndpoint().getPath());
				if(serverSideclient != null){
					requestProvider.unregisterChildProvider(serverSideclient);
					response = new HttpResponse(requestMethod,HttpStatus.OK);
				} else {
					response = new HttpResponse(requestMethod,HttpStatus.NOT_IMPLEMENTED);
				}
				break;
			case INVOKE_METHOD:
				path = getObjectPath(request);
				String methodName = request.getXHeader(CimXHeader.METHOD_NAME.toString());
				target = requestProvider.get(path);
				if(target == null || methodName == null || methodName.isEmpty()){
					break;	// Object not found
				}
				parser = new MOFParser(requestProvider);				
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
			case FILTER:
				path = new ObjectPath(request.getXHeader(CimXHeader.OBJECT_PATH.toString()));
				FqlFilter filter = new FqlFilter(request.getXHeader(CimXHeader.FILTER_STRING.toString()));
				resultset = requestProvider.filter(path,filter);
				b = new StringBuilder("");
				ns0 = null;
				for(StructureValue e : resultset){
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
	 * Get a server-side cimClient that can talk back to a client side receiver
	 * @param clientURL - url for the client
	 * @param localPath - our end point for the current provider
	 * @return cimClient, or Null if the client could not be constructed
	 */
	private CimClient getServerSideClient(URL clientURL,String localPath) {
		if(config != null){
			try {
				String uri = (config.isSecure() ? "https://" : "http://")+config.getHostName()+":"+config.getServerPort()+localPath;
				URI ourURI = new URI(uri);
				return new CimClient(clientURL,ourURI,config.getProxyHost(),config.getProxyPort());
			} catch (URISyntaxException e) {
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Unable to construct client to respond");
			}
		} else if(defaultProvider.getroviderEndpoint() != null){
			return new CimClient(clientURL,defaultProvider.getroviderEndpoint(),config.getProxyHost(),config.getProxyPort());
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
		// System.out.println(path.toURL());
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
		case ALREADY_EXISTS:
			status = HttpStatus.CONFLICT;
			break;
		case ACCESS_DENIED:
			status = HttpStatus.FORBIDDEN;
			break;
		case NO_SUCH_PROPERTY:
		case NOT_FOUND:
		case METHOD_NOT_FOUND:
		case METHOD_NOT_AVAILABLE:
			status = HttpStatus.NOT_FOUND;
			break;
		case INVALID_PARAMETER:
			status = HttpStatus.BAD_REQUEST;
			break;
		default:
			break;
		}
		return new HttpResponse(method,status,MimeType.PLAINTEXT, e.toString()+CRLF);
	}

	@Override
	public void shutdown() {
		if(defaultProvider != null) defaultProvider.shutdown();
		if(!providers.isEmpty()) {
			for(Provider p : providers.values()) {
				p.shutdown();
			}
		}
		return;
	}
}
