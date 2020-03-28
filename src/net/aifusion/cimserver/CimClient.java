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
 * Created Aug 6, 2017 by sharad
 */
package net.aifusion.cimserver;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Vector;
import java.util.logging.Logger;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimEvent;
import net.aifusion.metamodel.CimEventType;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimListener;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.providers.Provider;

/**
 * Class to implement the client side interface for a Cim Server.
 * The client implements a {@linkplain Provider} interface as well as a {@linkplain CimListener} to enable the application to receive server-side events.
 * The client forwards all requests to the server, and does not (currently) cache any data on the client side.
 * @author Sharad Singhal
 */
public class CimClient implements Provider, CimListener {
	/** Logger for this class */
	private static final Logger logger = Logger.getLogger(CimClient.class.getName());
	/** content type to use when sending requests */
	private static final String contentType = "text/mof;charset="+Constants.byteEncoding.name().toLowerCase();
	/** user agent definition */
	private static final String userAgent = "CimClient/1.0";
	/** connection time out */
	private static final int connectionTimeout = 10000;
	/** Server to which this client is talking */
	private URL serverURL;
	/** Proxy to use, if any */
	private Proxy proxy = null;
	/** Debugging flag */
	private boolean debug = false;
	/** Flag to indicate if connections should be kept alive */
	private boolean keepAlive = true;
	/** URL for this client */
	private URL clientURL = null;

	/**
	 * Class to hold request/response values from the server 
	 * @author Sharad Singhal
	 */
	private class CimResponse {
		URL reqUrl;
		String requestMethod;
		private HashMap<String,String> reqHeaders = new HashMap<String,String>();
		String reqBody;
		HttpStatus status = HttpStatus.OK;
		private HashMap<String,String> respHeaders = new HashMap<String,String>();
		String respBody;
		String error;
		@Override
		public String toString() {
			StringBuilder b = new StringBuilder();
			b.append(requestMethod).append(" ").append(reqUrl.toString()).append("\n");
			if(!reqHeaders.isEmpty()){
				for(Entry<String, String> e : reqHeaders.entrySet()){
					b.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
				}
			}
			b.append("ReqBody [").append(reqBody).append("]\n\n");
			b.append("Response [").append(status).append("]\n");
			if(!respHeaders.isEmpty()){
				for(Entry<String, String> e : respHeaders.entrySet()){
					b.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
				}
			}
			b.append("RespBody [").append(respBody).append("]\n");
			b.append("Error [").append(error).append("]\n");
			return b.toString();
		}
		public ExceptionReason getReason() {
			switch(status){
			case NOT_IMPLEMENTED:
				return ExceptionReason.NOT_SUPPORTED;
			default:
				return ExceptionReason.FAILED;
			}
		}
		public boolean hasBody(){
			return !(respBody == null || respBody.isEmpty());
		}
	}

	/**
	 * Create a CimClient using a server URL and an optional proxy
	 * @param serverURL - server to which this client should connect
	 * @param clientURL - the URL of this client (if any) for inbound communication. Null if no inbound connections expected.
	 * @param proxyHost - outbound HTTP proxy host, if any
	 * @param proxyPort - outbound HTTP proxy port, if any
	 */
	public CimClient(URL serverURL, URL clientURL, String proxyHost, int proxyPort){
		this.serverURL = serverURL;
		this.clientURL = clientURL;
		if(proxyHost != null && proxyPort > 0){
			proxy = new Proxy(Proxy.Type.HTTP,new InetSocketAddress(proxyHost,proxyPort));
		}
		// TODO: Add Cookie Store management. Note that by default, HttpURLConnection will use java.net.HttpCookie
		// rather than cimfusion.httpserver.HttpCookie, so at the moment, just use the default in-memory implementation
		CookieHandler.setDefault( new CookieManager( null, CookiePolicy.ACCEPT_ALL ) );
		return;
	}

	/**
	 * Create a new CimClient using a configuration
	 * @param serverURL - CimServer to which this client should connect
	 * @param configuration - configuration for the client. Maybe null if client configuration is not needed for Http
	 */
	public CimClient (URL serverURL, HttpConfiguration configuration){
		this.serverURL = serverURL;
		if(configuration != null){
			String proxyHost = configuration.getProxyHost();
			int proxyPort = configuration.getProxyPort();
			if(proxyHost != null && proxyPort != 0)
				proxy = new Proxy(Proxy.Type.HTTP,new InetSocketAddress(proxyHost,proxyPort));
			// TODO: Add Cookie Store management. Note that by default, HttpURLConnection will use java.net.HttpCookie
			// rather than cimfusion.httpserver.HttpCookie, so at the moment, just use the default in-memory implementation
			CookieHandler.setDefault( new CookieManager( null, CookiePolicy.ACCEPT_ALL ) );

			String hostName = configuration.getHostName();
			int portNumber = configuration.getServerPort();
			if(hostName != null && portNumber > 0){
				try {
					clientURL = new URL(configuration.isSecure() ? "https" : "http",hostName,portNumber,"/");
				} catch (MalformedURLException e) {
					logger.warning("Invalid URL specification for client "+e.toString());
				}
			}
		}
		return;
	}

	/**
	 * Set the keep-alive value for connections by the client
	 * @param keepAlive - true (default) to keep connections alive. False to close connections after each request
	 */
	public void setKeepAlive(boolean keepAlive){
		this.keepAlive = keepAlive;
		return;
	}

	/*
	 * *************************************
	 * Helper Methods
	 * *************************************
	 */

	/**
	 * Get a new connection that is targeted to the server
	 * @param h - CimIntrinsic for this request
	 * @return - connection to use
	 */
	private HttpURLConnection getConnection(CimHeader h){
		return getConnection(h,null);
	}

	/**
	 * Get a new or existing connection to the server for the next request
	 * @param h - CimHeader to use
	 * @param path - path to access. If null, the request is targeted to the server itself instead of the resource defined by path
	 * @return - connection for the next request
	 */
	private HttpURLConnection getConnection(CimHeader h, ObjectPath path){
		// target URL to use
		// URL url = (path == null) ? serverURL : path.getURL(serverURL.getProtocol(), serverURL.getAuthority());
		URL url = serverURL;
		try {
			HttpURLConnection connection = (HttpURLConnection)(proxy != null ? url.openConnection(proxy) : url.openConnection());
			connection.setRequestMethod(h.getHttpMethod().toString());
			connection.setRequestProperty(HttpHeader.HOST.toString(), url.getHost());
			connection.setRequestProperty(HttpHeader.ACCEPT.toString(), MimeType.MOF.getType()+","+MimeType.PLAINTEXT.getType());
			connection.setRequestProperty(HttpHeader.USER_AGENT.toString(), userAgent);
			connection.setRequestProperty(CimXHeader.INTRINSIC.toString(), h.toString());
			if(CimXHeader.OBJECT_PATH.appliesTo(h)) connection.setRequestProperty(CimXHeader.OBJECT_PATH.toString(), path.toString());
			connection.setConnectTimeout(connectionTimeout);
			connection.setReadTimeout(connectionTimeout);
			return connection;
		} catch (IOException e) {
			throw new ModelException("CimClient-Unable to create connection to "+url,e);
		}
	}

	/**
	 * Get a response from the server when no body is being sent
	 * @param connection - connection to use
	 * @return Response from the server
	 */
	private CimResponse getResponse(HttpURLConnection connection){
		return getResponse(connection,null);
	}

	/**
	 * Get a response from the server
	 * @param connection - connection to the server
	 * @param httpBody - body to send to the server
	 * @return - response from the server
	 */
	private CimResponse getResponse(HttpURLConnection connection, String httpBody){
		CimResponse response = new CimResponse();
		boolean doOutput = false;
		String requestMethod = connection.getRequestMethod();
		String close = connection.getRequestProperty(HttpHeader.CONNECTION.toString());
		if(close == null || close.isEmpty()) connection.setRequestProperty(HttpHeader.CONNECTION.toString(), 
				keepAlive ? "keep-alive" : "close");
		if(debug){
			response.requestMethod = requestMethod;
			response.reqBody = httpBody;
			response.reqUrl = connection.getURL();
			Map<String,List<String>> reqHeaders = connection.getRequestProperties();
			for(String key : reqHeaders.keySet()){
				StringBuilder b = new StringBuilder();
				for(String l : reqHeaders.get(key)){
					b.append(l).append(", ");
				}
				if(b.length() > 2) b.setLength(b.length()-2);
				response.reqHeaders.put(key, b.toString());
			}
		}
		try {
			switch(requestMethod){
			case "POST":	// sends output and expects input
			case "PUT":		// sends output, but does not expect input
				doOutput = true;
				connection.setDoOutput(true);
				break;
			case "GET":		// gets input, but does not send output
			case "DELETE":	// no input or output
			case "HEAD":
			case "OPTIONS":
				break;
			default:
				throw new ModelException("CimClient#getResponse(): Method "+connection.getRequestMethod()+" not understood in getResponse()");
			}

			// if we need to do output send it
			if(doOutput){
				byte [] buffer = (httpBody == null) ? new byte[0] : httpBody.getBytes(Constants.byteEncoding);
				connection.setRequestProperty(HttpHeader.CONTENT_LENGTH.toString(), String.valueOf(buffer.length));
				connection.setRequestProperty(HttpHeader.CONTENT_TYPE.toString(), contentType);
				if(debug){
					response.reqHeaders.put(HttpHeader.CONTENT_LENGTH.toString(), String.valueOf(buffer.length));
					response.reqHeaders.put(HttpHeader.CONTENT_TYPE.toString(), contentType);
				}
				connection.connect();
				OutputStream out = connection.getOutputStream();
				if(buffer.length > 0){
					out.write(buffer);
					out.flush();
				}
				out.close();
			} else {
				connection.connect();
			}

			// get status
			// logger.info("CimClient#GetResponse: Get Status\n");
			int responseCode = connection.getResponseCode();
			long contentLength = connection.getContentLengthLong();
			response.status = HttpStatus.lookup(responseCode);
			// logger.info("CimClient#GetResponse: Status = "+response.status+"\n");
			if(debug){
				Map<String,List<String>> respHeaders = connection.getHeaderFields();
				for(String key : respHeaders.keySet()){
					StringBuilder b = new StringBuilder();
					for(String l : respHeaders.get(key)){
						b.append(l).append(", ");
					}
					if(b.length() > 2) b.setLength(b.length()-2);
					response.respHeaders.put(key, b.toString());
				}
			}
			if(responseCode >= 200 && responseCode < 300){
				// logger.info("Content-Length: "+contentLength);
				if(contentLength > 0){
					InputStream in = connection.getInputStream();
					byte [] buf = new byte[(int) contentLength];
					int bytesRead = 0;
					while(bytesRead < contentLength){
						int read = in.read(buf, bytesRead, (int) (contentLength-bytesRead));
						bytesRead += read;
					}
					in.close();
					String contentType = connection.getContentType();
					Charset charSet = contentType != null && contentType.contains("charset=") ?
							Charset.forName(contentType.substring(contentType.indexOf("charset=")+8)) : Constants.byteEncoding;
							response.respBody = new String(buf,0,bytesRead,charSet);
				} else {
					response.respBody = "";
				}
			} else if(responseCode >= 400){
				if(contentLength > 0){
					InputStream in = connection.getErrorStream();
					byte [] buf = new byte[(int) contentLength];
					int bytesRead = 0;
					while(bytesRead < contentLength){
						int read = in.read(buf, bytesRead, (int) (contentLength-bytesRead));
						bytesRead += read;
					}
					in.close();
					String contentType = connection.getContentType();
					Charset charSet = contentType != null && contentType.contains("charset=") ?
							Charset.forName(contentType.substring(contentType.indexOf("charset=")+8)) : Constants.byteEncoding;
							response.error = new String(buf,0,bytesRead,charSet);
				} else {
					response.error = "";
				}
			}
			// else we have a 100 or 300 level code. No input expected...
		} catch (IOException e) {
			logger.info("CimClient#getResponse(): Error Exception - "+e.toString());
			// TODO: deal with the exception
		} finally {
			String closeConnection = connection.getHeaderField(HttpHeader.CONNECTION.toString());
			if("close".equalsIgnoreCase(closeConnection)){
				connection.disconnect();
			}
		}
		if(debug) logger.info(response.toString());
		return response;
	}

	/*
	 * ********************
	 * Provider Interface
	 * ********************
	 */

	@Override
	public List<NameSpacePath> getNameSpaces() {
		HttpURLConnection connection = getConnection(CimHeader.GET_NAMESPACES);
		CimResponse response = getResponse(connection);
		Vector<NameSpacePath> paths = new Vector<NameSpacePath>();
		if(HttpStatus.OK.equals(response.status)){
			String [] vals = response.respBody.split("\\s+");
			for(String v : vals){
				if(v.isEmpty()) continue;
				paths.add(new NameSpacePath(v.trim()));
			}
		}
		return paths;
	}

	@Override
	public NamedElement get(ObjectPath path) {
		// get an existing element
		HttpURLConnection connection = getConnection(CimHeader.GET_ELEMENT, path);
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status) && response.hasBody()){
			BufferedCache cache = new BufferedCache(this);
			MOFParser parser = new MOFParser(cache);
			BufferedReader reader = new BufferedReader(new StringReader(response.respBody));
			parser.parse(reader, path.getNameSpacePath());
			NamedElement element = cache.getBufferedElement(path);
			cache.shutdown();
			return element;
		}
		return null;
	}

	@Override
	public boolean contains(ObjectPath path) {
		HttpURLConnection connection = getConnection(CimHeader.HAS_ELEMENT,path);
		CimResponse response = getResponse(connection);
		return HttpStatus.OK.equals(response.status);
	}

	@Override
	public boolean put(NamedElement element) {
		HttpURLConnection connection = getConnection(CimHeader.PUT_ELEMENT,element.getObjectPath());
		connection.setRequestProperty(CimXHeader.NAMESPACE_PATH.toString(), element.getNameSpacePath().toString());
		StringBuilder b = new StringBuilder();
		b.append(element.toMOF());
		CimResponse response = getResponse(connection,b.toString());
		// response will be
		//  OK if the element exists, and was updated
		//  CREATED if the element did not exist, and was created
		//  // NO_CONTENT if we did not send anything there
		if(HttpStatus.OK.equals(response.status) || HttpStatus.CREATED.equals(response.status)){
			return true;
		}
		return false;
	}

	@Override
	public boolean delete(ObjectPath path) {
		HttpURLConnection connection = getConnection(CimHeader.DELETE_ELEMENT,path);
		CimResponse response = getResponse(connection, null);
		// DELETE will return OK or NOT_MODIFIED
		return HttpStatus.OK.equals(response.status);
	}

	@Override
	public List<NamedElement> getElements(String elementTypes, String localNameSpaces, String elementNames, boolean locateSubTypes) {
		HttpURLConnection connection = getConnection(CimHeader.GET_ELEMENTS,null);
		connection.setRequestProperty(CimXHeader.ELEMENT_TYPES.toString(), elementTypes == null ? "null" : elementTypes);
		connection.setRequestProperty(CimXHeader.NAME_SPACES.toString(), localNameSpaces == null ? "null" : localNameSpaces);
		connection.setRequestProperty(CimXHeader.ELEMENT_NAMES.toString(), elementNames == null ? "null" : elementNames);
		connection.setRequestProperty(CimXHeader.LOCATE_SUBCLASS.toString(), String.valueOf(locateSubTypes));
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)){
			BufferedCache cache = new BufferedCache(this);
			MOFParser parser = new MOFParser(cache);
			parser.parse(new BufferedReader(new StringReader(response.respBody)), Constants.defaultNameSpacePath);
			return cache.getElements(elementTypes, localNameSpaces, elementNames, locateSubTypes);
		}
		return new Vector<NamedElement>();
	}

	@Override
	public void shutdown() {
		// Note that the client cannot shutdown the server.
		// We send an HEAD request to the server with Connection: close to indicate to the server 
		// (and the local HttpClient) that resources can be freed
		HttpURLConnection connection = getConnection(CimHeader.SHUT_DOWN);
		connection.setRequestProperty(HttpHeader.CONNECTION.toString(), "close");
		CimResponse response = getResponse(connection);
		if(!HttpStatus.OK.equals(response.status)){
			logger.warning("CimClient#shutdown() did not return OK from server");
		}
		return;
	}

	@Override
	public boolean addListener(CimEventType type, CimListener listener) {
		HttpURLConnection connection = getConnection(CimHeader.ADD_LISTENER);
		connection.setRequestProperty(CimXHeader.EVENT_TYPE.toString(), type.toString());
		connection.setRequestProperty(CimXHeader.CIM_URL.toString(), listener.getURL().toString());
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)) return true;
		return false;
	}

	@Override
	public void removeListener(CimEventType type, CimListener listener) {
		HttpURLConnection connection = getConnection(CimHeader.REMOVE_LISTENER);
		connection.setRequestProperty(CimXHeader.EVENT_TYPE.toString(), type.toString());
		connection.setRequestProperty(CimXHeader.CIM_URL.toString(), listener.getURL().toString());
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)){
			return;
		}
		throw new ModelException(response.status+" : "+response.error);
	}

	@Override
	public boolean hasListener(CimEventType type, CimListener listener) {
		throw new ModelException("CimClient.hasListener() not yet implemented");
	}

	@Override
	public void registerChildProvider(Provider child) {
		URL childURL = child.getURL();
		if(childURL == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Child provider must have valid URL");
		HttpURLConnection connection = getConnection(CimHeader.REGISTER_PROVIDER);
		connection.setRequestProperty(CimXHeader.CIM_URL.toString(), childURL.toString());
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)) return;
		throw new ModelException(ExceptionReason.FAILED,response.status+":"+response.error);
	}

	@Override
	public void unregisterChildProvider(Provider child) {
		URL childURL = child.getURL();
		if(childURL == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Child provider must have valid URL");
		HttpURLConnection connection = getConnection(CimHeader.UNREGISTER_PROVIDER);
		connection.setRequestProperty(CimXHeader.CIM_URL.toString(), childURL.toString());
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)) return;
		throw new ModelException(ExceptionReason.FAILED,response.error);
	}

	@Override
	public List<String> getPropertyNames(ObjectPath path) {
		HttpURLConnection connection = getConnection(CimHeader.GET_PROPERTY_NAMES,path);
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)){
			Vector<String> propertyNames = new Vector<String>();
			String [] vals = response.respBody.split("\\s+");
			for(String v : vals){
				if(v.isEmpty()) continue;
				propertyNames.add(v.trim());
			}
			return propertyNames;
		}
		throw new ModelException(ExceptionReason.NOT_FOUND,response.error);
	}

	@Override
	public DataType getPropertyType(ObjectPath path, String propertyName) {
		HttpURLConnection connection = getConnection(CimHeader.GET_PROPERTY_TYPE,path);
		connection.setRequestProperty(CimXHeader.PROPERTY_NAME.toString(), propertyName);
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)){
			String dataType = response.respBody.trim().toUpperCase();
			// TODO: This will require parser help if the property is complex
			return DataType.valueOf(dataType);
		}
		throw new ModelException(ExceptionReason.NO_SUCH_PROPERTY,response.error);
	}

	@Override
	public DataValue getPropertyValue(ObjectPath path, String propertyName) {
		HttpURLConnection connection = getConnection(CimHeader.GET_PROPERTY_VALUE,path);
		connection.setRequestProperty(CimXHeader.PROPERTY_NAME.toString(), propertyName);
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)){
			String propertyType = connection.getHeaderField(CimXHeader.PROPERTY_TYPE.toString());
			DataType type = DataType.valueOf(propertyType.toUpperCase());
			// TODO: May need to strip quotes from MOF strings here. Need to check with different
			// primitive strings and arrays
			if(type.isPrimitive()){
				response.respBody = response.respBody.trim();
				return new DataValue(propertyType,response.respBody);
			} else {
				MOFParser parser = new MOFParser(new BufferedCache(this));
				return parser.parsePropertyValue(path, propertyName, new ByteArrayInputStream(response.respBody.getBytes()));
			}
		}
		throw new ModelException(ExceptionReason.NO_SUCH_PROPERTY,response.error);
	}

	@Override
	public void setPropertyValue(ObjectPath path, String propertyName, DataValue propertyValue) {
		HttpURLConnection connection = getConnection(CimHeader.SET_PROPERTY_VALUE,path);
		connection.setRequestProperty(CimXHeader.PROPERTY_NAME.toString(), propertyName);
		connection.setRequestProperty(CimXHeader.PROPERTY_TYPE.toString(), propertyValue.getType().toString());
		CimResponse response = getResponse(connection,propertyValue.toMOF());
		if(HttpStatus.OK.equals(response.status)){
			return;
		}
		throw new ModelException(response.status+":"+response.error);
	}

	@Override
	public List<String> getMethodNames(ObjectPath path) {
		HttpURLConnection connection = getConnection(CimHeader.GET_METHOD_NAMES,path);
		CimResponse response = getResponse(connection);
		if(HttpStatus.OK.equals(response.status)){
			Vector<String> methodNames = new Vector<String>();
			String [] vals = response.respBody.split("\\s+");
			for(String v : vals){
				if(v.isEmpty()) continue;
				methodNames.add(v.trim());
			}
			return methodNames;
		}
		throw new ModelException(ExceptionReason.FAILED,response.error);
	}

	@Override
	public DataType getMethodReturnType(ObjectPath path, String methodName) {
		HttpURLConnection connection = getConnection(CimHeader.GET_METHOD_TYPE,path);
		connection.setRequestProperty(CimXHeader.METHOD_NAME.toString(), methodName);
		CimResponse response = getResponse(connection,null);
		if(HttpStatus.OK.equals(response.status)){
			String dataType = response.respBody.trim().toUpperCase();
			return DataType.valueOf(dataType);
		}
		throw new ModelException(ExceptionReason.METHOD_NOT_FOUND,response.error);
	}

	@Override
	public List<CimParameter> getMethodParameters(ObjectPath path, String methodName) {
		// TODO: This is inefficient.. need a better way of getting parameters
		NamedElement element = get(path);
		if(element == null) throw new ModelException(ExceptionReason.NOT_FOUND,"No such element: "+path.toString());
		ElementType t = element.getElementType();
		switch(t){
		case CLASS:
		case INTERFACE:
			CimClass clas = (CimClass)element;
			return clas.getMethodParameters(methodName);
		case INSTANCE:
			return ((CimInstance)element).getCreationClass().getMethodParameters(methodName);
		default:
			throw new ModelException(ExceptionReason.INVALID_CLASS,path.toString()+"is not a class"); 
		}
	}

	@Override
	public DataValue invokeMethod(ObjectPath path, String methodName, List<CimParameter> methodParameters) {
		HttpURLConnection connection = getConnection(CimHeader.INVOKE_METHOD,path);
		connection.setRequestProperty(CimXHeader.METHOD_NAME.toString(), methodName);
		StringBuilder b = new StringBuilder();
		for(CimParameter p : methodParameters){
			b.append(p.getValue().toMOF()).append(",");
		}
		b.setLength(b.length()-1);
		if(debug) System.out.println(b.toString());
		CimResponse response = getResponse(connection, b.toString());
		if(HttpStatus.OK.equals(response.status)){
			// TODO: This requires parser modifications
			// parse the returned data value from the method invocation
			throw new ModelException(ExceptionReason.NOT_SUPPORTED,response.toString());
		}
		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE);
	}

	@Override
	public List<StructureValue> executeQuery(String query) {
		HttpURLConnection connection = getConnection(CimHeader.EXECUTE_QUERY);
		CimResponse response = getResponse(connection,query);
		if(HttpStatus.OK.equals(response.status)){
			Vector<StructureValue> instances = new Vector<StructureValue>();
			if(response.respBody != null && response.respBody.length() > 0){
				BufferedCache cache = new BufferedCache(this);
				MOFParser parser = new MOFParser(cache);
				parser.parse(new BufferedReader(new StringReader(response.respBody)), Constants.defaultNameSpacePath);
				for(NamedElement e : cache.getElements("instance", null, null, false)){
					instances.add((CimInstance) e);
				}
			}
			return instances;
		}
		throw new ModelException(response.status+":"+response.error);
	}

	@Override
	public void notify(CimEvent event) {
		HttpURLConnection connection = getConnection(CimHeader.SEND_EVENT);
		CimResponse response = getResponse(connection,event.toString());
		if(HttpStatus.OK.equals(response.status)){
			return;
		}
		throw new ModelException(response.getReason(),response.error);
	}

	@Override
	public Repository getRepository() {
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,"CimClient.getRepository() not supported");
	}

	@Override
	public URL getURL() {
		return clientURL;
	}

	@Override
	public int hashCode() {
		return serverURL.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof CimClient)) return false;
		CimClient other = (CimClient) obj;
		return serverURL.equals(other.serverURL);
	}
}
