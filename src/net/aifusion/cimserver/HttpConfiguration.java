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
 * Created Jan 29, 2017 by Sharad Singhal
 * Last Modified March 13, 2020 by Sharad Singhal
 */
package net.aifusion.cimserver;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Vector;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimStructure;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.utils.Java2Cim;

/**
 * Class to represent a Cim server configuration
 * @author Sharad Singhal
 */
@Export(qualifiers="Description(\"Http Configuration\"),Version(\""+HttpConfiguration.version+"\")")
public class HttpConfiguration {
	/** Default configuration Key to use */
	private static final String defaultID = "defaultConfig";
	/** Default host name */
	private static final String defaultHost = Constants.defaulAuthority.substring(0, Constants.defaulAuthority.indexOf(":"));
	/** Default host port */
	private static final int defaultPort = Integer.parseInt(Constants.defaulAuthority.substring(Constants.defaulAuthority.indexOf(":")+1));
	/** Use HTTPS for connections */
	private static final boolean defaultSecure = false;
	/** Time to wait on idle connection (ms) */
	private static final int defaultTimeout = 5000;
	/** Maximum concurrent sessions. 0 implies no limit */
	private static final int defaultMaxSessions = 0;
	/** Default certificate store */
	private static final String defaultKeyStore = "resources/keyStore.jks";
	/** Trusted certificates */
	private static final String defaultTrustStore = "resources/trustStore.jks";
	/** Default server identity */
	private static final String defaultX500Principal = "CN="+defaultHost+", OU=aifusion.com, O=aifusion, C=US, L=Milpitas, ST=California";
	/** Default handler for responses */
	private static final String defaultHandler = "CimHandler";
	/** Default CIM repository. Null implies an in-memory cache */
	private static final String defaultRepository = null;
	/** Default location for storing cookies */
	private static final String defaultCookieStore = null;
	/** Server Configuration class version */
	protected static final String version = "2.1.0";
	/** Default configuration directory */
	private static final String defaultConfigDirectory = "resources/config";

	/** identifier for this configuration */
	private String id = defaultID;
	/** Host name for the server */
	private String hostName = defaultHost;
	/** Server TCP port */
	private int port = defaultPort;
	/** true if server is secure */
	private boolean isSecure = defaultSecure;
	/** server socket time out in ms */
	private int timeout = defaultTimeout;
	/** maximum sessions, 0 implies no maximum */
	private int maxSessions = defaultMaxSessions;
	/** Name of the repository to use */
	private String repositoryName = defaultRepository;
	/** Name of the key store to use, if any. The key store contains the server credentials */
	private String keyStoreName = defaultKeyStore;
	/** Name of the trust store to use, if any. The trust store contains trusted foreign certificates */
	private String trustStoreName = defaultTrustStore;
	/** Key store password to use */
	private String keyStorePassword = null;
	/** Trust store password to use */
	private String trustStorePassword = null;
	/** X500 Principal name for this server */
	private String x500Principal = defaultX500Principal;
	/** request handler for the server */
	private String requestHandler = defaultHandler;
	/** directory to store cookies */
	private String cookieStore = defaultCookieStore;
	/** proxy host, if any */
	private String proxyHost = defaultHost;
	/** proxy port, if any. 0 implies no proxy being used */
	private int proxyPort = 0;
	/** Provider to use in the CIM Handler */
	private String providerName = null;

	/**
	 * Create a default configuration
	 */
	public HttpConfiguration(){
		return;
	}

	/**
	 * Create a CIMServer configuration
	 * @param configuration - configuration to use. If null, a default configuration is used
	 * @see HttpConfiguration#getConfiguration(String, String, String)
	 */
	public HttpConfiguration(StructureValue configuration) {
		if(configuration != null){
			for(String pName : configuration.getPropertyNames()){
				DataValue v = configuration.getPropertyValue(pName);
				// System.out.println(pName+" ["+v+"]");
				if(v == null) continue;
				switch(pName.toLowerCase()){
				case "id":
					id = v.toString();
					break;
				case "hostname":
					hostName = v.toString();
					break;
				case "serverport":
					port =  (Integer) v.getValue();
					break;
				case "proxyhost":
					 proxyHost = v.toString();
					break;
				case "proxyport":
					proxyPort =  (Integer) v.getValue();
					break;
				case "servertimeout":
					timeout =  (Integer) v.getValue();
					break;
				case "maxsessions":
					maxSessions =  (Integer) v.getValue();
					break;
				case "secure":
					isSecure = (Boolean) v.getValue();
					break;
				case "keystore":
					keyStoreName = v.toString();
					break;
				case "truststore":
					trustStoreName = v.toString();
					break;
				case "cookiestore":
					cookieStore = v.toString();
					break;
				case "keystorepassword":
					keyStorePassword = v.toString();
					break;
				case "truststorepassword":
					trustStorePassword = v.toString();
					break;
				case "x500principal":
					x500Principal = v.toString();
					break;
				case "requesthandler":
					requestHandler = v.toString();
					break;
				case "repository":
					repositoryName = v.toString();
					break;
				case "provider":
					providerName = v.toString();
					break;
				default:
					throw new ModelException("HttpConfiguration- Property "+pName+" not yet handled");
				}
			}
		}
		/* create the keystore and trust stores if we are given that info
		if(keyStoreName != null && trustStoreName != null && keyStorePassword != null && trustStorePassword != null){
			try {
				KeyStore keyStore = Credentials.getKeyStore(keyStoreName,keyStorePassword.toCharArray());
				KeyStore trustStore = Credentials.getKeyStore(trustStoreName, trustStorePassword.toCharArray());
				// initialize with a newly created credential if the hostname does not exist in the stores
				if(!keyStore.containsAlias(hostName)){
					// generate a key pair
					KeyPair keyPair = EncryptionUtility.generatePublicKeyPair();

					// create self-signed credentials
					long validFor = 365 * 24 * 60 * 60;	// valid for a year
					X500Principal server = new X500Principal(x500Principal);
					X500PrivateCredential authCredential = Credentials.generateCredentials(keyPair, validFor, server);
					X509Certificate cert = authCredential.getCertificate();

					// save the private credentials in the keyStore, and persist it
					keyStore.setKeyEntry(hostName, keyPair.getPrivate(), keyStorePassword.toCharArray(),new Certificate[]{cert});
					FileOutputStream fout = new FileOutputStream(keyStoreName);
					keyStore.store(fout,keyStorePassword.toCharArray());
					fout.close();

					// save the server certificate in the trustStore (we trust ourselves), and persist it
					trustStore.setCertificateEntry(hostName, cert);
					fout = new FileOutputStream(trustStoreName);
					keyStore.store(fout,trustStorePassword.toCharArray());
					fout.close();
				}
			} catch(KeyStoreException | IOException | NoSuchAlgorithmException | CertificateException e){
				throw new ModelException("ServerConfiguration- unable to create keystore "+keyStoreName,e);
			}
		}
		*/
		return;
	}

	/**
	 * Get the configuration ID (unique identifier for the configuration)
	 * @return - configuration ID 
	 */
	@Export(qualifiers="Description(\"Configuration ID\"),Key",defaultValue="\""+defaultID+"\"")
	public String getId(){
		return id;
	}

	/**
	 * Get the Host name for the server
	 * @return - host name for the server
	 */
	@Export(qualifiers="Description(\"Host Name\")")
	public String getHostName(){
		return hostName;
	}
	
	/**
	 * Get the name of the repository to use
	 * @return - name of the repository. Null if none defined
	 */
	@Export(qualifiers="Description(\"Repository Name\")")
	public String getRepository(){
		return repositoryName;
	}

	/**
	 * Get the server port
	 * @return - server port for the server
	 */
	@Export(qualifiers="Description(\"Server Port\")")
	public int getServerPort(){
		return port;
	}

	/**
	 * Get the server time out
	 * @return - time out in ms
	 */
	@Export(qualifiers="Description(\"Server Time Out (ms). If 0, no timeout enforced\")")
	public int getServerTimeout(){
		return timeout;
	}

	/**
	 * Get maximum sessions accepted by server. 0 for no maximums
	 * @return - maximum sessions accepted
	 */
	@Export(qualifiers="Description(\"Maximum sessions accepted by server. if 0, no maximum enforced\")")
	public int getMaxSessions(){
		return maxSessions;
	}

	/**
	 * Check if the server uses https
	 * @return - true if the server should use https, false otherwise
	 */
	@Export(qualifiers="Description(\"Flag to indicate if server is secure (i.e., uses https)\")")
	public boolean isSecure(){
		return isSecure;
	}
	
	/**
	 * Get the name of the cookie store to use for managing cookies, if any
	 * @return - name of the cookie store to use
	 */
	@Export(qualifiers="Description(\"Name of Cookie Store to use, if any\")")
	public String getCookieStore(){
		return cookieStore;
	}

	/**
	 * Get the name of the key store to use for managing known keys, if any
	 * @return - name of the key store to use
	 */
	@Export(qualifiers="Description(\"Name of key store to use, if any\")")
	public String getKeyStore(){
		return keyStoreName;
	}

	/**
	 * Get the password used to protect the key store, if any
	 * @return - password for the key store. Null if none defined
	 */
	@Export(qualifiers="Description(\"password for the key store to use, if any\")")
	public String  getKeyStorePassword() {
		return keyStorePassword;
	}

	/**
	 * Get the name of the trusted certificate store to use, if any
	 * @return - name of the trusted certificate store to use
	 */
	@Export(qualifiers="Description(\"Name of trust store to use, if any\")")
	public String getTrustStore(){
		return trustStoreName;
	}

	/**
	 * Get the password used to protect the trust store, if any
	 * @return - password for the trust store. Null if none defined
	 */
	@Export(qualifiers="Description(\"password for the trust store to use, if any\")")
	public String getTrustStorePassword() {
		return trustStorePassword;
	}

	/**
	 * Get the X500 Principal distinguished name for server credentials
	 * @return - X500 name used by the server
	 */
	@Export(qualifiers="Description(\"X500 principal distinguished name used for server credentials\")")
	public String getX500Principal() {
		return x500Principal;
	}

	/**
	 * Get the name of the request handler class used by this server
	 * @return - name of the request handler class to use
	 */
	@Export(qualifiers="Description(\"Name of the request handler class to use\")")
	public String getRequestHandler() {
		return requestHandler;
	}
	
	/**
	 * Get the Host name for a proxy, if any defined
	 * @return - proxy name for the client
	 */
	@Export(qualifiers="Description(\"Proxy Name\")")
	public String getProxyHost(){
		return proxyHost;
	}
	
	/**
	 * Get the server port
	 * @return - server port for the server
	 */
	@Export(qualifiers="Description(\"Proxy Port\")")
	public int getProxyPort(){
		return proxyPort;
	}
	
	/**
	 * Get the CIM provider used in CimHandler
	 * @return - name of the class to use in CimHandler
	 */
	@Export(qualifiers="Description(\"Name of the provider to use in the handler class\")")
	public String getProvider(){
		return providerName;
	}
	
	/**
	 * Get a configuration corresponding to given id (must not be null)
	 * @param id - identity for the configuration. Default is used if null
	 * @param nameSpace - nameSpace for the configuration. Default is used if null
	 * @param directory - configuration directory. Default is used if null
	 * @return - Server configuration. Null if no such configuration exists
	 */
	public static HttpConfiguration getConfiguration(String id,String nameSpace, String directory){
		String repo = (directory == null) ? defaultConfigDirectory : directory;
		NameSpacePath path = (nameSpace == null) ? Constants.defaultNameSpacePath : new NameSpacePath(nameSpace);
		String configId = (id == null) ? defaultID : id;
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("ID", new DataValue(configId));
		ObjectPath configPath = new ObjectPath(ElementType.STRUCTUREVALUE,"AIFusion_HttpConfiguration",path,keys, null);
		PersistentCache repository = new PersistentCache(repo);
		if(repository.contains(configPath)){
			StructureValue c = (StructureValue) repository.get(configPath);
			repository.shutdown();
			return new HttpConfiguration(c);
		}
		repository.shutdown();
		return null;
	}

	/**
	 * print out help on the standard output
	 */
	private static void showHelp(){
		System.out.println("Use:\n$ HttpConfiguration options\nwhere options can be:");
		System.out.println("\t-help | -Help				# show help (this message) (must be only argument)");
		System.out.println("\t-r repository				# Name of the repository to use [repsitory]");
		System.out.println("\t-n namespacePath			# NameSpacePath to use [aifusion]");
		System.out.println("\t-c configurationId		# ConfigurationID to use [defaultID]");
		System.out.println("\t-cp configPath			# Path to the configuration directory [resources/config]");
		System.out.println("\t-hostname host name		# server host name [localhost]");
		System.out.println("\t-serverport port			# server port number [8085]");
		System.out.println("\t-proxyhost hostname		# proxy host name [null]");
		System.out.println("\t-proxyport port			# server port number [8080]");
		System.out.println("\t-servertimeout timeout	# server time out [5000]");
		System.out.println("\t-maxsessions sessions		# maximum sessions [0]");
		System.out.println("\t-secure [true|false]		# server uses https [false]");
		System.out.println("\t-keystore keystore		# keystore to use [keystore.jks]");
		System.out.println("\t-cookiestore cookiestore	# cookie directory to use [null]");
		System.out.println("\t-truststore truststore	# truststore to use [truststore.jks]");
		System.out.println("\t-keystorepassword pass	# keystore password [null]");
		System.out.println("\t-truststorepassword pass	# truststore passord [null]");
		System.out.println("\t-x500principal dn			# x500Principal DN for server [CN=localhost,OU=cimfusion.com,O=cimfusion,C=US,L=Cupertino,ST=California]");
		System.out.println("\t-requesthandler className	# default request handler [CimHandler]");
		System.out.println("\t-provider className		# default provider to use in the handler [BasicProvider]");
		return;
	}
	/**
	 * Create an Http configuration.
	 * @param args - Arguments are
	 * <dl>
	 * <dt>-help | -Help</dt><dd> show help (must be only argument)</dd>
	 * <dt>-repository repositoryName</dt><dd>Name of the repository to use [repository]</dd>
	 * <dt>-n namespacePath</dt><dd>NameSpacePath to use [aifusion]</dd>
	 * <dt>-id configurationId</dt><dd>ConfigurationID to use [defaultID]</dd>
	 * <dt>-cp configPath</dt><dd>Configuration directory [resources/config]</dd>
	 * <dt>-hostname host name</dt><dd>server host name [localhost]</dd>
	 * <dt>-serverport port</dt><dd>server port number [8085]</dd>
	 * <dt>-servertimeout timeout</dt><dd>server time out in ms. 0 implies no limit [5000]</dd>
	 * <dt>-maxsessions sessions</dt><dd>maximum sessions. 0 implies no limit [0]</dd>
	 * <dt>-secure [true|false]</dt><dd>server uses https [false]</dd>
	 * <dt>-cookiestore cookieStore</dt><dd>cookieStore to use [resources/cookies]</dd>
	 * <dt>-keystore keystore</dt><dd>keystore to use [keystore.jks]</dd>
	 * <dt>-truststore truststore</dt><dd>truststore to use [truststore.jks]</dd>
	 * <dt>-keystorepassword pass</dt><dd>keystore password [null]</dd>
	 * <dt>-truststorepassword pass</dt><dd>truststore password [null]</dd>
	 * <dt>-x500principal dn</dt><dd>x500Principal DN for server [CN=localhost,OU=cimfusion.com,O=cimfusion,C=US,L=Belmont,ST=California]</dd>
	 * <dt>-requesthandler className</dt><dd>default request handler [CimHandler]</dd>
	 * <dt>-proxyHost hostName</dt><dd>proxy host [null]</dd>
	 * <dt>-proxyPort port</dt><dd>proxy port [8080]</dd>
	 *
	 * </dl>
	 */
	public static void main(String [] args){
		// check if we need to print out help
		if(args.length == 1 && (args[0].startsWith("-h") || args[0].startsWith("-H"))){
			showHelp();
			return;
		}
		// get default properties
		HashMap<String,DataValue> propertyValues = new HashMap<String,DataValue>();
		HttpConfiguration conf = new HttpConfiguration();
		for(Method m : conf.getClass().getDeclaredMethods()){
			if((m.getModifiers() & Modifier.STATIC) != 0) continue;
			if(!m.isAnnotationPresent(Export.class)) continue; // ignore un-exported methods
			String pName = m.getName().startsWith("get") ? m.getName().substring(3) : m.getName().substring(2);
			try {
				Object retValue = m.invoke(conf, (Object[])null);
				DataType t = DataType.getTypeForClass(m.getReturnType());
				propertyValues.put(pName.toLowerCase(), new DataValue(t,retValue));
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				e.printStackTrace();
				return;
			}
		}
		
		// get the arguments from the command line to construct the configuration
		HashMap<String,String> options = new HashMap<String,String>();
		Vector<String> files = new Vector<String>();
		getArgs(args,options,files);
		
		String repo = options.containsKey("cp") ? options.get("cp") : defaultConfigDirectory;
		NameSpacePath path = options.containsKey("n") ? new NameSpacePath(options.get("n")) : Constants.defaultNameSpacePath;
		
		PersistentCache repository = new PersistentCache(repo);
		
		// get the configuration class
		ObjectPath configClassPath = new ObjectPath(ElementType.STRUCTURE,"aifusion_HttpConfiguration",path,null, null);
		CimStructure configClass = repository.contains(configClassPath) ? (CimStructure) repository.get(configClassPath) : 
			(CimStructure) Java2Cim.getModelForClass(HttpConfiguration.class, repository);
		
		// get the configuration instance
		for(Entry<String,String> entry : options.entrySet()){
			String key = entry.getKey();
			String value = entry.getValue();
			// System.out.println("<"+key+" ::= "+value+">");
			if(configClass.hasProperty(key)){
				// System.out.println("Add <"+key+" ::= "+value+">");
				propertyValues.put(key.toLowerCase(), new DataValue(configClass.getPropertyType(key).toString(),value));
			}
		}
		
		// if keystore or truststore passwords are not defined, create random passwords
		for(String v : new String[]{"keystorepassword","truststorepassword"}){
			DataValue pass = propertyValues.get(v);
			if(pass == null || pass.getValue() == null){
				propertyValues.put(v, new DataValue(getRandomString(18)));
			}
		}
		StructureValue config = StructureValue.createStructureValue(configClass, propertyValues, null);
		// CimInstance config = CimInstance.createInstance(configClass, propertyValues, null);
		repository.put(config);
	//	System.out.println(configClass.toMOF());
	//	System.out.println(config.toMOF());
		repository.shutdown();
		return;
	}
	
	/**
	 * Get a random string of ascii characters
	 * @param len - length of string
	 * @return random string
	 */
	private static String getRandomString(int len){
		String ascii = "!#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~";
		StringBuilder b = new StringBuilder();
		Random r = new Random();
		for(int i = 0; i < len; i++){
			b.append(ascii.charAt(r.nextInt(ascii.length())));
		}
		return b.toString();
	}

	/**
	 * Get a hashmap containing {name, value} pairs passed as an argument list.
	 * Each {name, value} pair is represented as [-name value] ... in the argument list. In
	 * case the value contains spaces, it can be enclosed in braces.
	 * The returned Hashmap is keyed by name (sans the '-') and contains the corresponding
	 * value. Note that names are converted to lower case, so are case insensitive
	 * @param argv - string array to be parsed
	 * @return - argument map
	 */
	private static void getArgs(String [] argv, Map<String,String> options, List<String> files){
		for(int i=0; i<argv.length; i++ ){
			String token = argv[i];
			if(!token.startsWith("-")){
				files.add(token);
				continue;
			} else if(i < argv.length-1){
				token = token.substring(1); // strip the - sign in front
				String value = argv[++i];	// get the value, and move forward
				if("\\*".equals(value)) value = "*";
				if(value.startsWith("{")){	// have a quoted value
					StringBuilder b = new StringBuilder(value);
					if(!value.endsWith("}")){
						while(i < argv.length-1){
							b.append(" ");
							if("\\*".equals(argv[i+1])) argv[i+1] = "*";
							b.append(argv[++i]);
							if(argv[i].endsWith("}")) break;
						}
					}
					value = b.substring(1,b.length()-1);	// strip the braces from value
				}
				options.put(token.toLowerCase(), value);
				// System.out.println(token+ " "+value);
			}
		}
		return;
	}


}
