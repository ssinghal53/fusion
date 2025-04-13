/**
 * Copyright 2017, 2025 Sharad Singhal, All Rights Reserved
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
 * Last Modified Mar 31, 2025 by Sharad Singhal
 */
package net.aifusion.cimserver;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map.Entry;

import net.aifusion.metamodel.CimStructure;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.JavaModelMapper;
import net.aifusion.metamodel.ModelUtilities;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.metamodel.StructureValue;
import net.aifusion.utils.Java2Cim;

/**
 * Class to represent a Cim server configuration
 * @author Sharad Singhal
 */
@Export(qualifiers="Description(\"Cim Server Configuration\"),Version(\""+CimServerConfiguration.version+"\")")
public class CimServerConfiguration extends HttpConfiguration {
	/** Default configuration Key to use */
	private static final String defaultID = "defaultConfig";
	/** Default CIM repository. Null implies an in-memory cache */
	private static final String defaultRepository = null;
	/** Server Configuration class version */
	protected static final String version = "2.1.1";
	/** Default configuration directory */
	private static final String defaultConfigDirectory = "resources/config";
	/** Name of the repository to use */
	private String repositoryName = defaultRepository;
	/** Default Provider to use in the CIM Handler. Null implies a BasicProvider */
	private String providerName = null;
	/** Providers to use in the CIM Handler. Null implies that only the default provider is known */
	private String [] providerNames = null;

	/**
	 * Create a default configuration
	 */
	public CimServerConfiguration(){
		return;
	}

	/**
	 * Create a CIMServer configuration
	 * @param configuration - configuration to use. If null, a default configuration is used
	 * @see CimServerConfiguration#getConfiguration(String, String, String)
	 */
	public CimServerConfiguration(StructureValue configuration) {
		super(configuration);
		if(configuration != null){
			for(String pName : configuration.getPropertyNames()){
				DataValue v = configuration.getPropertyValue(pName);
				// System.out.println(pName+" ["+v+"]");
				if(v == null) continue;
				switch(pName.toLowerCase()){
				case "repository":
					repositoryName = v.toString();
					break;
				case "provider":
					providerName = v.toString();
					break;
				case "providernames":
					providerNames = (String []) v.getValue(); 
					break;
				default:
					break;
				}
			}
		}
		return;
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
	 * Get the CIM provider used in CimHandler
	 * @return - name of the class to use in CimHandler
	 */
	@Export(qualifiers="Description(\"Name of the default provider to use in the handler class\")")
	public String getProvider(){
		return providerName;
	}
	
	/**
	 * Get additional providers to be used if multiple providers are network accessible from the server
	 * @return list of providers to add to the server. Each is of the form serverEndpoint|ProviderName|repositoryLocation
	 */
	@Export(qualifiers="Description(\"Names of the providers to use in the handler class. Each is of the form serverEndpoint|ProviderClassName[|repository]\")")
	public String [] getProviderNames() {
		return providerNames;
	}
	
	/**
	 * Get a configuration corresponding to given id (must not be null)
	 * @param id - identity for the configuration. Default is used if null
	 * @param nameSpace - nameSpace for the configuration. Default is used if null
	 * @param directory - configuration directory. Default is used if null
	 * @return - Server configuration. Null if no such configuration exists
	 */
	public static CimServerConfiguration getConfiguration(String id,String nameSpace, String directory){
		String repo = (directory == null) ? defaultConfigDirectory : directory;
		NameSpacePath path = (nameSpace == null) ? Constants.defaultNameSpacePath : new NameSpacePath(nameSpace);
		String configId = (id == null) ? defaultID : id;
		HashMap<String,DataValue> keys = new HashMap<String,DataValue>();
		keys.put("ID", new DataValue(configId));
		ObjectPath configPath = new ObjectPath(ElementType.STRUCTUREVALUE,"AIFusion_CimServerConfiguration",path,keys, null);
		PersistentCache repository = new PersistentCache(repo);
		if(repository.contains(configPath)){
			StructureValue c = (StructureValue) repository.get(configPath);
			repository.shutdown();
			return new CimServerConfiguration(c);
		}
		repository.shutdown();
		return null;
	}
	
	

	@Override
	public String toString() {
		CimStructure s = (CimStructure) Java2Cim.getModelForClass(getClass(), new InMemoryCache());
		StructureValue v = JavaModelMapper.createCimValueFromJavaObject(s, this);
		return v.toMOF();
	}

	/**
	 * print out help on the standard output
	 */
	private static void showHelp(){
		System.out.println("Use:\n$ CimServerConfiguration options\nwhere options can be:");
		System.out.println("\t-help | -Help				# show help (this message) (must be only argument)");
		System.out.println("\t-c configurationId		# ConfigurationID to use [defaultID]");
		System.out.println("\t-cp configPath			# Path to the configuration directory [resources/config]");
		System.out.println("\t-cookiestore cookiestore	# cookie directory to use [null]");
		System.out.println("\t-hostname host name		# server host name [localhost]");
		System.out.println("\t-keystore keystore		# keystore to use [keystore.jks]");
		System.out.println("\t-keystorepassword pass	# keystore password [null]");
		System.out.println("\t-maxsessions sessions		# maximum sessions [0]");
		System.out.println("\t-n namespacePath			# NameSpacePath to use [aifusion]");
		System.out.println("\t-provider className		# default provider to use in the handler [BasicProvider]");
		System.out.println("\t-proxyhost hostname		# proxy host name [null]");
		System.out.println("\t-proxyport port			# server port number [8080]");
		System.out.println("\t-r repository				# Name of the repository to use [repsitory]");
		System.out.println("\t-requesthandler className	# default request handler [CimHandler]");
		System.out.println("\t-secure [true|false]		# server uses https [false] (not implemented)");
		System.out.println("\t-serverport port			# server port number [8085]");
		System.out.println("\t-servertimeout timeout	# server time out [5000]");
		System.out.println("\t-truststore truststore	# truststore to use [truststore.jks]");
		System.out.println("\t-truststorepassword pass	# truststore passord [null]");
		System.out.println("\t-x500principal dn			# x500Principal DN for server [CN=localhost,OU=cimfusion.com,O=cimfusion,C=US,L=Cupertino,ST=California]");
		System.out.println("\t-logenabled [true|false]	# true if logging is enabled [false]");
		System.out.println("\t-loglevel level			# level for logging [INFO]");
		System.out.println("\t-logfile filename			# filename for logging [server.log]");
		System.out.println("\t-providerNames {names}	# names of providers. Each name is a triple endpoint|javaClassname|repository [{ }]");
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
	 * <dt>-ProviderNames {name name name}</dt><dd>names of providers to use. Each name is a triple serverendpoint|providerclass|repositorylocation</dd>
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
		CimServerConfiguration conf = new CimServerConfiguration();
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
		options = ModelUtilities.getArgs(args);
		
		String repo = options.containsKey("cp") ? options.get("cp") : defaultConfigDirectory;
		NameSpacePath path = options.containsKey("n") ? new NameSpacePath(options.get("n")) : Constants.defaultNameSpacePath;
		
		PersistentCache repository = new PersistentCache(repo);
		
		// get the configuration class
		ObjectPath configClassPath = new ObjectPath(ElementType.STRUCTURE,"aifusion_HttpConfiguration",path,null, null);
		CimStructure configClass = repository.contains(configClassPath) ? (CimStructure) repository.get(configClassPath) : 
			(CimStructure) Java2Cim.getModelForClass(CimServerConfiguration.class, repository);
		
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
				propertyValues.put(v, new DataValue(ModelUtilities.getRandomString(18)));
			}
		}
		StructureValue config = StructureValue.createStructureValue(configClass, propertyValues, null);
		repository.put(config);
		repository.shutdown();
		return;
	}
	
}
