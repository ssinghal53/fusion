/**
 * Copyright 2019, Sharad Singhal, All Rights Reserved
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
 * Created July 19, 2019 by Sharad Singhal
 */
package net.aifusion.utils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Date;
import java.text.DateFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Vector;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.aifusion.metamodel.BufferedCache;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.PersistentCache;
import net.aifusion.metamodel.Repository;

/**
 * Factory class to generate Java code stubs from Cim Class definitions
 * @author Sharad Singhal
 */
public class Cim2Java {
	/** Logger for this class */
	private static final Logger logger = Logger.getLogger(Cim2Java.class.getName());
	/** Stub generator version */
	private static String version = "0.0.1";
	/** Date for this stub generator version */
	private static String dated = "July 19, 2019";
	/** Header inserted at the top of the generated java class */
	private static String copyRightHeader = "/*\n * CIM to Java Stub Generator Version "+version+" dated "+dated+"\n"+
							" * Stubs may be used without restriction except for retaining this comment in source code\n */\n";
	
	/** Annotations added to every generated java class */
	private Vector<String> knownImports = new Vector<String>();
	
	/** Local imports for the current class */
	private HashSet<String> localImports = new HashSet<String>();
	/** Java package for class being constructed */
	private String javaClassPackage = null;
	/** StringBuilder containing (partial) java code */
	private StringBuilder code = new StringBuilder();
	/** Known mappings for CIM-Java class names */
	private HashMap<String,String> cimNameToJavaName = new HashMap<String,String>();
	/** Known java method names generated */
	private HashSet<String> methodNames = new HashSet<String>();
	/** Cim Repository to use for the stub generator */
	private Repository repository;
	/** Default package name */
	private String defaultPackage = "";
	
	/** Debugging flag */
	private static boolean debug = false;
	/** Generate detail comments */
	private static boolean genComments = true;
	/** Generate generator header */
	private static boolean genHeader = true;
	
	/** Default version number */
	private static String defaultVersion;
	/** Default schema */
	private static String defaultSchema;
	static {
		try {
			defaultSchema = (String) Export.class.getDeclaredMethod("schema").getDefaultValue();
			defaultVersion = (String) Export.class.getDeclaredMethod("version").getDefaultValue();
		} catch (NoSuchMethodException | SecurityException e) {
			// Should not happen
			e.printStackTrace();
		}
	}
	
	/**
	 * Create a CIM - java stub generator
	 * @param programArgs - program arguments
	 * @param r - repository to use
	 * @see Cim2Java#main(String[]) Java Stub Generator command line options
	 */
	public Cim2Java(HashMap<String,String> programArgs, Repository r){
		repository = r;
		knownImports.add(Export.class.getName());
		if(programArgs.containsKey("p")) defaultPackage = programArgs.get("p");
		if(programArgs.containsKey("v")) defaultVersion = programArgs.get("v");
		genComments = programArgs.containsKey("c") && programArgs.get("c").startsWith("t") ? true : false;
		genHeader = programArgs.containsKey("h") && programArgs.get("h").startsWith("f") ? false : true;
		if(programArgs.containsKey("l")){
			Level level = Level.parse(programArgs.get("l").toUpperCase());
			setLogger(level, null);
		}
		return;
	}
	
	/**
	 * Create the file header for the java source file
	 * @param element - CimStructure to use
	 * @return - StringBuilder containing the global header (copyright, package, imports) for the java source file
	 */
	private String getFileHeader(NamedElement element){
		StringBuilder b = new StringBuilder();
		// add the copyright header
		if(genHeader){
			b.append(copyRightHeader);
			b.append("// Stub generated at ");
			b.append(DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG).format(new Date(System.currentTimeMillis())));
			b.append("\n");
			// add the commented MOF file
			b.append(getCommentedMOF("",element.toMOF(), true));
			b.append("\n");
		}
		return b.toString();
	}
	
	/**
	 * Get a java comment containing the MOF string for the class
	 * @param prefix - string prefix to be used at the start of the line
	 * @param mofString - mof representation of the string
	 * @param createDelimiter - true if a comment delimiter is needed around the MOF comment
	 * @return - mofString as a java comment with each new line prepended by the given prefix
	 */
	private String getCommentedMOF(String prefix, String mofString, boolean createDelimiter){
		StringBuilder mofComment = new StringBuilder(prefix);
		mofComment.append(createDelimiter ? "/*\n ******** MOF INPUT ************\n * " : "/* ");		
		mofComment.append(prefix);
		for(int i=0; i < mofString.length(); i++){
			char c = mofString.charAt(i);
			mofComment.append(c);
			if(c == '\n' && i < mofString.length()-1){
				mofComment.append(prefix);
				mofComment.append(" * ");
			}
		}
		mofComment.append(prefix);
		mofComment.append(createDelimiter ? " *********************/\n" : " */\n");
		return mofComment.toString();
	}
	
	/**
	 * Get the package name (if any) for the java class. If the PackagePath qualifier is present,
	 * it is used as the package name, else the default package is returned.
	 * @param c - CimClass to use
	 * @return - String containing package name. May be empty if no PROVIDER or UMLPackagePath qualifiers exist,
	 * and no default is specified on command line
	 */
	protected String getJavaPackageName(NamedElement c){
		if(c.hasQualifier("PackagePath")){
			String packagePath = c.getQualifierValue("PackagePath").toString();
			return packagePath.replaceAll("::", ".");
		} else if(c.hasQualifier("UMLPackagePath")){
			String packagePath = c.getQualifierValue("UMLPackagePath").toString();	// for V2 classes
			return packagePath.replaceAll("::", ".");
		}
		return defaultPackage;
	}
	
	/**
	 * Return a string containing the generated java class stub
	 * @param c - CimClass to use for generating stub
	 * @return - string containing the content of the java class
	 */
	public synchronized String generateStub(NamedElement c){
		if(debug) logger.info("Generate Stub for\n" + c.toMOF());
		
		// initialize variables
		localImports.clear();	// reference and parameter imports, if any
		methodNames.clear();	// java method names generated for this stub
		code.setLength(0); 		// generated java code
		JavaFeature classFeature = new JavaFeature(c,"",repository);
		
		StringBuilder b = new StringBuilder();
		b.append(getFileHeader(c));
		b.append(classFeature.getJavaCode());
		return b.toString();
	}
	/**
	 * Get the name of the java class
	 * @param c - cim class to use
	 * @return - java class name
	 */
	protected String getJavaClassName(NamedElement c){
		String name = c.getName();
		name = name.substring(name.indexOf('_')+1);
		char first[] = name.toCharArray();
		boolean isUpperCase = Character.isUpperCase(first[0]);
		String javaName;
		if(isUpperCase) {
			javaName = name;
		} else {
			first[0] -= 32;
			javaName = new String(first);
		}
		return javaName;
	}
	
	/*
	 * *************************************
	 * Logging management to bypass all the java restrictions
	 * *************************************
	 */
	/**
	 * Set up the log facility
	 */
	private void setLogger(Level level, String logFile){
		// set the log file if given
		if(logFile != null){
			try {
				logger.addHandler(new FileHandler(logFile));
			} catch (Exception ex){
				logger.warning("Cim2Java : error creating log file "+ex.toString());
			}
		}

		// set log level. Default is system dependent
		logger.setLevel(level);

		// The default console handler restricts log messages to INFO level.
		// if our log level value is lower, reset the default handlers to our level
		if(level.intValue() < Level.INFO.intValue()){
			Logger rootLogger = logger;
			while(rootLogger.getParent() != null) rootLogger = rootLogger.getParent();
			Handler [] handlers = rootLogger.getHandlers();
			for(Handler h : handlers)
				h.setLevel(level);
		}
		return;
	}
	
	public static void showHelp(String javaCommand){
		System.out.println(copyRightHeader);
		System.out.println("Use:\n$ java "+javaCommand+" [options] mofFile ...");
		System.out.println("where options consist of (defaults in [])");
		System.out.println("\t-s srcDirectory\tsource directory used to write java files [src]");
		System.out.println("\t-v version\tdefault version number (should be of form nn.nn.nn) to use [0.0.1]");
		System.out.println("\t-o [true|false]\toverwrite existing java files in case of conflict [false]");
		System.out.println("\t-c [true|false]\tadd method-level comments with MOF in generated code [false]");
		System.out.println("\t-r repository\tpersistent repository used to resolve input classes [none]");
		System.out.println("\t-l logLevel\tlogging level for messages [warning]");
		System.out.println("\t-s [true|false]\tsave current mof input in repository (only if used with -r) [false]");
		System.out.println("\t-n nameSpace\tname space to use in repository (only if used with -r) [/root/local]");
		System.out.println("\t-i [true|false] autogenerate standard qualifier types in repository [true]");
		System.out.println("\t-m [2|3] input contains cim metamodel version 2 or 3 mof [3]");
		return;
	}
	
	/**
	 * Program to generate java stubs from one or more MOF files. This program accepts one or more
	 * MOF files, and generates java stubs from the MOF files. Qualifiers in the MOF files are used
	 * to create the appropriate java constructs. The program should be executed as follows:<br>
	 * "$ java Cim2Java [options] mofFile ..."<br>
	 * where options can be (defaults in [])
	 * <dl>
	 * <dt>-h [true|false]</dt><dd>put Cim2Java header in code [true]</dd>
	 * <dt>-s srcDirectory</dt><dd>source directory used to write java files [src1]</dd>
	 * <dt>-v version</dt><dd>default version number (should be of form nn.nn.nn) to use [0.0.1]</dd>
	 * <dt>-o [true|false]</dt><dd>overwrite existing java files in case of conflict [false]</dd>
	 * <dt>-c [true|false]</dt><dd>add method-level comments with MOF in generated code [false]</dd>
	 * <dt>-r repository</dt><dd>persistent repository used to resolve input classes [none]</dd>
	 * <dt>-l logLevel</dt><dd>logging level for messages [warning]</dd>
	 * <dt>-n nameSpace</dt><dd>name space to use in repository (only if used with -r) [/root/local]</dd>
	 * </dl>
	 * The stub generator recognizes the following qualifiers in the MOF description
	 * <dl>
	 * <dt>Abstract</dt><dd>Generate an abstract java class</dd>
	 * <dt>Interface</dt><dd>Generate an interface java class</dd>
	 * <dt>Static</dt><dd>Generate a static method or property in java</dd>
	 * <dt>Deprecated</dt><dd>Label a java class, method, or property as @Deprecated</dd>
	 * <dt>Provider</dt><dd>Use the value as a java class name for this MOF class</dd>
	 * <dt>[UML]PackagePath</dt><dd>Use the value as the package name for the java class</dd>
	 * <dt>Version</dt><dd>add the version value to the version annotation parameter in the java class</dd>
	 * <dt>Implements</dt><dd>Add the defined interfaces to the java class</dd>
	 * <dt>Read</dt><dd>The property is readable</dd>
	 * <dt>Write</dt><dd>The property is writable</dd>
	 * <dt>Override</dt><dd>annotate the property or method as overriding a superclass property or method using @Override</dd>
	 * </dl>
	 * 
	 * @param args - program arguments
	 */
	public static void main(String[] args) {
		// if no arguments given, show help
		if(args.length == 0){
			showHelp("Cim2Java");
			return;
		}
		// collect all options
		HashMap<String, String> programArgs = new HashMap<String,String>();
		Vector<String> mofFiles = new Vector<String>();
		for (int i = 0; i < args.length; i++) {
			String token = args[i];
			if (!token.startsWith("-")){
				mofFiles.add(token);
				continue;
			} else if(i < args.length -1){
				token = token.substring(1); // strip the - sign in front
				String value = args[++i];
				if (value.startsWith("{") && !value.endsWith("}")) { // have a quoted value
					StringBuilder b = new StringBuilder(value);
					while (i < args.length - 1) {
						b.append(" ");
						b.append(args[++i]);
						if (args[i].endsWith("}")) break;
					}
					value = b.substring(1, b.length() - 1); // strip the braces from value
				}
				programArgs.put(token, value);
			}
		}
		// source directory for java sources
		String srcDirectory = programArgs.containsKey("s") ? programArgs.get("s") : "src2";
		// true if we wish to overwrite existing java files
		boolean overwrite = programArgs.containsKey("o") && programArgs.get("o").startsWith("t") ? true : false;
		
		/** CIM Repository to use */
		BufferedCache repository;
		
		// create the repository
		if(programArgs.containsKey("r")){
			PersistentCache cache = new PersistentCache(programArgs.get("r"));
			repository = new BufferedCache(cache);
		} else {
			repository = new BufferedCache(new InMemoryCache());
		}
		
		NameSpacePath nameSpacePath =  programArgs.containsKey("n") ? new NameSpacePath(programArgs.get("n")) : Constants.defaultNameSpacePath;
		
		// parse the input files
		MOFParser parser = new MOFParser(repository);
		for(String file : mofFiles){
			if(debug) logger.info("Parsing "+file);
			parser.parse(file,nameSpacePath);
		}
		
		// create the source directory if needed
		File dir = new File(srcDirectory);
		if(!dir.exists() && !dir.mkdir()){
			logger.severe("Cim2Java -- unable to create source directory " + dir.getAbsolutePath());
			return;
		} else if(!dir.isDirectory()){
			logger.severe("Cim2Java -- "+dir.toString()+" is not a directory");
			return;
		}
		logger.info("Generating Stubs");
		// create the stub generator
		Cim2Java generator = new Cim2Java(programArgs,repository);
		
		// Generate java files for all incoming MOF files
		for(NamedElement c : repository.getBufferedElements()){
			if(c.getElementType() == ElementType.QUALIFIERTYPE || 
					c.getElementType() == ElementType.STRUCTUREVALUE ||
					c.getElementType() == ElementType.INSTANCE) continue;
			if(debug) logger.info("Generate code for "+c.getFullName());
			String javaClassContent = generator.generateStub(c);
			String javaClassName = generator.getJavaClassName(c) + ".java";
			String packageName = generator.getJavaPackageName(c);
			if(packageName.length() > 0){
				packageName = packageName.replaceAll("\\.", "/");
				String [] pathElements = packageName.split("/");
				StringBuilder path = new StringBuilder();
				for(String p : pathElements){
					// we assume that the name space is absolute, so cannot contain .. segments
					if(p == null || p.equals("") || p.equals("..")) continue;
					path.append("/");
					path.append(p);
					// check if directory exists
					dir = new File(srcDirectory+path.toString());
					if(!dir.exists() && !dir.mkdir()){
						logger.severe("Cim2Java -- unable to create directory " + dir.getAbsolutePath());
						return;
					}
				}
			}
			File javaFile = new File(srcDirectory +(packageName.length() == 0 ? "/" : "/"+packageName+"/")+javaClassName);
			try {
				if(!javaFile.exists() || overwrite){
					logger.fine("Writing file : "+javaFile.getPath());
					FileWriter writer = new FileWriter(javaFile);
					writer.write(javaClassContent);
					writer.close();
				} else {
					logger.fine("File already exists: "+javaFile.getPath());
				}
			} catch(IOException ex){
				ex.printStackTrace();
			}
		}
		repository.shutdown();
		return;
	}

}
