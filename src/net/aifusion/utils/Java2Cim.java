/**
 * Copyright 2016, Sharad Singhal, All Rights Reserved
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
 * Created Jan 2, 2016 by Sharad Singhal
 * Last Modified Jan 17, 2020 by Sharad Singhal
 */
package net.aifusion.utils;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.JavaModelMapper;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.Repository;

/**
 * Tool to introspect java classes and create CIM definitions
 * @author Sharad Singhal
 */
public class Java2Cim {
	private static final Logger logger = Logger.getLogger(Java2Cim.class.getName());
    /** Tool version **/
    private static String toolVersion = "0.0.5";
	/** Date for this version */
	private static String dated = "September 15, 2020";
	/** Header inserted at the top of the generated CIM output */
	private static String header = "/*\n * Java to CIM Generator Version "+toolVersion+" dated "+dated+"\n"+
							" * Copyright 2016-2020, Sharad Singhal\n"+
							" * All Rights Reserved\n"+
							" * Definition generated at "+
							DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG).format(new Date())+
							"\n */\n";
    /** debugging flag */
    private static boolean debug = false;
    
	/**
	 * Get all CIM classes needed to resolve a given Java class
	 * @param cls Exported java class for which the corresponding CIM definitions are required
	 * @return - list containing all elements necessary to resolve the given class in CIM.
	 */
	public static List<NamedElement> getCimModelListForClass(Class<?> cls) {
		InMemoryCache cache = new InMemoryCache();
		getModelForClass(cls,cache);
		return cache.getElements(null, null, null, false);
	}
	
    /**
	 * Get the CIM Element definition corresponding to an Exported java class using the provided repository.
	 * If the CIM element definition is already in the repository, it is returned, else the given class (and its superclasses)
	 * and its interfaces (and those in the superclasses) are checked and any definitions not already in the repository are
	 * entered in the repository.
	 * @param javaClass - Class&lt;? extends Object&gt; java class. The class must be annotated with an Export annotation
     * @param repository - Cim Repository to use as a cache.
     * @return NamedElement corresponding to the Java Class. null returned in case of errors
	 * @see Export
	 */
	public static synchronized NamedElement getModelForClass(Class<?> javaClass, Repository repository){
		try {
			// get the object path for this element
			ObjectPath p = JavaModelMapper.getObjectPathFromClass(javaClass);
			if(debug) System.out.println("Java2Cim: Checking if "+p.toString()+" exists in Repository");
			
			// if factory repository does not contain definition, load it, and return it
			if(!repository.contains(p)){
				if(debug) System.out.println("Java2Cim: Loading "+p.toString()+" to Repository");
				return loadModelForClass(javaClass,p,repository);
			}
			// return definition from factory repository
			return repository.get(p);
		} catch (Exception ex){
			if(debug) System.out.flush();
			logger.log(Level.INFO, "Unable to create CIM definitions", ex);
			return null;
		}
	}
    

	/**
	 * @param javaClass - java class being introspected
	 * @param path - object path for the element being constructed
	 * @param repository - CIM repository to use
	 * @return - constructed Named Element
	 */
    private static NamedElement loadModelForClass(Class<? extends Object> javaClass, ObjectPath path, Repository repository) {
    	if(debug) System.out.println("Java2Cim: Try loading Class "+javaClass.getName()+" Path "+path.toURL());
    	NamedElement superType = null;
    	// check the superClass hierarchy in the repository up to (but not including) Object.class
    	if(javaClass.getSuperclass() != null && !javaClass.getSuperclass().equals(Object.class)){
    		ObjectPath superTypePath = JavaModelMapper.getObjectPathFromClass(javaClass.getSuperclass());
    		if(superTypePath != null){
    			// superclass is an annotated class
    			if (!repository.contains(superTypePath)) {
    				// if superclass is not in repository, recursively add it
    				superType = loadModelForClass(javaClass.getSuperclass(),superTypePath, repository);
    			} else {
    				// else just use the version in the repository
    				superType = repository.get(superTypePath);
    			}
    		} else {
    			// superType is not annotated, recurse to load its model, if any, skipping any non-annotated classes
    			superType = loadModelForClass(javaClass.getSuperclass(),path,repository);
    		}
    	}
    	// at this point, we have loaded all superTypes, and superType contains the immediate superType of the class being constructed
    	// (or is null, if there is no superType)
    	
    	// if the current class is not annotated and it has an annotated superclass,
    	// we return the closest annotated superclass in the recursion
    	Export cls = javaClass.getAnnotation(Export.class);
    	if(cls == null) {
    		if(debug) System.out.println("Java2Cim: Unwind "+javaClass.getName()+" Return with superType "+(superType == null ? "NULL" : superType.getName()));
    		return superType;
    	}

    	// Have an annotated java class. Create the cim definition
    	if(debug) System.out.println("Java2Cim: Build class "+javaClass.getName());

    	// get the mof corresponding to the class being loaded, and parse it into the repository
    	String mof = getClassMOF(javaClass,superType == null ? null : superType.getName(), cls, path, repository);
    	// if(debug) System.out.println("Java2Cim: Parsing:\n"+b.toString());
    	if(debug) System.out.println("Java2Cim: Parsing:----\n"+mof+"----");
    	try {
    		// ByteArrayInputStream in = new ByteArrayInputStream(b.toString().getBytes());
    		ByteArrayInputStream in = new ByteArrayInputStream(mof.getBytes());
    		MOFParser parser = new MOFParser(repository);
    		parser.parse(in, path.getNameSpacePath());
    	} catch (Exception ex){
    		throw new ModelException("Java2Cim: ["+javaClass.getName()+"] Annotation Error caused error in output MOF:\n\n"+mof+"\n",ex);
    		// throw new ModelException("Java2Cim: ["+javaClass.getName()+"] Annotation Error caused error in output MOF:\n\n"+b.toString()+"\n",ex);
    	}
    	NamedElement currentClass =  repository.get(path);
    	
    	if(debug) System.out.println("Java2Cim: returning\n"+currentClass);
    	return currentClass;
    }
    
    /**
     * Get the MOF representation of a class
     * @param javaClass - java class being introspected
     * @param superType - CIM superType name of current class. Null if none
     * @param cls - annotation for current class
     * @param path - object path for the current class
     * @param repository - CIM repository to use to resolve classes
     * @return - string containing the MOF rspresentation of the class
     */
    private static String getClassMOF(Class<?> javaClass, String superType, Export cls, ObjectPath path, Repository repository){
    	List<Class<?>> dependentClasses = null;
    	// Now build the actual class definition.
    	StringBuilder b = new StringBuilder();
    	// add the MappingStrings qualifier (to linked class), and any declared qualifiers
    	b.append("[").append("MappingStrings{\"").append(Constants.fusionMap).append(javaClass.getName()).append("\"}");
    	String version = javaClass.getAnnotation(Export.class).version();
    	if(!Constants.defaultVersion.equals(version)) b.append(",Version(\"").append(version).append("\")");
    	switch(path.getElementType()){
    	case ENUMERATION:
    		b.append(addEnumeration(javaClass));
    		break;
    	case INTERFACE:
    	case STRUCTURE:
    	case CLASS:
    		// check if the class is abstract
    		int modifiers = javaClass.getModifiers();
    		if(Modifier.isAbstract(modifiers)){
    			b.append(", Abstract");
    		}
    		// if(Modifier.isInterface(modifiers)){
    		// 	b.append(", Interface");
    		// }
    		HashSet<CimClass> interfaces = new HashSet<CimClass>();
    		// check if this class implements any known interfaces
    		Class<?>[] intfs = javaClass.getInterfaces();
    		if(intfs.length > 0){
    			StringBuilder bi = new StringBuilder();
    			for(Class<?> intf : intfs){
    				// get the CIM definition(s) for the interface(s)
    				ObjectPath interfacePath = JavaModelMapper.getObjectPathFromClass(intf);
    				if(debug) System.out.println("Locate Interface "+interfacePath.toURL());
    				if(interfacePath != null){
    					CimClass interfaceClass = (CimClass) (repository.contains(interfacePath) ? repository.get(interfacePath) : 
    						loadModelForClass(intf,interfacePath, repository));	
    					if(interfaceClass != null) {
    						bi.append("\"");
    						bi.append(interfaceClass.getName());
    						bi.append("\",");
    						interfaces.add(interfaceClass);
    					}
    				}
    			}
    			if(bi.length() > 0 && bi.charAt(bi.length()-1) == ','){
    				bi.setLength(bi.length()-1);	// one or more interfaces added
    			}
    			if(bi.length() > 0){
    				b.append(", Implements{");
    				b.append(bi.toString());
    				b.append("}");
    			}
    		}
    		// Add class qualifiers
    		String classQualifiers = cls.qualifiers();
    		if(classQualifiers.length() > 0){
    			b.append(", ").append(classQualifiers);
    		}
    		b.append("]\n");
    		b.append(path.getElementType().toMOF());
    		b.append(" ");
    		// add the class name and superclass name, if any
    		b.append(path.getName());
    		if(superType != null){
    			b.append(" : ");
    			b.append(superType);
    		}
    		b.append(" {\n");
    		// add the content of this class definition, and get any other classes that need resolution
    		dependentClasses = addContent(javaClass,path,b,repository);
    		b.append("};\n");
    		break;
    	default:
    		throw new ModelException("Java2Cim: Data type "+path.getElementType()+" not yet handled.");
    	}
    	if(dependentClasses != null) {
    		for(Class<?> dc : dependentClasses) {
    			if(debug) System.out.println("Java2Cim: Adding dependency "+dc.getName());
    			Export dce = dc.getAnnotation(Export.class);
    			ObjectPath dcPath = JavaModelMapper.getObjectPathFromClass(dc);
    			String mof = getClassMOF(dc,path.getName(),dce,dcPath,repository);
    			if(debug) System.out.println("Java2Cim: Dependency Class MOF: "+mof);
    			b.append(mof);
    		}
    	}
    	return b.toString();
    }


	/**
	 * @param javaClass - java class being introspected
	 * @param path - path to the CIM element being constructed
	 * @param b - current CIM contents
	 * @param repository - repository to be used for resolving names
	 * @return - list of other classes that are necessary for resolution. Null if none
	 */
    private static List<Class<?>> addContent(Class<? extends Object> javaClass, ObjectPath path, StringBuilder b, Repository repository) {
    	// locate exported fields (structures or enumerations)
    	for(Class<?> c : javaClass.getDeclaredClasses()){
    		if(!Modifier.isPublic(Modifier.PUBLIC)) continue;
    		if(!c.isAnnotationPresent(Export.class)) continue;
    		String supType = JavaModelMapper.getCimSuperClassName(javaClass);
    		ObjectPath p = JavaModelMapper.getObjectPathFromClass(c);
    		String mof = getClassMOF(c, supType, c.getAnnotation(Export.class), p, repository);
    		b.append(mof);
    	}
    	
    	// locate exported public methods
    	Map<String, FeatureBinding> locatedFeatures = getFeatures(javaClass);
    	if(locatedFeatures.size() == 0) return null;
    	
    	String currentClass = path.getName();
    	List<Class<?>> dependentClasses = null;
    	for(String key : locatedFeatures.keySet()){
    		FeatureBinding binding = locatedFeatures.get(key);
    		// check if this method is a property (or reference) declaration
    		if(debug) System.out.println("Java2Cim: Checking Java Method "+key+" :: "+binding.toMOF()+" ::");
    		
    		// TODO -- we are here
    		
    		Map<String,Class<?>> refTypes = binding.getReferencedTypes();
    		for(String r : refTypes.keySet()){
    			if(debug) System.out.println("Java2Cim: Check "+r);
    			Class<?> c = refTypes.get(r);
    			if(javaClass.equals(c)) {
    				// skip self-references
    				continue;
    			} else if(javaClass.isAssignableFrom(c)) {
    				if(debug) System.out.println("Java2Cim: "+javaClass.getName()+" is assignable from "+c.getName());
    				// Check if dependency exists in the repository
    				ObjectPath p = JavaModelMapper.getObjectPathFromClass(c);
    				if(repository.contains(p)) {
    					if(debug) System.out.println("Java2Cim: "+c.getName()+" "+p+" found in repository" );
    					continue;
    				}
    				if(dependentClasses == null) dependentClasses = new Vector<Class<?>>();
    				if(debug) System.out.println("Java2Cim: "+c.getName()+" added to dependency classes");
    				dependentClasses.add(c);
    				continue;	// skip self and subclass references
    			}
    			ElementType t = JavaModelMapper.getCimElementType(c);
    			if(debug) System.out.println("Java2Cim: Locate "+c.getName()+" "+t);
    			ObjectPath p = new ObjectPath(t,r,path.getNameSpacePath(),null, null);
    			if(!repository.contains(p)){
    				if(debug) System.out.println("Java2Cim: Loading required class "+r+" : "+c.getName());
    				NamedElement n = getModelForClass(c, repository);
    				if(debug) System.out.println(n.toMOF());
    			}
    		}
    		b.append("\t").append(binding.toMOF());
    	}
    	return dependentClasses;
    }
    
    /**
     * Get the method bindings defined in this class
     * @return - Map containing all bindings
     */
    private static Map<String,FeatureBinding> getFeatures(Class<? extends Object> javaClass){
    	HashMap<String,FeatureBinding> bindings = new HashMap<String,FeatureBinding>();
    	for(Method m : javaClass.getDeclaredMethods()){
    		if(!m.isAnnotationPresent(Export.class)) continue;
    		if(!Modifier.isPublic(m.getModifiers())){
    			logger.warning(javaClass.getName()+": Exported Method "+m.getName()+" is not public. Ignored");
    			continue;
    		}
    		CimFeature f = new CimFeature(m);
    		if(!bindings.containsKey(f.getName())) bindings.put(f.getName(), new FeatureBinding(f.getName()));
    		FeatureBinding b = bindings.get(f.getName());
    		b.addFeature(f);
    	}
    	return bindings;
    }
	
	/**
	 * Get the definition of a CIM Enumeration
	 * @param javaClass - java class (Enumeration) to be introspected
	 * @return - string containing the CIM representation of the enumeration
	 */
	private static String addEnumeration(Class<? extends Object> javaClass){
		StringBuilder b = new StringBuilder();
		try {
			// validate that the incoming java class is an enum, and cast it into an enum
			if(!javaClass.isEnum()) throw new ModelException("Class "+javaClass.getName()+" is not an enumeration");
			Class<? extends Enum> javaEnum = (Class<? extends Enum>) javaClass;
			if(debug) System.out.println("Java Class Name: "+javaEnum.getName());
			
			// get the name of the CIM enumeration (ensures that it is exported)
			String cimEnumName = JavaModelMapper.getCimClassName(javaEnum);
			if(cimEnumName == null) throw new ModelException("Class "+javaClass.getName()+" is not an exported class");
			if(debug) System.out.println("Cim Class Name: "+cimEnumName);
			
			boolean hasValueMethod = false;
			DataType cimType = DataType.STRING;
			
			// Check if the Enum has a "value()" method declared in it, and if so locate the type of value
			for(Method m : javaEnum.getDeclaredMethods()) {
				if(debug) System.out.println("Java method: "+m.toString()+" "+m.getName());
				if("value".equals(m.getName())) {
					hasValueMethod = true;
					cimType = DataType.getTypeForClass(m.getReturnType());
					if(!(cimType.isInteger() || cimType.isString()) || cimType.isArray())
						throw new ModelException(ExceptionReason.INVALID_ENUMERATION_CONTEXT,javaClass.getName()+": Enumerations must be of type string or integer");
					break;
				}
			}
			
			// add any declared qualifiers (Note that MappingStrings has already been added before this method is called)
			String declaredQualifiers = javaClass.getAnnotation(Export.class).qualifiers();
			if(declaredQualifiers.length() > 0){
				b.append(",").append(declaredQualifiers);
			}
			b.append("]\n");
			
			// now construct the enumeration definition. Note that Java Enums cannot extend other Enums so no supertype is present
			// and Java Enum values do not have @Export annotation so we do not have qualifiers on enumeration values
			b.append("Enumeration ").append(cimEnumName).append(" : ").append(cimType.toMOF()).append(" {");
			// get defined constants within the enumeration, and their respective values
			Object[] enumConstants = javaEnum.getEnumConstants();
			for(Object o : enumConstants){
				b.append("\n\t").append(o.toString());
				if(hasValueMethod) {
					Class<?> sub = o.getClass();
					Method valueMethod = sub.getDeclaredMethod("value");
					Object val = valueMethod.invoke(o);
					b.append(" = ");
					if(cimType == DataType.STRING)
						b.append("\"").append(val).append("\"");
					else 
						b.append(val);
				}
				b.append(",");
			}
		} catch (NoSuchMethodException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new ModelException(javaClass.getName()+": Unable to create Enumeration",e);
		}
		if(b.length() > 0) b.setLength(b.length() - 1);
		b.append("\n};\n");
		return b.toString();
	}


	/**
	 * Provide a help message on standard output
	 * @param javaCommand - java command for commandline
	 */
	public static void help(String javaCommand){
		System.out.println(header);
		System.out.println("\nUsage: ");
		System.out.println("java "+javaCommand+" [-h] javaClassName ...");
		System.out.println("\t-h provides help (this message)");
		System.out.println("\tjavaClassName is the name of a java class loadable by the system class loader");
		return;
	}

	/**
	 * Program to generate the MOF representation for one or more java classes. Prints out the MOF representation
	 * of the classes (if any) on the standard output. The classes must be annotated with <code>@Export</code> annotations.
	 * Usage is<br>
	 * $ java -jar Java2Cim.jar [-h] javaClassName ...<br>
	 * where the -h option provides help (this message) and javaClassName is the name of a class loadable by the system class loader.
	 * <p>The program auto-generates the following qualifiers in the MOF description by java code introspection. These qualifiers should NOT be added
	 * in the annotation</p>
	 * <dl>
	 * <dt>Abstract</dt><dd>Generated for abstract java classes</dd>
	 * <dt>Interface</dt><dd>Generated for interface classes</dd>
	 * <dt>Static</dt><dd>Generated for a static method or property type</dd>
	 * <dt>Implements</dt><dd>Generated for interfaces defined in the java class</dd>
	 * <dt>Read</dt><dd>Generated for properties which have a "getter" method in the java code</dd>
	 * <dt>Write</dt><dd>Generated for properties which have a "setter" method in the java code</dd>
	 * <dt>MappingStrings</dt><dd>Generated for Enumerations, Structures, and Classes to allow bindings between CIM and Java</dd>
	 * <dt>Version</dt><dd>Version numbers on classes are generated using the classVersion annotation parameter</dd>
	 * </dl>
	 * <p>The following qualifiers should be provided in the annotations for correct MOF generation</p>
	 * <dl>
	 * <dt>Association</dt><dd>Annotate the MOF class with the Association qualifier</dd>
	 * <dt>Author</dt><dd>provide the name of the author for the MOF class</dd>
	 * <dt>Deprecated</dt><dd>Label a MOF class, method, or property as @deprecated. Should match @deprecated annotation in code</dd>
	 * <dt>PackagePath</dt><dd>Use the value as the package name for the MOF class</dd>
	 * <dt>Override</dt><dd>annotate the property or method as overriding a superclass property or method. Should match @Override in code</dd>
	 * </dl>
	 * <p>In addition, the following annotation parameters can be used to generate the proper MOF constructs</p>
	 * <dl>
	 * <dt>schema="ABC"</dt><dd>identify the schema within which this class resides</dd>
	 * <dt>version="m.n.u"</dt><dd>identify the version of the class. Translated to the VERSION qualifier in MOF</dd>
	 * <dt>nameSpace="/namespacePath"</dt><dd>CIM namespacepath within which this class resides</dd>
	 * <dt>name="cimName"</dt><dd>use cimName as the name of the cim element in MOF</dd>
	 * </dl>
	 * @param args - list of class names (accessible to the class loader) for which MOF is needed.
	 */
	public static void main(String[] args) {
		if(args == null || args.length == 0 || args[0].startsWith("-h") || args[0].startsWith("-H")){
			help("Java2Cim");
			return;
		}
		if(args != null && args.length > 0){
			try {
				for(int i = 0; i<args.length; i++){
					System.out.println(header);
					Class<?> cls = Java2Cim.class.getClassLoader().loadClass(args[i]);
					List<NamedElement> parsedElements = Java2Cim.getCimModelListForClass(cls);
					for(NamedElement c : parsedElements){
						System.out.println(c.toMOF());
					}
				}
			} catch (Exception e){
				e.printStackTrace();
			}
		}
	}
}
