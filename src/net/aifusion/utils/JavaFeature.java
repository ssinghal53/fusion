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
 * Created Jul 26, 2019 by sharad
 */
package net.aifusion.utils;

import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.logging.Logger;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimEnumeration;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.CimStructure;
import net.aifusion.metamodel.Constants;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ElementType;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.Export;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.MOFParser;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.ModelUtilities;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.QualifiedElement;
import net.aifusion.metamodel.Qualifier;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;

/**
 * Class to define a CIM Feature, and the corresponding Java bindings
 * Used in Cim2Java
 * @author Sharad Singhal
 */
class JavaFeature {
	private static final Logger logger = Logger.getLogger(JavaFeature.class.getName());
	private static String defaultNameSpace; 
	/** Cim element to convert to java */
	private NamedElement feature;
	/** Indent tab for this feature */
	private String tab;
	/** Cim repository used to locate dependencies */
	private Repository repository;
	/** java package for the feature */
	private String javaPackage;
	/** CIM NameSpacePath for the Feature */
	private NameSpacePath cimNameSpacePath;
	/** CIM Element type for this feature */
	private ElementType featureType;
	/** CIM Name of the feature */
	private String cimName;
	/** Schema associated with this feature */
	private String cimSchema;
	/** Version of this feature */
	private String featureVersion;
	/** Imports required for this feature */
	private TreeSet<String> localImports = new TreeSet<String>();
	/** Code associated with this feature */
	private StringBuilder code = new StringBuilder();
	/** Implemented interfaces, if any */
	private Vector<String> interfaces = new Vector<String>();
	/** debugging flag */
	private boolean debug = false;
	/** Use Cim StructureValue in constructor(s) */
	private boolean cimConstructor = true;
	/** TODO: Contained features within this feature, if any (not needed for V2 MOF) */
	private Vector<JavaFeature> embeddedFeatures = new Vector<JavaFeature>();
	
	static {
		try {
			defaultNameSpace = (String) Export.class.getDeclaredMethod("nameSpace").getDefaultValue();
		} catch (NoSuchMethodException | SecurityException e) {
			throw new ModelException("Internal error - should not happen");
		}
	}

	/**
	 * Create a CIM Feature
	 * @param feature - CIM feature to translate
	 * @param tab - tab to include before this feature in Java
	 * @param repository - repository to use for resolution
	 * @param cimConstructor - use StructureValue in constructor
	 */
	public JavaFeature(NamedElement feature, String tab, Repository repository, boolean cimConstructor) {
		this.feature = feature;
		this.tab = tab;
		this.repository = repository;
		this.cimConstructor = cimConstructor;
		// initialize other globals
		initGlobals();
		// obtain feature specific code
		switch(featureType) {
		case STRUCTURE:
		case CLASS:
			code.append(classFeature((CimStructure) feature));
			break;
		case ENUMERATION:
			if(feature.getSuperType() != null) {
				logger.warning(feature.getObjectPath()+" : Java does not support Enum supertypes. Code is incorrect.");
			}
			code.append(enumFeature((CimEnumeration) feature));
			break;
		default:
			throw new ModelException("Internal Error- Feature does not implement "+feature.getElementType()+":"+feature.getName());
		}
		return;
	}

	/**
	 * Initialize all globals needed for code generation of this feature
	 */
	private void initGlobals() {
		featureType = feature.getElementType();
		cimNameSpacePath = feature.getNameSpacePath();
		javaPackage = getJavaPackageName(feature);
		cimName = feature.getName();
		cimSchema = getCimSchema(feature);
		featureVersion = feature.hasQualifier("VERSION") ? feature.getQualifierValue("VERSION").toString() : null;

		localImports.clear();
		code.setLength(0);

		// known imports that all features use
		localImports.add(Export.class.getName());
		if(featureType != ElementType.ENUMERATION){	
			if(cimConstructor) {
				localImports.add(StructureValue.class.getName());
				localImports.add(DataValue.class.getName());
			} else {
				localImports.add(Map.class.getName());
			}
		}

		// import for superType, if any
		NamedElement superType = feature.getSuperType();
		if(superType != null) {
			String superTypeJavaPackage = getJavaPackageName(superType);
			if(!javaPackage.equals(superTypeJavaPackage)) {
				String imp = superTypeJavaPackage + "." + getJavaName(superType);
				localImports.add(imp);
			}
		}
		// imports for the feature
		switch(featureType) {
		case CLASS:
			// imports for methods
			CimClass c = (CimClass) feature;
			if(c.getMethodNames().size() > 0) {
				// method stubs use these
				localImports.add(ModelException.class.getName());
				localImports.add(ExceptionReason.class.getName());
			}
			for(String mName : c.getMethodNames()) {
				DataType t = c.getMethodReturnType(mName);
				if(t.isArray()) t = t.getComponentType();
				if(t.isVoid()) continue;
				if(t.isPrimitive()) {	
					Class<?> javaType = t.getClassForType();
					if(!javaType.getName().startsWith("java.lang")) {
						localImports.add(javaType.getName());
					}
				} else if(t.isReference()){
					String refClass = c.getReferencedClass(mName);
					NamedElement e = locate(refClass,cimNameSpacePath);
					String pkg = getJavaPackageName(e);
					if(!javaPackage.equals(pkg)) {
						String imp = pkg + "." + getJavaName(e);
						if(!localImports.contains(imp)) localImports.add(imp);
					}
					if(!localImports.contains(ObjectPath.class.getName())) localImports.add(ObjectPath.class.getName());
				} else if(t.isEnumerationValue()){
					CimEnumeration en = c.getReferencedEnum(mName);
					String pkg = getJavaPackageName(en);
					if(!javaPackage.equals(pkg)) {
						String imp = pkg + "." + getJavaName(en);
						if(!localImports.contains(imp)) localImports.add(imp);
					}
				} else if(t.isStructureValue() || t.isInstanceValue()){
					CimStructure st = c.getReferencedStructure(mName);
					String pkg = getJavaPackageName(st);
					if(!javaPackage.equals(pkg)) {
						String imp = pkg + "." + getJavaName(st);
						if(!localImports.contains(imp)) localImports.add(imp);
					}
				} else {	// void type does not need imports
					throw new ModelException(cimName+"-- InitGlobals does not yet handle method return types for "+t);
				}
				// imports for method parameters
				for(CimParameter p : c.getMethodParameters(mName)) {
					t = p.getDataType();
					if(t.isArray()) t = t.getComponentType();
					if(t.isPrimitive()) {	
						Class<?> javaType = t.getClassForType();
						if(!javaType.getName().startsWith("java.lang")) {
							localImports.add(javaType.getName());
						}
					} else if(t.isReference()){
						String refClass = p.getRefClassName();
						NamedElement e = locate(refClass,cimNameSpacePath);
						String pkg = getJavaPackageName(e);
						if(!javaPackage.equals(pkg)) {
							String imp = pkg + "." + getJavaName(e);
							if(!localImports.contains(imp)) localImports.add(imp);
						}
						if(!localImports.contains(ObjectPath.class.getName())) localImports.add(ObjectPath.class.getName());
					} else if(t.isEnumerationValue()){
						CimEnumeration en = p.getEnum();
						String pkg = getJavaPackageName(en);
						if(!javaPackage.equals(pkg)) {
							String imp = pkg + "." + getJavaName(en);
							if(!localImports.contains(imp)) localImports.add(imp);
						}
					} else if(t.isStructureValue() || t.isInstanceValue()){
						CimStructure st = p.getStruct();
						String pkg = getJavaPackageName(st);
						if(!javaPackage.equals(pkg)) {
							String imp = pkg + "." + getJavaName(st);
							if(!localImports.contains(imp)) localImports.add(imp);
						}
					} else {
						throw new ModelException(cimName+"-- InitGlobals does not yet handle parameter types for "+t);
					}
				}
			}
			// flow down to next case to get property imports
		case STRUCTURE:
			// imports for property types
			CimStructure s = (CimStructure) feature;
			for(String pName : s.getPropertyNames()) {
				DataType t = s.getPropertyType(pName);
				if(t.isArray()) t = t.getComponentType();
				if(t.isVoid()) continue;
				if(t.isPrimitive()) {	
					Class<?> javaType = t.getClassForType();
					if(!javaType.getName().startsWith("java.lang")) {
						localImports.add(javaType.getName());
					}
				} else if(t.isReference()){
					String refClass = s.getReferencedClass(pName);
					NamedElement e = locate(refClass,cimNameSpacePath);
					String pkg = getJavaPackageName(e);
					if(!javaPackage.equals(pkg)) {
						String imp = pkg + "." + getJavaName(e);
						if(!localImports.contains(imp)) localImports.add(imp);
					}
					if(!localImports.contains(ObjectPath.class.getName())) localImports.add(ObjectPath.class.getName());
				} else if(t.isEnumerationValue()){
					CimEnumeration en = s.getReferencedEnum(pName);
					String pkg = getJavaPackageName(en);
					if(!javaPackage.equals(pkg)) {
						String imp = pkg + "." + getJavaName(en);
						if(!localImports.contains(imp)) localImports.add(imp);
					}
				} else if(t.isStructureValue()){
					CimStructure st = s.getReferencedStructure(pName);
					String pkg = getJavaPackageName(st);
					if(!javaPackage.equals(pkg)) {
						String imp = pkg + "." + getJavaName(st);
						if(!localImports.contains(imp)) localImports.add(imp);
					}
				} else if(t.isInstanceValue()){
					CimStructure st = s.getReferencedStructure(pName);
					String pkg = getJavaPackageName(st);
					if(!javaPackage.equals(pkg)) {
						String imp = pkg + "." + getJavaName(st);
						if(!localImports.contains(imp)) localImports.add(imp);
					}
				}  else {
					throw new ModelException(cimName+"-- InitGlobals does not yet handle property types for "+t);
				}
			}
			break;
		case ENUMERATION:
			CimEnumeration e = (CimEnumeration) feature;
			DataType enumType = e.getDataType();	// enums are only String or one of the Integer types
			Class<?> javaType = enumType.getClassForType();
			if(!javaType.getName().startsWith("java.lang")) {
				localImports.add(javaType.getName());
			}
			break;
		default:
			throw new ModelException(cimName+"-- InitGlobals does not yet handle "+feature.getElementType());
		}
		return;
	}

	/**
	 * Locate a NamedElement with a given CIM Name
	 * @param elementName - name of the element
	 * @param defaultNameSpace - default name space to search
	 * @return - named element if found. Null elementName returns NULL (void)
	 * @throws ModelException if no such element found
	 */
	private NamedElement locate(String elementName,NameSpacePath defaultNameSpace) {
		if(debug) System.out.println("\nLocate "+defaultNameSpace+"/"+elementName);
		if(elementName == null) return null;	// void return type gets a null elementName
		if(elementName.isEmpty()) throw new ModelException("Empty element requested");
		
		// check current element hierarchy for the element name
		NamedElement c = locateElement(elementName,feature);
		if(c != null) {
			if(debug) System.out.println("Found "+c.getObjectPath());
			return c;
		}
		if(debug) System.out.println("* Search "+defaultNameSpace+"/"+elementName);
		// check repository with default name space for element
		for(ElementType t : new ElementType[] {ElementType.CLASS,ElementType.STRUCTURE,ElementType.ENUMERATION,ElementType.INTERFACE}) {
			ObjectPath path = new ObjectPath(t,elementName,defaultNameSpace,null,null);
			if(repository.contains(path)){
				c = repository.get(path);
				break;
			}
		}
		if(c != null) {
			if(debug) System.out.println("Found "+c.getObjectPath());
			return c;
		}
		if(debug) System.out.println("* Search "+elementName+" in other namespaces");
		// check other name spaces for element
		List<NamedElement> elements = repository.getElements("Class,Structure,Interface,Enumeration", null, elementName, false);
		if(elements.isEmpty()) {
			logger.warning("Definition not found for "+elementName);
			throw new ModelException("Definition not found for "+elementName);
		} else if(elements.size() > 1) {
			logger.warning("Multiple definitions found for "+elementName+" using "+elements.get(0).getFullName());
		}
		if(debug) System.out.println(" Found "+elements.get(0).getObjectPath());
		return elements.get(0);
	}
	
	/**
	 * Locate an element defined as an embedded definition in a hierarchy
	 * @param elementName - name of the element being searched
	 * @param e element whose hierarchy is to be checked
	 * @return - element with the name, if found. Null otherwise
	 */
	private NamedElement locateElement(String elementName, NamedElement e) {
		if(e == null) throw new ModelException("LocateElement - search value is null");
		if(debug) System.out.println("\tLocate "+elementName+" in "+e.getObjectPath());
		// check the element to see if it matches
		if(e.getName().equalsIgnoreCase(elementName)) return e;
		// check embedded elements
		switch(e.getElementType()) {
		case STRUCTURE:
		case CLASS:
		case INTERFACE:
			// note that the getAll*() methods also traverse the superTypes, so we do not need to check superType
			CimStructure s = (CimStructure) e;
			for(String sName : s.getAllStructureNames()) {
				if(debug) System.out.println("\tchecking Structure "+sName);
				if(sName.equalsIgnoreCase(elementName)) return s.getStructure(sName);
			}
			for(String iName : s.getAllInterfaceNames()) {
				if(debug) System.out.println("\tchecking Interface "+iName);
				if(iName.equalsIgnoreCase(elementName)) return s.getInterface(iName);
			}
			for(String eName : s.getAllEnumerationNames()) {
				if(debug) System.out.println("\tchecking Enumeration "+eName);
				if(eName.equalsIgnoreCase(elementName)) return s.getEnumeration(eName);
			}
			break;
		case ENUMERATION:
			// check enum supertypes
			CimEnumeration en = (CimEnumeration) e;
			if(en.getSuperType() != null) {
				return locateElement(elementName,en.getSuperType());
			}
		default:
			break;
		}
		return null;
	}

	/**
	 * Get the java name associated with this feature
	 * @param e - element to get the name from
	 * @return the javaName
	 */
	public String getJavaName(QualifiedElement e) {
		String featureName = e.getName();
		String name = null;
		String javaName = featureName;
		if(featureName.contains("_")) {
			int sep = featureName.indexOf("_");
			name = featureName.substring(sep+1);
		} else {
			name = featureName;
		}
		// obtain java name
		char c[] = name.toCharArray();
		boolean isUpperCase = Character.isUpperCase(c[0]);
		switch(e.getElementType()) {
		case CLASS:
		case INTERFACE:
		case ENUMERATION:
		case STRUCTURE:	// java name is initial uppercase
			if(isUpperCase) {
				javaName = name;
			} else {
				c[0] -= 32;
				javaName = new String(c);
			}
			break;
		default:	// java name is initial lowercase
			if(isUpperCase) {
				c[0] += 32;
				javaName = new String(c);
			} else {
				javaName = name;
			}
			break;
		}
		return javaName;
	}

	/**
	 * Get the java package name (if any) for a NamedElement. If the PackagePath qualifier is present,
	 * it is used as the java package name else the default package is returned.
	 * @param element - NamedElement to use
	 * @return - String containing java package name. May be empty if no PackagePath qualifiers exist,
	 */
	protected String getJavaPackageName(NamedElement element){
		if(element.hasQualifier("PACKAGEPATH")){
			String packagePath = element.getQualifierValue("PACKAGEPATH").toString();
			return packagePath.replaceAll("::", ".");
		}
		return "";
	}

	/**
	 * Get the feature name in Java with an initial lowercase letter
	 * @param featureName - cim name of the feature
	 * @return - java name with lower case
	 */
	private String toLowerCase(String featureName) {
		String name = featureName;
		if(featureName.contains("_")) {
			int sep = featureName.indexOf("_");	
			name = featureName.substring(sep+1);
		} else {
			name = featureName;
		}
		// obtain java name
		char c[] = name.toCharArray();
		if(Character.isUpperCase(c[0])) {
			c[0] += 32;
			return new String(c);
		}
		return name;
	}

	/**
	 * Get the feature name in java with an initial upper case letter
	 * @param featureName - cim name of feature
	 * @return - java name with initial upper case
	 */
	private String toUpperCase(String featureName) {
		String name = featureName;
		if(featureName.contains("_")) {
			int sep = featureName.indexOf("_");	
			name = featureName.substring(sep+1);
		} else {
			name = featureName;
		}
		// obtain java name
		char c[] = name.toCharArray();
		if(Character.isLowerCase(c[0])) {
			c[0] -= 32;
			return new String(c);
		}
		return name;
	}

	/**
	 * Generate code for a structure/class feature
	 * @param struct - structure to generate code
	 * @return - java code for the feature
	 */
	private String classFeature(CimStructure struct) {
		StringBuilder b = new StringBuilder();	// mainline + getters/setters
		StringBuilder b2 = new StringBuilder();	// local property definitions
		StringBuilder b3 = new StringBuilder();	// constructor
		b.append("\n");
		b.append(getExport(struct.getQualifiers(),true, struct.getNameSpacePath().getLocalPath(), null));
		if(isTrue(struct.getQualifierValue("DEPRECATED"))) b.append("@Deprecated\n");
		b.append("public ");
		if(isTrue(struct.getQualifierValue("ABSTRACT"))) b.append("abstract ");
		b.append("class ").append(getJavaName(struct)).append(" ");
		if(struct.getSuperType() != null) b.append("extends ").append(getJavaName(struct.getSuperType())).append(" ");
		if(interfaces.size() > 0) {
			b.append("implements ");
			for(String s : interfaces) {
				NamedElement intf = locate(s,cimNameSpacePath);
				b.append(getJavaName(intf)).append(" ");
			}
		}
		b.append("{\n");
		// embedded enumerations and structures, if any
		for(String eName : struct.getEnumerationNames()) {
			CimEnumeration en = struct.getEnumeration(eName);
			embeddedFeatures.add(new JavaFeature(en,tab+"\t",repository, cimConstructor));
		}
		for(String sName : struct.getStructureNames()) {
			CimStructure st = struct.getStructure(sName);
			embeddedFeatures.add(new JavaFeature(st,tab+"\t",repository, cimConstructor));
		}
		for(JavaFeature f : embeddedFeatures) {
			b.append(f.getFeatureCode());
		}
		
		if(cimConstructor) {
			b3.append(tab).append("\tpublic ").append(getJavaName(struct)).append("(StructureValue sv){\n");
			if(struct.getSuperType() != null) {
				b3.append(tab).append("\t\tsuper(sv);\n");
			} else {
				b.append(tab).append("\tprivate StructureValue sv;\n");
				b3.append(tab).append("\t\tthis.sv = sv;\n");
				b3.append(tab).append("\t\tif(!sv.isInstanceOf(\"").append(struct.getName()).append("\")){\n");
				b3.append(tab).append("\t\t\tthrow new ModelException(ExceptionReason.INVALID_PARAMETER,\"Only ").append(struct.getName());
				b3.append(" or its subclasses can be used. found \"+sv.getName());\n\t\t}\n");
			}
		} else {
			b3.append(tab).append("\tpublic ").append(getJavaName(struct)).append("(Map<String,Object> args){\n");
			if(struct.getSuperType() != null) b3.append(tab).append("\t\tsuper(args);\n");
			b3.append(tab).append("\t\tfor(String pName : args.keySet()){\n");
			b3.append(tab).append("\t\t\tswitch(pName){\n");
		}
		// properties if any
		for(String pName : struct.getPropertyNames()) {
			DataType dt = struct.getPropertyType(pName);
			if(debug) System.out.println("-- Checking Property "+pName+" type "+dt);
			String refClass = null;
			NamedElement refElement = null;
			// TODO: Need to handle annotated classes here once DataType is fixed
			if(!dt.isPrimitive()) {
				switch(dt.getComponentType()) {
				case OBJECTPATH:
					refClass = struct.getReferencedClass(pName);
					if(debug) System.out.println("-- Find "+refClass);
					refElement = locate(refClass,cimNameSpacePath);
					break;
				case ENUMERATIONVALUE:
					refElement = struct.getReferencedEnum(pName);
					refClass = refElement.getName();
					break;
				case STRUCTUREVALUE:
				case INSTANCEVALUE:
					refElement = struct.getReferencedStructure(pName);
					refClass = refElement.getName();
					break;
				default:
					// should not happen
					throw new ModelException("Unknown type "+dt);
				}
			}
			String jName = toLowerCase(pName);
			boolean isStatic = (Boolean) struct.getPropertyQualifierValue(pName, "STATIC").getValue();
			if(!cimConstructor) {
				// add property to class fields
				if(isTrue(struct.getPropertyQualifierValue(pName,"DEPRECATED"))) b.append("\t@Deprecated\n");
				b.append("\tprivate ");
				if(isStatic) b.append("static ");
				b.append(getJavaType(dt,refClass)).append(" ");
				b.append(jName).append(";\n");
				// add property to constructor case statement
				b3.append(tab).append("\t\t\tcase \"").append(pName).append("\":\n");
				b3.append(tab).append("\t\t\t\t").append(jName).append(" = (").append(getJavaType(dt,refClass)).append(") args.get(\"").append(pName).append("\");\n");
				b3.append(tab).append("\t\t\t\tbreak;\n");
			}
			// add getter/setter
			// readable property
			if((Boolean)struct.getPropertyQualifierValue(pName, "READ").getValue()) {
				if(isTrue(struct.getPropertyQualifierValue(pName,"DEPRECATED"))) b2.append("\t@Deprecated\n");
				b2.append("\t").append(getExport(struct.getPropertyQualifiers(pName), false, struct.getNameSpacePath().getLocalPath(), dt.isReference() ? refClass : null));
				b2.append("\tpublic ");
				if(isStatic) b2.append("static ");
				b2.append(getJavaType(dt,refClass)).append(" ");
				b2.append(dt.isBoolean() ? "is" : "get").append(toUpperCase(pName)).append("(){\n");
				if(cimConstructor) {
					b2.append("\t\tDataValue v = getPropertyValue(\"").append(pName).append("\");\n");
					b2.append("\t\treturn ").append("(v == null || v.getValue() == null) ? null : (").append(getJavaType(dt,refClass)).append(") v.getValue();\n");
				} else {
					b2.append("\t\treturn ").append(jName).append(";\n");
				}
				b2.append("\t}\n");
			}
			// writable property
			if((Boolean)struct.getPropertyQualifierValue(pName, "WRITE").getValue()) {
				b2.append("\t@Export\n");
				if(isTrue(struct.getPropertyQualifierValue(pName,"DEPRECATED"))) b2.append("\t@Deprecated\n");
				b2.append("\tpublic ");
				if(isStatic) b2.append("static ");
				b2.append("void set").append(toUpperCase(pName)).append("( ");
				b2.append(getJavaType(dt,refClass));
				b2.append(" value){\n\t\t");
				if(cimConstructor) {
					if(!localImports.contains(DataType.class.getName())) {
						localImports.add(DataType.class.getName());
					}
					b2.append("setPropertyValue(\"").append(pName).append("\",new DataValue(DataType.");
					b2.append(dt).append(", value").append("));\n");
				} else {
					b2.append(jName).append(" = value;\n");
				}
				b2.append("\t\treturn;\n");
				b2.append("\t}\n");
			}
		}
		// classes may have methods defined
		if(struct.getElementType()==ElementType.CLASS) {
			CimClass c = (CimClass) struct;
			for(String mName : c.getMethodNames()) {
				DataType dt = c.getMethodReturnType(mName);
				String refClass = null;
				NamedElement refElement = null;
				boolean returnIsObjectPath = false;
				if(!dt.isPrimitive()) {
					switch(dt.getComponentType()) {
					case OBJECTPATH:
						refClass = c.getReferencedClass(mName);
						refElement = locate(refClass,cimNameSpacePath);
						returnIsObjectPath = true;
						break;
					case ENUMERATIONVALUE:
						refElement = c.getReferencedEnum(mName);
						refClass = refElement.getName();
						break;
					case STRUCTUREVALUE:
					case INSTANCEVALUE:
						refElement = c.getReferencedStructure(mName);
						refClass = refElement.getName();
						break;
					case VOID:
						break;
					default:
						// should not happen
						throw new ModelException("Unknown type "+dt);
					}
				}
				String jName = toLowerCase(mName);
				boolean isStatic = (Boolean) c.getMethodQualifierValue(mName, "STATIC").getValue();
				// method declaration
				b2.append("\t@Export\n");
				if(isTrue(c.getMethodQualifierValue(mName,"DEPRECATED"))) b2.append("\t@Deprecated\n");
				b2.append("\tpublic ");
				if(isStatic) b2.append("static ");
				b2.append(getJavaType(dt,refClass)).append(" ");
				b2.append(jName).append("(");
				for(CimParameter p : c.getMethodParameters(mName)) {
					DataType pt = p.getDataType();
					refClass = null;
					refElement = null;
					boolean pIsObjectPath = false;
					if(!pt.isPrimitive()) {
						switch(pt.getComponentType()) {
						case OBJECTPATH:
							refClass = p.getRefClassName();
							refElement = locate(refClass,cimNameSpacePath);
							pIsObjectPath = true;
							break;
						case ENUMERATIONVALUE:
							refElement = p.getEnum();
							refClass = refElement.getName();
							break;
						case STRUCTUREVALUE:
						case INSTANCEVALUE:
							refElement = p.getStruct();
							refClass = refElement.getName();
							break;
						default:
							// should not happen
							throw new ModelException("Unknown type "+dt);
						}
					}
					jName = toLowerCase(p.getName());
					if(isTrue(p.getQualifierValue("DEPRECATED"))) b2.append("\n\t\t@Deprecated");
					b2.append("\n\t\t@Export(name=\"");
					b2.append(p.getName()).append("\"");
					if(p.getQualifiers().size() > 0) {
						b2.append(", qualifiers =\"").append(getQualifiers(p.getQualifiers(), null)).append("\"");
					}
					b2.append(") ");
					b2.append(getJavaType(pt,refClass));
					b2.append(" ").append(jName).append(",");
				}
				if(b2.charAt(b2.length()-1) == ',') b2.setLength(b2.length()-1);
				b2.append("){\n");
				b2.append("\t\tthrow new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,\"Method not implemented\");\n");
				b2.append("\t}\n");
			}
		}
		b.append("\n");
		// complete the constructor
		if(cimConstructor) {
			// finish constructor
			b3.append(tab).append("\t\treturn;\n");
			b3.append(tab).append("\t}\n\n");
			// if we do not have a superType, add getter/setter methods used in superclasses
			// and ObjectPath getter
			if(struct.getSuperType() == null) {
				if(!localImports.contains(ObjectPath.class.getName())) localImports.add(ObjectPath.class.getName());
				if(!localImports.contains(ModelException.class.getName())) localImports.add(ModelException.class.getName());
				if(!localImports.contains(ExceptionReason.class.getName())) localImports.add(ExceptionReason.class.getName());
				if(!localImports.contains(CimStructure.class.getName())) localImports.add(CimStructure.class.getName());
				b2.append(tab).append("\n\tprotected DataValue getPropertyValue(String pName) {\n");
				b2.append(tab).append("\t\treturn sv.getPropertyValue(pName);\n");
				b2.append(tab).append("\t}\n");
				
				b2.append(tab).append("\tprotected void setPropertyValue(String pName, DataValue value) {\n");
				b2.append(tab).append("\t\tsv.setPropertyValue(pName,value);\n");
				b2.append(tab).append("\t\treturn;\n");
				b2.append(tab).append("\t}\n");
				
				b2.append(tab).append("\tpublic ObjectPath getObjectPath() {\n");
				b2.append(tab).append("\t\treturn sv.getObjectPath();\n");
				b2.append(tab).append("\t}\n");
				
				b2.append(tab).append("\tpublic CimStructure getCimStructure() {\n");
				b2.append(tab).append("\t\treturn sv.getCreationStruct();\n");
				b2.append(tab).append("\t}\n");
				
				b2.append(tab).append("\tpublic int hashCode() {\n");
				b2.append(tab).append("\t\treturn sv.hashCode();\n");
				b2.append(tab).append("\t}\n");
				
				b2.append(tab).append("\tpublic boolean equals(Object o) {\n");
				b2.append(tab).append("\t\tif(o == null || !(o instanceof ").append(getJavaName(struct)).append(")) return false;\n");
				b2.append(tab).append("\t\t").append(getJavaName(struct)).append(" other = (").append(getJavaName(struct)).append(") o;\n");
				b2.append(tab).append("\t\treturn sv.equals(other.sv);\n");
				b2.append(tab).append("\t}\n");
				
				
				b2.append(tab).append("\tpublic String toString() {\n");
				b2.append(tab).append("\t\treturn sv.toMOF();\n");
				b2.append(tab).append("\t}\n");
				
				// TODO: add generic functions here to b2 if we do not have a superclass
				// getStructureValue()
			} else {
				b2.append(tab).append("\tpublic boolean equals(Object o) {\n");
				b2.append(tab).append("\t\tif(!super.equals(o) || !(o instanceof ").append(getJavaName(struct)).append(")) return false;\n");
				b2.append(tab).append("\t\treturn true;\n");
				b2.append(tab).append("\t}\n\n");
			}
			
		} else {
			b3.append(tab).append("\t\t\tdefault:\n");
			b3.append(tab).append("\t\t\t\tbreak;\n");
			b3.append(tab).append("\t\t\t}\n");
			b3.append(tab).append("\t\t}\n\t\treturn;\n");
			b3.append(tab).append("\t}\n");
		}
		b.append(b3.toString());
		b.append(b2.toString());
		b.append("}\n");
		return b.toString();
	}

	/**
	 * Generate code for an enumeration feature
	 * @param cimEnum - enum feature
	 * @return java code for feature
	 */
	private String enumFeature(CimEnumeration cimEnum) {
		String javaName = getJavaName(cimEnum);
		StringBuilder b = new StringBuilder();
		b.append(getExport(cimEnum.getQualifiers(),true, cimEnum.getNameSpacePath().getLocalPath(), null));
		if(isTrue(cimEnum.getQualifierValue("DEPRECATED"))) b.append("@Deprecated\n");
		b.append("public ");
		if(isTrue(cimEnum.getQualifierValue("STATIC"))) b.append("static ");
		b.append("enum ").append(javaName).append(" {\n");
		Class<?> javaType = cimEnum.getDataType().getClassForType();
		boolean hasValues = cimEnum.hasDefinedValues();
		for(String vName : cimEnum.getKeys()) {
			b.append("\t").append(vName);
			if(hasValues) {
				b.append("(");
				switch(cimEnum.getDataType()) {
				case UINT8:
					b.append("new UInt8(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				case UINT16:
					b.append("new UInt16(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				case UINT32:
					b.append("new UInt32(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				case UINT64:
					b.append("new UInt64(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				case SINT8:
					b.append("new Byte(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				case SINT16:
					b.append("new Short(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				case SINT32:
					b.append("new Integer(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				case SINT64:
					b.append("new Long(\"").append(cimEnum.getDataValue(vName).toMOF()).append("\")");
					break;
				default:
					b.append(cimEnum.getDataValue(vName).toMOF());
					break;
				}
				b.append(")");
			}
			b.append(",\n");
		}
		b.setLength(b.length()-2);
		if(hasValues) {
			b.append(";\n\tprivate final ").append(javaType.getSimpleName()).append(" ").append("value;\n\t");
			b.append("private ").append(javaName).append("(").append(javaType.getSimpleName()).append(" value){\n\t\t");
			b.append("this.value = value;\n\t}");
			b.append("\n\tpublic ").append(javaType.getSimpleName()).append(" value(){\n\t\treturn value;\n\t}");
		}
		b.append("\n}\n");
		return b.toString();
	}

	/**
	 * Get exports associated with a feature
	 * @param q - qualified element for which export is needed
	 * @param showVersion - show version in the export
	 * @param localNameSpace - local namespace to use
	 * @param refClass - referenced class to add in export, if not null
	 * @return - string containing export annotation
	 */
	private String getExport(List<Qualifier> q, boolean showVersion, String localNameSpace, String refClass) {
		// parse the qualifiers on the feature
		String qualifiers = getQualifiers(q, null);
		// create the export statement
		StringBuilder b = new StringBuilder(tab);
		b.append("@Export(");
		if(showVersion) {
			if(cimSchema != null) {
				b.append("schema=\"").append(cimSchema).append("\"");
				b.append(",");
			}
			if(featureVersion != null) {
				b.append("version=\"").append(featureVersion).append("\"");
				b.append(",");
			}
			if(localNameSpace != null && !localNameSpace.equalsIgnoreCase(defaultNameSpace)) {
				b.append("nameSpace=\"").append(localNameSpace).append("\"");
				b.append(",");
				
			}
		}
		if(refClass != null) {
			b.append("refClass=\"").append(refClass).append("\",");
		}
		if(qualifiers != null && !qualifiers.isEmpty()) {
			b.append("qualifiers=\"");
			b.append(qualifiers).append("\"");
		}
		if(b.charAt(b.length()-1) == '(' || b.charAt(b.length()-1) == ',') b.setLength(b.length()-1);
		if(b.length() > tab.length()+7) b.append(")");
		b.append("\n");
		return b.toString();
	}

	/**
	 * Get the code corresponding to a qualifier list
	 * @param qualifiers - list of qualifiers
	 * @param javaMapping - java class name to add as a MappingString {} qualifier
	 * @return - string to include in Export annotation.
	 */
	private String getQualifiers(List<Qualifier> qualifiers, String javaMapping) {
		String refClass = javaMapping;
		StringBuilder quals = new StringBuilder();		
		for(Qualifier q : qualifiers) {
			// handle special cases
			switch(q.getLowerCaseName()) {
			case "abstract":
			case "deprecated":
			case "override":
			case "read":
			case "write":
			case "static":
				break;
			case "mappingstrings":
				// TODO: Check this logic
				if(q.hasNonNullValue()) {
					StringBuilder qv = new StringBuilder("MappingStrings {");
					String [] vals = (String []) q.getValue().getValue();
					for(String v : vals) {
						if(v.startsWith(Constants.fusionMap)) {
							if(refClass != null) throw new ModelException(ExceptionReason.ALREADY_EXISTS,"RefClass already exists "+refClass+" attempted to add "+v);
							refClass = v.substring(Constants.fusionMap.length());
						} else {
							qv.append(ModelUtilities.quote(v)).append(",");
						}
					}
					if(qv.charAt(qv.length()-1) == ',') {
						qv.setLength(qv.length()-1);
					}
					if(qv.length() > 16) {
						qv.append("}");
						quals.append(qv.toString()).append(",");
					}
				}
				break;
			case "out":
				// throw new ModelException("OUT qualifiers are not currently translated");
				logger.warning(feature.getName()+": OUT qualifiers are not currently translated");
				break;
			case "version":
				featureVersion = q.hasNonNullValue() ? q.getValue().toString() : null;
				break;
			case "implements":
				if(q.hasNonNullValue()) {
					String [] vals = (String []) q.getValue().getValue();
					for(String v : vals) {
						interfaces.add(v);
					}
				}
				break;
			default:
				quals.append(q.toMOF());
				quals.append(",");
				break;
			}
		}
		if(quals.length() > 0) quals.setLength(quals.length()-1);
		if(refClass != null) {
			throw new ModelException(ExceptionReason.NOT_SUPPORTED,feature.getName()+" refClass "+refClass+ " not yet handled");
		}
		return ModelUtilities.escape(quals.toString());
	}

	/**
	 * Get the cim schema (if defined) from a qualified element
	 * @param e - element to check
	 * @return - schema if defined in name. Null if the qualified element name does not have a schema
	 */
	public String getCimSchema(QualifiedElement e) {
		String featureName = e.getName();
		return featureName.contains("_") ? featureName.substring(0,featureName.indexOf("_")) : null;
	}


	/**
	 * Get the java type associated with a data type
	 * @param dt - CIM data type
	 * @param refClass - name of referenced class for complex types
	 * @return java type to include in code
	 */
	public String getJavaType(DataType dt,String refClass) {
		// TODO: This needs to understand annotated classes
		// Currently we map them to complex classes-- needs fixing
		StringBuilder b = new StringBuilder();
		DataType ct = dt.getComponentType();
		if(ct.isVoid()) {
			b.append("void");
		} else if(ct.isPrimitive()) {
			// primitive types
			Class<?> javaClass = dt.getClassForType();
			b.append(javaClass.getSimpleName());
		} else if(ct.isComplex()){
			// complex types -- needs fixing once we fix data types
			NamedElement e = locate(refClass,cimNameSpacePath);
			b.append(getJavaName(e));
			if(dt.isArray()) b.append("[]");
		} else {
			// reference types
			// if(needObjectPath) {
				b.append("ObjectPath");
			// } else {
			//	NamedElement e = locate(refClass,cimNameSpacePath);
			//	b.append(getJavaName(e));
			// }
			if(dt.isArray()) b.append("[]");
		}
		return b.toString();
	}


	/**
	 * Get the java code associated with this feature
	 * @return - java code associated with this feature (including imports and package declaration)
	 */
	public String getJavaCode() {
		StringBuilder b = new StringBuilder();
		// add the package path, if needed
		if(javaPackage != null && javaPackage.length() > 0) b.append("package ").append(javaPackage).append(";\n");
		// add local imports, if needed
		for(String imp : localImports) {
			b.append("import ").append(imp).append(";\n");
		}
		// add imports for embedded features, if needed
		for(JavaFeature f : embeddedFeatures) {
			for(String imp : f.localImports) {
				if(!localImports.contains(imp)) b.append("import ").append(imp).append(";\n");
			}
		}
		b.append(code.toString());
		return b.toString();
	}
	
	/**
	 * Get the code associated with this feature
	 * @return - code associated with this feature (without imports and package declaration)
	 */
	public String getFeatureCode() {
		return code.toString();
	}

	/**
	 * Get the CIM element type for this feature
	 * @return - CIM element type for this feature
	 */
	public ElementType getFeatureType() {
		return featureType;
	}

	/**
	 * Get the CIM name of this feature
	 * @return CIM name of this feature
	 */
	public String getCimName() {
		return cimName;
	}

	/**
	 * Get Schema associated with this feature
	 * @return schema associated with this feature
	 */
	public String getSchema() {
		return cimSchema;
	}

	/**
	 * Get Imports required for this feature
	 * @return set containing imports. Empty if none needed
	 */
	public Set<String> getImports(){
		return localImports;
	}

	/**
	 * Check if a qualifier has a true value
	 * @param dataValue - boolean qualifier to check
	 * @return value of qualifier
	 */
	private boolean isTrue(DataValue dataValue) {
		if(dataValue == null) return false;
		switch(dataValue.getType().getComponentType()) {
		case BOOLEAN:
			return (Boolean) dataValue.getValue();
		case STRING:
			return dataValue.getValue() != null;
		default:
			break;
		}
		throw new ModelException("Case "+dataValue.getType()+" not yet handled");
	}

	/**
	 * Test code
	 * @param args - command line arguments
	 */
	public static void main(String [] args) {
		InMemoryCache cache = new InMemoryCache();
		MOFParser parser = new MOFParser(cache);
		parser.parse(new ByteArrayInputStream(mof2.getBytes()), Constants.defaultNameSpacePath);
		for(NamedElement e : cache.getElements("Structure", null, null, false)){
			System.out.println("----------");
			System.out.println(e.toMOF());
			JavaFeature f = new JavaFeature(e,"",cache, true);
			System.out.println("----------");
			System.out.println(f.getJavaCode());
			System.out.println("----------\n");
		}

	}

	private static String mof = "[Description(\"Integer Valued Enumeration\"), PackagePath(\"newPath1::newPath2\")]\r\n" + 
			"Enumeration Fusion_integerEnum : SInt32 {\r\n" + 
			"	NAME1 = 0,\r\n" + 
			"	Name2 = 2,\r\n" + 
			"	name3 = 3\r\n" + 
			"};";
	
	private static String mof1 = "[Description(\"String Valued Enumeration\"), PackagePath(\"newPath1::newPath2\")]\r\n" + 
			"Enumeration Fusion_integerEnum : String {\r\n" + 
			"	NAME1 = \"0\",\r\n" + 
			"	Name2 = \"2\",\r\n" + 
			"	name3 = \"3\"\r\n" + 
			"};";
	
	private static String mof2 =  "	[Version (\"1.0.0\"), Abstract, PackagePath(\"rsam::core\"),\r\n" + 
			"		Description (\"RSAM_Entity is the base class for all RSAM entities.\")]\r\n" + 
			"Structure RSAM_Entity {\r\n" + 
			"    	[Key, Description ( \"The unique id of the entity.\")] \r\n" + 
			"    String Id;\r\n" + 
			"    	[Description ( \"Policies associated with this entity\")] \r\n" + 
			"    RSAM_Policy ref AssociatedPolicies [];\r\n" + 
			"};"
			+ "	[Version (\"1.0.0\"), PackagePath(\"rsam::core\"), Description ( \"Types of policies\")]\r\n" + 
			"Enumeration RSAM_PolicyType : String {\r\n" + 
			"		[Description(\"Configuration policies\")]\r\n" + 
			"	Configuration,\r\n" + 
			"		[Description(\"Scheduling policies for activities\")]\r\n" + 
			"	Scheduling,\r\n" + 
			"		[Description(\"Constraint policies for designers\")]\r\n" + 
			"	Constraint\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"/*\r\n" + 
			" * =============================================\r\n" + 
			" * Policy Scope. A class-scoped policy applies to\r\n" + 
			" * all instances of that type. An instance-scoped\r\n" + 
			" * policy applies only to a given instance of an\r\n" + 
			" * entity.\r\n" + 
			" * =============================================\r\n" + 
			"*/\r\n" + 
			"\r\n" + 
			"	[Version (\"1.0.0\"), PackagePath(\"rsam::core\"), Description ( \"Scope of policies\")]\r\n" + 
			"Enumeration RSAM_PolicyScope : String {\r\n" + 
			"		[Description(\"Class-scoped policies apply to all instances of the class or structure\")]\r\n" + 
			"	Class,\r\n" + 
			"		[Description(\"Instance-scoped policies apply only to the associated instance\")]\r\n" + 
			"	Instance\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"/*\r\n" + 
			" * =================================================\r\n" + 
			" * Policy Language. This enumeration can be extended\r\n" + 
			" * to provide additional policy languages\r\n" + 
			" * =================================================\r\n" + 
			"*/\r\n" + 
			"\r\n" + 
			"	[Version (\"1.0.0\"), PackagePath(\"rsam::core\"), Description ( \"Types of policies\")]\r\n" + 
			"Enumeration RSAM_PolicyLanguage : String {\r\n" + 
			"		[Description(\"Cauldron policies\")]\r\n" + 
			"	Cauldron\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"/*\r\n" + 
			" * ======================================================================\r\n" + 
			" * Policy Definition. Note that one of the policy assertions must provide\r\n" + 
			" * a reference to the appropriate class for class scoped policies.\r\n" + 
			" * Instance-scoped policies are associated at the RSAM_Entity level to\r\n" + 
			" * the appropriate instance\r\n" + 
			" * ======================================================================\r\n" + 
			"*/\r\n" + 
			"\r\n" + 
			"	[Version (\"1.0.0\"), Abstract, PackagePath(\"rsam::core\"), Description ( \"Base Class for policies\")]\r\n" + 
			"Structure RSAM_Policy : RSAM_Entity {\r\n" + 
			"		[Description(\"Type of policy\")]\r\n" + 
			"	RSAM_PolicyType Type;\r\n" + 
			"		[Description(\"Scope of policy. For class-scoped policies, one of the assertions must identify the corresponding class\")]\r\n" + 
			"	RSAM_PolicyScope Scope;\r\n" + 
			"		[Description(\"Specification language for the policy\")]\r\n" + 
			"	RSAM_PolicyLanguage Language;\r\n" + 
			"		[Description(\"Policy assertions defined in the policy language\"),Write]\r\n" + 
			"	String [] Assertions;\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"";
	

	private static String mofx = "[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.EnumBindingClass\" }, Description(\"Integer Valued Enumeration\"), PackagePath(\"newPath1::newPath2\"), Version(\"1.0.0\")]\r\n" + 
			"Enumeration CimFusion_EnumBindingClass : SInt32 {\r\n" + 
			"	NAME1 = 0,\r\n" + 
			"	Name2 = 2,\r\n" + 
			"	name3 = 3\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.InterfaceBindingClass\" }, Abstract]\r\n" + 
			"Interface test_interface {\r\n" + 
			"	[Write, Read(false)]\r\n" + 
			"	Boolean intfMethod;\r\n" + 
			"	Boolean BoolProp;\r\n" + 
			"};\r\n" +
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.MethodBindingSuperClass\" }]\r\n"+
			"Structure Cim_TestMethodsSup {\r\n"+
			"String StringProperty = \"Something\";\r\n"+
			"};\r\n" + 
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.MethodBindingClass\" }]\n"+
			"Class Cim_TestMethods : Cim_TestMethodsSup {\n"+
			"[Key] String Key;\n"+
			// "[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\" }]\n"+
			"AIFusion_EmbeddedStringEnum enumValueToEnum(AIFusion_EmbeddedStringEnum arg0);\n"+
			"String enumValueToString(AIFusion_EmbeddedStringEnum arg0);\n"+
			"String concatStringToEnumValue(AIFusion_EmbeddedStringEnum arg0, String arg1);\n"+
			"Void doSomething();\n};"+
			
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.PropertyBindingClass\" }, Description(\"Structure to test property bindings\")]\r\n" + 
			"Structure cim_test {\r\n" + 
			"	[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum\" }]\r\n" + 
			"	Enumeration CimFusion_EmbeddedStringEnum : String {\r\n" + 
			"		NAME1 = \"xyz\",\r\n" + 
			"		Name2 = \"abc\",\r\n" + 
			"		name3 = \"def\"\r\n" + 
			"	};\r\n" + 
			"	[Write]\r\n" + 
			"	SInt16 [] Va10;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt32 [] Va11;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt32 [] Va12;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt64 [] Va13;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt64 [] Va14;\r\n" + 
			"	[Write]\r\n" + 
			"	Real32 [] Va15;\r\n" + 
			"	[Write]\r\n" + 
			"	String V21;\r\n" + 
			"	[Write]\r\n" + 
			"	Char16 V20;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethods ref V23;\r\n" + 
			"	[Write]\r\n" + 
			"	Datetime V22;\r\n" + 
			"	[Write]\r\n" + 
			"	Real32 [] Va16;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethodssup V25;\r\n" + 
			"	[Write]\r\n" + 
			"	CimFusion_EmbeddedStringEnum V24;\r\n" + 
			"	[Write]\r\n" + 
			"	Real64 [] Va17;\r\n" + 
			"	[Write]\r\n" + 
			"	Real64 [] Va18;\r\n" + 
			"	[Write]\r\n" + 
			"	OctetString V27;\r\n" + 
			"	[Write]\r\n" + 
			"	Char16 [] Va19;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethods V26;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethodssup V29;\r\n" + 
			"	[Write]\r\n" + 
			"	CimFusion_EnumBindingClass V28;\r\n" + 
			"	[Write]\r\n" + 
			"	Boolean [] Va01;\r\n" + 
			"	[Write]\r\n" + 
			"	Boolean [] Va02;\r\n" + 
			"	[Write]\r\n" + 
			"	UInt8 [] Va03;\r\n" + 
			"	[Write]\r\n" + 
			"	UInt16 [] Va04;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethods V30;\r\n" + 
			"	[Static, Write]\r\n" + 
			"	String V31 = \"default\";\r\n" + 
			"	[Write]\r\n" + 
			"	UInt32 [] Va05;\r\n" + 
			"	[Write]\r\n" + 
			"	UInt64 [] Va06;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt8 [] Va07;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt8 [] Va08;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt16 [] Va09;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethods [] Va30;\r\n" + 
			"	[Static, Write]\r\n" + 
			"	String [] Va31;\r\n" + 
			"	[Write]\r\n" + 
			"	Boolean V01;\r\n" + 
			"	[Write]\r\n" + 
			"	UInt8 V03;\r\n" + 
			"	[Write]\r\n" + 
			"	Boolean V02;\r\n" + 
			"	[Write]\r\n" + 
			"	UInt32 V05;\r\n" + 
			"	[Write]\r\n" + 
			"	UInt16 V04;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt8 V07;\r\n" + 
			"	[Write]\r\n" + 
			"	UInt64 V06;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt16 V09;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt8 V08;\r\n" + 
			"	[Write]\r\n" + 
			"	Char16 [] Va20;\r\n" + 
			"	[Write]\r\n" + 
			"	String [] Va21;\r\n" + 
			"	[Write]\r\n" + 
			"	Datetime [] Va22;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethods ref [] Va23;\r\n" + 
			"	[Write]\r\n" + 
			"	CimFusion_EmbeddedStringEnum [] Va24;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethodssup [] Va25;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethods [] Va26;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt16 V10;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt32 V12;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt32 V11;\r\n" + 
			"	[Write]\r\n" + 
			"	OctetString [] Va27;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt64 V14;\r\n" + 
			"	[Write]\r\n" + 
			"	CimFusion_EnumBindingClass [] Va28;\r\n" + 
			"	[Write]\r\n" + 
			"	SInt64 V13;\r\n" + 
			"	[Write]\r\n" + 
			"	cim_testmethodssup [] Va29;\r\n" + 
			"	[Write]\r\n" + 
			"	Real32 V16;\r\n" + 
			"	[Write]\r\n" + 
			"	Real32 V15;\r\n" + 
			"	[Write]\r\n" + 
			"	Real64 V18;\r\n" + 
			"	[Write]\r\n" + 
			"	Real64 V17;\r\n" + 
			"	[Write]\r\n" + 
			"	Char16 V19;\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.MethodBindingClass\" }]\r\n" + 
			"Class cim_testmethods : cim_testmethodssup {\r\n" + 
			"	[Key]\r\n" + 
			"	String Key;\r\n" + 
			"	CimFusion_EmbeddedStringEnum enumValueToEnum(CimFusion_EmbeddedStringEnum arg0);\r\n" + 
			"	String enumValueToString(CimFusion_EmbeddedStringEnum arg0);\r\n" + 
			"	String concatStringToEnumValue(CimFusion_EmbeddedStringEnum arg0, String arg1);\r\n" + 
			"	Void doSomething();\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.CimMethodTestClass\" }]\r\n" + 
			"Class cim_methodtest {\r\n" + 
			"	Char16 [] mVa19(Char16 [] arg0);\r\n" + 
			"	Datetime [] mVa22(Datetime [] arg0);\r\n" + 
			"	Cim_TestMethods ref [] mVa23(Cim_TestMethods ref [] arg0);\r\n" + 
			"	CimFusion_EnumBindingClass [] mVa24(CimFusion_EnumBindingClass [] arg0);\r\n" + 
			"	cim_testmethodssup [] mVa25(cim_testmethodssup [] arg0);\r\n" + 
			"	cim_testmethods [] mVa26(cim_testmethods [] arg0);\r\n" + 
			"	cim_testmethods mV30(cim_testmethods arg0);\r\n" + 
			"	OctetString [] mVa27(OctetString [] arg0);\r\n" + 
			"	[Static]\r\n" + 
			"	String mV31(String arg0);\r\n" + 
			"	CimFusion_EnumBindingClass [] mVa28(CimFusion_EnumBindingClass [] arg0);\r\n" + 
			"	cim_testmethodssup [] mVa29(cim_testmethodssup [] arg0);\r\n" + 
			"	Char16 [] mVa20(Char16 [] arg0);\r\n" + 
			"	String [] mVa21(String [] arg0);\r\n" + 
			"	cim_testmethodssup mV29(cim_testmethodssup arg0);\r\n" + 
			"	SInt8 [] mVa08(SInt8 [] arg0);\r\n" + 
			"	SInt16 [] mVa09(SInt16 [] arg0);\r\n" + 
			"	SInt32 [] mVa11(SInt32 [] arg0);\r\n" + 
			"	SInt32 [] mVa12(SInt32 [] arg0);\r\n" + 
			"	SInt64 [] mVa13(SInt64 [] arg0);\r\n" + 
			"	SInt64 [] mVa14(SInt64 [] arg0);\r\n" + 
			"	Real32 [] mVa15(Real32 [] arg0);\r\n" + 
			"	Real32 [] mVa16(Real32 [] arg0);\r\n" + 
			"	Char16 mV20(Char16 arg0);\r\n" + 
			"	Real64 [] mVa17(Real64 [] arg0);\r\n" + 
			"	Real64 [] mVa18(Real64 [] arg0);\r\n" + 
			"	Cim_TestMethods ref mV23(Cim_TestMethods ref arg0);\r\n" + 
			"	[Static]\r\n" + 
			"	String [] getVa31(String [] arg0);\r\n" + 
			"	CimFusion_EnumBindingClass mV24(CimFusion_EnumBindingClass arg0);\r\n" + 
			"	Datetime mV22(Datetime arg0);\r\n" + 
			"	OctetString mV27(OctetString arg0);\r\n" + 
			"	CimFusion_EnumBindingClass mV28(CimFusion_EnumBindingClass arg0);\r\n" + 
			"	cim_testmethodssup mV25(cim_testmethodssup arg0);\r\n" + 
			"	cim_testmethods mV26(cim_testmethods arg0);\r\n" + 
			"	SInt16 [] mVa10(SInt16 [] arg0);\r\n" + 
			"	Real64 mV18(Real64 arg0);\r\n" + 
			"	Char16 mV19(Char16 arg0);\r\n" + 
			"	Boolean [] mVa01(Boolean [] arg0);\r\n" + 
			"	Boolean [] mVa02(Boolean [] arg0);\r\n" + 
			"	UInt8 [] mVa03(UInt8 [] arg0);\r\n" + 
			"	UInt16 [] mVa04(UInt16 [] arg0);\r\n" + 
			"	UInt32 [] mVa05(UInt32 [] arg0);\r\n" + 
			"	UInt64 [] mVa06(UInt64 [] arg0);\r\n" + 
			"	SInt8 [] mVa07(SInt8 [] arg0);\r\n" + 
			"	SInt32 mV12(SInt32 arg0);\r\n" + 
			"	SInt64 mV13(SInt64 arg0);\r\n" + 
			"	SInt16 mV10(SInt16 arg0);\r\n" + 
			"	SInt32 mV11(SInt32 arg0);\r\n" + 
			"	Real32 mV16(Real32 arg0);\r\n" + 
			"	Real64 mV17(Real64 arg0);\r\n" + 
			"	SInt64 mV14(SInt64 arg0);\r\n" + 
			"	Real32 mV15(Real32 arg0);\r\n" + 
			"	SInt16 mV09(SInt16 arg0);\r\n" + 
			"	SInt8 mV07(SInt8 arg0);\r\n" + 
			"	SInt8 mV08(SInt8 arg0);\r\n" + 
			"	String getV21(String arg0);\r\n" + 
			"	Boolean mV01(Boolean arg0);\r\n" + 
			"	Boolean mV02(Boolean arg0);\r\n" + 
			"	Void mV00();\r\n" + 
			"	UInt32 mV05(UInt32 arg0);\r\n" + 
			"	cim_testmethods [] mVa30(cim_testmethods [] arg0);\r\n" + 
			"	UInt64 mV06(UInt64 arg0);\r\n" + 
			"	UInt8 mV03(UInt8 arg0);\r\n" + 
			"	UInt16 mV04(UInt16 arg0);\r\n" + 
			"};\r\n" + 
			"\r\n" + 
			"[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.IsGetterPropertyClass\" }, Description(\"Structure to test isGetter property bindings\")]\r\n" + 
			"Structure test_class {\r\n" + 
			"	[Write]\r\n" + 
			"	String PF7;\r\n" + 
			"	String PF6;\r\n" + 
			"	[Write, Description(\"write only property\"), Read(false)]\r\n" + 
			"	Boolean P1;\r\n" + 
			"	[Description(\"read-only property\")]\r\n" + 
			"	Boolean P2;\r\n" + 
			"	[Write, Description(\"read/write property\")]\r\n" + 
			"	Boolean P3;\r\n" + 
			"	[Description(\"readonly only property with isGetter\")]\r\n" + 
			"	Boolean P4;\r\n" + 
			"	[Write, Description(\"read/write only property with isGetter\")]\r\n" + 
			"	Boolean P5;\r\n" + 
			"	[Description(\"readonly only property with get() method\")]\r\n" + 
			"	Boolean P6;\r\n" + 
			"	[Write, Description(\"read/write property with get() method\")]\r\n" + 
			"	Boolean P7;\r\n" + 
			"	[Description(\"read/write only property with isGetter\")]\r\n" + 
			"	String isPF5;\r\n" + 
			"	[Description(\"readonly only property with isGetter\")]\r\n" + 
			"	String isPF4;\r\n" + 
			"	[Description(\"read/write property with get() method\")]\r\n" + 
			"	String isPF7;\r\n" + 
			"	[Description(\"readonly only property with get() method\")]\r\n" + 
			"	String isPF6;\r\n" + 
			"	[Write, Description(\"write only property\"), Read(false)]\r\n" + 
			"	String PF1;\r\n" + 
			"	[Write, Description(\"read/write property\")]\r\n" + 
			"	String PF3;\r\n" + 
			"	[Description(\"read-only property\")]\r\n" + 
			"	String PF2;\r\n" + 
			"	[Write, Read(false)]\r\n" + 
			"	String PF5;\r\n" + 
			"};\r\n" + 
			"\r\n";
}
