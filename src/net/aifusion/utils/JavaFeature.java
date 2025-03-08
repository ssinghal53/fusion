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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.logging.Logger;

import javax.lang.model.SourceVersion;

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
				if(t.isVoid()) continue;	// void type does not need imports
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
				} else {
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
	 * Get the java name associated with a qualified element
	 * @param e - qualified element to get the name from
	 * @return the javaName
	 */
	public String getJavaName(QualifiedElement e) {
		String elementName = e.getName();
		String javaName = elementName;
		String name = elementName;
		ElementType et = e.getElementType();
		switch(et) {
		case CLASS:
		case INTERFACE:
		case ENUMERATION:
		case STRUCTURE:
			// Strip "Schema_" from the name
			if(elementName.contains("_")) {
				int sep = elementName.indexOf("_");
				name = elementName.substring(sep+1);
			}
			break;
		default:
			break;
		}
		
		// obtain java name
		char c[] = name.toCharArray();
		boolean isUpperCase = Character.isUpperCase(c[0]);
		switch(et) {
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
		// if javaName is a java keyword, translate it
		if(!SourceVersion.isName(javaName)) {
			System.out.println("Found Java keyword as name - "+name);
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
	 * @param dt - associated data type
	 * @return - java name with lower case
	 */
	private String toLowerCase(String featureName, DataType dt) {
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
			name = new String(c);
		}
		// if javaName is a java keyword, translate it
		if(!SourceVersion.isName(name)) {
			name = dt.equals(DataType.BOOLEAN) ? "is"+name : "j"+name;
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
		StringBuilder b = new StringBuilder();	// main-line + getters/setters
		StringBuilder b2 = new StringBuilder();	// local property definitions
		StringBuilder b3 = new StringBuilder();	// constructor
		b.append("\n");
		b.append(getExport(struct.getQualifiers(),true, struct.getNameSpacePath().getLocalPath(), null, null));
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
			boolean isArrayProperty = dt.isArray();
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
			String jName = toLowerCase(pName,dt);
			boolean isStatic = (Boolean) struct.getPropertyQualifierValue(pName, "STATIC").getValue();
			if(!cimConstructor) {
				// add property to class fields
				if(isTrue(struct.getPropertyQualifierValue(pName,"DEPRECATED"))) b.append("\t@Deprecated\n");
				b.append("\tprivate ");
				if(isStatic) b.append("static ");
				b.append(getJavaType(dt,refClass)).append(" ");
				b.append(jName);
				DataValue pv = struct.getDefaultPropertyValue(pName);
				if(pv != null) {
					if(isArrayProperty) {
						DataType dc = dt.getComponentType();
						b.append(" = { ");
						Object [] values = (Object[]) pv.getValue();
						for(Object v : values) {
							switch(dc) {
							case ENUMERATIONVALUE:
								b.append(getJavaType(dt,refClass)).append(".").append(v);
								break;
							case UINT8:
								b.append("new UInt8(\"").append(v).append("\")");
								break;
							case UINT16:
								b.append("new UInt16(").append(v).append(")");
								break;
							case UINT32:
								b.append("new UInt32(").append(v).append(")");
								break;
							case UINT64:
								b.append("new UInt64(").append(v).append(")");
								break;
							case DATETIME:
								b.append("new DateTime(\"").append(v).append("\")");
								break;
							case STRING:
								b.append("\"").append(v).append("\"");
								break;
							default:
								b.append(v);
								break;
							}
							b.append(",");
						}
						b.setLength(b.length()-1);
						b.append(" }");
						
					} else {
						b.append(" = ");
						switch(dt) {
						case ENUMERATIONVALUE:
							b.append(getJavaType(dt,refClass)).append(".").append(pv.getValue());
							break;
						case UINT8:
							b.append("new UInt8(\"").append(pv.getValue()).append("\")");
							break;
						case UINT16:
							b.append("new UInt16(").append(pv.getValue()).append(")");
							break;
						case UINT32:
							b.append("new UInt32(").append(pv.getValue()).append(")");
							break;
						case UINT64:
							b.append("new UInt64(").append(pv.getValue()).append(")");
							break;
						case DATETIME:
							b.append("new DateTime(\"").append(pv.getValue()).append("\")");
							break;
						case STRING:
							b.append("\"").append(pv.getValue()).append("\"");
							break;
						default:
							b.append(pv.getValue());
							break;
						}
					}
				}
				b.append(";\n");
				// add property to constructor case statement
				b3.append(tab).append("\t\t\tcase \"").append(pName).append("\":\n");
				b3.append(tab).append("\t\t\t\t").append(jName).append(" = (").append(getJavaType(dt,refClass)).append(") args.get(\"").append(pName).append("\");\n");
				b3.append(tab).append("\t\t\t\tbreak;\n");
			}
			// add getter/setter
			// readable property
			if((Boolean)struct.getPropertyQualifierValue(pName, "READ").getValue()) {
				if(isTrue(struct.getPropertyQualifierValue(pName,"DEPRECATED"))) b2.append("\t@Deprecated\n");
				// TODO: add defaultValue to Export if default value is present
				b2.append("\t").append(getExport(struct.getPropertyQualifiers(pName), false, struct.getNameSpacePath().getLocalPath(), dt.isReference() ? refClass : null,
						struct.getDefaultPropertyValue(pName)));
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
				String jName = toLowerCase(mName,dt);
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
					jName = toLowerCase(p.getName(),dt);
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
		b.append(getExport(cimEnum.getQualifiers(),true, cimEnum.getNameSpacePath().getLocalPath(), null, null));
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
	 * @param dataValue - default value, if not null
	 * @return - string containing export annotation
	 */
	private String getExport(List<Qualifier> q, boolean showVersion, String localNameSpace, String refClass, DataValue dataValue) {
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
			NamedElement e = locate(refClass,cimNameSpacePath);
			String javaClass = getJavaPackageName(e) + "." + getJavaName(e);
			b.append("refClass=\"").append(javaClass).append("\",");
		}
		if(dataValue != null) {
			b.append("defaultValue=\"").append(ModelUtilities.escape(dataValue.toString())).append("\",");
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
}
