/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Feb 17, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utility methods to map data structures between java and CIM classes. This class is used internally within Fusion, and
 * should not be used in application code.
 * @author Sharad Singhal
 */
public class JavaModelMapper {
	/** Logger for this class */
	private static Logger logger = Logger.getLogger(JavaModelMapper.class.getName());	
	/** Pattern to parse qualifiers for package path */
    private static Pattern packagePattern = Pattern.compile("(?i)^.*PackagePath\\s*\\(\\s*\\\"([^\"]+)(.*)$");
	/** flag for debugging */
	private static boolean debug = false;
	/**
	 * All methods in this class should be static, so we do not need to instantiate this class
	 */
	private JavaModelMapper() {
		return;
	}
	
	// TODO: This class needs to be re-factored to accommodate DataType.DEFINEDVALUE types
	
	/*
	 * *************************************
	 * Internal helper methods
	 * *************************************
	 */
		
	/**
	 * Get all annotated methods from a class (or superclasses/interfaces)
	 * @param c - class to introspect
	 * @return - vector containing all annotated methods
	 */
	private static Vector<Method> getAnnotatedMethods(Class<?> c){
		Vector<Method> v = new Vector<Method>();
		// get annotated methods for this class, including those in interfaces and super classes
		// TODO: the javadoc for getMethods() says that it will not return static methods from superTypes or interfaces
		for(Method m : c.getMethods()){
			Export cls = m.getAnnotation(Export.class);
			if(cls == null) continue;
			if(!v.contains(m)) v.add(m);
		}
		return v;
	}
	
	/**
	 * Check if a given java type matches a given CIM data type
	 * @param javaClass - java type to check
	 * @param cimType - CIM data type to check
	 * @param refClass - name of the expected CIM class (for ObjectReferences)
	 * @param struct - expected CIM structure (for StructureValues)
	 * @param enumeration - expected CIM enumeration (for EnumerationValues)
	 * @return - true if the types match, false otherwise
	 */
	private static boolean javaTypeMatchesCimType(Class<?> javaClass, DataType cimType, String refClass, CimStructure struct, CimEnumeration enumeration) {
		// cimType corresponding to the java type
		DataType javaType = DataType.getTypeForClass(javaClass);
		if(debug) System.out.println("JavaModelMapper: Matching "+cimType+" against "+javaType+" ["+(javaClass == null ? "Null" : javaClass.getName())+"]");
		// both must be arrays, or singletons
		if(cimType.isArray() != javaType.isArray()) return false;
		// if we have arrays, convert them to component types
		if(cimType.isArray()){
			cimType = cimType.getComponentType();
			javaType = javaType.getComponentType();
		}
		// validate the type
		switch(cimType){
		case OBJECTPATH:
			if(refClass == null) throw new ModelException("RefClass cannot be null in JavaModelMapper#javaTypeMatches");
			// referenced class name must match the given java class
			return refClass.equalsIgnoreCase(getCimClassName(javaClass)) ? true : false;
		case ENUMERATIONVALUE:
			if(enumeration == null) throw new ModelException("Enumeration cannot be null in JavaModelMapper#javaTypeMatches");
			// validate that we have a java enum that matches the expected CIM enumeration
			return javaEnumMatchesCimEnum(javaClass, enumeration);
		case STRUCTUREVALUE:
			if(struct == null) throw new ModelException("Internal error-- Struct cannot be null in JavaModelMapper#javaTypeMatches");
			// validate that we have a java class that matches the given CIM class
			return javaStructureMatchesCimStruct(javaClass,struct);
		case INSTANCEVALUE:
			if(struct == null) throw new ModelException("Internal error-- Struct cannot be null in JavaModelMapper#javaTypeMatches");
			// validate that we have a java class that matches the given CIM class
			if(!CimClass.class.isAssignableFrom(struct.getClass())){
				throw new ModelException("Internal error-- Expected CimClass, found CimStructure in JavaModelMapper#javaTypeMatches");
			}
			return javaClassMatchesCimClass(javaClass,(CimClass) struct);
		default:
			// all other types
			return cimType == javaType;
		}
	}
	
	/**
	 * Check if a java class (or array of classes) matches a CimClass
	 * @param javaClass - the java class to be matched
	 * @param struct - cim class definition to use
	 * @return - true if the java class matches the cim class, false otherwise
	 */
	private static boolean javaClassMatchesCimClass(Class<?> javaClass, CimClass struct) {
		if(debug) System.out.println("Check "+struct.toMOF()+" against "+javaClass.getName());
    	// get the java type (in case incoming type is array type)
    	Class<?> javaType = javaClass.isArray() ? javaClass.getComponentType() : javaClass;
    	if(debug) System.out.println("Check if Java type is a class "+javaType.getName());
    	if(isStructure(javaType)) return false;
    	if(debug) System.out.println("Check if class names match "+javaType.getName()+" "+struct.getName());
    	if(!struct.getName().equalsIgnoreCase(getCimClassName(javaType))) return false;
    	// validate that all features in the CIM class are present in the Java Class
    	HashMap<String,DataType> features = new HashMap<String,DataType>();
    	for(Method m : getAnnotatedMethods(javaType)){
    		String featureName = getFeatureName(m);
    		if(featureName == null) continue;
    		if(features.containsKey(featureName)) continue; // we will see duplicates for property getter/setters
    		Class<?> featureType = getFeatureType(m);
    		DataType dt = DataType.getTypeForClass(featureType);
    		features.put(featureName, dt);
    	}
    	// TODO: currently we only check property/method names -- should do a deeper inspection
    	// check that all CIM properties are defined in the java class
    	Map<String,CimProperty> props = struct.getAllProperties();
    	for(String key : props.keySet()){
    		if(features.containsKey(key)) continue;
    		if(debug) System.out.println("Did not find feature "+key);
    		return false;
    	}
    	// check that all CIM methods are defined in the java class
    	Map<String,CimMethod> methods = struct.getAllMethods();
    	for(String key : methods.keySet()){
    		if(features.containsKey(key)) continue;
    		if(debug) System.out.println("did not find feature "+key);
    		return false;    		
    	}
    	return true;
	}

	/**
	 * Check if a given java class (or array of classes) matches a CimStructure
	 * @param javaClass - java class to test
	 * @param struct - expected CIM Structure
	 * @return - true if match, false otherwise
	 */
	private static boolean javaStructureMatchesCimStruct(Class<?> javaClass, CimStructure struct) {
		if(debug) System.out.println("Check "+struct.toMOF()+" against "+javaClass.getName());
    	// get the java type (in case incoming type is array type)
    	Class<?> javaType = javaClass.isArray() ? javaClass.getComponentType() : javaClass;
    	if(debug) System.out.println("Check if Java type is a structure "+javaType.getName());
    	if(!isStructure(javaType)) return false;
    	if(debug) System.out.println("Check if structure names match "+javaType.getName()+" "+struct.getName());
    	if(!struct.getName().equalsIgnoreCase(getCimClassName(javaType))) return false;
    	// validate that all features in the CIM structure are present in the Java Class
    	HashMap<String,DataType> features = new HashMap<String,DataType>();
    	for(Method m : getAnnotatedMethods(javaType)){
    		String featureName = getFeatureName(m);
    		if(featureName == null) continue;
    		if(features.containsKey(featureName)) continue;
    		Class<?> featureType = getFeatureType(m);
    		DataType dt = DataType.getTypeForClass(featureType);
    		features.put(featureName, dt);
    	}
    	// TODO: currently we only check names-- should do a deeper inspection
    	Map<String,CimProperty> props = struct.getAllProperties();
    	for(String key : props.keySet()){
    		if(features.containsKey(key)) continue;
    		if(debug) System.out.println("Did not find feature "+key);
    		return false;
    	}
    	return true;
	}

	/**
     * Check if a java enum type (or array of enums) matches a CIM Enumeration. A match is declared if the Java enum is exported, and
     * all fields declared in the java enum are present in the CIM enumeration and vice versa, and the corresponding enum values match.
	 * @param javaType - java type to match. Must be enum or enum[] with the @Export annotation
	 * @param cimEnumType - CIM Enumeration to match
     * @return - true if match, false if not matched
     */
	protected static boolean javaEnumMatchesCimEnum(Class<? extends Object> javaType, CimEnumeration cimEnumType){
    	if(debug) System.out.println("Check "+cimEnumType.toMOF()+" against "+javaType.getName());
    	// get the java type (in case incoming type is array type)
    	Class<?> javaEnumType = javaType.isArray() ? javaType.getComponentType() : javaType;
    	if(debug) System.out.println("Check if Java type is Enum: "+javaType.getName());
    	if(!javaEnumType.isEnum()) return false;
    	if(debug) System.out.println("Check if Java type is exported "+javaType.getName());
    	if(!javaEnumType.isAnnotationPresent(Export.class)) return false;
    	if(debug) System.out.println("Check enum data type matches");
    	// get the data type for the enumeration, and validate that it matches the cimEnumType
    	try {
    		Method enumMethod = javaEnumType.getMethod("value", (Class<?>[])null);
    		DataType cimType = DataType.getTypeForClass(enumMethod.getReturnType());
    		if(!cimType.equals(cimEnumType.getDataType())){
    			if(debug) System.out.println(javaType.getName()+" Expected Enum Type "+cimEnumType.getDataType()+", found "+enumMethod.getReturnType());
    			return false;
    		}
    	} catch (NoSuchMethodException | SecurityException e) {
    		throw new ModelException(ExceptionReason.INVALID_ENUMERATION_CONTEXT,"JavaModelMapper#JavaEnumMatches unable to locate value() method in "+javaType.getName());
    	}
    	if(debug) System.out.println("Check all Java enum fields are present in CIM enum, and they match");
    	Enum<?>[] fields = (Enum[]) javaEnumType.getEnumConstants();
    	HashSet<String> seen = new HashSet<String>();
    	for(Enum<?> f : fields){
    		if(!cimEnumType.hasKey(f.name())) return false;
    		EnumerationValue cv = cimEnumType.getValue(f.name());
    		if(!JavaEnumValueMatches(f,cv)) return false;
    		seen.add(f.name());
    	}
    	if(debug) System.out.println("Check all CIM enum keys are are present in Java Enum");
    	for(String key : cimEnumType.getKeys()){
    		if(seen.contains(key)) continue;
    		return false;
    	}
    	if(debug) System.out.println("Enum Matched");
    	return true;
    }
	
	/**
	 * Check if a java enum value matches a CIM Enumeration value
	 * @param javaEnumValue - java enumeration value to match
	 * @param cimEnumerationValue - Cim Enumeration value to match
	 * @return - true if match, false otherwise
	 */
	protected static boolean JavaEnumValueMatches(Enum<?> javaEnumValue, EnumerationValue cimEnumerationValue){
		Class<? extends Enum> javaEnumType = javaEnumValue.getClass();
		if(debug) System.out.println("Check "+cimEnumerationValue.toMOF()+" against "+javaEnumValue);
		String cimEnumName = cimEnumerationValue.getName();
		DataValue cimEnumValue = cimEnumerationValue.getDataValue();

		if(debug) System.out.println("Check field in enum");
		if(!javaEnumValue.name().equalsIgnoreCase(cimEnumName)){
			if(debug) System.out.println("Enum not Matched. Expected "+cimEnumName+" found "+javaEnumValue.name());
			return false;
		};
		try {
			Method valueMethod = javaEnumType.getDeclaredMethod("value");
			Object val = valueMethod.invoke(javaEnumValue);
			DataValue javaValue = new DataValue(cimEnumValue.getType(),val);
			if(!cimEnumValue.equals(javaValue)){
				if(debug) System.out.println("JavaModelMapper#JavaEnumValueMatches: Expected value "+cimEnumValue+" found "+javaValue);
				return false;
			}
		} catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e1) {
			throw new ModelException(ExceptionReason.INVALID_ENUMERATION_CONTEXT,cimEnumName+" does not match "+javaEnumValue.name());
		}
		if(debug) System.out.println("Enum Value Matched");
		return true;
	}
	
	/**
	 * Convert a java object to a CIM dataValue
	 * @param javaObject - java object to be converted 
	 * @param type - CIM data type to use for conversion
	 * @param refClass - referenced className for objectPaths
	 * @param struct - expected structure for StructureValue
	 * @param enum1 - expected enumeration for EnumerationValue
	 * @return - converted DataValue
	 */
	private static DataValue convertJavaValueToCimValue(Object javaObject, DataType type, String refClass, CimStructure struct, CimEnumeration enum1) {
		try {
			return new DataValue(type,javaObject);
		} catch(Exception e){
			// check if both are arrays or both are singletons
			if(type.isArray() != javaObject.getClass().isArray()){
				if(debug) logger.fine("DataType "+type+" does not match Java type (isArray mismatch) "+javaObject.getClass());
				throw e;
			}
			// if we have arrays, convert them to component types
			if(type.isArray()){
				throw new ModelException("Array Values not yet implemented");
			} else {
				// handle object references
				switch(type){
				case OBJECTPATH:	// check a class reference against an annotated java class
					if(refClass == null) throw new ModelException("Internal error-- RefClass cannot be null in JavaModelMapper#convertJavaObjectToCim");
					throw new ModelException("StructureValue not yet implemented");
				case ENUMERATIONVALUE:	// check that the returned java Enum can be mapped to the correct Enumeration
					if(enum1 == null) throw new ModelException("Internal error-- Enumeration cannot be null in JavaModelMapper#convertJavaObjectToCim");
					// TODO: the returned enum key (or String value) must map to a valid Enumeration value in the CIM
					throw new ModelException("Enumeration not yet implemented");
				case STRUCTUREVALUE:	// check that the returned java Map can be mapped to the correct STRUCTURE value
					if(struct == null) throw new ModelException("Internal error-- Struct cannot be null in JavaModelMapper#convertJavaObjectToCim");
					// TODO: the returned Map should be valid property values for the given CIM Structure
					throw new ModelException("StructureValue not yet implemented");
				default:
					throw new ModelException("Internal Error-- ["+type+","+javaObject+"] not handled in JavaModelMapper#convertJavaObjectToCim");
				}
			}
		}
	}
	
	/**
	 * Transform an incoming method parameter to an expected parameter type. Handles conversions
	 * of primitive types (e.g., boolean <-> Boolean or Boolean[] <-> boolean[]) as needed.
	 * @param expectedType - type of parameter expected by the underlying java invocation
	 * @param givenParam - incoming parameter value for the java invocation
	 * @return - object containing expected parameter value to be be used
	 */
	protected static Object getInvocationParameter(Class<?> expectedType, Object givenParam){
		if(givenParam == null) return null;
		Class<?> givenType = givenParam.getClass();
		if(debug) System.out.println("Check Type "+givenType.getName()+" (given), against "+expectedType.getName()+" (expected)");
		// check if the parameter types match
		if(debug) System.out.println("Check if equal:");
		if(givenType.equals(expectedType)){
			return givenParam;
		}

		// if expected parameter is assignable from given parameter, return assigned value
		if(debug) System.out.println("Check if assignable:");
		if(expectedType.isAssignableFrom(givenType)){
			return expectedType.cast(givenParam);
		}

		// handle primitive types (e.g., boolean <-> Boolean)
		if(debug) System.out.println("Check primitive types:");
		if((expectedType.isPrimitive() || givenType.isPrimitive()) &&
				(expectedType == Boolean.TYPE && givenType == Boolean.class ||
				expectedType == Integer.TYPE && givenType == Integer.class ||
				expectedType == Byte.TYPE && givenType == Byte.class ||
				expectedType == Short.TYPE && givenType == Short.class ||
				expectedType == Long.TYPE && givenType == Long.class ||
				expectedType == Float.TYPE && givenType == Float.class ||
				expectedType == Double.TYPE && givenType == Double.class ||
				expectedType == Character.TYPE && givenType == Character.class ||
				givenType == Boolean.TYPE && expectedType == Boolean.class ||
				givenType == Integer.TYPE && expectedType == Integer.class ||
				givenType == Byte.TYPE && expectedType == Byte.class ||
				givenType == Short.TYPE && expectedType == Short.class ||
				givenType == Long.TYPE && expectedType == Long.class ||
				givenType == Float.TYPE && expectedType == Float.class ||
				givenType == Double.TYPE && expectedType == Double.class ||
				givenType == Character.TYPE && expectedType == Character.class)){
			return givenParam;
		}

		// handle primitive arrays
		if(expectedType.isArray() && givenType.isArray() && 
				(expectedType.getComponentType().isPrimitive() ||
						givenType.getComponentType().isPrimitive())){
			if(debug) System.out.println("Primitive Array Check:");
			// construct an array of the appropriate type
			Class<?> pType = givenType.getComponentType();
			Class<?> eType = expectedType.getComponentType();
			if(eType == Boolean.TYPE && pType == Boolean.class){
				Boolean [] params = (Boolean[])givenParam;
				boolean [] a = new boolean[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(eType == Integer.TYPE && pType == Integer.class){
				Integer [] params = (Integer[])givenParam;
				int [] a = new int[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(eType == Byte.TYPE && pType == Byte.class){
				Byte[] params = (Byte[])givenParam;
				byte [] a = new byte[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(eType == Short.TYPE && pType == Short.class){
				Short[] params = (Short[])givenParam;
				short [] a = new short[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(eType == Long.TYPE && pType == Long.class){
				Long[] params = (Long[])givenParam;
				long [] a = new long[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(eType == Float.TYPE && pType == Float.class){
				Float[] params = (Float[]) givenParam;
				float [] a = new float[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(eType == Double.TYPE && pType == Double.class){
				Double[] params = (Double[])givenParam;
				double [] a = new double[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(eType == Character.TYPE && pType == Character.class){
				Character[] params = (Character[])givenParam;
				char [] a = new char[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Boolean.TYPE && eType == Boolean.class){
				boolean[] params = (boolean[])givenParam;
				Boolean [] a = new Boolean[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Integer.TYPE && eType == Integer.class){
				int[] params = (int[])givenParam;
				Integer [] a = new Integer[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Byte.TYPE && eType == Byte.class){
				byte[] params = (byte[])givenParam;
				Byte [] a = new Byte[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Short.TYPE && eType == Short.class){
				short[] params = (short[])givenParam;
				Short [] a = new Short[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Long.TYPE && eType == Long.class){
				long[] params = (long[])givenParam;
				Long [] a = new Long[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Float.TYPE && eType == Float.class){
				float[] params = (float[])givenParam;
				Float [] a = new Float[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Double.TYPE && eType == Double.class){
				double[] params = (double[])givenParam;
				Double [] a = new Double[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			} else if(pType == Character.TYPE && eType == Character.class){
				char[] params = (char[])givenParam;
				Character [] a = new Character[params.length];
				for(int i = 0; i<a.length; i++){
					a[i] = params[i];
				}
				return a;
			}
		}
		throw new ModelException(ExceptionReason.TYPE_MISMATCH,"JavaModelMapper#getInvocationParameter: Parameter expected "+expectedType.getName()+" found "+givenType.getName());
	}
	
	/*
	 * **************************************
	 * Methods to introspect Java annotations to bind to MOF elements
	 * **************************************
	 */
	
	/**
	 * Get the package path annotation needed on MOF to allow binding to a java class
	 * @param javaClass - java class (must be non-null) to introspect
	 * @return - string containing package path
	 */
	public static String getPackagePath(Class<?> javaClass){
		// check if the java class is an annotated class with PackagePath qualifier defined. If so return it
		Export exp = javaClass.getAnnotation(Export.class);
		if(exp != null){
			String qualifiers = exp.qualifiers();
			Matcher m = packagePattern.matcher(qualifiers);
			if(m.matches() && m.groupCount() == 2){
				return m.group(1);
			}
		}
		// no annotation, or no matching qualifier, construct the package path from the java class path
		return javaClass.getPackage().getName().replaceAll("\\.", "::");
	}
	
	/**
	 * Get the CIM element type (Enumeration, Interface, Structure or Class) corresponding to a java class
	 * @param javaClass - class to introspect
	 * @return - Element type for the class
	 */
	public static ElementType getCimElementType(Class<?> javaClass) {
		// TODO: Thiis needs repair-- these should be annotated classes
		if(javaClass.isEnum()){
			return ElementType.ENUMERATION;
		} else if(javaClass.isInterface()){
			return ElementType.INTERFACE;
		} else if(isStructure(javaClass)){
			return ElementType.STRUCTURE;
		} 
		return ElementType.CLASS;
	}
	
	/**
	 * Check if a class can map to a CIM Structure
	 * @param javaClass - java class to check
	 * @return - true if the given java class can be linked to a CIM structure, false otherwise
	 */
	public static boolean isStructure(Class<?> javaClass) {
		Export annotation = javaClass.getAnnotation(Export.class);
		if(annotation == null) return false;	// not an annotated class
		if(javaClass.isEnum()) return false;	// enumeration class
		if(javaClass.isInterface()) return false;	// interface class
		// if(annotation.isStructure()) return true;	// isStructure() annotation given
		// check if all exported methods in the class hierarchy are non-key property methods
		Vector<Method> methods = getAnnotatedMethods(javaClass);
		for(Method m : methods){
			if(!isPropertyMethod(m)) return false;	// found a non-property method
			
		}
		// we possibly have a structure; check if forceClass() is present
		return annotation.forceClass() ? false : true;
	}
	
	/**
	 * Get the CIM class name corresponding to an annotated java class
	 * @param javaClassName - name of the java class to introspect
	 * @return - name of the corresponding CIM element in Schema_ClassName form. Null if the class is not annotated
	 * @see Class#getName()
	 */
	public static String getCimClassName(String javaClassName){
		// NOTE: Embedded classes require the '$' to indicate to the classloader that they are embedded classes.
		// thus names obtained by class.getName() will load, but not those obtained by class.getCanonicalName().
		try {
			return getCimClassName(Class.forName(javaClassName));
		} catch (Exception e) {
			throw new ModelException(ExceptionReason.NOT_FOUND,"Unable to load java class "+javaClassName,e);
		}
	}
	
	/**
	 * Obtain the CIM class name corresponding to some annotated java class.
	 * @param javaClass - java class to introspect
	 * @return - name of the corresponding CIM element in schema_className form. Returns null if the class does not have an @Export annotation
	 */
	public static String getCimClassName(Class<?> javaClass){
		// if class is an array, get the base class 
		if(javaClass.isArray()){
			javaClass = javaClass.getComponentType();
		}
		// get the annotation
		Export cls = javaClass.getAnnotation(Export.class);
		if(cls == null) {
			return null;
		}
		// construct the CIM class name
		String className = !cls.name().isEmpty() ? cls.name() : javaClass.getSimpleName();
		String schema = !cls.schema().isEmpty() ? cls.schema() : Constants.defaultSchema;
		return schema + "_" + className;
	}
	
	/**
	 * Get the CIM name of the superType of a java class. The java class hierarchy is traversed to find the nearest annotated
	 * superclass, and its CIM name is returned.
	 * @param javaClass - java class to introspect
	 * @return - CIM name of the nearest CIM superType, if any. Null returned if no CIM superType exists
	 */
	public static String getCimSuperClassName(Class<?> javaClass){
		if(javaClass == null) return null;
		// get the superClass for this class
		Class<?> sup = javaClass.getSuperclass();
		// if no further reduction possible, return null
		if(sup == null || sup == Object.class || sup == Enum.class) return null;
		// check if superClass is an annotated class. If so, return its CIM name
		// else recurse up to the next annotated class
		String superType = getCimClassName(sup);
		return superType != null ? superType : getCimSuperClassName(sup);
	}

	/**
	 * Get the name of the CIM feature represented by an exported Java Method
	 * @param javaMethod - exported Java method to test
	 * @return - name of the CIM feature (property or method) represented by this java method. Null if the java method is not exported
	 */
	public static String getFeatureName(Method javaMethod){
		Export cls = javaMethod.getAnnotation(Export.class);
		// non-exported method returns null
		if(cls == null) return null;
		
		// if method is annotated with name, return it
		String name = cls.name();
		if(!name.isEmpty()) return name;	
		
		// get the name of the java method
		name = javaMethod.getName();
		// handle getXXX() and setXXX() methods
		if(name.startsWith("get") && isGetter(javaMethod) ||
				name.startsWith("set") && isSetter(javaMethod)) return name.substring(3);	
		
		// handle isXXX() method
		if(name.startsWith("is") && isGetter(javaMethod) &&
				DataType.getTypeForClass(javaMethod.getReturnType()).isBoolean()) return name.substring(2);
		
		// method name is the feature name
		return name;
	}
	
	/**
	 * Get the java type for a CIM feature represented by an exported Java Method<br>
	 * This is the java type for the property getter or setter, or type returned by a method
	 * @param javaMethod - java method representing the feature
	 * @return - java type corresponding to the feature
	 */
	public static Class<?> getFeatureType(Method javaMethod){
		Class<?> returnType = javaMethod.getReturnType();	// method return type
		// Test if this method represents a property setter, if so, we return the property type
		Parameter [] parameters = javaMethod.getParameters();
		if(returnType == void.class && parameters.length == 1 && DataType.isCimType(parameters[0].getType())) return parameters[0].getType();
		// getter and methods return type
		return returnType;
	}
	
	/**
	 * Get the CIM class referenced by a java method defining a complex property or method	
	 * @param m - java method to introspect
	 * @return - CIM class name. Empty string for non-complex properties or methods
	 */
	public static String getRefCimClass(Method m){
		String className = getMappedJavaClassName(m);
		return className.isEmpty() ? className : getCimClassName(className);
	}
	
	/**
	 * Get the CIM class referenced by a java parameter defining a complex property (or method)	
	 * @param p - java parameter to introspect
	 * @return - CIM class name. Empty for non-complex parameters
	 */
	public static String getRefCimClass(Parameter p){
		String className = getMappedJavaClassName(p);
		return className.isEmpty() ? className : getCimClassName(className);
	}
	
	/**
	 * Get the name of the java class referenced by a property or returned by a method
	 * @param m - Method to introspect
	 * @return - java class name for complex properties, else empty string
	 */
	public static String getMappedJavaClassName(Method m){
		Class<?> javaType = getFeatureType(m);
		DataType cimType = DataType.getTypeForClass(javaType);
		
		// primitive or void types do not need mapping strings
		if(cimType.isPrimitive() || cimType == DataType.VOID) return "";

		// have a complex feature type, check if we can infer the class
		Class<?> type = javaType.isArray() ? javaType.getComponentType() : javaType;
		String mappedClass = type.getName();
		
		// refClass annotation must be present for java classes corresponding to CIM complex types
		if(type == CimInstance.class || type == StructureValue.class || type == EnumerationValue.class || type == ObjectPath.class){
			Export annotation = m.getAnnotation(Export.class);
			if(annotation == null || annotation.refClass().isEmpty())
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,"RefClass annotation required on method "+m.getName());
			mappedClass = annotation.refClass();
		}
		return mappedClass;
	}
	
	/**
	 * Get the name of the java class referenced by method parameter
	 * @param p - parameter to introspect
	 * @return - java class name for complex properties, else empty string
	 */
	public static String getMappedJavaClassName(Parameter p){
		Class<?> javaType = p.getType();
		DataType cimType = DataType.getTypeForClass(javaType);
		if(cimType.isPrimitive() || cimType == DataType.VOID) return "";
		
		// have complex type; check if we can infer the class
		Class<?> type = javaType.isArray() ? javaType.getComponentType() : javaType;
		String mappedClass = type.getName();
		
		// if we have generic classes being returned, annotation must be present for mapping string
		if(type == CimInstance.class || type == StructureValue.class || type == EnumerationValue.class || type == ObjectPath.class){
			Export annotation = p.getAnnotation(Export.class);
			if(annotation == null || annotation.refClass().isEmpty())
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,"RefClass Annotation required on parameter "+p.getName());
			mappedClass = annotation.refClass();
		}
		return mappedClass;
	}
	
	/**
	 * Get the MappingString{} qualifier for an exported method that returns complex values or references
	 * @param m - method to introspect
	 * @return - MappingString qualifier. Empty string if the method is not a complex value or a reference
	 */
	public static String getMappingString(Method m){
		String mappedClass = getMappedJavaClassName(m);
		if(mappedClass.isEmpty()) return "";
		StringBuilder b = new StringBuilder("MappingStrings{\"");
		b.append(Constants.fusionMap).append(mappedClass).append("\"}");
		return b.toString();
	}
		
	/**
	 * Get the MappingString{} qualifier for a method parameter that represents a complex value or reference
	 * @param p - parameter to introspect
	 * @return - mappingString qualifier. Empty string if the parameter is not complex or reference
	 */
	public static String getMappingString(Parameter p){
		String mappedClass = getMappedJavaClassName(p);
		if(mappedClass.isEmpty()) return "";
		StringBuilder b = new StringBuilder("MappingStrings{\"");
		b.append(Constants.fusionMap).append(mappedClass).append("\"}");
		return b.toString();
	}
	
	/**
	 * Get the version associated with with an annotated java class
	 * @param javaClass - java class to introspect
	 * @return - CIM version of the class
	 */
	public static String getClassVersion(Class<?> javaClass){
		Export annotation = javaClass.getAnnotation(Export.class);
		if(annotation != null) {
			// check if we have version specified in a qualifier annotation
			String qualifiers = annotation.qualifiers();
			if(!qualifiers.isEmpty()){
				Pattern pattern = Pattern.compile("(?i)^.*Version\\s*\\(\\s*\\\"([^\"]+)(.*)$");
				Matcher m = pattern.matcher(qualifiers);
				if(m.matches() && m.groupCount() == 2){
					return m.group(1);
				}
			}
			// check if we have the version annotation
			if(!annotation.version().equals(Constants.defaultVersion)) return annotation.version();
		}
		// no version annotation or qualifier given
		return Constants.defaultVersion;
	}
	
	/**
	 * Test if an exported method represents a CIM property
	 * @param javaMethod - exported method to test
	 * @return - true if this method is a CIM property, false otherwise
	 */
	public static boolean isPropertyMethod(Method javaMethod){
		// a method is a property method if it is either a getter or a setter
		return isGetter(javaMethod) || isSetter(javaMethod);
	}
	
	/**
	 * Test if an exported java method represents a property getter
	 * @param javaMethod - exported method to test
	 * @return - true if the method is a getter, false otherwise
	 */
	public static boolean isGetter(Method javaMethod){
		// a method is a property getter if it has no parameters, and returns a valid CIM data type
		Class<?> returnType = javaMethod.getReturnType();
		Parameter [] parameters = javaMethod.getParameters();
		return parameters.length == 0 && returnType != void.class && DataType.isCimType(returnType) ? true : false;
	}
	
	/**
	 * Test if an exported java method represents a property setter
	 * @param javaMethod - java method to test
	 * @return - true if the method is a property setter, false otherwise
	 */
	public static boolean isSetter(Method javaMethod){
		// a method is a property setter if it returns void, and has one parameter which is a valid CIM data type
		Class<?> returnType = javaMethod.getReturnType();
		Parameter [] parameters = javaMethod.getParameters();
		return returnType == void.class && parameters.length == 1 && DataType.isCimType(parameters[0].getType()) ? true : false;
	}
	
	/**
	 * Get the object path corresponding to a Java class. Note that the class must be bound to a CIM element to be accessible
	 * via the object path
	 * @param javaClass - java class to introspect
	 * @return - object path corresponding to the corresponding NamedElement. Null if the java class is not an annotated class
	 */
	public static ObjectPath getObjectPathFromClass(Class<?> javaClass){
		// Check that the class is exported, and get the CIM name of the class
		String cimClassName = getCimClassName(javaClass);
		if(debug) System.out.println("JavaModelMapper#getObjectPathFromClass: cimClassName "+cimClassName);
		if(cimClassName == null) return null;
		
		// get the Element Type corresponding to the class
		ElementType elementType = getCimElementType(javaClass);
		
		// get the namespace path corresponding to the class
		NameSpacePath path = getNameSpacePathFromClass(javaClass);
		return new ObjectPath(elementType,cimClassName,path,null, null);
	}
	
	/**
	 * Get a namespace path from an annotated java class
	 * @param javaClass - java class to be introspected
	 * @return - namespace path from the java annotation
	 */
	public static NameSpacePath getNameSpacePathFromClass(Class<?> javaClass) {
		if(javaClass.isArray()) {
			javaClass = javaClass.getComponentType();
		}
		Export exp = javaClass.getAnnotation(Export.class);
		if(exp == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,javaClass.getName()+" is not an annotated class");
		return new NameSpacePath(exp.nameSpace());
	}
	
	// Tested to here in JavaModelMapperTest
	/* TODO:  -------- we are here ------------ */
	
    /*
     * ***********************************************
     * Java bindings to CIM Methods and method invocation
     * ***********************************************
     */
	
	/**
	 * Validate that a java method can be bound to a CIM Method
	 * @param cimMethod - CimMethod to be used for binding
	 * @param javaMethod - Java Method to be used for binding 
	 * @param implObject - implementation object to be used for invocation.
	 */
	public static void validateMethodBinding(CimMethod cimMethod, Method javaMethod, Object implObject) {
		// validate that the method names match
		String cimName = cimMethod.getFullName();		// used in exception messages
		String javaName = getFeatureName(javaMethod);	// CIM name corresponding to the feature
		if(!cimMethod.getName().equalsIgnoreCase(javaName)){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,cimName+": does not match java method name "+javaName);
		}
		
		// validate that the java return type matches the CIM return type
		DataType cimType = cimMethod.getReturnedType();
		Class<?> javaType = javaMethod.getReturnType();
		if(!javaTypeMatchesCimType(javaType,cimType, cimMethod.getRefClassName(), cimMethod.getStruct(), cimMethod.getEnum())){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,cimName+
					": Error binding Method. Expected return type "+cimType.toString()+" found "+javaType.getName());
		}
		
		// validate that the java parameters match
		Class<?>[] paramTypes = javaMethod.getParameterTypes();
		List<CimParameter> cimParameters = cimMethod.getParameters();
		//check that the number of parameters matches
		if(paramTypes.length != cimParameters.size()) throw new ModelException(ExceptionReason.TYPE_MISMATCH,
				cimName+": Error binding Method. Expected 0 parameters, found "+paramTypes.length);
		// check that the java type can be converted to the expected cim type
		// NOTE that this logic assumes that the java method arguments are in the same order as defined in the CIM method
		for(int i = 0; i < paramTypes.length; i++){
			cimType = cimParameters.get(i).getDataType();
			javaType = paramTypes[i];
			if(!javaTypeMatchesCimType(javaType,cimType, cimMethod.getRefClassName(), cimMethod.getStruct(), cimMethod.getEnum())){
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,cimName+
							": Error binding Parameter "+i+". Expected type "+cimType.toString()+" found "+javaType.getName());
			}
		}
		
		// Check that STATIC methods map, and for non-static methods, the implementation object implements
		// the desired method
		boolean cimMethodIsStatic = cimMethod.isStatic();
		boolean javaMethodIsStatic = (javaMethod.getModifiers() & Modifier.STATIC) != 0;
		if(javaMethodIsStatic != cimMethodIsStatic){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,
					cimName+": Method "+javaMethod.getName()+" does not match in its STATIC modifiers");
		} else if(!javaMethodIsStatic){
			// have a instance method, check that the implementation object contains the method
			if(implObject == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,
					cimName+": Implementation object cannot be null for non-static methods");
			// check that the implObject implements the method
			Class<?> javaClass = implObject.getClass();
			try {
				Method implMethod = javaClass.getMethod(javaMethod.getName(), javaMethod.getParameterTypes());
				if(!implMethod.equals(javaMethod)){
					throw new ModelException(ExceptionReason.METHOD_NOT_FOUND,
							cimName+": Implementation class "+javaClass.getName()+" does not implement method "+javaMethod.getName());
				}
			} catch (SecurityException e) {
				throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,
						cimName+": Method "+javaMethod.getName()+" is not accessible in implementation class "+javaClass.getName());
			} catch (NoSuchMethodException e) {
				throw new ModelException(ExceptionReason.METHOD_NOT_FOUND,
						cimName+": Implementation class "+javaClass.getName()+" does not implement method "+javaMethod.getName());
			}
		}
		return;
	}

	/**
	 * Invoke a Java method bound to a CIM Method
	 * @param cimMethod - CIM Method being invoked
	 * @param boundObject - bound java object
	 * @param boundMethod - bound java method
	 * @param cimParameters - CIM parameters to be used in this method call
	 * @return - Data Value returned from the method. Null if the method returns void
	 */
    public static DataValue invokeMethod(CimMethod cimMethod, Object boundObject, Method boundMethod,List<CimParameter> cimParameters) {
    	DataType cimType = cimMethod.getReturnedType();
    	if(boundMethod == null){
    		throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,"JavaModelMapper: Method ["+cimMethod.getFullName()+"] is not available");
    	}
    	if(debug) System.out.println("Invoke: "+cimMethod.getFullName());
    	// check the number of parameters declared and the number of parameters in the invocation
    	Class<?>[] methParamTypes = boundMethod.getParameterTypes();
    	int declParams = methParamTypes.length;											// number of parameters declared in java method
    	int actualParams = cimParameters == null ? 0 : cimParameters.size();			// number of actual parameters passed to the invoker
    	if(declParams != actualParams) throw new ModelException(ExceptionReason.INVALID_PARAMETER,cimMethod.getFullName()+
    			" Expected "+declParams+" parameters, found "+actualParams);
    	// get the invocation parameter list
    	Object [] invocationParams = new Object[declParams];
    	for(int i=0; i< declParams; i++){
    		invocationParams[i] = getInvocationParameter(methParamTypes[i], cimParameters.get(i).getValue().getValue());
    	}
    	if(debug){
    		for(int i = 0; i<invocationParams.length;i++){
    			System.out.println("Invoked\t"+invocationParams[i].getClass().getName());
    		}
    	}
    	try {    		
    		// invoke the operation
    		Object returnObject = boundMethod.invoke(boundObject, invocationParams);
    		return cimType == DataType.VOID ? null : new DataValue(cimType,returnObject);
    	} catch (Exception e) {
    		if(!(e instanceof ModelException)){
    			throw new ModelException(cimMethod.getFullName()+": Method invocation error ",e);
    		} else {
    			throw (ModelException) e;
    		}
    	}
    }

	/*
	 * ***********************************************
	 * Java bindings to Properties and property getters and setters
	 * ***********************************************
	 */
	
	
	/**
	 * Validate that a pair of Java methods (getter and setter pair) can be bound to a cim property
	 * @param cimProperty - CimProperty to be used for binding
	 * @param getter - getter method to be bound
	 * @param setter - setter method to be bound
	 * @param implObject - Java implementation object to be used for invocation
	 */
	public static void validatePropertyBinding(CimProperty cimProperty, Method getter, Method setter, Object implObject){
		String cimName = cimProperty.getOriginClass()+"#"+cimProperty.getName();
		DataType dataType = cimProperty.getDataType();
		// check the getter for match with the bindings given
		if(getter != null){
			Class<?>[] paramTypes = getter.getParameterTypes();
			Class<?> javaReturnType = getter.getReturnType();
			// getter method should not declare any parameters
			if(paramTypes.length != 0) throw new ModelException(ExceptionReason.TYPE_MISMATCH,
					cimName+": Error binding property getter. Expected 0 parameters, found "+paramTypes.length);
			// validate that the java type can be converted to the expected cim type
			if(!javaTypeMatchesCimType(javaReturnType,dataType, cimProperty.getRefClassName(), cimProperty.getStruct(), cimProperty.getEnum())){
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,cimName+
							": Error binding property getter. Expected type "+dataType.toString()+" found "+javaReturnType.getName());
			}
		}
		// check the setter against the bindings given
		if(setter != null){
			// property is writable, and method is available
			Class<?>[] paramTypes = setter.getParameterTypes();
			Class<?> javaReturnType = setter.getReturnType();
			// setter must declare one matching parameter
			if(paramTypes.length != 1) throw new ModelException(ExceptionReason.TYPE_MISMATCH,
					cimName+": Error binding property setter. Expected 1 parameter, found "+paramTypes.length);
			// setter must return void
			if(javaReturnType != void.class) throw new ModelException(ExceptionReason.TYPE_MISMATCH,
					cimName+": Error binding property setter. Expected void type found "+javaReturnType.getName());
			
			if(!javaTypeMatchesCimType(paramTypes[0],dataType,cimProperty.getRefClassName(),cimProperty.getStruct(),cimProperty.getEnum())){
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,cimName+
						": Error binding property setter. Expected type "+dataType.toString()+" found "+paramTypes[0].getName());
			}
		}
		// Check that STATIC methods map, and for non-static methods, the implementation object implements
		// the desired method
		boolean cimPropertyIsStatic = cimProperty.isStatic();
		for(Method javaMethod : new Method[]{getter, setter}){
			if(javaMethod == null) continue;
			boolean javaMethodIsStatic = (javaMethod.getModifiers() & Modifier.STATIC) != 0;
			if(javaMethodIsStatic != cimPropertyIsStatic){
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,
						cimName+": Method "+javaMethod.getName()+" does not match in its STATIC modifiers");
			} else if(!javaMethodIsStatic){
				// have a instance method, check that we can bind to the implementation
				if(implObject == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,
						cimName+": Implementation object cannot be null for non-static properties");
				// check that the implObject implements the method
				Class<?> javaClass = implObject.getClass();
				try {
					Method implMethod = javaClass.getMethod(javaMethod.getName(), javaMethod.getParameterTypes());
					if(!implMethod.equals(javaMethod)){
					throw new ModelException(ExceptionReason.METHOD_NOT_FOUND,
							cimName+": Implementation class "+javaClass.getName()+" does not implement method "+javaMethod.getName());
					}
				} catch (SecurityException e) {
					throw new ModelException(ExceptionReason.METHOD_NOT_AVAILABLE,
							cimName+": Method "+javaMethod.getName()+" is not accessible in implementation class "+javaClass.getName());
				} catch (NoSuchMethodException e) {
					throw new ModelException(ExceptionReason.METHOD_NOT_FOUND,
							cimName+": Implementation class "+javaClass.getName()+" does not implement method "+javaMethod.getName());
				}
			}
		}
		return;
	}
	
	/**
	 * Read a property value from a Java Object. Note that this method assumes that the getter has been validated when the property
	 * was bound to the java object, and does not re-check the data types or method arguments
	 * @param cimProperty - Cim property to use for reading value
	 * @param getter - getter method associated with the property
	 * @param javaObject - Java Implementation object bound to the property
	 * @return - DataValue containing the property value
	 */
	public static DataValue readPropertyValue(CimProperty cimProperty, Method getter, Object javaObject) {
		try {
			Object javaReturn = getter.invoke(javaObject, (Object[])null);
			DataType type = cimProperty.getDataType();
			return convertJavaValueToCimValue(javaReturn,type,cimProperty.getRefClassName(),cimProperty.getStruct(),cimProperty.getEnum());
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new ModelException(cimProperty.getFullName()+": Error reading bound property",e);
		}
	}

	/**
	 * Write a property value to a java object. Note that this method assumes that the setter has been validated when
	 * the property was bound to the java object, and does not re-check the data types or method arguments
	 * @param cimProperty - Cim Property bound to this setter
	 * @param setter - setter method in the java object
	 * @param javaObject - java implementation object
	 * @param value - Cim DataValue to write to the object
	 */
	public static void writePropertyValue(CimProperty cimProperty, Method setter, Object javaObject, DataValue value) {
		try {
			setter.invoke(javaObject, getInvocationParameter(setter.getParameterTypes()[0], value.getValue()));
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new ModelException(cimProperty.getFullName()+": Error writing data value "+value,e);
		}
	}
	
	
	/*
	 * *******************************************
	 * Left over code from earlier implementations
	 * *******************************************
	 *
	
	/**
     * Check if the method signature on a CIM method matches a java method
     * TODO: This needs revision to new version of the binding
     * @param cimMethod - CimMethod to be matched
     * @param javaMethod - java method to be used for matching
     * @return - true if the CIM Method matches the java method, false otherwise
     *
    private static boolean methodSignatureMatches(CimMethod cimMethod, Method javaMethod){
    	if(debug) System.out.println("*******************\nCheck "+cimMethod.toMOF()+" against "+javaMethod.toGenericString());
    	
    	if(debug) System.out.println("Check Parameters...");
    	
		// check that all java parameter types match
		Class<?> javaParams[] = javaMethod.getParameterTypes();
		List<CimParameter> cimParams = cimMethod.getParameters();
		
		boolean needMapForReturn = false;
		boolean haveMapParameter = javaParams.length > 0 && Map.class.isAssignableFrom(javaParams[javaParams.length-1]) ? true : false;
		for(int i=0, j=0; i < cimParams.size(); i++){
			// skip [IN(false)] parameters
			CimParameter cp = cimParams.get(i);
			if(!cp.isInput()){
				// map output is necessary if we have [Out(true)] parameters
				if(cp.isOutput()) needMapForReturn = true;
				if(debug) System.out.println("\tSkip non-input parameter "+cp.getName());
				continue;
			}
			if(j >= javaParams.length && !haveMapParameter){
				if(debug) System.out.println("Insufficient Java Parameters");
				throw new ModelException(ExceptionReason.TYPE_MISMATCH,
						cimMethod.getName()+": Error binding. Expected "+cimParams.size()+" parameters, found "+javaParams.length);
				
			}
			if(j == javaParams.length && haveMapParameter) continue;	// all remaining parameters will map here
			Class<?> jp = javaParams[j++];
			if(debug) System.out.println("\tChecking parameter "+cp.getName()+" against "+jp.getName());
			if(!paramTypeMatches(cp,jp))
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,
						cimMethod.getName()+": Expected "+cp.getDataType()+" found "+jp.getName());
		}
		
		if(debug) System.out.println((needMapForReturn ? "Map Required " : "")+"Check ReturnType...");
		// check that the return type matches
		Class<?> returnType = javaMethod.getReturnType();	// java return type
		
		// check if map is returned
		if(Map.class.isAssignableFrom(returnType)){
			if(debug) System.out.println("\tJava Method Returned Map\n...Matched");
			return true;
		} else if(needMapForReturn){
			if(debug) System.out.println("\tJava Method Needs to Return Map\n...NOT Matched");
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,
					javaMethod.getName()+": Expected (java.util.Map) for return type found "+returnType);
		}
		
		// actual types match
		if(cimMethod.getReturnedType() == DataType.getTypeForClass(returnType)){
			if(debug) System.out.println("...Matched");
	    	return true;
		}
		
		// Enums (or Enum arrays) returned in java match to strings (or string arrays) in CIM
		// if(returnType.isEnum() && cimMethod.getReturnedType() == DataType.STRING && javaEnumMatches(cimMethod,returnType)) {
		if(returnType.isEnum()){
			// java return type is enum. Cim type must be String with ValueMap qualifier
			if(debug) System.out.println("\tCIM returns STRING and Java returns ENUM\n...Matched");
			// return;
			throw new ModelException("Not yet implmented");
		}
		// if(returnType.isArray() && cimMethod.getReturnedType() == DataType.STRING_ARRAY && 
		//		returnType.getComponentType().isEnum() && javaEnumMatches(cimMethod,returnType)){
		if(returnType.isArray() && returnType.getComponentType().isEnum()){
			if(debug) System.out.println("\tCIM returns STRING[] and Java returns ENUM[]\n...Matched");
			// return;	
			throw new ModelException("Not yet implmented");
		}
		
    	// known items did not match
		if(debug) System.out.println("Expected "+cimMethod.getReturnedType()+" found "+returnType+"\n...NOT Matched");
		return false;
    }

	/**
	 * Check if a java type matches a CIM Structure. A match is declared if the java type can be
	 * cast to a Map<?,?> class
	 * @param javaType - java type to match.
	 * @param structure - CIM Structure to match
	 * @return - true if match, false otherwise
	 *
	private static boolean javaMapMatches(Class<?> javaType, CimStructure structure) {
		if(debug) System.out.println("Check "+structure.toMOF()+" against "+javaType.getCanonicalName());
		// get the java type (in case incoming type is array type)
    	Class<?> javaMapType = javaType.isArray() ? javaType.getComponentType() : javaType;
    	if(debug) System.out.println("Check if Java type is Map: "+javaType.getCanonicalName());
    	if(!Map.class.isAssignableFrom(javaMapType)) return false;
    	if(debug) System.out.println("Structure Matched");
		return true;
	}

	/**
	 * Check if a Cim Reference type matches a java type. A match is declared if the java type is exported
	 * and the name constructed from the annotation matches the given CIM class name
	 * @param refClassName - name of the CIM reference type
	 * @param javaType - java type to match
	 * @return - true if match, false otherwise
	 *
	private static boolean javaClassMatches(String refClassName, Class<?> javaType) {
		if(debug) System.out.println("Check "+refClassName+" against "+javaType.getCanonicalName());
    	// get the java type (in case incoming type is array type)
    	Class<?> javaClassType = javaType.isArray() ? javaType.getComponentType() : javaType;
    	if(debug) System.out.println("Check if Java type is exported "+javaType.getCanonicalName());
    	if(!javaClassType.isAnnotationPresent(Export.class)) return false;
    	if(debug) System.out.println("Check if Java type is refClass: "+javaType.getCanonicalName());
    	String className = getCimClassName(javaType);
    	if(className == null || !className.equalsIgnoreCase(refClassName)) return false;
    	if(debug) System.out.println("RefClass Matched");
    	return true;
	}
	
	/*
	 * *******************************************
	 * Conversion methods to convert between types
	 * *******************************************
	 *
	
	/**
	 * Given a parameter type that is an enum, or array of enums, and a corresponding string (or String []) object,
	 * return an appropriate Enum (or array of Enum) values
	 * @param expectedType - type (<? extends Enum> or <? extends Enum> []) of parameter expected
	 * @param param - String (or String []) representation
	 * @return Enum (or Enum[]) of type paramType, with values corresponding to string value(s) provided
	 *
	private static Object getEnumForString(Class<?> expectedType, Object param){
		if(debug) System.out.println("****** GetEnumForString "+ expectedType.getCanonicalName()+":"+param.toString());
		if(expectedType.isArray()){
			Class<Enum> eType = (Class<Enum>) expectedType.getComponentType();
			String [] params = (String [])param;
			Object enums = Array.newInstance(eType, params.length);
			for(int i = 0; i < params.length; i++){
				Enum ev = Enum.valueOf(eType, ((String) params[i]).toUpperCase());
				if(debug) System.out.println("getEnumforString ["+i+"] = "+ev);
				Array.set(enums, i, ev);
			}
			return enums;
		} else {
			Class<Enum> eType = (Class<Enum>) expectedType;
			Object o = Enum.valueOf(eType, ((String)param).toUpperCase());
			if(debug) System.out.println("getEnumForString returning enum " +o.toString());
			return o;
		}
	}
	
    /**
     * Given a Enum (or Enum []) object, return a corresponding String (or String []) 
     * @param enumValue - enum values
     * @return - string (or string []) value
     */
    private static Object getStringForEnum(Object enumValue){
    	if(enumValue.getClass().isArray()){
    		Enum<?>[] input = (Enum<?>[])enumValue;
    		String [] returnValue = new String[input.length];
    		for(int i = 0; i< input.length; i++){
    			returnValue[i] = input[i].toString();
    		}
    		return returnValue;
    	} else {
    		return enumValue.toString();
    	}
    }
	
    /*
     * Check if a CimParameter matches a given java type
     * @param p - CimParameter to be matched
     * @param javaType - java type to be used for matching
     * @return - true if CimParameter matches the given java type, false otherwise
     *
    private static boolean paramTypeMatches(CimParameter p, Class<? extends Object> javaType){
    	// all types match a hashMap
    	// TODO: Must check generic arguments to match data type in the map here
    	if(Map.class.isAssignableFrom(javaType)) return true;
    	// CIM data type declared in the model
    	DataType cType = p.getDataType();
    	// CIM data type corresponding to java type
		DataType jType = DataType.getTypeForClass(javaType);
		
		if(debug) System.out.println("\t\tChecking Parameter Type "+p.getDataType()+" against type "+javaType.getCanonicalName());
		// return true if CimParameter type matches computed java parameter type
		if(cType.equals(jType)) return true;
    	
    	// Handle enum types
		if(javaType.isEnum()){
			if(debug) System.out.println("\t\tChecking Parameter "+p.toMOF()+" against Enum "+javaType.getCanonicalName());
			// return (cType == DataType.STRING && javaEnumMatches(p,javaType)) ? true : false;
			throw new ModelException("Not yet implemented");
		} else if(javaType.isArray() && javaType.getComponentType().isEnum()){
			if(debug) System.out.println("\t\tChecking Parameter "+p.toMOF()+" against Enum[] "+javaType.getCanonicalName());
			// return (cType == DataType.STRING_ARRAY && javaEnumMatches(p,javaType)) ? true : false;
			throw new ModelException("Not yet implemented");
		}
		
		// Check for reference parameter. Here pType will return INVALID, and getCimDataType will return
		// either OBJECTPATH or OBJECTPATH_ARRAY
		
		if(p.isReference()){
			if(debug) System.out.println("\t\tCheck array types match");
			// Check that both are arrays (or neither one is)
			if(p.isArray() != javaType.isArray()) return false;
			if(debug) System.out.println("\t\tChecking Reference Type "+p.getRefClassName()+" against type "+javaType.getCanonicalName());
			// get CIM reference class name for this parameter
			String refCimName = p.getRefClassName();
			
		   	// if java class is annotated, check that the cimName,cimVersion matches the annotation
	    	String cimClassName = getCimClassName(javaType);
	    	if(cimClassName != null){
	    		if(cimClassName.equalsIgnoreCase(refCimName)){
	    			// DataValue cv = p.getQualifierValue("REFVERSION");
	    			// if(cv != null && !nvPair[1].equalsIgnoreCase((String) cv.getValue())) return false;
	    			return true;
	    		}
	    		return false;
	    	}
	    	
	    	// if CIM definition contains the REFCLASS annotation, check that java class name matches it
	    	DataValue cn = p.getQualifierValue("REFCLASS");
	    	if(cn != null && javaType.getName().equalsIgnoreCase((String)cn.getValue())){
	    		return true;
	    	}			
		}    	
    	return false;
    }
    
    /**
     * Invoke a java operation using java reflection methods
     * @param targetImplementation - target java implementation to be used for the method
     * @param operationName - name of the operation to be performed
     * @param m - method to be invoked
     * @param params - method parameters
     * @return - object returned by the java method
     * @throws ModelException - in case of errors
     *
    private synchronized Object invokeOperation(Object targetImplementation, String operationName, Method m, Object... params) {
    	try {
    		if(m == null){
    			throw new ModelException(ExceptionReason.ACCESS_DENIED,"JavaModelMapper["+operationName+"()]");
    		}
    		if(debug) System.out.println("Invoke: "+operationName);
    		
    		// check the number of parameters declared and the number of parameters in the invocation
    		Class<?>[] methParamTypes = m.getParameterTypes();
    		int declParams = methParamTypes.length;											// number of parameters declared in java method
    		Class<?> lastParamType = declParams == 0 ? null : methParamTypes[declParams-1];	// last declared parameter type in the java method
    		int actualParams = params == null ? 0 : params.length;							// number of actual parameters passed to the invoker
    		
    		if(debug){
    			System.out.println("Declared Parameters: "+declParams+" Received Parameters: "+actualParams);
    			System.out.println("Last Parameter Type: "+(declParams == 0 ? "NULL" : lastParamType.getName()));

    			for(int i=0; i<methParamTypes.length; i++){
    				System.out.println("Expect\t"+methParamTypes[i].getName());
    			}
    			if(params != null){
    				for(int i=0; i<params.length; i++){
    					System.out.println("Actual\t"+params[i].getClass().getName());
    				}
    			}
    		}
    		
    		// check number of parameters
    		if(actualParams < declParams || (declParams == 0 && actualParams > 0)){
    			// fewer than declared parameters received or parameters sent to a method that does not expect them. Throw exception
    			throw new ModelException(ExceptionReason.INVALID_PARAMETER,operationName+
    					" Expected "+declParams+" parameters, found "+actualParams);
    		}
    		
    		// get the invocation parameter list
    		Object [] invocationParams = new Object[declParams];
    		for(int i=0; i< declParams-1; i++){
    			// get all parameters except the last one
    			invocationParams[i] = getInvocationParameter(methParamTypes[i], params[i]);
    		}
    		// get the last parameter. Note that for vararg methods, the last parameter is declared as an array,
    		// and actual parameters may exceed the number of declared parameters
    		if(declParams > 0){
    			// get the last parameter
    			if(declParams < actualParams){
    				// last declared parameter must be an array parameter
    				if(!lastParamType.isArray()){
    					// fewer than declared parameters received for a non-varArg method. Throw exception
    	    			throw new ModelException(ExceptionReason.INVALID_PARAMETER,operationName+
    	    					" Expected "+declParams+" parameters, found "+actualParams);
    				}
    				Object last = Array.newInstance(lastParamType.getComponentType(), actualParams-declParams+1);
    				if(debug) System.out.println("Create: "+last.getClass().getName()+" of length "+(actualParams-declParams+1));
    				for(int i = declParams-1; i < actualParams; i++){
    					Object value = getInvocationParameter(lastParamType.getComponentType(), params[i]);
    					Array.set(last, i+declParams-1, value);
    				}
    				invocationParams[declParams-1] = last;
    			} else if(lastParamType.isArray()){
    				try {
    					invocationParams[declParams-1] = getInvocationParameter(lastParamType, params[declParams-1]);
    				} catch(ModelException ex){
    					invocationParams[declParams-1] = getInvocationParameter(lastParamType, new Object[]{params[declParams-1]});
    				}
    			} else {
    				invocationParams[declParams-1] = getInvocationParameter(lastParamType, params[declParams-1]);
    			}
    		}
    		
    		if(debug){
    			for(int i = 0; i<invocationParams.length;i++){
    				System.out.println("Invoked\t"+invocationParams[i].getClass().getName());
    			}
    		}
    		
    		// invoke the operation
    		Object returnObject = m.invoke(targetImplementation, invocationParams);
    		
    		// transform any returned enums (or enum arrays) back to String values
    		if(m.getReturnType().isEnum() || (m.getReturnType().isArray() && 
    				m.getReturnType().getComponentType().isEnum())){
    			returnObject = getStringForEnum(returnObject);
    		}
    		return returnObject;
    		
    	} catch (Exception e) {
    		if(!(e instanceof ModelException)){
    		    	throw new ModelException(operationName+": Method invocation error ",e);
    		    } else {
    		    	throw (ModelException) e;
    		    }
    	}
    }
    */
    
}
