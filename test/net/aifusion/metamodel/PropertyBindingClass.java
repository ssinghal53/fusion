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
 * Created Nov 8, 2015 by Sharad Singhal
 * Last Modified March 13, 2020 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;

/**
 * Test class to check Property bindings for CimFusion classes.
 * @author Sharad Singhal
 */
@Export(name="Test",schema="Cim",qualifiers="Description(\"Structure to test property bindings\")")
public class PropertyBindingClass {
	
	/**
	 * Class to test binding CIM Enumerations to Java Enums
	 * @author Sharad Singhal
	 */
	@Export
	public enum EmbeddedStringEnum {
		NAME1("xyz"),
		Name2("abc"),
		name3("def");
		
		private String value;
		private EmbeddedStringEnum(String value){
			this.value = value;
		}
		
		public String value(){
			return value;
		}
	}
	
	// note v00 is void, so we do not use it

	/*
	 * Boolean ([Boolean|boolean]) values
	 */
	Boolean v01, va01[];
	boolean v02, va02[];
	@Export
	public Boolean getV01(){
		return v01;
	}
	@Export
	public void setV01(Boolean v){
		v01 = v;
		return;
	}
	@Export
	public Boolean [] getVa01(){
		return va01;
	}
	@Export
	public void setVa01(Boolean [] v){
		va01 = v;
		return;
	}
	@Export
	public boolean getV02(){
		return v02;
	}
	@Export
	public void setV02(boolean v){
		v02 = v;
		return;
	}
	@Export
	public boolean [] getVa02(){
		return va02;
	}
	@Export
	public void setVa02(boolean [] v){
		va02 = v;
		return;
	}

	/*
	 * UInt8 values
	 */
	UInt8 v03,va03[];
	@Export
	public UInt8 getV03(){
		return v03;
	}
	@Export
	public void setV03(UInt8 v){
		v03 = v;
		return;
	}
	@Export
	public UInt8 [] getVa03(){
		return va03;
	}
	@Export
	public void setVa03(UInt8 [] v){
		va03 = v;
		return;
	}

	/*
	 * UInt16 values
	 */
	UInt16 v04,va04[];
	@Export
	public UInt16 getV04(){
		return v04;
	}
	@Export
	public void setV04(UInt16 v){
		v04 = v;
		return;
	}
	@Export
	public UInt16 [] getVa04(){
		return va04;
	}
	@Export
	public void setVa04(UInt16 [] v){
		va04 = v;
		return;
	}

	/*
	 * UInt32 values
	 */

	UInt32 v05,va05[];
	@Export
	public UInt32 getV05(){
		return v05;
	}
	@Export
	public void setV05(UInt32 v){
		v05 = v;
		return;
	}
	@Export
	public UInt32 [] getVa05(){
		return va05;
	}
	@Export
	public void setVa05(UInt32 [] v){
		va05 = v;
		return;
	}
	
	/*
	 * UInt064 values
	 */
	UInt64 v06, va06[];
	@Export
	public UInt64 getV06(){
		return v06;
	}
	@Export
	public void setV06(UInt64 v){
		v06 = v;
		return;
	}
	@Export
	public UInt64 [] getVa06(){
		return va06;
	}
	@Export
	public void setVa06(UInt64 [] v){
		va06 = v;
		return;
	}
	
	/*
	 * SInt08 ([Byte|byte]) values
	 */
	Byte v07, va07[];
	byte v08, va08[];
	@Export
	public Byte getV07(){
		return v07;
	}
	@Export
	public void setV07(Byte v){
		v07 = v;
		return;
	}
	@Export
	public Byte [] getVa07(){
		return va07;
	}
	@Export
	public void setVa07(Byte [] v){
		va07 = v;
		return;
	}
	@Export
	public byte getV08(){
		return v08;
	}
	@Export
	public void setV08(byte v){
		v08 = v;
		return;
	}
	@Export
	public byte [] getVa08(){
		return va08;
	}
	@Export
	public void setVa08(byte [] v){
		va08 = v;
		return;
	}
	
	/*
	 * SInt16 ([Short|short]) values
	 */
	Short v09, va09[];	// SInt16
	short v10, va10[];	// SInt16
	@Export
	public Short getV09(){
		return v09;
	}
	@Export
	public void setV09(Short v){
		v09 = v;
		return;
	}
	@Export
	public Short [] getVa09(){
		return va09;
	}
	@Export
	public void setVa09(Short [] v){
		va09 = v;
		return;
	}
	@Export
	public short getV10(){
		return v10;
	}
	@Export
	public void setV10(short v){
		v10 = v;
		return;
	}
	@Export
	public short [] getVa10(){
		return va10;
	}
	@Export
	public void setVa10(short [] v){
		va10 = v;
		return;
	}
	
	/*
	 * SInt32 ([Integer|int]) values
	 */
	Integer v11, va11[];
	int v12, va12[];
	@Export
	public Integer getV11(){
		return v11;
	}
	@Export
	public void setV11(Integer v){
		v11 = v;
		return;
	}
	@Export
	public Integer [] getVa11(){
		return va11;
	}
	@Export
	public void setVa11(Integer [] v){
		va11 = v;
		return;
	}
	@Export
	public int getV12(){
		return v12;
	}
	@Export
	public void setV12(int v){
		v12 = v;
		return;
	}
	@Export
	public int [] getVa12(){
		return va12;
	}
	@Export
	public void setVa12(int [] v){
		va12 = v;
		return;
	}
	
	/*
	 * SInt64 ([Long|long]) values
	 */
	Long v13, va13[];	// SINT64
	long v14, va14[];	// SINT64
	@Export
	public Long getV13(){
		return v13;
	}
	@Export
	public void setV13(Long v){
		v13 = v;
		return;
	}
	@Export
	public Long [] getVa13(){
		return va13;
	}
	@Export
	public void setVa13(Long [] v){
		va13 = v;
		return;
	}
	@Export
	public long getV14(){
		return v14;
	}
	@Export
	public void setV14(long v){
		v14 = v;
		return;
	}
	@Export
	public long [] getVa14(){
		return va14;
	}
	@Export
	public void setVa14(long [] v){
		va14 = v;
		return;
	}
	
	/*
	 * Real32 ([Float|float])values
	 */
	Float v15, va15[];
	float v16, va16[];
	@Export
	public Float getV15(){
		return v15;
	}
	@Export
	public void setV15(Float v){
		v15 = v;
		return;
	}
	@Export
	public Float [] getVa15(){
		return va15;
	}
	@Export
	public void setVa15(Float [] v){
		va15 = v;
		return;
	}
	@Export
	public float getV16(){
		return v16;
	}
	@Export
	public void setV16(float v){
		v16 = v;
		return;
	}
	@Export
	public float [] getVa16(){
		return va16;
	}
	@Export
	public void setVa16(float [] v){
		va16 = v;
		return;
	}
	
	/*
	 * Real64 ([Double|double]) Values
	 */	
	Double v17, va17[];
	double v18, va18[];
	@Export
	public Double getV17(){
		return v17;
	}
	@Export
	public void setV17(Double v){
		v17 = v;
		return;
	}
	@Export
	public Double [] getVa17(){
		return va17;
	}
	@Export
	public void setVa17(Double [] v){
		va17 = v;
		return;
	}
	@Export
	public double getV18(){
		return v18;
	}
	@Export
	public void setV18(double v){
		v18 = v;
		return;
	}
	@Export
	public double [] getVa18(){
		return va18;
	}
	@Export
	public void setVa18(double [] v){
		va18 = v;
		return;
	}
	
	/*
	 * Char16 ([Character|char])values
	 */
	Character v19, va19[];
	char v20, va20[];
	@Export
	public Character getV19(){
		return v19;
	}
	@Export
	public void setV19(Character v){
		v19 = v;
		return;
	}
	@Export
	public Character [] getVa19(){
		return va19;
	}
	@Export
	public void setVa19(Character [] v){
		va19 = v;
		return;
	}
	@Export
	public char getV20(){
		return v20;
	}
	@Export
	public void setV20(char v){
		v20 = v;
		return;
	}
	@Export
	public char [] getVa20(){
		return va20;
	}
	@Export
	public void setVa20(char [] v){
		va20 = v;
		return;
	}
	
	/*
	 * String values
	 */
	String v21, va21[];
	@Export
	public String getV21(){
		return v21;
	}
	@Export
	public void setV21(String v){
		v21 = v;
		return;
	}
	@Export
	public String [] getVa21(){
		return va21;
	}
	@Export
	public void setVa21(String [] v){
		va21 = v;
		return;
	}
	
	/*
	 * DateTime values
	 */
	DateTime v22, va22[];
	@Export
	public DateTime getV22(){
		return v22;
	}
	@Export
	public void setV22(DateTime v){
		v22 = v;
		return;
	}
	@Export
	public DateTime [] getVa22(){
		return va22;
	}
	@Export
	public void setVa22(DateTime [] v){
		va22 = v;
		return;
	}
	
	/*
	 * Generic ObjectPath (xyz ref) values. RefClass MUST be used to indicate
	 * returned class value
	 */
	ObjectPath v23, va23[];
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public ObjectPath getV23(){
		return v23;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public void setV23(ObjectPath v){
		v23 = v;
		return;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public ObjectPath [] getVa23(){
		return va23;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public void setVa23(ObjectPath [] v){
		va23 = v;
		return;
	}
	
	/*
	 * Generic EnumerationValue. RefClass MUST be used to indicate the correct enumeration
	 */
	EnumerationValue v24, va24[];
	@Export(refClass="net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum")
	public EnumerationValue getV24(){
		return v24;
	}
	@Export(refClass="net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum")
	public void setV24(EnumerationValue v){
		v24 = v;
		return;
	}
	@Export(refClass="net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum")
	public EnumerationValue [] getVa24(){
		return va24;
	}
	@Export(refClass="net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum")
	public void setVa24(EnumerationValue [] v){
		va24 = v;
		return;
	}
	
	/*
	 * Generic Structure Value (Structure xyz) values. RefClass MUST be used to indicate correct Class
	 * Note that the StructureValue CANNOT cause a self-reference loop (i.e., must not be Assignable to PropertyBindingClass)
	 */
	StructureValue v25, va25[];
	@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")
	public StructureValue getV25(){
		return v25;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")
	public void setV25(StructureValue v){
		v25 = v;
		return;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")
	public StructureValue [] getVa25(){
		return va25;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")
	public void setVa25(StructureValue [] v){
		va25 = v;
		return;
	}
	
	/*
	 * Generic InstanceValue (xyz) values. RefClass MUST be used to indicate correct class
	 * Note that the CimInstance CANNOT cause a self-reference loop (i.e., must not be Assignable to PropertyBindingClass)
	 */
	CimInstance v26, va26[];
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public CimInstance getV26(){
		return v26;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public void setV26(CimInstance v){
		v26 = v;
		return;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public CimInstance [] getVa26(){
		return va26;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public void setVa26(CimInstance [] v){
		va26 = v;
		return;
	}
	
	/*
	 * OctetString
	 */
	OctetString v27, va27[];
	@Export
	public OctetString getV27(){
		return v27;
	}
	@Export
	public void setV27(OctetString v){
		v27 = v;
		return;
	}
	@Export
	public OctetString [] getVa27(){
		return va27;
	}
	@Export
	public void setVa27(OctetString [] v){
		va27 = v;
		return;
	}
	
	/*
	 * ********************************************
	 * Methods returning java classes (enums, classes)
	 * The returned class must be an exported class. The exported class can be used
	 * to generate the corresponding MOF parameters
	 * ********************************************
	 */
	
	/*
	 * Exported Java Enums map to EnumerationValue.
	 * Note that the @Export on the java class allows us to resolve the enum, and we do not need classRef annotation
	 */
	EnumBindingClass v28, va28[];
	@Export
	public EnumBindingClass getV28(){
		return v28;
	}
	@Export
	public void setV28(EnumBindingClass v){
		v28 = v;
		return;
	}
	@Export
	public EnumBindingClass [] getVa28(){
		return va28;
	}
	@Export
	public void setVa28(EnumBindingClass [] v){
		va28 = v;
		return;
	}
	
	/*
	 * Exported Java classes with isStructure annotation map to StructureValue
	 * Note that these classes have getters/setters but no other exported methods
	 * * Note that the StructureValue CANNOT cause a self-reference loop (i.e., must not be Assignable to PropertyBindingClass)
	 */
	MethodBindingSuperClass v29, va29[];
	@Export
	public MethodBindingSuperClass getV29(){
		return v29;
	}
	
	@Export
	public void setV29(MethodBindingSuperClass v){
		v29 = v;
		return;
	}
	
	@Export
	public MethodBindingSuperClass [] getVa29(){
		return va29;
	}
	
	@Export
	public void setVa29(MethodBindingSuperClass [] v){
		va29 = v;
		return;
	}
	
	/*
	 * Exported Java classes without isStructure annotation map to CimInstance Values
	 * Note that these classes can have getters/setters as well as exported methods
	 */
	MethodBindingClass v30, va30[];
	@Export
	public MethodBindingClass getV30(){
		return v30;
	}

	@Export
	public void setV30(MethodBindingClass v){
		v30 = v;
		return;
	}

	@Export
	public MethodBindingClass [] getVa30(){
		return va30;
	}

	@Export
	public void setVa30(MethodBindingClass [] v){
		va30 = v;
		return;
	}
	
	/*
	 * Static properties
	 */
	static String v31 = "default";
	static String va31[] = {"default"};
	
	@Export(defaultValue="\"default\"")
	public static String getV31(){
		return v31;
	}
	@Export
	public static void setV31(String v){
		v31 = v;
		return;
	}
	
	@Export
	public static String[] getVa31(){
		return va31;
	}
	@Export
	public static void setVa31(String[] v){
		va31 = v;
		return;
	}

	/*
	 * Helper methods for testing
	 */
	/**
	 * Class to represent all property bindings defined in this class
	 * @author Sharad Singhal
	 *
	 */
	public class PropertyBinding {
		/** Name of the property */
		String propertyName;
		/** Getter method for the property */
		Method getter;
		/** Setter method for the property */
		Method setter;
		/** CIM data type for this property */
		DataType dataType;
		/** Java type for this property */
		Class<?> javaType;
		@Override
		public String toString() {
			StringBuilder b = new StringBuilder(propertyName);
			b.append("= {");
			b.append(dataType);
			b.append(" [");
			b.append(javaType);
			b.append("]\n\t");
			b.append(getter);
			b.append("\n\t");
			b.append(setter);
			b.append("\n}");
			return b.toString();
		}
	}
	/**
	 * Get the bindings defined in this class
	 * @return - HashSet containing all bindings
	 */
	public HashMap<Class<?>,PropertyBinding> getMethods(){
		LinkedHashMap<Class<?>,PropertyBinding> props = new LinkedHashMap<Class<?>,PropertyBinding>();
		
		// collect all public methods declared in this class that are exported
		TreeMap<String,Method> declaredMethods = new TreeMap<String,Method>();
		for(Method m : PropertyBindingClass.class.getDeclaredMethods()){
			if(!Modifier.isPublic(m.getModifiers())) continue;	// not a public method
			if(m.getAnnotation(Export.class) == null) continue;	// not exported
			
			if(Modifier.isStatic(m.getModifiers())) continue;	// we are currently not exporting static methods
			if(declaredMethods.containsKey(m.getName())){
				throw new ModelException(ExceptionReason.ALREADY_EXISTS,"Exported Methods must be unique: Found "+m.getName());
			}
			declaredMethods.put(m.getName(), m);	
		}
		// iterate through declared methods to get property definitions
		HashSet<String> getSetPairs = new HashSet<String>();
		for(String key : declaredMethods.keySet()){
			// check if this method is a property declaration
			if(key.startsWith("get") || key.startsWith("set")){
				// Check if we have already seen this underlying property, else add to seen property and process
				String pName = key.substring(3);
				if(getSetPairs.contains(pName)) continue;
				getSetPairs.add(pName);
				
				// create the property binding
				PropertyBinding p = new PropertyBinding();
				p.propertyName = pName;
				p.getter = declaredMethods.get("get"+pName);
				p.setter = declaredMethods.get("set"+pName);
				p.javaType = p.getter.getReturnType();
				try {
					p.dataType = DataType.getTypeForClass(p.javaType);
					props.put(p.javaType,p);
				} catch(Exception ex){
					System.out.println("Could not locate CIM Type for "+p.javaType);
				}
			}
		}
		return props;
	}
	
	public HashMap<Class<?>,PropertyBinding> getStaticMethods(){
		LinkedHashMap<Class<?>,PropertyBinding> props = new LinkedHashMap<Class<?>,PropertyBinding>();
		
		// collect all public methods declared in this class that are exported
		TreeMap<String,Method> declaredMethods = new TreeMap<String,Method>();
		for(Method m : PropertyBindingClass.class.getDeclaredMethods()){
			if(!Modifier.isPublic(m.getModifiers())) continue;	// not a public method
			if(m.getAnnotation(Export.class) == null) continue;	// not exported
			
			if(!Modifier.isStatic(m.getModifiers())) continue;	// we are only exporting static methods
			if(declaredMethods.containsKey(m.getName())){
				throw new ModelException(ExceptionReason.ALREADY_EXISTS,"Exported Methods must be unique: Found "+m.getName());
			}
			declaredMethods.put(m.getName(), m);	
		}
		// iterate through declared methods to get property definitions
		HashSet<String> getSetPairs = new HashSet<String>();
		for(String key : declaredMethods.keySet()){
			// check if this method is a property declaration
			if(key.startsWith("get") || key.startsWith("set")){
				// Check if we have already seen this underlying property, else add to seen property and process
				String pName = key.substring(3);
				if(getSetPairs.contains(pName)) continue;
				getSetPairs.add(pName);
				
				// create the property binding
				PropertyBinding p = new PropertyBinding();
				p.propertyName = pName;
				p.getter = declaredMethods.get("get"+pName);
				p.setter = declaredMethods.get("set"+pName);
				p.javaType = p.getter.getReturnType();
				p.dataType = DataType.getTypeForClass(p.javaType);
				props.put(p.javaType,p);
			}
		}
		return props;
	}
	
	public String toMOF(){
		StringBuilder b = new StringBuilder();
		Package pkg = PropertyBindingClass.class.getPackage();
		String packagePath = PropertyBindingClass.class.getPackage().getName().replace(".", "::");
		b.append("[PackagePath(\"").append(packagePath).append("\")");
		b.append(",MappingStrings{\"").append(Constants.fusionMap+"PropertyBindingClass").append("\"}]\n");
		b.append("Structure Cim_Test {\n");
		Map<Class<?>,PropertyBinding> props = getMethods();
		// TODO: add code here to correctly create references to Enumerations, Structures, embedded Instances, and References
		for(PropertyBinding p : props.values()){
			switch(p.dataType){
			case ENUMERATIONVALUE:
			case ENUMERATIONVALUE_ARRAY:
				
			case STRUCTUREVALUE:
			case STRUCTUREVALUE_ARRAY:
				
			case INSTANCEVALUE:
			case INSTANCEVALUE_ARRAY:
				
			case OBJECTPATH:
			case OBJECTPATH_ARRAY:
				b.append("\t[Write,MappingStrings{\"").append(Constants.fusionMap+"net.aifusion.metamodel.MethodBindingClass").append("\"}]\n\t");
				b.append("Cim_TestMethods ref");
				break;
			default:
				b.append("\t[write]\n\t").append(p.dataType);
				break;
			}
			
			if(p.dataType.isArray())b.append(" []");
			b.append(" ").append(p.propertyName).append(";\n");
		}
		for(PropertyBinding p : getStaticMethods().values()){
			b.append("\t[Static,Write]\n");
			b.append("\t").append(p.dataType);
			if(p.dataType.isArray())b.append(" []");
			b.append(" ").append(p.propertyName).append(";\n");
		}
		b.append("};\n");
		return b.toString();
		
	}
}
