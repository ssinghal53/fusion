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
 * Created Jul 17, 2016 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Class to test CimMethod
 * @author Sharad Singhal
 */
@Export(name="MethodTest",schema="Cim")
public class CimMethodTestClass {
	
	// void values
	@Export
	public void mV00(){
		return;
	}

	/*
	 * Boolean ([Boolean|boolean]) values
	 */
	@Export
	public Boolean mV01(Boolean v){
		return v;
	}
	@Export
	public Boolean [] mVa01(Boolean [] v){
		return v;
	}
	@Export
	public boolean mV02(boolean v){
		return v;
	}
	@Export
	public boolean [] mVa02(boolean [] v){
		return v;
	}

	/*
	 * UInt8 values
	 */
	@Export
	public UInt8 mV03(UInt8 v){
		return v;
	}
	@Export
	public UInt8[] mVa03(UInt8 [] v){
		return v;
	}

	/*
	 * UInt16 values
	 */
	@Export
	public UInt16 mV04(UInt16 v){
		return v;
	}
	@Export
	public UInt16[] mVa04(UInt16 [] v){
		return v;
	}

	/*
	 * UInt32 values
	 */
	
	@Export
	public UInt32 mV05(UInt32 v){
		return v;
	}
	@Export
	public UInt32 [] mVa05(UInt32 [] v){
		return v;
	}
	
	/*
	 * UInt064 values
	 */	
	@Export
	public UInt64 mV06(UInt64 v){
		return v;
	}
	
	@Export
	public UInt64[] mVa06(UInt64 [] v){
		return v;
	}
	
	/*
	 * SInt08 ([Byte|byte]) values
	 */
	@Export
	public Byte mV07(Byte v){
		return v;
	}
	@Export
	public Byte [] mVa07(Byte [] v){
		return v;
	}
	
	@Export
	public byte mV08(byte v){
		return v;
	}
	@Export
	public byte [] mVa08(byte [] v){
		return v;
	}
	
	/*
	 * SInt16 ([Short|short]) values
	 */
	@Export
	public Short mV09(Short v){
		return v;
	}
	@Export
	public Short [] mVa09(Short [] v){
		return v;
	}
	@Export
	public short mV10(short v){
		return v;
	}
	@Export
	public short [] mVa10(short [] v){
		return v;
	}
	
	/*
	 * SInt32 ([Integer|int]) values
	 */
	@Export
	public Integer mV11(Integer v){
		return v;
	}
	@Export
	public Integer [] mVa11(Integer [] v){
		return v;
	}
	@Export
	public int mV12(int v){
		return v;
	}
	@Export
	public int [] mVa12(int [] v){
		return v;
	}
	
	/*
	 * SInt64 ([Long|long]) values
	 */
	@Export
	public Long mV13(Long v){
		return v;
	}
	@Export
	public Long [] mVa13(Long [] v){
		return v;
	}
	@Export
	public long mV14(long v){
		return v;
	}
	@Export
	public long [] mVa14(long [] v){
		return v;
	}
	
	/*
	 * Real32 ([Float|float])values
	 */
	@Export
	public Float mV15(Float v){
		return v;
	}
	@Export
	public Float [] mVa15(Float [] v){
		return v;
	}
	@Export
	public float mV16(float v){
		return v;
	}
	@Export
	public float [] mVa16(float [] v){
		return v;
	}
	
	/*
	 * Real64 ([Double|double]) Values
	 */	
	@Export
	public Double mV17(Double v){
		return v;
	}
	@Export
	public Double [] mVa17(Double [] v){
		return v;
	}
	@Export
	public double mV18(double v){
		return v;
	}
	@Export
	public double [] mVa18(double [] v){
		return v;
	}
	
	/*
	 * Char16 ([Character|char])values
	 */
	@Export
	public Character mV19(Character v){
		return v;
	}
	@Export
	public Character [] mVa19(Character [] v){
		return v;
	}
	@Export
	public char mV20(char v){
		return v;
	}
	@Export
	public char [] mVa20(char [] v){
		return v;
	}
	
	/*
	 * String values
	 */
	@Export
	public String getV21(String v){
		return v;
	}
	@Export
	public String [] mVa21(String [] v){
		return v;
	}
	
	/*
	 * DateTime values
	 */
	@Export
	public DateTime mV22(DateTime v){
		return v;
	}
	@Export
	public DateTime [] mVa22(DateTime [] v){
		return v;
	}

	/*
	 * Generic ObjectPath (xyz ref) values. RefClass MUST be used to indicate
	 * returned class value
	 */
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public ObjectPath mV23(@Export(refClass="net.aifusion.metamodel.MethodBindingClass") ObjectPath v){
		return v;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public ObjectPath [] mVa23(@Export(refClass="net.aifusion.metamodel.MethodBindingClass") ObjectPath [] v){
		return v;
	}
	
	/*
	 * Generic EnumerationValue. RefClass MUST be used to indicate the correct enumeration
	 */
	@Export(refClass="net.aifusion.metamodel.EnumBindingClass")
	public EnumerationValue mV24(@Export(refClass="net.aifusion.metamodel.EnumBindingClass")EnumerationValue v){
		return v;
	}
	@Export(refClass="net.aifusion.metamodel.EnumBindingClass")
	public EnumerationValue [] mVa24(@Export(refClass="net.aifusion.metamodel.EnumBindingClass")EnumerationValue [] v){
		return v;
	}
	
	/*
	 * Generic Structure Value (Structure xyz) values. RefClass MUST be used to indicate correct Class
	 * Note that the StructureValue CANNOT cause a self-reference loop (i.e., must not be Assignable to PropertyBindingClass)
	 */
	@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")
	public StructureValue mV25(@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")StructureValue v){
		return v;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")
	public StructureValue [] mVa25(@Export(refClass="net.aifusion.metamodel.MethodBindingSuperClass")StructureValue [] v){
		return v;
	}
	
	/*
	 * Generic InstanceValue (xyz) values. RefClass MUST be used to indicate correct class
	 * Note that the CimInstance CANNOT cause a self-reference loop (i.e., must not be Assignable to PropertyBindingClass)
	 */
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public CimInstance mV26(@Export(refClass="net.aifusion.metamodel.MethodBindingClass")CimInstance v){
		return v;
	}
	@Export(refClass="net.aifusion.metamodel.MethodBindingClass")
	public CimInstance [] mVa26(@Export(refClass="net.aifusion.metamodel.MethodBindingClass")CimInstance [] v){
		return v;
	}
	
	/*
	 * OctetString
	 */
	@Export
	public OctetString mV27(OctetString v){
		return v;
	}
	@Export
	public OctetString [] mVa27(OctetString [] v){
		return v;
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
	@Export
	public EnumBindingClass mV28(EnumBindingClass v){
		return v;
	}
	@Export
	public EnumBindingClass [] mVa28(EnumBindingClass [] v){
		return v;
	}
	
	/*
	 * Exported Java classes with isStructure annotation map to StructureValue
	 * Note that these classes have getters/setters but no other exported methods
	 * Note that the StructureValue CANNOT cause a self-reference loop (i.e., must not be Assignable to PropertyBindingClass)
	 */
	@Export
	public MethodBindingSuperClass mV29(MethodBindingSuperClass v){
		return v;
	}
	@Export
	public MethodBindingSuperClass [] mVa29(MethodBindingSuperClass [] v){
		return v;
	}
	
	/*
	 * Exported Java classes without isStructure annotation map to CimInstance Values
	 * Note that these classes can have getters/setters as well as exported methods
	 */
	@Export
	public MethodBindingClass mV30(MethodBindingClass v){
		return v;
	}
	@Export
	public MethodBindingClass [] mVa30(MethodBindingClass [] v){
		return v;
	}
	
	/*
	 * Static methods
	 */
	@Export(defaultValue="\"default\"")
	public static String mV31(String v){
		return v;
	}
	@Export
	public static String[] getVa31(String[] v){
		return v;
	}
}
