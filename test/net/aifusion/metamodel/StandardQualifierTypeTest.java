/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Nov 22, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test cases for standard qualifier types
 * @author Sharad Singhal
 */
public class StandardQualifierTypeTest {
	/** Known standard qualifier types */
	private static StandardQualifierType qualifiers[] = {
		StandardQualifierType.ABSTRACT,
		StandardQualifierType.AGGREGATIONKIND,
		StandardQualifierType.ARRAYTYPE,
		StandardQualifierType.BITMAP,
		StandardQualifierType.BITVALUES,
		StandardQualifierType.COUNTER,
		StandardQualifierType.DEPRECATED,
		StandardQualifierType.DESCRIPTION,
		StandardQualifierType.EMBEDDEDOBJECT,
		StandardQualifierType.EXPERIMENTAL,
		StandardQualifierType.GAUGE,
		StandardQualifierType.IN,
		StandardQualifierType.ISPUNIT,
		StandardQualifierType.KEY,
		StandardQualifierType.MAPPINGSTRINGS,
		StandardQualifierType.MAX,
		StandardQualifierType.MIN,
		StandardQualifierType.MODELCORRESPONDENCE,
		StandardQualifierType.OCL,
		StandardQualifierType.OUT,
		StandardQualifierType.OVERRIDE,
		StandardQualifierType.PACKAGEPATH,
		StandardQualifierType.PUNIT,
		StandardQualifierType.READ,
		StandardQualifierType.REQUIRED,
		StandardQualifierType.STATIC,
		StandardQualifierType.TERMINAL,
		StandardQualifierType.VERSION,
		StandardQualifierType.WRITE,
		StandardQualifierType.XMLNAMESPACENAME,
		StandardQualifierType.IMPLEMENTS
	};
	/** Mof Representation */
	private static String [] mof = new String []{
		"Qualifier Abstract : Boolean = false Scope(Class, Association, Enumeration, Structure, Interface) Policy(Restricted);\n",
		"Qualifier AggregationKind : String = \"None\" Scope(Reference) Policy(DisableOverride);\n",
		"Qualifier ArrayType : String = \"Bag\" Scope(Property, Parameter, Method, Reference) Policy(DisableOverride);\n",
		"Qualifier BitMap : String[] Scope(Method, Property, Parameter);\n",
		"Qualifier BitValues : String[] Scope(Method, Property, Parameter);\n",
		"Qualifier Counter : Boolean = false Scope(Method, Property, Parameter) Policy(DisableOverride);\n",
		"Qualifier Deprecated : String[] Scope(Any) Policy(Restricted);\n",
		"Qualifier Description : String Scope(Any);\n",
		"Qualifier EmbeddedObject : Boolean = false Scope(Method, Property, Parameter) Policy(DisableOverride);\n",
		"Qualifier Experimental : Boolean = false Scope(Any) Policy(Restricted);\n",
		"Qualifier Gauge : Boolean = false Scope(Method, Property, Parameter) Policy(DisableOverride);\n",
		"Qualifier In : Boolean = true Scope(Parameter) Policy(DisableOverride);\n",
		"Qualifier IsPUnit : Boolean = false Scope(Method, Property, Parameter);\n",
		"Qualifier Key : Boolean = false Scope(Property, Reference) Policy(DisableOverride);\n",
		"Qualifier MappingStrings : String[] Scope(Any);\n",
		"Qualifier Max : UInt32 Scope(Reference);\n",
		"Qualifier Min : UInt32 = 0 Scope(Reference);\n",
		"Qualifier ModelCorrespondence : String[] Scope(Any);\n",
		"Qualifier OCL : String[] Scope(Class, Association, Structure, Interface, Property, Method, Parameter);\n",
		"Qualifier Out : Boolean = false Scope(Parameter) Policy(DisableOverride);\n",
		"Qualifier Override : Boolean = false Scope(Method, Property, Reference, Parameter) Policy(Restricted);\n",
		"Qualifier PackagePath : String Scope(Class, Enumeration, Structure, Interface);\n",
		"Qualifier PUnit : String Scope(Method, Property, Parameter);\n",
		"Qualifier Read : Boolean = true Scope(Property, Reference);\n",
		"Qualifier Required : Boolean = false Scope(Method, Property, Reference, Parameter) Policy(DisableOverride);\n",
		"Qualifier Static : Boolean = false Scope(Method, Property, Reference) Policy(DisableOverride);\n",
		"Qualifier Terminal : Boolean = false Scope(Class, Enumeration, Structure);\n",
		"Qualifier Version : String Scope(Interface, Class, Association, Enumeration, Structure) Policy(Restricted);\n",
		"Qualifier Write : Boolean = false Scope(Property, Reference);\n",
		"Qualifier XMLNamespaceName : String = null Scope(Parameter, Property, Reference, Method);\n",
		"Qualifier Implements : String[] Scope(Class, Structure) Policy(Restricted);\n"
	};
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("StandardQualifierType ");
		assertEquals(qualifiers.length,StandardQualifierType.values().length);
		assertEquals(qualifiers.length,mof.length);		
	}
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}
	
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#hasDefaultValue()}.
	 */
	@Test
	public final void testHasDefaultValue() {
		boolean [] hasDefault = {
				true,	//ABSTRACT
				true,	//AGGREGATIONKIND
				true,	//ARRAYTYPE
				false,	//BITMAP
				false,	//BITVALUES
				true,	//COUNTER
				false,	//DEPRECATED
				false,	//DESCRIPTION
				true,	//EMBEDDEDOBJECT
				true,	//EXPERIMENTAL
				true,	//GAUGE
				true,	//IN
				true,	//ISPUNIT
				true,	//KEY
				false,	//MAPPINGSTRINGS
				false,	//MAX
				true,	//MIN
				false,	//MODELCORRESPONDENCE
				false,	//OCL
				true,	//OUT
				true,	//OVERRIDE
				false,	//PACKAGEPATH
				false,	//PUNIT
				true,	//READ
				true,	//REQUIRED
				true,	//STATIC
				true,	//TERMINAL
				false,	//VERSION
				true,	//WRITE
				true,	//XMLNAMESPACE
				false,	//IMPLEMENTS
				
		};
		for(int i = 0; i < qualifiers.length; i++) {
			StandardQualifierType q = qualifiers[i];
			// array types do not have have default values
			if(q.hasArrayValue()){
				assertEquals(false,q.hasDefaultValue());
				continue;
			}
			assertEquals(hasDefault[i],q.hasDefaultValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#hasArrayValue()}.
	 */
	@Test
	public final void testHasArrayValue() {
		boolean [] isArray = {
			false,	// ABSTRACT
			false,	// AGGREGATIONKIND
			false,	// ARRAYTYPE
			true,	// BITMAP
			true,	// BITVALUES
			false,	// COUNTER
			true,	// DEPRECATED
			false,	// DESCRIPTION
			false,	// EMBEDDEDOBJECT
			false,	// EXPERIMENTAL
			false,	// GAUGE
			false,	// IN
			false,	// ISPUNIT
			false,	// KEY
			true,	// MAPPINGSTRINGS
			false,	// MAX
			false,	// MIN
			true,	// MODELCORRESPONDENCE
			true,	// OCL
			false,	// OUT
			false,	// OVERRIDE
			false,	// PACKAGEPATH
			false,	// PUNIT
			false,	// READ
			false,	// REQUIRED
			false,	// STATIC
			false,	// TERMINAL
			false,	// VERSION
			false,	// WRITE
			false,	// XMLNAMESPACE
			true	// IMPLEMENTS
		};
		assertEquals(qualifiers.length,isArray.length);
		for(int i = 0; i < qualifiers.length; i++){
			assertEquals(isArray[i],qualifiers[i].hasArrayValue());
		}		
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		for(int i = 0; i < qualifiers.length; i++){			
			assertEquals(mof[i],qualifiers[i].toMOF());
		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getScopes()}.
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#hasScope(Scope)}.
	 */
	@Test
	public final void testGetScopes() {
		Scope [][] scopes = {
				{Scope.CLASS, Scope.ASSOCIATION,Scope.ENUMERATION, Scope.STRUCTURE, Scope.INTERFACE },	//ABSTRACT 
				{Scope.REFERENCE, },	//AGGREGATIONKIND 
				{Scope.PROPERTY, Scope.PARAMETER, Scope.METHOD, Scope.REFERENCE },	//ARRAYTYPE 
				{Scope.METHOD, Scope.PROPERTY, Scope.PARAMETER },	//BITMAP 
				{Scope.METHOD, Scope.PROPERTY, Scope.PARAMETER },	//BITVALUES 
				{Scope.METHOD, Scope.PROPERTY, Scope.PARAMETER },	//COUNTER 
				{Scope.ANY},	//DEPRECATED 
				{Scope.ANY},	//DESCRIPTION 
				{Scope.METHOD, Scope.PROPERTY, Scope.PARAMETER},	//EMBEDDEDOBJECT 
				{Scope.ANY},	//EXPERIMENTAL 
				{Scope.METHOD, Scope.PROPERTY, Scope.PARAMETER },	//GAUGE 
				{Scope.PARAMETER },	//IN 
				{Scope.METHOD, Scope.PROPERTY, Scope.PARAMETER},	//ISPUNIT 
				{Scope.PROPERTY, Scope.REFERENCE },	//KEY 
				{Scope.ANY },	//MAPPINGSTRINGS 
				{Scope.REFERENCE },	//MAX 
				{Scope.REFERENCE },	//MIN 
				{Scope.ANY },	//MODELCORRESPONDENCE 
				{Scope.CLASS, Scope.ASSOCIATION,Scope.STRUCTURE,Scope.INTERFACE,Scope.PROPERTY, Scope.METHOD, Scope.PARAMETER },	//OCL 
				{Scope.PARAMETER },	//OUT 
				{Scope.METHOD, Scope.PROPERTY, Scope.REFERENCE, Scope.PARAMETER },	//OVERRIDE 
				{Scope.CLASS, Scope.ENUMERATION, Scope.STRUCTURE , Scope.INTERFACE},	//PACKAGEPATH 
				{Scope.METHOD, Scope.PROPERTY, Scope.PARAMETER },	//PUNIT 
				{Scope.PROPERTY, Scope.REFERENCE, },	//READ 
				{Scope.METHOD, Scope.PROPERTY, Scope.REFERENCE, Scope.PARAMETER },	//REQUIRED 
				{Scope.METHOD, Scope.PROPERTY, Scope.REFERENCE },	//STATIC 
				{Scope.CLASS,Scope.ENUMERATION, Scope.STRUCTURE },	//TERMINAL 
				{Scope.INTERFACE, Scope.CLASS, Scope.ASSOCIATION, Scope.ENUMERATION, Scope.STRUCTURE },	//VERSION 
				{Scope.PROPERTY, Scope.REFERENCE },	//WRITE 
				{Scope.PARAMETER, Scope.PROPERTY, Scope.REFERENCE, Scope.METHOD },	//XMLNAMESPACE 
				{Scope.CLASS, Scope.STRUCTURE }	//IMPLEMENTS 
		};
		
		for(int i = 0; i < qualifiers.length; i++){
			// System.out.println(qualifiers[i].toMOF());
			List<Scope> l = qualifiers[i].getScopes();
			for(int j = 0; j < scopes[i].length; j++){
				assertEquals(scopes[i][j],l.get(j));
				assertTrue(qualifiers[i].hasScope(scopes[i][j]));
			}
		}		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getPolicy()}.
	 */
	@Test
	public final void testGetPolicy() {
		Policy [] policy = {
			Policy.RESTRICTED,	//ABSTRACT
			Policy.DISABLEOVERRIDE,	//AGGREGATIONKIND
			Policy.DISABLEOVERRIDE,	//ARRAYTYPE
			null,	//BITMAP
			null,	//BITVALUES
			Policy.DISABLEOVERRIDE,	//COUNTER
			Policy.RESTRICTED,	//DEPRECATED
			null,	//DESCRIPTION
			Policy.DISABLEOVERRIDE,	//EMBEDDEDOBJECT
			Policy.RESTRICTED,	//EXPERIMENTAL
			Policy.DISABLEOVERRIDE,	//GAUGE
			Policy.DISABLEOVERRIDE,	//IN
			null,	//ISPUNIT
			Policy.DISABLEOVERRIDE,	//KEY
			null,	//MAPPINGSTRINGS
			null,	//MAX
			null,	//MIN
			null,	//MODELCORRESPONDENCE
			null,	//OCL
			Policy.DISABLEOVERRIDE,	//OUT
			Policy.RESTRICTED,	//OVERRIDE
			null,	//PACKAGEPATH
			null,	//PUNIT
			null,	//READ
			Policy.DISABLEOVERRIDE,	//REQUIRED
			Policy.DISABLEOVERRIDE,	//STATIC
			null,	//TERMINAL
			Policy.RESTRICTED,	//VERSION
			null,	//WRITE
			null,	//XMLNAMESPACE
			Policy.RESTRICTED	//IMPLEMENTS
		};
		
		assertEquals(qualifiers.length,policy.length);
		for(int i = 0; i < qualifiers.length; i++){
			// System.out.println(qualifiers[i].toMOF());
			assertEquals(policy[i],qualifiers[i].getPolicy());
		}		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getDataType()}.
	 */
	@Test
	public final void testGetDataType() {
		DataType [] dataType = {
			DataType.BOOLEAN,	//ABSTRACT
			DataType.STRING,	//AGGREGATIONKIND
			DataType.STRING,	//ARRAYTYPE
			DataType.STRING_ARRAY,	//BITMAP
			DataType.STRING_ARRAY,	//BITVALUES
			DataType.BOOLEAN,	//COUNTER
			DataType.STRING_ARRAY,	//DEPRECATED
			DataType.STRING,	//DESCRIPTION
			DataType.BOOLEAN,	//EMBEDDEDOBJECT
			DataType.BOOLEAN,	//EXPERIMENTAL
			DataType.BOOLEAN,	//GAUGE
			DataType.BOOLEAN,	//IN
			DataType.BOOLEAN,	//ISPUNIT
			DataType.BOOLEAN,	//KEY
			DataType.STRING_ARRAY,	//MAPPINGSTRINGS
			DataType.UINT32,	//MAX
			DataType.UINT32,	//MIN
			DataType.STRING_ARRAY,	//MODELCORRESPONDENCE
			DataType.STRING_ARRAY,	//OCL
			DataType.BOOLEAN,	//OUT
			DataType.BOOLEAN,	//OVERRIDE
			DataType.STRING,	//PACKAGEPATH
			DataType.STRING,	//PUNIT
			DataType.BOOLEAN,	//READ
			DataType.BOOLEAN,	//REQUIRED
			DataType.BOOLEAN,	//STATIC
			DataType.BOOLEAN,	//TERMINAL
			DataType.STRING,	//VERSION
			DataType.BOOLEAN,	//WRITE
			DataType.STRING,	//XMLNAMESPACE
			DataType.STRING_ARRAY	//IMPLEMENTS
		};
		assertEquals(qualifiers.length,dataType.length);
		for(int i = 0; i < qualifiers.length; i++){
			assertEquals(dataType[i],qualifiers[i].getDataType());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getDefaultValue()}.
	 */
	@Test
	public final void testGetDefaultValue() {
		DataValue [] dataValue = {
				new DataValue(DataType.BOOLEAN,false),	//ABSTRACT
				new DataValue(DataType.STRING,"None"),	//AGGREGATIONKIND
				new DataValue(DataType.STRING,"Bag"),	//ARRAYTYPE
				null,	//BITMAP
				null,	//BITVALUES
				new DataValue(DataType.BOOLEAN,false),	//COUNTER
				null,	//DEPRECATED
				null,	//DESCRIPTION
				new DataValue(DataType.BOOLEAN,false),	//EMBEDDEDOBJECT
				new DataValue(DataType.BOOLEAN,false),	//EXPERIMENTAL
				new DataValue(DataType.BOOLEAN,false),	//GAUGE
				new DataValue(DataType.BOOLEAN,true),	//IN
				new DataValue(DataType.BOOLEAN,false),	//ISPUNIT
				new DataValue(DataType.BOOLEAN,false),	//KEY
				null,	//MAPPINGSTRINGS
				null,	//MAX
				new DataValue(DataType.UINT32,new UInt32((long)0)),	//MIN
				null,	//MODELCORRESPONDENCE
				null,	//OCL
				new DataValue(DataType.BOOLEAN,false),	//OUT
				new DataValue(DataType.BOOLEAN,false),	//OVERRIDE
				null,	//PACKAGEPATH
				null,	//PUNIT
				new DataValue(DataType.BOOLEAN,true),	//READ
				new DataValue(DataType.BOOLEAN,false),	//REQUIRED
				new DataValue(DataType.BOOLEAN,false),	//STATIC
				new DataValue(DataType.BOOLEAN,false),	//TERMINAL
				null,	//VERSION
				new DataValue(DataType.BOOLEAN,false),	//WRITE
				new DataValue(DataType.STRING,null),	//XMLNAMESPACE
				null,	//IMPLEMENTS
				
		};
		assertEquals(dataValue.length,qualifiers.length);
		for(int i = 0; i < qualifiers.length; i++){
			// System.out.println(qualifiers[i].toMOF());
			assertEquals(dataValue[i],qualifiers[i].getDefaultValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getQualifier(java.lang.Object, net.aifusion.metamodel.NameSpacePath)}.
	 */
	@Test
	public final void testGetQualifierObjectNameSpacePath() {
		NameSpacePath p = new NameSpacePath("http://someHost/somePath");
		for(int i = 0; i < qualifiers.length; i++){
			QualifierType qt = qualifiers[i].getQualifierType(p);	// note that the namespace in qt and q must match
			DataValue value = new DataValue(qt.getDataType(),null);	// null is a valid dataValue for all types
			Qualifier q = qualifiers[i].getQualifier(value,p);
			assertEquals(q.getQualifierType(),qt);
			assertTrue(q.hasValue());
			assertEquals(value,q.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getQualifier()}.
	 */
	@Test
	public final void testGetQualifier() {
		for(int i = 0; i < qualifiers.length; i++){
			QualifierType qt = qualifiers[i].getQualifierType();
			Qualifier q = qualifiers[i].getQualifier();
			assertEquals(q.getQualifierType(),qt);
			assertFalse(q.hasValue());
			// note that Boolean qualifiers will return true if they are defined with no value
			if(qt.getDataType() != DataType.BOOLEAN){
				assertEquals(qt.getDefaultValue(),q.getValue());
			} else {
				assertEquals(new DataValue(true),q.getValue());
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getQualifierTypes()}.
	 */
	@Test
	public final void testGetQualifierTypes() {
		QualifierType [] qualifierTypes = StandardQualifierType.getQualifierTypes();
		assertEquals(qualifiers.length,qualifierTypes.length);
		for(int i = 0; i < qualifiers.length; i++){
			assertEquals(mof[i],qualifierTypes[i].toMOF());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getQualifierType()}.
	 */
	@Test
	public final void testGetQualifierType() {
		for(int i = 0; i < qualifiers.length; i++){
			assertEquals(mof[i],qualifiers[i].getQualifierType().toMOF());
		}
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#getQualifierType(net.aifusion.metamodel.NameSpacePath)}.
	 */
	@Test
	public final void testGetQualifierTypeNameSpacePath() {
		NameSpacePath p = new NameSpacePath("http://user:pass@localhost:80/root");
		for(int i = 0; i < qualifiers.length; i++){
			QualifierType q = qualifiers[i].getQualifierType();
			assertEquals(Constants.defaultNameSpacePath,q.getObjectPath().getNameSpacePath());
			q = qualifiers[i].getQualifierType(p);
			assertEquals(p,q.getObjectPath().getNameSpacePath());
			
		}
	}
	/**
	 * test method for {@link net.aifusion.metamodel.StandardQualifierType#appliesTo(ElementType)}.
	 */
	@Test
	public final void testAppliesTo(){
		for(StandardQualifierType q : qualifiers){
			for(Scope s : Scope.values()){
				for(ElementType e : ElementType.values()){
					if(q.hasScope(s) && s.appliesTo(e)){
						assertTrue(q.appliesTo(e));
					} 
				}
			}

		}
	}
	
	/**
	 * Test method for {@link net.aifusion.metamodel.StandardQualifierType#isKnownType(String)}.
	 */
	@Test
	public final void testIsStandardQualifierType(){
		for(StandardQualifierType q : qualifiers){
			assertTrue(StandardQualifierType.isKnownType(q.name()));
		}
		assertFalse(StandardQualifierType.isKnownType("foo"));
	}
}
