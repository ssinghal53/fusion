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
 * Created July 8, 2017 by Sharad Singhal
 */
package net.aifusion.metamodel;

 import org.junit.runner.RunWith;
 import org.junit.runners.Suite;
 import org.junit.runners.Suite.SuiteClasses;

 @RunWith(Suite.class)
 @SuiteClasses({ 
	 net.aifusion.metamodel.ModelExceptionTest.class,
	 net.aifusion.metamodel.ScopeTest.class,
	 net.aifusion.metamodel.PolicyTest.class,
	 net.aifusion.metamodel.ElementTypeTest.class,
	 net.aifusion.metamodel.ElementTest.class,
	 net.aifusion.metamodel.ExportTest.class,
	 net.aifusion.metamodel.OctetStringTest.class,
	 net.aifusion.metamodel.DateTimeTest.class,
	 // net.aifusion.metamodel.EncryptionUtilityTest.class,
	 net.aifusion.metamodel.NameSpacePathTest.class,
	 net.aifusion.metamodel.ObjectPathTest.class,
	 net.aifusion.metamodel.DataTypeTest.class,
	 net.aifusion.metamodel.DataValueTest.class,
	 net.aifusion.metamodel.QualifierTypeTest.class,
	 net.aifusion.metamodel.QualifierTest.class,
	 net.aifusion.metamodel.StandardQualifierTypeTest.class,
	 net.aifusion.metamodel.QualifiedElementTest.class,
	 net.aifusion.metamodel.CimEventTypeTest.class,
	 net.aifusion.metamodel.NamedElementTest.class,
	 net.aifusion.metamodel.EnumerationValueTest.class,
	 net.aifusion.metamodel.EnumerationTest.class,
	 net.aifusion.metamodel.StructureValueTest.class,
	 net.aifusion.metamodel.CimPropertyTest.class,
	 net.aifusion.metamodel.CimStructureTest.class,
	 net.aifusion.metamodel.CimParameterTest.class,
	 net.aifusion.metamodel.CimMethodTest.class,
	 net.aifusion.metamodel.InMemoryRepositoryTest.class,
	 net.aifusion.metamodel.MOFParserTest.class,
	 net.aifusion.metamodel.PersistentCacheTest.class,
	 net.aifusion.metamodel.JavaModelMapperTest.class,
	 net.aifusion.metamodel.JavaBindingTest.class

	 // TODO: add test for CimClass, CimInstance
	 // TODOD: Add tests for CimIndication
	 // TODO: Add tests for events on all Repository, Provider, and NamedElement tests

 })
 public class AllTests {

 }
