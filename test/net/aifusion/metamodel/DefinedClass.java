/**
 * Copyright 2020 Sharad Singhal. All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
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
 * Created Sep 15, 2020 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Class to test defined types created by annotations<br>
 * used in EnumBindingClass, InterfaceBindingClass, IsGetterPropertyClass, MethodBindingClass, PropertyBindingClass
 * @author Sharad Singhal
 * 
 */
@Export(schema="TEST")
public class DefinedClass {
	/**
	 * Create an annotated class
	 */
	public DefinedClass() {
		return;
	}
	@Export(qualifiers = "Key")
	public String getId() {
		return "id0";
	}
	
	/*
	 * ********************************************
	 * Note that all annotated classes should provide
	 * a toString() method that outputs the value in
	 * MOF format, and equals()/hashcode() methods
	 * to allow checks for equality within Fusion
	 * *********************************************
	 */
	
	@Override
	public String toString() {
		return "[MappingStrings{ \"Bind.CF|net.aifusion.metamodel.DefinedClass\" }] Structure TEST_DefinedClass { [Key] String Id; };\n";
	}
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof DefinedClass)) return false;
		return true;
	}
	@Override
	public int hashCode() {
		return "id0".hashCode();
	}
	
	
}
