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
 * Created Dec 31, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import net.aifusion.metamodel.PropertyBindingClass.EmbeddedStringEnum;

/**
 * Class to test Method bindings
 * @author Sharad Singhal
 */
@Export(name="TestMethods",schema="Cim",version="6.0.0")
public class MethodBindingClass extends MethodBindingSuperClass {
	
	private String key = "classKey";
	
	// note that instances require a key valued property
	@Export(qualifiers="Key")
	public String getKey(){
		return key;
	}
	
	@Export
	public String enumValueToString(EmbeddedStringEnum enumValue){
		return enumValue.value();
	}
	
	@Export
	public String concatStringToEnumValue(EmbeddedStringEnum enumValue, String s){
		return enumValue.value()+s;
	}
	
	@Export(refClass="net.aifusion.metamodel.PropertyBindingClass$EmbeddedStringEnum")
	public EnumerationValue enumValueToEnum(EmbeddedStringEnum enumValue){
		return new EnumerationValue("Value","EnumName",new DataValue("Value"),null);
	}
	
	@Export
	public void doSomething(){
		return;
	}
}
