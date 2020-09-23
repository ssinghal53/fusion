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
 * Created Sep 21, 2020 by sharad
 */
package net.aifusion.metamodel;

import java.util.Map;

/**
 * Class to test Java bindings for Structure values
 * @author Sharad Singhal
 *
 */
@Export(schema = "Test",name = "StrVal",version = "1.0.0",nameSpace = "/tests",qualifiers = "Description(\"Test Class for binding properties\")")
public class StructureValueClass {
	private String p1 = "DIGEST";
	private String id = null;

	public StructureValueClass(Map<String,Object> args){
		for(String pName : args.keySet()){
			switch(pName){
			case "Id":
				id = (String) args.get("Id");
				break;
			case "P1":
				p1 = (String) args.get("P1");
				break;
			default:
				break;
			}
		}
		if(id == null) id = "DefaultId";
		return;
	}

	@Export(qualifiers="Key,Description(\"Key property\")")
	public String getId(){
		return id;
	}
	@Export(defaultValue="\"DIGEST\"",qualifiers="Description(\"Other instance property\")")
	public String getP1(){
		return p1;
	}
	@Export
	public void setP1(String value) {
		this.p1 = value;
		return;
	}

	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append(id).append(" ");
		b.append(p1);
		return b.toString();
	}
	
	
}
