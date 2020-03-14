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
 * Created Feb 20, 2016 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Class to test isGetter property combinations
 * @author Sharad Singhal
 */
@Export(name="Class",schema="Test",qualifiers="Description(\"Structure to test isGetter property bindings\")")
public class IsGetterPropertyClass {
	
	Boolean p1=false,p2=false,p3=false,p4=false,p5=false,p6=false,p7=false;
	String s1;
	
	// isBoolean(T) isProperty(F) getProperty(F) setProperty(T)
	// [write,read(false)] boolean p1;
	@Export(qualifiers="Description(\"write only property\")")
	public void setP1(Boolean p){
		p1 = p;
	}
	//  isBoolean(T)  isProperty(F) getProperty(T) setProperty(F)
	// boolean p2;
	@Export(qualifiers="Description(\"read-only property\")")
	public Boolean getP2(){
		return p2;
	}
	//  isBoolean(T) isProperty(F) getProperty(T) setProperty(T)
	// [write] boolean p3;
	@Export(qualifiers="Description(\"read/write property\")")
	public Boolean getP3(){
		return p3;
	}
	@Export
	public void setP3(Boolean p){
		p3 = p;
	}
	//  isBoolean(T) isProperty(T) getProperty(F) setProperty(F)
	// boolean p4;
	@Export(qualifiers="Description(\"readonly only property with isGetter\")")
	public Boolean isP4(){
		return p4;
	}
	//  isBoolean(T) isProperty(T) getProperty(F) setProperty(T)
	// [write] boolean p5;
	@Export(qualifiers="Description(\"read/write only property with isGetter\")")
	public Boolean isP5(){
		return p5;
	}
	@Export
	public void setP5(Boolean p){
		p5 = p;
	}
	//  isBoolean(T) isProperty(T) getProperty(T) setProperty(F)
	// boolean p6; boolean getp6();
	@Export(qualifiers="Description(\"readonly only property with get() method\")")
	public Boolean isP6(){
		return p6;
	}
	@Export
	public Boolean getP6(){
		return p6;
	}
	
	//  isBoolean(T) isProperty(T) getProperty(T) setProperty(T)
	// [write] boolean p7; boolean getp7();
	@Export(qualifiers="Description(\"read/write property with get() method\")")
	public Boolean isP7(){
		return p7;
	}
	@Export
	public Boolean getP7(){
		return p7;
	}
	@Export
	public void setP7(Boolean p){
		p7 = p;
	}
	
	// isBoolean(F) isProperty(F) getProperty(F) setProperty(T)
	// [write,read(false)] String pf1;
	@Export(qualifiers="Description(\"write only property\")")
	public void setPF1(String p){
		s1 = p;
	}
	//  isBoolean(F)  isProperty(F) getProperty(T) setProperty(F)
	// String pf2;
	@Export(qualifiers="Description(\"read-only property\")")
	public String getPF2(){
		return s1;
	}
	//  isBoolean(F) isProperty(F) getProperty(T) setProperty(T)
	// [write] String pf3;
	@Export(qualifiers="Description(\"read/write property\")")
	public String getPF3(){
		return s1;
	}
	@Export
	public void setPF3(String p){
		s1 = p;
	}
	//  isBoolean(F) isProperty(T) getProperty(F) setProperty(F)
	// String ispf4;
	@Export(qualifiers="Description(\"readonly only property with isGetter\")")
	public String isPF4(){
		return s1;
	}
	//  isBoolean(F) isProperty(T) getProperty(F) setProperty(T)
	// String ispf5; [write,read(false)] String pf5;
	@Export(qualifiers="Description(\"read/write only property with isGetter\")")
	public String isPF5(){
		return s1;
	}
	@Export
	public void setPF5(String p){
		s1 = p;
	}
	//  isBoolean(F) isProperty(T) getProperty(T) setProperty(F)
	// String ispf6; String pf6;
	@Export(qualifiers="Description(\"readonly only property with get() method\")")
	public String isPF6(){
		return s1;
	}
	@Export
	public String getPF6(){
		return s1;
	}
	
	//  isBoolean(F) isProperty(T) getProperty(T) setProperty(T)
	// String ispf7; [write] String pf7;
	@Export(qualifiers="Description(\"read/write property with get() method\")")
	public String isPF7(){
		return s1;
	}
	@Export
	public String getPF7(){
		return s1;
	}
	@Export
	public void setPF7(String p){
		s1 = p;
	}
}
