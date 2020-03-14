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
 * Created Jun 27, 2016 by Sharad Singhal
 */
package net.aifusion.utils;

import net.aifusion.metamodel.Export;

/**
 * Class used to test Java2Cim converter
 * @author Sharad Singhal
 */
@Export(name="TestClass",schema="CIM",version="1.0.0")
public class GetSetClass {
	
	private String ID = "keyID";
	private String value = null;
	private String getSetValue = null;
	private Boolean isGetSetValue = null;
	private Boolean isIsGetGetSetValue = null;

	/**
	 * Key property-- only getter defined (Case 4)
	 * @return - key to this instance
	 */
	@Export(qualifiers="Key,Description(\"Case 4 - getter only\")")
	public String getID(){
		return ID;
	}
	
	/**
	 * Set only property (Case 2)
	 * @param value - value to set
	 */
	@Export(qualifiers="Description(\"Case 2 - setter only\")")
	public void setWriteOnlyValue(String value){
		this.value = value;
		return;
	}
	
	/**
	 * Method to check setWriteOnlyValue. Note that since this method is not exported, it is not a CIM method
	 * @return - value written through getWriteOnlyValue
	 */
	public String getWriteOnlyValue(){
		return value;
	}
	
	/**
	 * Getter/setter property (Case 6)
	 * @return - property value
	 */
	@Export(qualifiers="Description(\"Case 6 - get/set only\")")
	public String getGetSetValue(){
		return getSetValue;
	}
	
	/**
	 * Getter/Setter property (Case 6)
	 * @param getSetValue - value to set
	 */
	@Export
	public void setGetSetValue(String getSetValue){
		this.getSetValue = getSetValue;
		return;
	}
	
	
	/**
	 * isGetter/setter property (Case 3)
	 * @return - property value
	 */
	@Export(qualifiers="Description(\"Case 3 - isGet/set only\")")
	public Boolean isIsGetSetValue(){
		return isGetSetValue;
	}
	
	/**
	 * isGetter/Setter property (Case 3)
	 * @param isGetSetValue - value to set
	 */
	@Export
	public void setIsGetSetValue(Boolean isGetSetValue){
		this.isGetSetValue = isGetSetValue;
		return;
	}
	
	/**
	 * IsGet only property
	 * @return - property value
	 */
	@Export(qualifiers="Description(\"Case 1 - isGet only\")")
	public Boolean isIsGetOnlyValue(){
		return false;
	}
	
	/**
	 * Getter/IsGetter property (Case 5)
	 * @return - property value
	 */
	@Export(qualifiers="Description(\"Case 5 - get/IsGet only\")")
	public Boolean getIsGetGetValue(){
		return false;
	}
	
	/**
	 * Getter/Setter property (Case 5)
	 * @return property value
	 */
	@Export(qualifiers="Description(\"Case 5 - get/IsGet only\")")
	public Boolean isIsGetGetValue(){
		return true;
	}
	
	/**
	 * Getter/IsGetter property (Case 7)
	 * @return - property value
	 */
	@Export(qualifiers="Description(\"Case 7 - get/IsGet/Set only\")")
	public Boolean getIsGetGetSetValue(){
		return isIsGetGetSetValue;
	}
	
	/**
	 * Getter/IsGetter property (Case 7)
	 * @return - property value
	 */
	@Export(qualifiers="Description(\"Case 7 - get/IsGet/Set only\")")
	public Boolean isIsGetGetSetValue(){
		return isIsGetGetSetValue;
	}
	
	/**
	 * Getter/Setter property (Case 7)
	 * @param getIsGetSetValue - value to set
	 */
	@Export
	public void setIsGetGetSetValue(Boolean getIsGetSetValue){
		this.isIsGetGetSetValue = getIsGetSetValue;
	}
	
	/**
	 * String method - (Case 0)
	 * @param input - input to method
	 * @return - input value
	 */
	@Export(qualifiers="Description(\"Case 0 - method\")")
	public String echo(String input){
		return input;
	}

}
