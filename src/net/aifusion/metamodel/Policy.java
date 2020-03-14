/**
 * Copyright 2013, Sharad Singhal, All Rights Reserved
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
 * Created Dec 31, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Enumeration defining qualifier policies
 * @author Sharad Singhal
 */
public enum Policy {
	// defined as Policy(String mofName, boolean defaultValue)
	/** The Qualifier can be overridden. Default policy is TRUE */
	ENABLEOVERRIDE("EnableOverride",true),
	/** The Qualifier cannot be overridden. Default policy is FALSE */
	DISABLEOVERRIDE("DisableOverride",false),
	/** The Qualifier applies only to the class in which it is declared. Default policy is FALSE */
	RESTRICTED("Restricted",false);
	
	/* 
	 * Note that at most only one policy can be specified on a qualifier. This implies four choices:
	 * 1. No policy is specified. This means that the qualifier propagates to subclasses (restricted = false), and it
	 *    can be overridden in subclasses (enableOverride = true, disableOverride = false)
	 * 2. EnableOverride = true is specified. This is the same as case 1.
	 * 3. DisableOverride = true. this means that the qualifier cannot be overridden, and it will propagate to subclasses.
	 * 4. Restricted = true. This means that the qualifier will not propagate to subclasses (and hence the override flag
	 *    does not matter). 
	 */
	
	/** MOF name for this Policy */
	private final String mofName;
	/** default dataValue for this policy */
	private boolean defaultValue;
	/**
	 * Construct a new qualifier policy
	 * @param defaultValue - default dataValue for this qualifier policy
	 */
	private Policy(String mofName, boolean defaultValue){
		this.mofName = mofName;
		this.defaultValue = defaultValue;
		return;
	}
	
	/**
	 * Get the mof name of this policy
	 * @return - mixed case name for the policy
	 */
	public final String toMOF(){
		return mofName;
	}
	
	/**
	 * Get the default dataValue of this policy
	 * @return - true if the default dataValue of this policy is true, false otherwise
	 */
	public final boolean getValue(){
		return defaultValue;
	}
	
}