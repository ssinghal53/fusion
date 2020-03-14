/**
 * Copyright 2017 Sharad Singhal, All Rights Reserved
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
 * Created Oct 1, 2017 by sharad
 */
package net.aifusion.cql;

import net.aifusion.metamodel.DataType;

/**
 * Known CQL Functions
 * @author Sharad Singhal
 */
enum Functions {
	// numeric functions(resultType)
	DateTimeToMicroSeconds(DataType.UINT64),
	StringToUint(DataType.UINT64),
	StringToSint(DataType.SINT64),
	StringToReal(DataType.REAL64),
	// String functions
	UpperCase(DataType.STRING),
	LowerCase(DataType.STRING),
	NumericToString(DataType.STRING),
	ReferenceToString(DataType.STRING),
	InstanceOf(DataType.STRING),
	// Path functions
	ClassPath(DataType.OBJECTPATH),
	ObjectPath(DataType.OBJECTPATH),
	// DateTime functions
	CurrentDateTime(DataType.DATETIME),
	DateTime(DataType.DATETIME),
	MicroSecondToInterval(DataType.DATETIME),
	MicroSecondToTimeStamp(DataType.DATETIME);

	/** Data Type for the function result */
	private DataType resultType;
	
	/**
	 * Create the function definition
	 * @param resultType
	 */
	private Functions(DataType resultType){
		this.resultType = resultType;
	}
	
	/**
	 * Get the data type returned by this function
	 * @return - data type of the result
	 */
	DataType getType(){
		return resultType;
	}
	
	/**
	 * Lookup a function using its name
	 * @param function - name of the function
	 * @return - corresponding function. Null if no such function exists
	 */
	static Functions lookup(String function){
		if(function == null || function.isEmpty()) return null;
		for(Functions f : values()){
			if(function.equalsIgnoreCase(f.toString())) return f;
		}
		return null;
	}

}
