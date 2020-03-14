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
 * Created Oct 7, 2017 by Sharad Singhal
 */
package net.aifusion.cql;

/**
 * Node to represent a class path
 * @author Sharad Singhal
 */
class ClassPath extends Node {
	private String localPath = null;
	private String className = null;

	/**
	 * Create a ClassPath node, e.g., [/path/path.../:]class_name
	 * @param name - name for this classPath
	 */
	ClassPath(String name) {
		super(Operator.CLASS_PATH, name, null);
		int index = name.indexOf(":");
		if(index > 0){
			localPath = name.substring(0,index);
			className = name.substring(index+1);
		} else {
			localPath = null;
			className = name;
		}
		return;
	}
	
	/**
	 * Get the name of the class defined in this class path
	 * @return - name of the class
	 */
	String getClassName(){
		return className;
	}
	
	/**
	 * Get the local path associated with this class path
	 * @return - local path, if any defined, else null
	 */
	String getLocalPath(){
		return localPath;
	}
	
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder(super.toString());
		b.append( " [localPath = ").append(localPath).append(" class = ").append(className).append("]");
		return b.toString();
	}
}
