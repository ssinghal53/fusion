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
 * Created Dec 26, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.io.BufferedReader;
import java.io.InputStream;

/**
 * This interface is implemented by all CIM parsers
 * @author Sharad Singhal
 */
public interface Parser {
	/**
	 * Parse the specification given in a file into the parser repository
	 * @param fileName - name of the file to be parsed
	 * @param path - optional namespace. If non-null, the parser will initialize to the given path before parsing the file
	 */
	public void parse(String fileName, NameSpacePath path);
	
	/**
	 * Parse the specification given in a buffered stream into the parser repository
	 * @param input - input reader
	 * @param path - optional name space. if non-null, the parser will initialize to the name space before parsing the input
	 */
	public void parse(BufferedReader input, NameSpacePath path);
	
	/**
	 * Parse the specification given in an inputstream into the parser repository
	 * @param input - input stream
	 * @param path - optional name space. if non-null, the parser will initialize to the name space before parsing the input
	 */
	public void parse(InputStream input, NameSpacePath path);
	
	/**
	 * Get the repository being used by the parser
	 * @return - repository being used by the parser
	 */
	public Repository getRepository();
	
	/**
	 * Parse a structure value from an incoming indication
	 * @param indication - value of the indication
	 * @param path - optional name space. if non-null, the parser will initialize to the name space before parsing the input
	 * @return - incoming structure value; null if none found
	 */
	public StructureValue parseValue(String indication, NameSpacePath path);
	
}
