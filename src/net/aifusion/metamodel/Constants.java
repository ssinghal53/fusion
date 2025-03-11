/**
 * Copyright 2014,2025 Sharad Singhal, All Rights Reserved
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
 * Created Nov 29, 2014 by Sharad Singhal
 * Last Modified Mar 2, 2025 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
Global constants used in the meta-model classes
@author Sharad Singhal
 */
public class Constants {
	/** Default schema name, if nothing is defined (AIFusion)*/
	public static final String defaultSchema = "AIFusion";
	/** Default scheme to be used (http)*/
	public static final String defaultScheme = "http";
	/** Default host name to be used */
	public static final String defaultHost = "localhost";
	/** Default port number to be used */
	public static final int defaultPort = 8085;
	/** default authority (host:port) if none specified(localhost:8085)*/
	public static final String defaulAuthority = defaultHost+":"+defaultPort;
	/** Default local path if none specified (/aifusion)*/
	public static final String defaultLocalPath = "/aifusion";
	/** Default name space path ("http://localhost:8085/aifusion")*/
	public static final NameSpacePath defaultNameSpacePath = new NameSpacePath(defaultScheme+"://"+defaulAuthority+defaultLocalPath);
	/** Default repository location for persistent objects (repository)*/
	public static final String defaultRepositoryLocation = "repository";
	/** MappingStrings qualifier definition as fusiononMap JavaClassPath [# methodName] */
	public static final String fusionMap = "Bind.CF|";
	/** Default version (if none given) assumed for mapped java classes (0.0.1)*/
	public static final String defaultVersion = "0.0.1";
	/** Byte encoding for strings and character arrays (UTF-8) */
	public static final Charset byteEncoding = StandardCharsets.UTF_8;
	/** Copyright text */
	public static final String copyright = "Copyright (c) 2013-2025, Sharad Singhal\r\n" +
			"Copyright (c) 2020, Hewlett Packard Enterprise Development LP\r\n" + 
			"All rights reserved.\r\n";
	/** License text */
	public static final String license = "Redistribution and use in source and binary forms, with or without\r\n" + 
			"modification, are permitted provided that the following conditions are met:\r\n" + 
			"1. Redistributions of source code must retain the above copyright notice, this\r\n" + 
			"   list of conditions and the following disclaimer.\r\n" + 
			"2. Redistributions in binary form must reproduce the above copyright notice,\r\n" + 
			"   this list of conditions and the following disclaimer in the documentation\r\n" + 
			"   and/or other materials provided with the distribution.\r\n" + 
			"3. Neither the name of the copyright holder nor the names of its\r\n" + 
			"   contributors may be used to endorse or promote products derived from\r\n" + 
			"   this software without specific prior written permission.\r\n" + 
			"   \r\n" + 
			"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\r\n" + 
			"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\r\n" + 
			"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\r\n" + 
			"DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE\r\n" + 
			"FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\r\n" + 
			"DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR\r\n" + 
			"SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER\r\n" + 
			"CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,\r\n" + 
			"OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\r\n" + 
			"OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\r\n";
	/** Current library version (1.0.0-R1")*/
	public static final String libraryVersion = "1.1.0-R1";
	/**
	 * Global Constants
	 * This class only contains static constants used in the metamodel classes. It should not be instantiated.
	 */
	private Constants() {
		return;
	}

}
