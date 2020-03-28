/**
 * Copyright 2017, Sharad Singhal, All Rights Reserved
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
 * Created Jan 29, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

/**
 * Known MIME types to the Server
 * @author Sharad Singhal
 */
public enum MimeType {
	// extension, type
	/** Plain text type */
	PLAINTEXT("txt","text/plain"),
	/** MOF text type */
	MOF("mof","text/mof"),
	/** HTML type */
	HTML("html","text/html"),
	/** Plain text type */
	TEXT("text","text/*"),
	// note that BINARY must be first occurrence of application/octet-stream
	/** Binary stream type */
	BINARY("bin","application/octet-stream"),
	/** Chunked ang zipped type */
	ZIP("zip","application/octet-stream");
	
	private String extension;
	private String type;
	
	/**
	 * Create a mime type
	 * @param extension - file extension
	 * @param type - corresponding mime type
	 */
	private MimeType(String extension, String type){
		this.extension = extension;
		this.type = type;
		return;
	}
	
	/**
	 * Get an extension for this type
	 * @return - extension associated with this type
	 */
	public String getExtension(){
		return extension;
	}
	
	/**
	 * Get the mime type associated with this type
	 * @return - mime type associated with this type
	 */
	public String getType(){
		return type;
	}

	/**
	 * Look up a mime type associated with a given extension
	 * @param h - extension for the type
	 * @return - first matching MimeType that matches the extension. Default is BINARY (RFC 2616)
	 */
	public static MimeType lookup(String h) {
		for(MimeType t : MimeType.values()){
			if(t.type.equalsIgnoreCase(h)) return t;
		}
		return BINARY;
	}
}
