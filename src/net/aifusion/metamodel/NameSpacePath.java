/**
 * Copyright 2013, 2025 Sharad Singhal, All Rights Reserved
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
 * Last Modified April 2, 2025 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * Class to represent a CIM name space path. A name space represents a group of (distributed) entities that are part of a logical grouping, and
 * serves to separate names within that grouping from other name spaces. Name spaces should be constructed to provide global name separation.
 * @author Sharad Singhal
 */
public class NameSpacePath {
	/** URI representation of this path */
	private URI pathURI = null;
	/** Local model path of this path */
	private String localPath = null;
	/** Resource end point part of this path */
	private String resourcePath = null;
	
	/**
	 * Create a namespace from a string representation. The path provided MUST include a valid local path<br>
	 * @param path - namespace path representation in form "http://authority/pathElement0/pathelement1/...". A path element of '+' serves
	 * as a delimiter between the resource name and the namespace used, i.e. http://authority/resource/+/pathname implies that the resource
	 * is available at http://authority/resource, and the object uses /pathname in the object path
	 */
	public NameSpacePath(String path){
		try {
			URI uri = new URI(path).normalize();
			String modelPath = uri.getPath();
			if(modelPath != null) modelPath = ModelUtilities.normalizePath(modelPath);
			if(modelPath == null || modelPath.isEmpty() || modelPath.contains("_")) 
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,"NameSpacePath must reduce to non-empty path without _ characters"+path);			
			pathURI = new URI(uri.getScheme(),uri.getAuthority(),modelPath,null,null);
			if(modelPath.contains("/+/")) {
				resourcePath = modelPath.substring(0, modelPath.indexOf("/+/")+1);	// up to "/+/" and including the starting "/"
				localPath = modelPath.substring(modelPath.indexOf("/+/")+2);	// from "/+/" but including the trailing "/"
			} else {
				localPath = modelPath;
			}
			// System.out.println("Resource Path : "+resourcePath+" Local Path : "+this.localPath);
		} catch(URISyntaxException ex){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Illegal name space path "+path+"\n",ex);
		}
		return;
	}
	
	/**
	 * Create a name space from a string representation.<br>
	 * @param scheme - scheme for this name space (e.g., "http")
	 * @param authority - authority for this name space (e.g., (e.g. user:pass@hostname:port))
	 * @param path - name space path representation in form "/pathElement0/pathelement1/...". Must be non-null. A path element of '+' serves
	 * as a delimiter between the resource name and the namespace used, i.e. http://authority/resource/+/pathname implies that the resource
	 * is available at http://authority/resource, and the object uses /pathname as the localPath
	 */
	public NameSpacePath(String scheme, String authority, String path){
		try {
			if(path == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"path must not be null");
			URI uri  = new URI(scheme,authority,path,null,null).normalize();
			String modelPath = uri.getPath();
			modelPath = ModelUtilities.normalizePath(modelPath);
			if(modelPath == null || modelPath.isEmpty() || modelPath.contains("_")) 
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,"NameSpacePath must reduce to non-empty path without _ characters"+path);
			pathURI = new URI(uri.getScheme(),uri.getAuthority(),modelPath,null,null);
			if(modelPath.contains("/+/")) {
				resourcePath = modelPath.substring(0, path.indexOf("/+/")+1);	// up to "/+/" and including the starting "/"
				localPath = modelPath.substring(path.indexOf("/+/")+2);			// from "/+/" but including the trailing "/"
			} else {
				localPath = modelPath;
			}
		} catch(URISyntaxException ex){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Illegal name space path "+scheme+"://"+authority+path+"\n",ex);
		}
		return;
	}
	
	/**
	 * Get the name space scheme (e.g., http or https)
	 * @return - name space scheme (null if none set)
	 */
	public String getScheme(){
		return pathURI.getScheme();
	}
	
	/**
	 * Get the authority info (e.g., user:pass@host:port) for this name space path
	 * @return authority (null if none set)
	 */
	public String getAuthority(){
		return pathURI.getAuthority();
	}
	
	/**
	 * Get the local name space path from this namespace path
	 * @return - string representation of local path (e.g., /root/cimv2) or null of no local path is defined
	 */
	public String getLocalPath(){
		return localPath == null ? pathURI.getPath() : localPath;
	}
	
	/**
	 * Get the resource end point from this name space path
	 * @return - String representation of the resource path (e.g., /serverEndPoint/). Default (/) returned if none defined
	 */
	public String getResourcePath() {
		return resourcePath == null ? Constants.defaultResourcePath : resourcePath;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString(){
		return pathURI.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return pathURI.hashCode();
	}
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof NameSpacePath)) return false;
		NameSpacePath other = (NameSpacePath)obj;
		return pathURI.equals(other.pathURI);
	}
}
