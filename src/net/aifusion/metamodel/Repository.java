/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Apr 20, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.List;

/**
 * Interface implemented by all repository classes
 * @author Sharad Singhal
 */
public interface Repository {
	
	/*
	 * ************************************
	 * Content operations on the repository
	 * ************************************
	 */
	
	/**
	 * Put a named element into the repository
	 * @param element - element to be added
	 * @return - true if successfully added, false otherwise
	 */
	public boolean put(NamedElement element);
	
	/**
	 * Get a named element from the repository
	 * @param path - object path corresponding to the named element
	 * @return - Named Element corresponding to the element. Null returned if no such element exists in the repository
	 */
	public NamedElement get(ObjectPath path);
	
	/**
	 * Check if a named element exists in the repository
	 * @param path - object path corresponding to the named element
	 * @return - true if the path exists in the repository, false otherwise
	 */
	public boolean contains(ObjectPath path);
	
	/**
	 * Delete a named element from the repository
	 * @param path - object path corresponding to the named element
	 * @return - true if deletion succeeded, false otherwise
	 */
	public boolean delete(ObjectPath path);
	
	/**
	 * Get a list of all name spaces known to this repository. Note that because implementations can add or remove
	 * name spaces as elements are added or deleted, this list may not be complete, and depends on the repository 
	 * implementation
	 * @return - list of name spaces
	 */
	public List<NameSpacePath> getNameSpaces();
	
	/**
	 * Get elements from this repository. Depending on the size of the repository, this method can be very expensive in 
	 * time and/or memory
	 * @param elementTypes - Optional comma separated list of element types to retrieve. Must be a NamedElement type
	 * (QualifierType, Enumeration, Interface, Structure, Class, StructureValue or Instance). A null will retrieve all types.
	 * Note that standard qualifier types are "built in" and are not returned by this method.
	 * @param localNameSpaces - Optional comma separated list of local name spaces to search. A null will retrieve 
	 * elements from all name spaces
	 * @param elementNames - Optional comma-separated list of element names to retrieve. A null will retrieve all elements of the given type.
	 * For instances, all instances corresponding to the class name are returned
	 * @param locateSubTypes - if true, locate all subtypes of the given element types (e.g., all subclasses of the requested class).
	 * @return - list of elements that match the given criteria. An empty list is returned if no matching elements are found
	 * @see ElementType
	 */
	public List<NamedElement> getElements(String elementTypes, String localNameSpaces, String elementNames, boolean locateSubTypes);
	
	/**
	 * Find all structure value instances based on some filter criteria
	 * @param filter - filter to apply
	 * @return - list of structureValues that pass the given filter. Empty list if none available
	 */
	public List<StructureValue> filter(CimFilter filter);
	
	/* 
	 * *************************************************************
	 * General methods to support defaults and repository management
	 * *************************************************************
	 */
	
	/**
	 * Shut down the repository. This method MUST be called before exiting to
	 * cleanly finish all pending tasks and release resources in the repository
	 */
	public void shutdown();

}
