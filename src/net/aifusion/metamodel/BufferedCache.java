/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Dec 13, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.List;

/**
 * Class to implement a simple in-memory buffer backed by a repository for parsing messages. The buffer uses a
 * backing store to resolve any classes not contained in it. However, any objects added to (or deleted from)
 * the buffer do not affect the objects in the backing store. The buffer does not provide any persistence.
 * @author Sharad Singhal
 */
public class BufferedCache extends InMemoryCache {
	/** backing store for this repository */
	private Repository backingStore;

	/**
	 * Create a repository buffer with a backing store for additional definitions.
	 * New definitions are added to the buffer, while the backing store is used for
	 * any additional definitions needed for input resolution
	 * @param backingStore - Repository backing this buffer
	 */
	public BufferedCache(Repository backingStore){
		super();
		this.backingStore = backingStore;
		return;
	}
	
	/**
	 * Get the backing store for this repository
	 * @return - Repository backing this repository
	 */
	public Repository getBackingStore(){
		return backingStore;
	}

	/**
	 * Get a named element from the buffer. If the element does not exist in the buffer,
	 * it is retrieved from the backing store.
	 * @param path - object path for the element being requested
	 * @return - named element from the buffer (or the backing store). Null if none found
	 * @see #getBufferedElement(ObjectPath)
	 */
	@Override
	public NamedElement get(ObjectPath path) {
		// get element from buffer. If not found, get it from the backing store
		NamedElement element = super.get(path);
		return element != null ? element : backingStore.get(path);
	}
	/**
	 * Check if an element exists in the buffer. If the element does not exist in the
	 * buffer, it is checked in the backing store
	 * @param path object path for the element being checked
	 * @return true if the buffer (or the backing store) contains the element, false otherwise
	 */
	@Override
	public boolean contains(ObjectPath path) {
		boolean hasElement = super.contains(path);
		return hasElement ? true : backingStore.contains(path);
	}
	
	/**
	 * Get the list of name spaces defined in the buffer (or the backing store)
	 * @return list of all name spaces
	 */
	@Override
	public List<NameSpacePath> getNameSpaces() {
		List<NameSpacePath> nameSpaces = super.getNameSpaces();
		nameSpaces.addAll(backingStore.getNameSpaces());
		return nameSpaces;
	}
	
	/**
	 * Get a named element from the buffer. Only the buffer is checked for the element during retrieval
	 * @param path - objectPath for the element being requested
	 * @return - named element from the buffered values
	 * @see #get(ObjectPath)
	 */
	public NamedElement getBufferedElement(ObjectPath path){
		return super.get(path);
	}
	
	/**
	 * Get the named elements from the buffer, but not the backing store
	 * @return - list of all elements from this repository
	 */
	public List<NamedElement> getBufferedElements(){
		return super.getElements(null,null,null,false);
	}

	/**
	 * Get all elements from the buffer (as well as from the backing store)
	 * @param elementTypes - comma separated list of element types to search. Null implies all types
	 * @param localNameSpaces - comma separated list of local name spaces to search. Null implies all name spaces
	 * @param elementNames - comma separated list of element names to retrieve. Null matches all element names
	 * @param locateSubtypes - true if subclasses should be collected, false if only the named elements are required
	 * @return - list of retrieved elements. Empty if no elements found
	 */
	public List<NamedElement> getAllElements(String elementTypes,String localNameSpaces,String elementNames,boolean locateSubtypes){
		List<NamedElement> allElements = getElements(elementTypes,localNameSpaces,elementNames,locateSubtypes);
		allElements.addAll(backingStore.getElements(elementTypes, localNameSpaces, elementNames, locateSubtypes));
		return allElements;
	}
	
	/*
	 * Note that put(), delete(), getDefaultNameSpace(), getElements() and shutdown() do not access
	 * the backing store and do not need special handling here. Also shutdown() does not affect
	 * the underlying backing store.
	 * 
	 * It is possible for elements to be duplicated between the buffer and the backing store if they
	 * are added to the buffer, since BufferedCache.put() does not check for existence in the backing
	 * store. 
	 */
}
