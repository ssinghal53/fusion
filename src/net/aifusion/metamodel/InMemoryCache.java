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
 * Created Dec 12, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.HashMap;
import java.util.List;
import java.util.Vector;

/**
 * In-memory repository supporting multiple namespaces, but no persistence
 * @author Sharad Singhal
 */
public class InMemoryCache implements Repository {
	/** Known repositories keyed by localPath */
	private HashMap<String,InMemoryRepository> repositories = new HashMap<String,InMemoryRepository>();
	
	/**
	 * Create an in-memory CIM repository that supports multiple namespaces, but no persistence
	 * @see StandardQualifierType
	 */
	public InMemoryCache() {
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#put(net.aifusion.metamodel.NamedElement)
	 */
	@Override
	public synchronized boolean put(NamedElement element) {
		if(element == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Cannot insert null elements in repository");
		String localPath = element.getObjectPath().getLocalPath();
		InMemoryRepository repository = repositories.get(localPath);
		if(repository == null){
			repository = new InMemoryRepository(element.getNameSpacePath());
			repositories.put(localPath,repository);
		}
		return repository.put(element);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#get(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public NamedElement get(ObjectPath path) {
		if(path == null) return null;
		InMemoryRepository repository = repositories.get(path.getLocalPath());
		if(repository != null) return repository.get(path);
		// if a standard qualifier type is requested, construct it even if no corresponding repository exists
		if(path.getElementType() == ElementType.QUALIFIERTYPE && StandardQualifierType.isKnownType(path.getName())){
			return StandardQualifierType.valueOf(path.getName().toUpperCase()).getQualifierType(path.getNameSpacePath());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#hasElement(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public boolean contains(ObjectPath path) {
		if(path == null) return false;
		InMemoryRepository repository = repositories.get(path.getLocalPath());
		if(repository != null) return repository.contains(path);
		// standard qualifier types are always known, even if no corresponding repository exists
		if(path.getElementType() == ElementType.QUALIFIERTYPE && StandardQualifierType.isKnownType(path.getName())){
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#delete(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public synchronized boolean delete(ObjectPath path) {
		if(path == null) return false;
		InMemoryRepository repository = repositories.get(path.getLocalPath());
		if(repository == null) return false;
		return repository.delete(path);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#shutdown()
	 */
	@Override
	public synchronized void shutdown() {
		for(Repository r : repositories.values()){
			r.shutdown();
		}
		repositories.clear();
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#getNameSpaces()
	 */
	@Override
	public List<NameSpacePath> getNameSpaces() {
		Vector<NameSpacePath> nameSpaces = new Vector<NameSpacePath>();
		for(Repository r : repositories.values()){
			nameSpaces.addAll(r.getNameSpaces());
		}
		return nameSpaces;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#executeQuery(net.aifusion.metamodel.CimQuery)
	 */
	@Override
	public List<NamedElement> getElements(String elementTypes, String localNameSpaces, String elementNames, boolean locateSubTypes) {
		// check if the repository has a matching name space (if requested)
		Vector<NamedElement> elements = new Vector<NamedElement>();
		if(localNameSpaces != null){
			String [] nameSpaces = localNameSpaces.split(",");
			for(String nameSpace : nameSpaces){
				String key = nameSpace.trim().toLowerCase();
				if(!repositories.containsKey(key)) continue;
				elements.addAll(repositories.get(key).getElements(elementTypes, key, elementNames, locateSubTypes));
			}
		} else {
			for(InMemoryRepository r : repositories.values()){
				elements.addAll(r.getElements(elementTypes, null, elementNames, locateSubTypes));
			}
		}
		return elements;
	}

	@Override
	public List<StructureValue> filter(CimFilter filter) {
		Vector<StructureValue> values = new Vector<StructureValue>();
		for(Repository r : repositories.values()) {
			values.addAll(r.filter(filter));
		}
		return values;
	}
}
