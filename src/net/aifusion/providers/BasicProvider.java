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
package net.aifusion.providers;

import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Vector;

import net.aifusion.metamodel.CimClass;
import net.aifusion.metamodel.CimEventType;
import net.aifusion.metamodel.CimFilter;
import net.aifusion.metamodel.CimIndication;
import net.aifusion.metamodel.CimInstance;
import net.aifusion.metamodel.CimListener;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.CimStructure;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.CimEnumeration;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.InMemoryCache;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.NameSpacePath;
import net.aifusion.metamodel.NamedElement;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;

import net.aifusion.cql.CimQuery;

/**
 * This class represents a basic provider that can be extended by other classes to implement providers. The provider will persist
 * information if the repository passed in the constructor is persistent.
 * @author Sharad Singhal
 */
public class BasicProvider implements Provider {
	/** Repository for holding model elements within this provider */
	private Repository repository;	
	/** children for this provider -- keyed by namespace path */
	private HashMap<NameSpacePath,Provider> children = new HashMap<NameSpacePath,Provider>();
	/** event listeners for this Provider */
	private HashMap<CimEventType,List<CimListener>> listeners = new HashMap<CimEventType,List<CimListener>>();

	/**
	 * Create a basic provider. If no repository is given (i.e., repository == null), an in-memory cache is initialized within the provider.
	 * @param repository - repository to use for the provider
	 */
	public BasicProvider(Repository repository) {
		this.repository = (repository != null) ? repository : new InMemoryCache();
		return;
	}
	
	// TODO: We still need to implement a caching strategy, that allows elements to be cached in the local provider once they have been seen
	
	/*
	 * *************************
	 * Provider interface methods
	 * *************************
	 */
	
	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Provider#executeQuery(java.lang.String)
	 */
	@Override
	public List<StructureValue> executeQuery(String query) {
		CimQuery q = new CimQuery(query);
		return q.executeQuery(repository);
	}
	
	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Provider#registerProvider(net.aifusion.metamodel.Provider)
	 */
	@Override
	public void registerChildProvider(Provider child) {
		for(NameSpacePath np : child.getNameSpaces()){
			if(!children.containsKey(np)){
				children.put(np, child);
				continue;
			}
			// TODO: do we need to "unwind" inserted elements so we have an all-or-nothing insertion?
			throw new ModelException(ExceptionReason.ALREADY_EXISTS,"NameSpacePath "+
					np.toString()+" already exists in provider");
		}
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Provider#unregisterProvider(net.aifusion.metamodel.Provider)
	 */
	@Override
	public void unregisterChildProvider(Provider child) {
		if(children.isEmpty()) return;
		synchronized(children) {
			for(NameSpacePath np : child.getNameSpaces()){
				children.remove(np, child);
			}
		}
		return;
	}
	
	/* (non-Javadoc)
	 * @see net.aifusion.providers.Provider#getPropertyNames(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public List<String> getPropertyNames(ObjectPath path) {
		List<String> names = new Vector<String>();
		if(repository.contains(path)){
			NamedElement element = repository.get(path);
			switch(element.getElementType()){
			case INSTANCE:
				names.addAll(((CimInstance) element).getPropertyNames());
				return names;
			case INTERFACE:
				names.addAll(((CimStructure) element).getPropertyNames());
				return names;
			case CLASS:
				names.addAll(((CimClass) element).getPropertyNames());
				return names;
			case ENUMERATION:
				names.addAll(((CimEnumeration) element).getKeys());
				return names;
			case STRUCTURE:
				names.addAll(((CimStructure) element).getPropertyNames());
				return names;
			case STRUCTUREVALUE:
				names.addAll(((StructureValue) element).getPropertyNames());
				return names;
			default:
				break;
			}
		}
		Provider child = locateProvider(path);
		if(child != null) {
			names.addAll(child.getPropertyNames(path));
			return names;
		}
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider could not locate "+path.toString());
	}
	
	/* (non-Javadoc)
	 * @see net.aifusion.providers.Provider#getPropertyType(net.aifusion.metamodel.ObjectPath, java.lang.String)
	 */
	@Override
	public DataType getPropertyType(ObjectPath path, String propertyName) {
		if(repository.contains(path)){
			NamedElement element = repository.get(path);
			switch(element.getElementType()){
			case INSTANCE:
				return ((CimInstance) element).getPropertyType(propertyName);
			case CLASS:
				return ((CimClass) element).getPropertyType(propertyName);
			case ENUMERATION:
				// note that all enumeration keys have the same data type
				if(((CimEnumeration)element).hasKey(propertyName))
				 	return ((CimEnumeration) element).getDataType();
				else 
					throw new ModelException(ExceptionReason.NOT_FOUND,"Enumeration "+element.getName()+" does not have key "+propertyName);
			case STRUCTURE:
				return ((CimStructure) element).getPropertyType(propertyName);
			case STRUCTUREVALUE:
				return (((StructureValue) element).getPropertyType(propertyName));
			case INTERFACE:
				return (((CimStructure) element).getPropertyType(propertyName));
			default:
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Element Type "+element.getElementType()+" does not support gettable properties");
			}
		}
		Provider child = locateProvider(path);
		if(child != null) return child.getPropertyType(path, propertyName);
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider: Element "+path.toString()+" not found");
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Provider#getPropertyValue(net.aifusion.metamodel.ObjectPath, java.lang.String)
	 */
	@Override
	public DataValue getPropertyValue(ObjectPath path, String propertyName) {
		if(repository.contains(path)){
			NamedElement element = repository.get(path);
			switch(element.getElementType()){
			case INSTANCE:
				return ((CimInstance) element).getPropertyValue(propertyName);
			case CLASS:
				return ((CimClass) element).getPropertyValue(propertyName);
			case ENUMERATION:
				 return ((CimEnumeration) element).getDataValue(propertyName);
			case STRUCTURE:
				return ((CimStructure) element).getPropertyValue(propertyName);
			case STRUCTUREVALUE:
				return (((StructureValue) element).getPropertyValue(propertyName));
			case INTERFACE:
				return (((CimStructure) element).getPropertyValue(propertyName));
			default:
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Element Type "+element.getElementType()+" does not support gettable properties");
			}
		}
		Provider child = locateProvider(path);
		if(child != null) return child.getPropertyValue(path, propertyName);
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider: Element "+path.toString()+" not found");
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Provider#setPropertyValue(net.aifusion.metamodel.ObjectPath, java.lang.String, net.aifusion.metamodel.DataValue)
	 */
	@Override
	public void setPropertyValue(ObjectPath path, String propertyName, DataValue propertyValue) {
		if(repository.contains(path)){
			synchronized(repository){
				NamedElement element = repository.get(path);
				switch(element.getElementType()){
				case INSTANCE:
					((CimInstance) element).setPropertyValue(propertyName,propertyValue);
					repository.put(element);	// put the element back in the repository to ensure update
					break;
				case STRUCTUREVALUE:
					((StructureValue) element).setPropertyValue(propertyName,propertyValue);
					repository.put(element);	// put the element back in the repository to ensure update
					break;
				case CLASS:
					((CimClass) element).setPropertyValue(propertyName,propertyValue);
					repository.put(element);	// put the element back in the repository to ensure update
					break;
				case STRUCTURE:
					((CimStructure) element).setPropertyValue(propertyName,propertyValue);
					repository.put(element);	// put the element back in the repository to ensure update
				default:	// interfaces do not support settable properties
					throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Element Type "+element.getElementType()+" does not support settable properties");
				}
			}
			if(hasListener(CimEventType.UPDATED, null)){
				generateEvent(CimEventType.UPDATED,path.toString()+"#"+propertyName);
			}
			return;
		}
		Provider child = locateProvider(path);
		if(child != null) {
			child.setPropertyValue(path, propertyName, propertyValue);
			return;
		}
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider: Element "+path.toString()+" not found");
	}
	
	/* (non-Javadoc)
	 * @see net.aifusion.providers.Provider#getMethodNames(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public List<String> getMethodNames(ObjectPath path) {
		List<String> names = new Vector<String>();
		if(repository.contains(path)){
			NamedElement element = repository.get(path);
			switch(element.getElementType()){
			case INSTANCE:
				names.addAll(((CimInstance) element).getMethodNames());
				return names;
			case CLASS:
				names.addAll(((CimClass) element).getMethodNames());
				return names;
			case INTERFACE:
				if(element instanceof CimClass) names.addAll(((CimClass) element).getMethodNames());
				return names;
			default:
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Element Type "+element.getElementType()+" does not support methods");
			}
		}
		Provider child = locateProvider(path);
		if(child != null) {
			names.addAll(child.getMethodNames(path));
			return names;
		}
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider could not locate "+path.toString());
	}

	/* (non-Javadoc)
	 * @see net.aifusion.providers.Provider#getMethodReturnType(net.aifusion.metamodel.ObjectPath, java.lang.String)
	 */
	@Override
	public DataType getMethodReturnType(ObjectPath path, String methodName) {
		if(repository.contains(path)){
			NamedElement element = repository.get(path);
			switch(element.getElementType()){
			case INSTANCE:
				return ((CimInstance) element).getMethodReturnType(methodName);
			case CLASS:
				return ((CimClass) element).getMethodReturnType(methodName);
			case INTERFACE:
				return (element instanceof CimClass) ? ((CimClass) element).getMethodReturnType(methodName) : null;
			default:
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Element Type "+element.getElementType()+" does not support methods");
			}
		}
		Provider child = locateProvider(path);
		if(child != null) return child.getMethodReturnType(path, methodName);
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider: Element "+path.toString()+" not found");
	}

	/* (non-Javadoc)
	 * @see net.aifusion.providers.Provider#getMethodParameters(net.aifusion.metamodel.ObjectPath, java.lang.String)
	 */
	@Override
	public List<CimParameter> getMethodParameters(ObjectPath path, String methodName) {
		if(repository.contains(path)){
			NamedElement element = repository.get(path);
			switch(element.getElementType()){
			case INSTANCE:
				return ((CimInstance) element).getMethodParameters(methodName);
			case CLASS:
				return ((CimClass) element).getMethodParameters(methodName);
			case INTERFACE:
				return (element instanceof CimClass) ? ((CimClass) element).getMethodParameters(methodName) : null;
			default:
				throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Element Type "+element.getElementType()+" does not support methods");
			}
		}
		Provider child = locateProvider(path);
		if(child != null) return child.getMethodParameters(path, methodName);
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider: Element "+path.toString()+" not found");
	}
	
	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Provider#invokeMethod(net.aifusion.metamodel.ObjectPath, java.lang.String, java.util.List)
	 */
	@Override
	public DataValue invokeMethod(ObjectPath path, String methodName, List<CimParameter> methodParameters) {
		if(repository.contains(path)){
			DataValue returnValue = null;
			synchronized(repository){
				NamedElement element = repository.get(path);
				// TODO: Note that if the method call results in changes in property values, we
				// will need to push the element back into the store (do a put()) to ensure persistence
				// in case the repository is persistent
				switch(element.getElementType()){
				case INSTANCE:
					returnValue = ((CimInstance) element).invokeMethod(methodName,methodParameters);
					break;
				case CLASS:
					returnValue = ((CimClass) element).invokeMethod(methodName,methodParameters);
					break;
				default:
					throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Element Type "+element.getElementType()+" does not support method invocations");
				}
			}
			if(hasListener(CimEventType.INVOKED, null)){
				generateEvent(CimEventType.INVOKED,path.toString()+"#"+methodName);
			}
			return returnValue;
		}
		Provider child = locateProvider(path);
		if(child != null) return child.invokeMethod(path, methodName,methodParameters);
		throw new ModelException(ExceptionReason.NOT_FOUND,"Provider: Element "+path.toString()+" not found");
	}
	
	/* (non-Javadoc)
	 * @see net.aifusion.providers.Provider#getRepository()
	 */
	@Override
	public Repository getRepository() {
		return repository;
	}
	
	/*
	 * ***********************
	 * Repository interface methods
	 * ***********************
	 */
	
	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#put(net.aifusion.metamodel.NamedElement)
	 */
	@Override
	public boolean put(NamedElement element) {
		boolean status = false;
		// check if the local repository contains the namespacePath, if so use it
		NameSpacePath elementPath = element.getNameSpacePath();
		List<NameSpacePath> localPaths = repository.getNameSpaces();
		if(localPaths.contains(elementPath)){
			status = repository.put(element);
			if(status && hasListener(CimEventType.ADDED, null)){
				generateEvent(CimEventType.ADDED, element.toMOF());
			}
			return status;
		}
		Provider child = locateProvider(element.getObjectPath());
		if(child != null) return child.put(element);
		status = repository.put(element);
		if(status && hasListener(CimEventType.ADDED, null)){
			generateEvent(CimEventType.ADDED,element.toMOF());
		}
		return status;		
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#get(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public NamedElement get(ObjectPath path) {
		if(repository.contains(path)) return repository.get(path);
		Provider child = locateProvider(path);
		return child == null ? null : child.get(path);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#hasElement(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public boolean contains(ObjectPath path) {
		if(repository.contains(path)) return true;
		Provider child = locateProvider(path);
		return child == null ? false : child.contains(path);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#delete(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public boolean delete(ObjectPath path) {
		boolean status = false;
		if(repository.contains(path)){
			status = repository.delete(path);
			if(status && hasListener(CimEventType.REMOVED, null)){
				generateEvent(CimEventType.REMOVED, path.toString());
			}
			return status;
		}
		Provider child = locateProvider(path);
		return child == null ? false : child.delete(path);
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#shutdown()
	 */
	@Override
	public synchronized void shutdown() {
		// note that we DO NOT shutdown children recursively at the moment, since a given
		// child may be linked to multiple parents. This is subject to change
		if(!children.isEmpty()){
			children.clear();
			listeners.clear();
		}
		repository.shutdown();
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#getNameSpaces()
	 */
	@Override
	public List<NameSpacePath> getNameSpaces() {
		List<NameSpacePath> paths = repository.getNameSpaces();
		paths.addAll(children.keySet());
		return paths;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#executeQuery(net.aifusion.metamodel.CimQuery)
	 */
	@Override
	public List<NamedElement> getElements(String elementTypes, String localNameSpaces, String elementNames, boolean locateSubTypes) {
		List<NamedElement> elements = repository.getElements(elementTypes, localNameSpaces, elementNames, locateSubTypes);
		HashSet<Provider> seen = new HashSet<Provider>();
		for(Provider child : children.values()){
			if(seen.contains(child)) continue;
			elements.addAll(child.getElements(elementTypes, localNameSpaces, elementNames, locateSubTypes));
			seen.add(child);
		}
		return elements;
	}
	
	@Override
	public List<StructureValue> filter(ObjectPath path, CimFilter filter) {
		return repository.filter(path,filter);
	}

	/* *******************************
	 * Local Housekeeping and methods
	 * *******************************
	 */

	/**
	 * Locate a child provider known to this provider that has an element in it
	 * @param path - ObjectPath of the element to search
	 * @return - Provider containing the element corresponding to the objectPath, null if none found
	 */
	private Provider locateProvider(ObjectPath path){
		// first check if a provider with the same namespace is known to us
		if(children.containsKey(path.getNameSpacePath())) return children.get(path.getNameSpacePath());
		// if not, locate a child that has the same localPath as the given ObjectPath, and knows about this object path
		// Currently note that in this case an arbitrary child is picked--
		// there is no guarantee that others do not know about the path
		String localPath = path.getLocalPath();
		for(NameSpacePath np : children.keySet()){
			if(localPath.equals(np.getLocalPath())){
				Provider candidate = children.get(np);
				if(candidate.contains(path)) return candidate;
			}
		}
		return null;
	}
	
	/*
	 * *************************************
	 * CimEventGenerator methods
	 * Note that currently we do NOT delegate listeners to child providers
	 * *************************************
	 */
	
	@Override
	public boolean addListener(CimEventType type, CimListener listener) {
		if(listeners.containsKey(type)){
			List<CimListener> list = listeners.get(type);
			if(!list.contains(listener)) list.add(listener);
		} else {
			Vector<CimListener> list = new Vector<CimListener>();
			list.add(listener);
			listeners.put(type, list);
		}
		return true;
	}

	@Override
	public void removeListener(CimEventType type, CimListener listener) {
		if(listeners.containsKey(type)){
			List<CimListener> list = listeners.get(type);
			if(list.contains(listener)) list.remove(listener);
			if(list.isEmpty()) listeners.remove(type);
		}
		return;
	}
	
	@Override
	public boolean hasListener(CimEventType type, CimListener listener){
		if(!listeners.containsKey(type)) return false;
		if(listener == null) return true;
		List<CimListener> list = listeners.get(type);
		return list.contains(listener);
	}
	
	@Override
	public URL getURL() {
		return null;
	}
	
	/*
	 * *************************************
	 * Helper methods
	 * *************************************
	 */
	
	/**
	 * Raise an event to all registered listeners. This method should be used by subclasses to send events
	 * @param event - event to be sent to all interested listeners
	 */
	protected void generateEvent(CimEventType type,String description){
		if(listeners.containsKey(type)){
			CimIndication event = new CimIndication(type,this,description);
			List<CimListener> list = listeners.get(type);
			for(CimListener l : list){
				l.notify(event);
			}
		}
		return;
	}
}
