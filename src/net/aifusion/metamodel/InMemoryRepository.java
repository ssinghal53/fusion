/**
 * Copyright 2014, 2018 Sharad Singhal, All Rights Reserved
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
 * Last Modified Jan 2, 2018 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.UUID;
import java.util.Vector;

/**
 * Simple in-memory cache for model objects. Supports a single namespace and no persistence.
 * @author Sharad Singhal
 *
 */
public class InMemoryRepository implements Repository {
	/** namespace path for this repository */
	private NameSpacePath repositoryPath;
	/** local path for this repository */
	private String localPath;
	/** User-defined qualifier types held in this repository. Keyed by lower case qualifier name */
	private LinkedHashMap<String,QualifierType> qualifierTypes = new LinkedHashMap<String,QualifierType>();
	/** Global enumerations held in this repository. Keyed by lower case enumeration name */
	private LinkedHashMap<String,CimEnumeration> enumerations = new LinkedHashMap<String,CimEnumeration>();
	/** Global interfaces held in this repository. Keyed by lower case interface name */
	private LinkedHashMap<String,CimInterface> interfaces = new LinkedHashMap<String,CimInterface>();
	/** Global Structures held in this repository. Keyed by lower case structure name. Values are keyed by UUID */
	private LinkedHashMap<String,StructureAndValues> structures = new LinkedHashMap<String,StructureAndValues>();
	/** Classes and instances held in this repository. Keyed by lower case class name. Instances are keyed by objectPath.UUID() */
	private LinkedHashMap<String,ClassAndInstances> classAndInstances = new LinkedHashMap<String,ClassAndInstances>();

	/** All standard qualifier types. Keyed by lower case qualifier type name */
	private HashMap<String,QualifierType> standardQualifierTypes = new HashMap<String,QualifierType>();
	
	/**
	 * Container to hold a singleton CIM class and all instances of that CIM class. Instances are keyed by UUID
	 * @author Sharad Singhal
	 */
	private class ClassAndInstances {
		CimClass cimClass = null;
		LinkedHashMap<UUID,CimInstance> instances = null;
	}
	
	/**
	 * Container to hold a singleton CIM Structure, and all instances of that CIM Structure, keyed by UUID
	 * @author Sharad Singhal
	 *
	 */
	private class StructureAndValues {
		CimStructure struct = null;
		LinkedHashMap<UUID,StructureValue> values = null;
	}
	
	/**
	 * Create an in-memory CIM repository using the default name space. This repository supports only the default namespace,
	 * and no persistence. Standard qualifier types are built-in, and do not have to be inserted in the repository
	 * @see Constants#defaultNameSpacePath
	 * @see StandardQualifierType
	 */
	public InMemoryRepository(){
		this(Constants.defaultNameSpacePath);
		return;
	}

	/**
	 * Create an in-memory CIM repository using the given name space. No persistence is supported. Standard qualifier types
	 * are built-in, and do not have to be inserted in the repository
	 * @param path - name space to be used in the repository. Must not be null.
	 * @see StandardQualifierType
	 */
	public InMemoryRepository(NameSpacePath path) {
		if(path == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"namespace must not be null");
		repositoryPath = path;
		localPath = repositoryPath.getLocalPath();
		// initialize all standard qualifier types for this name space
		for(StandardQualifierType q : StandardQualifierType.values()){
			standardQualifierTypes.put(q.name().toLowerCase(),q.getQualifierType(path));
		}
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#put(net.aifusion.metamodel.NamedElement)
	 */
	@Override
	public boolean put(NamedElement element) {
		// validate that element is not null, and the namespace path matches
		if(element == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Null elements cannot be inserted in repository");
		if(!localPath.equals(element.getObjectPath().getLocalPath())){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,element.getName()+": expected LocalPath "+repositoryPath.getLocalPath()+
					" found "+element.getObjectPath().getLocalPath());
		}
		String elementName = element.getLowerCaseName();
		switch(element.getElementType()){
		case QUALIFIERTYPE:
			synchronized(qualifierTypes){
				if(qualifierTypes.containsKey(elementName)){
					// if repository already contains this qualifier type, validate that the inserted element is the same
					QualifierType old = qualifierTypes.get(elementName);
					return old.equals(element);
				} else if(standardQualifierTypes.containsKey(elementName)){
					// if we want to insert a standard qualifier type, validate that the inserted element is the same
					return standardQualifierTypes.get(elementName).equals(element);
				} else {
					// have a new qualifier type. Insert it
					qualifierTypes.put(elementName, (QualifierType) element);
				}
			}
			break;
		case ENUMERATION:
			synchronized(enumerations){
				if(enumerations.containsKey(elementName)){
					CimEnumeration oldEnum = enumerations.get(elementName);
					return oldEnum.equals(element);
				} else {
					enumerations.put(elementName, (CimEnumeration) element);
				}
			}
			break;
		case STRUCTURE:
			synchronized(structures){
				if(structures.containsKey(elementName)){
					StructureAndValues oldStructAndValues = structures.get(elementName);
					return oldStructAndValues.struct.equals(element);
				} else {
					StructureAndValues pair = new StructureAndValues();
					pair.struct = (CimStructure)element;
					structures.put(elementName, pair);
				}
			}
			break;
		case STRUCTUREVALUE:
			synchronized(structures){
				StructureAndValues pair = structures.get(elementName);
				if(pair == null){
					pair = new StructureAndValues();
					pair.struct = ((StructureValue)element).getCreationStruct();
					structures.put(elementName, pair);
				}
				if(pair.values == null) pair.values = new LinkedHashMap<UUID,StructureValue>();
				pair.values.put(element.getObjectPath().getUUID(),(StructureValue)element);
			}
			break;
		case INTERFACE:
			synchronized(interfaces){
				if(interfaces.containsKey(elementName)){
					CimInterface oldInterface = interfaces.get(elementName);
					return oldInterface.equals(element);
				} else {
					interfaces.put(elementName, (CimInterface) element);
				}
			}
			break;
		case CLASS:
			synchronized(classAndInstances){
				if(classAndInstances.containsKey(elementName)){
					ClassAndInstances oldClassAndInstances = classAndInstances.get(elementName);
					return oldClassAndInstances.cimClass.equals(element);
				} else {
					ClassAndInstances pair = new ClassAndInstances();
					pair.cimClass = (CimClass) element;
					classAndInstances.put(elementName, pair);
				}
			}
			break;
		case INSTANCE:
			synchronized(classAndInstances){
				ClassAndInstances pair = classAndInstances.get(elementName);
				if(pair == null){
					// insertion of a new instance without an existing class definition also inserts the 
					// creation class in the repository
					pair = new ClassAndInstances();
					pair.cimClass = ((CimInstance)element).getCreationClass();
					classAndInstances.put(elementName, pair);
				}
				if(pair.instances == null) pair.instances = new LinkedHashMap<UUID,CimInstance>();
				pair.instances.put(element.getObjectPath().getUUID(), (CimInstance) element);
			}
			break;
		default:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,element.getName()+": ElementType "+element.getElementType()+" cannot be added to respository");
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#get(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public NamedElement get(ObjectPath path) {
		// validate that element is not null, and the namespace path matches
		if(path == null || !localPath.equals(path.getLocalPath())){
			return null;
		}
		String elementName = path.getLowerCaseName();
		switch(path.getElementType()){
		case QUALIFIERTYPE:
			 return standardQualifierTypes.containsKey(elementName) ? standardQualifierTypes.get(elementName) :
				 qualifierTypes.get(elementName);
		case ENUMERATION:
			return enumerations.get(elementName);
		case STRUCTURE:
			StructureAndValues sv = structures.get(elementName);
			return sv != null ? sv.struct : null;
		case STRUCTUREVALUE:
			sv = structures.get(elementName);
			if(sv == null || sv.values == null) return null;
			return sv.values.get(path.getUUID());
		case INTERFACE:
			return interfaces.get(elementName);
		case CLASS:
			ClassAndInstances ci = classAndInstances.get(elementName);
			return ci != null ? ci.cimClass : null;	
		case INSTANCE:
			ClassAndInstances pair = classAndInstances.get(elementName);
			if(pair == null || pair.instances == null) return null;
			return pair.instances.get(path.getUUID());
		default:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,path.getElementType() + "is not yet implemented");
		}
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#hasElement(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public boolean contains(ObjectPath path) {
		if(path == null || !localPath.equals(path.getLocalPath())){
			return false;
		}
		String elementName = path.getLowerCaseName();
		switch(path.getElementType()){
		case QUALIFIERTYPE:
			return standardQualifierTypes.containsKey(elementName) || qualifierTypes.containsKey(elementName);
		case ENUMERATION:
			return enumerations.containsKey(elementName);
		case STRUCTURE:
			return structures.containsKey(elementName);
		case STRUCTUREVALUE:
			StructureAndValues sv = structures.get(elementName);
			return (sv == null || sv.values == null) ? false : sv.values.containsKey(path.getUUID());
		case INTERFACE:
			return interfaces.containsKey(elementName);
		case CLASS:
			ClassAndInstances pair = classAndInstances.get(elementName);
			return (pair == null || pair.cimClass == null) ? false : true;
		case INSTANCE:
			pair = classAndInstances.get(elementName);
			return (pair == null || pair.instances == null) ? false : pair.instances.containsKey(path.getUUID());
		default:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,path.getElementType() + " is not yet implemented");
		}
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#delete(net.aifusion.metamodel.ObjectPath)
	 */
	@Override
	public boolean delete(ObjectPath path) {
		if(path == null || !localPath.equals(path.getLocalPath())){
			return false;
		}
		NamedElement element = null;
		String elementName = path.getLowerCaseName();
		switch(path.getElementType()){
		case QUALIFIERTYPE:
			// note that standard qualifier types cannot be deleted
			synchronized(qualifierTypes){
				element = qualifierTypes.remove(elementName);
			}
			break;
		case ENUMERATION:
			synchronized(enumerations){
				element = enumerations.remove(elementName);
			}
			break;
		case STRUCTURE:
			synchronized(structures){
				StructureAndValues sv = structures.get(elementName);
				if(sv != null && (sv.values == null || sv.values.isEmpty())){
					element = sv.struct;
					structures.remove(elementName);
				}
			}
			break;
		case STRUCTUREVALUE:
			synchronized(structures){
				StructureAndValues sv = structures.get(elementName);
				if(sv != null && sv.values != null){
					element = sv.values.remove(path.getUUID());
				}
			}
			break;
		case INTERFACE:
			synchronized(interfaces){
				element = interfaces.remove(elementName);
			}
			break;
		case CLASS:
			synchronized(classAndInstances){
				ClassAndInstances pair = classAndInstances.get(elementName);
				// A class can only be deleted if no instances exist
				// TODO: What happens to sub-types?
				if(pair != null && (pair.instances == null || pair.instances.isEmpty())){
					element = pair.cimClass;
					classAndInstances.remove(elementName);
				}
				break;
			}
		case INSTANCE:
			synchronized(classAndInstances){
				ClassAndInstances pair = classAndInstances.get(elementName);
				if(pair != null && pair.instances != null){
					element = pair.instances.remove(path.getUUID());
				}
				break;
			}
		default:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,path.getElementType() + "is not yet implemented");
		}
		return element != null;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#shutdown()
	 */
	@Override
	public synchronized void shutdown() {
		qualifierTypes.clear();
		enumerations.clear();
		interfaces.clear();
		structures.clear();
		classAndInstances.clear();
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#getNameSpaces()
	 */
	@Override
	public List<NameSpacePath> getNameSpaces() {
		ArrayList<NameSpacePath> nameSpaces = new ArrayList<NameSpacePath>(1);
		nameSpaces.add(repositoryPath);
		return nameSpaces;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.Repository#executeQuery(net.aifusion.metamodel.CimQuery)
	 */
	@Override
	public List<NamedElement> getElements(String elementTypes, String localNameSpaces, String elementNames, boolean locateSubTypes) {
		
		// check that if given, localNameSpaces requested include the repository path for this repository
		if(localNameSpaces != null){
			String [] names = localNameSpaces.split(",");
			boolean foundNameSpace = false;
			for(String ns : names){
				if(!localPath.equalsIgnoreCase(ns.trim())) continue;
				foundNameSpace = true;
				break;
			}
			if(!foundNameSpace) return new Vector<NamedElement>();
		}

		// construct the list of element types to retrieve
		LinkedHashSet<ElementType> requestedTypes = new LinkedHashSet<ElementType>();
		if(elementTypes != null){
			String [] inputTypes = elementTypes.split(",");
			for(String s : inputTypes){
				if((s = s.trim()).isEmpty()) continue;
				try {
					ElementType element = ElementType.valueOf(s.toUpperCase());
					if(!element.isNamedElement()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected NamedElement, found "+s);
					requestedTypes.add(element);
				} catch(Exception e){
					if(e instanceof ModelException) throw e;
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected NamedElement, found "+s,e);
				}
			}
			if(requestedTypes.isEmpty()) return new Vector<NamedElement>();
		} else {
			for(ElementType e : new ElementType[]{ElementType.QUALIFIERTYPE, ElementType.ENUMERATION,ElementType.INTERFACE,
					ElementType.STRUCTURE,ElementType.CLASS,ElementType.STRUCTUREVALUE,ElementType.INSTANCE}){
				requestedTypes.add(e);
			}
		}

		// locate the element names requested, if given
		String [] names = (elementNames == null) ? null : elementNames.split(",");
		if(names != null){
			for(int i = 0; i < names.length; i++){
				names[i] = names[i].trim().toLowerCase();
				if(names[i].isEmpty()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Empty or blank name found in "+elementNames);
			}
		}
		// filtered = true if element names are given
		boolean filtered = (names == null || names.length == 0) ? false : true;
		
		// collect the elements requested from the repository
		Vector<NamedElement> elements = new Vector<NamedElement>();
		for(ElementType elementType : requestedTypes){
			switch(elementType){
			case STRUCTUREVALUE:
				if(!structures.isEmpty()){
					if(filtered){
						for(String key : names){	// for all requested structures, do
							if(locateSubTypes){
								for(String structName : structures.keySet()){	// for all known structures, do
									CimStructure s = (CimStructure) get(new ObjectPath(ElementType.STRUCTURE,structName,repositoryPath,null, null));
									if(s != null && s.isSubTypeOf(key)){	// if this structure is subtype of requested structure, add the instances
										StructureAndValues sv = structures.get(structName);
										if(sv.values != null && !sv.values.isEmpty()) elements.addAll(sv.values.values());
									}
								}
							} else if(structures.containsKey(key)){
								StructureAndValues sv = structures.get(key);
								if(sv.values != null && !sv.values.isEmpty()) elements.addAll(sv.values.values());
							}
						}
					} else {
						for(StructureAndValues sv : structures.values()){
							if(sv.values != null && !sv.values.isEmpty())
								elements.addAll(sv.values.values());
						}
					}
				}
				break;
				// TODO: Do we also search for instances when asking for locateSubTypes of a structure
			case INSTANCE:
				if(!classAndInstances.isEmpty()){
					if(filtered){
						for(String key : names){	// for all requested classNames, do
							if(locateSubTypes){
								for(String className : classAndInstances.keySet()){	// for all known classes, do
									// note that we need to do a get here, since the class definition may not exist in the classAndInstances
									CimClass c = (CimClass) get(new ObjectPath(ElementType.CLASS,className,repositoryPath,null, null));	// get the class
									if(c != null && c.isSubTypeOf(key)){	// if this class is subtype of requested class, add the instances
										ClassAndInstances ci = classAndInstances.get(className);
										if(ci.instances != null && !ci.instances.isEmpty()) elements.addAll(ci.instances.values());
									}
								}
							} else if(classAndInstances.containsKey(key)){
								ClassAndInstances ci = classAndInstances.get(key);
								if(ci.instances != null && !ci.instances.isEmpty()) elements.addAll(ci.instances.values());
							}
						}
					} else {
						for(ClassAndInstances ci : classAndInstances.values()){
							if(ci.instances != null && !ci.instances.isEmpty())
								elements.addAll(ci.instances.values());
						}
					}
				}
				break;
			case STRUCTURE:
				if(!structures.isEmpty()){
					if(filtered){
						for(String key : names){
							if(locateSubTypes){
								for(String structName : structures.keySet()){	// for all known structures, do
									StructureAndValues sv = structures.get(structName);
									CimStructure s = sv.struct;
									if(s.isSubTypeOf(key)){	// if this structure is subtype of requested structure, add it
										elements.addElement(s);
									}
								}
								// note that classes can inherit from structures as well
								for(String className : classAndInstances.keySet()){	// for all known classes, do
									// note that we need to do a get here, since the class definition may not exist in the classAndInstances
									CimClass c = (CimClass) get(new ObjectPath(ElementType.CLASS,className,repositoryPath,null, null));	// get the class
									if(c != null && c.isSubTypeOf(key)){	// if this class is subtype of requested class, add the instances
										elements.add(c);
									}
								}
							} else if(structures.containsKey(key)){
								StructureAndValues sv = structures.get(key);
								elements.add(sv.struct);
							}
						}
					} else {
						for(StructureAndValues sv : structures.values()){
							elements.add(sv.struct);
						}
					}
				}
				break;
			case QUALIFIERTYPE:
				// NOTE that standardQualifier types are implicitly declared, and will not be retrieved here.
				// QualifierTypes do not have subtypes, so locateSubTypes is ignored
				// this allows this repository to act as a BufferedCache
				if(!qualifierTypes.isEmpty()){
					if(filtered){
						for(String key : names)
							if(qualifierTypes.containsKey(key)) elements.add(qualifierTypes.get(key));
					} else {
						elements.addAll(qualifierTypes.values());
					}
				}
				break;
			case ENUMERATION:
				// Enumerations do not have subtypes, so locateSubTypes is ignored
				if(!enumerations.isEmpty()){
					if(filtered){
						for(String key : names)
							if(enumerations.containsKey(key)) elements.add(enumerations.get(key));
					} else {
						elements.addAll(enumerations.values());
					}
				}
				break;
			case INTERFACE:
				if(!interfaces.isEmpty()){
					if(filtered){
						for(String key : names){
							if(locateSubTypes){
								for(String intfName : interfaces.keySet()){	// for all known interfaces, do
									CimInterface intf = interfaces.get(intfName);
									if(intf.isSubTypeOf(key)){	// if this interface is subtype of requested structure, add it
										elements.addElement(intf);
									}
								}
							} else if(interfaces.containsKey(key)){
								elements.add(interfaces.get(key));
							}
						}
					} else {
						elements.addAll(interfaces.values());
					}
				}
				break;
			case CLASS:
			default:
				if(!classAndInstances.isEmpty()){
					if(filtered){
						for(String key : names){
							if(locateSubTypes){
								for(String className : classAndInstances.keySet()){	// for all known classes, do
									// note that we need to do a get here, since the class definition may not exist in the classAndInstances
									// this is required for BufferedCache
									CimClass c = (CimClass) get(new ObjectPath(elementType,className,repositoryPath,null, null));	// get the class
									if(c != null && c.isSubTypeOf(key)){	// if this class is subtype of requested class, add it
										elements.add(c);
									}
								}
							} else if(classAndInstances.containsKey(key)){
								// TODO: This needs checking-- this means that we will NOT retrieve classes from the backing store
								// in bufferedCache-- is this correct?
								ClassAndInstances ci = classAndInstances.get(key);
								if(ci.cimClass != null) elements.add(ci.cimClass);
							}
						}
					} else {
						for(ClassAndInstances ci : classAndInstances.values()){
							if(ci.cimClass != null) elements.add(ci.cimClass);
						}	
					}
				}
				break;
			}
		}
		return elements;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		shutdown();
	}

	@Override
	public List<StructureValue> filter(CimFilter filter) {
		String className = filter.getStructurePath().getName();
		String nameSpace = filter.getStructurePath().getLocalPath();
		List<NamedElement> elements = getElements("StructureValue",nameSpace,className,true);
		Vector<StructureValue> values = new Vector<StructureValue>();
		for(NamedElement e : elements) {
			if(filter.satisfies((StructureValue) e, this)) values.add((StructureValue) e);
		}
		return values;
	}
}
