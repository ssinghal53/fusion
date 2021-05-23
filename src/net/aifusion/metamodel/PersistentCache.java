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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

/**
 * Class to implement a persistent repository that supports multiple name spaces
 * @author Sharad Singhal
 */
public class PersistentCache extends InMemoryCache {
	/** location for the repository */
	private String rootDirectory = Constants.defaultRepositoryLocation;
	/** sub directory to hold qualifier types */
	private static final String qualifierDirectory = "/qualifiertype";
	/** sub directory to hold class definitions */
	private static final String classDirectory = "/class";
	/** sub directory to hold interface definitions */
	private static final String interfaceDirectory = "/interface";
	/** sub directory to hold instances */
	private static final String instanceDirectory = "/instance";
	/** sub directory to hold structure definitions */
	private static final String structureDirectory = "/structure";
	/** sub directory to hold structure values */
	private static final String structureValueDirectory = "/structurevalue";
	/** sub directory to hold enumeration definitions */
	private static final String enumerationDirectory = "/enumeration";
	/** file suffix for all mof files */
	private static final String mofFileSuffix = ".mof";
	/** file suffix for list of superTypes for a type */
	private static final String superTypeFileSuffix = ".sup";
	

	/**
	 * Create a persistent repository using the file system
	 * @param repositoryLocation - file system root of the repository i.e., /user/repository
	 */
	public PersistentCache(String repositoryLocation) {
		super();
		if(repositoryLocation == null || repositoryLocation.isEmpty()) 
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"repository location cannot be null or empty");
		// TODO: Need to check root directory path for variants, but allow repositories with ./
		// this.rootDirectory = ModelUtilities.normalizePath(repositoryLocation);
		this.rootDirectory = repositoryLocation;
		checkDirectory(rootDirectory);
		return;
	}
	
	@Override
	public synchronized List<NameSpacePath> getNameSpaces() {
		HashSet<String> paths = new HashSet<String>();
		for(String directory : new String[]{qualifierDirectory,enumerationDirectory, structureDirectory, interfaceDirectory,
				classDirectory,structureValueDirectory,instanceDirectory}){
			String file = rootDirectory+directory;
			locateSubDirectories(file.length(),file,paths);
		}
		Vector<NameSpacePath> p = new Vector<NameSpacePath>();
		for(String s : paths){
			p.add(new NameSpacePath(s));
		}
		return p;	
	}
	
	@Override
	public synchronized boolean put(NamedElement element) {
		boolean success = super.put(element);					// insert the element to the in-memory cache
		return success ? writeElementToFile(element) : false;	// if successful, write it to the file
	}

	@Override
	public synchronized NamedElement get(ObjectPath path) {
		// retrieve an object from the in-memory cache
		NamedElement element = super.get(path);
		if(element != null) return element;
		// if not found, retrieve it from the file, and cache it in memory
		element = readElementFromFile(path);
		if(element != null) super.put(element);
		return element;
	}

	@Override
	public boolean contains(ObjectPath path) {
		if(super.contains(path)) return true;
		File file = new File(getFilePath(path));
		return file.exists();
	}

	@Override
	public synchronized boolean delete(ObjectPath path) {
		File file = new File(getFilePath(path));
		boolean success = false;
		if(file.exists()){	// delete the file from the persistent store
			success = file.delete();
		}
		// if successful, also delete from memory (the element may or may not be present in memory)
		if(success) super.delete(path);
		return success;
	}

	/**
	 * Get elements from this repository. Note that depending on the size of the repository, this method can be very expensive in time and/or memory.
	 * This method directly accesses the persistent store, and does not cache the results in the in-memory cache
	 * @param elementTypes - Optional comma separated list of element types to retrieve. Must be a NamedElement (Instance, Class, QualifierType, 
	 * Structure, structurevalue, Interface or enumeration). Null will select all element types.
	 * @param localNameSpaces - Optional comma separated list of local namespaces to search. A null will retrieve elements from all namespaces
	 * @param elementNames - Optional comma-separated list of element names to retrieve. A null will retrieve all elements of the given type
	 * @param locateSubTypes - if true, also retrieve subtypes of the requested element types
	 * @return list of elements that match the given criteria. An empty list is returned if no matching elements are found
	 */
	@Override
	public List<NamedElement> getElements(String elementTypes, String localNameSpaces, String elementNames, boolean locateSubTypes) {
		// System.out.println("Entering getElements");
		// create a cache with this cache as the backing store, and a MOF parser
		InMemoryCache cache = new InMemoryCache();
		MOFParser parser = new MOFParser(cache,this);
		
		// find the set of requested classNames, empty if none
		HashSet<String> classNames = new HashSet<String>();
		if(elementNames != null){
			for(String n : elementNames.split(",")){
				if(n.trim().isEmpty()) continue;
				classNames.add(n.trim());
				// System.out.println("\tClassName :"+n);
			}
		}
		
		// find the local paths to search, empty if none given
		Vector<String> localPaths = new Vector<String>();
		if(localNameSpaces != null){
			for(String localPath : localNameSpaces.split(",")){
				String path = ModelUtilities.normalizePath(localPath);
				if(path != null) localPaths.add(localPath);
				// System.out.println("\tLocalPath :"+path);
			}
		}

		// construct the list of element types to retrieve, include all types, if none given
		HashSet<ElementType> requestedTypes = new HashSet<ElementType>();
		if(elementTypes != null){
			String [] inputTypes = elementTypes.split(",");
			for(String s : inputTypes){
				s = s.trim();
				if(s.isEmpty()) continue;
				try {
					ElementType element = ElementType.valueOf(s.toUpperCase());
					if(!element.isNamedElement()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected NamedElement, found "+s);
					requestedTypes.add(element);
				} catch(Exception e){
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected NamedElement, found "+s,e);
				}
			}
			// note that an empty list (rather than null) will return no elements
			if(requestedTypes.isEmpty()) return new Vector<NamedElement>();
		} else {
			for(ElementType e : new ElementType[]{ElementType.QUALIFIERTYPE, ElementType.ENUMERATION,ElementType.STRUCTURE,
					ElementType.INTERFACE,ElementType.CLASS,ElementType.STRUCTUREVALUE,
					ElementType.INSTANCE}){
				requestedTypes.add(e);
			}
		}
		
		if(locateSubTypes && !classNames.isEmpty() && (requestedTypes.contains(ElementType.STRUCTURE) || 
				requestedTypes.contains(ElementType.STRUCTUREVALUE) ||
				requestedTypes.contains(ElementType.INTERFACE) ||
				requestedTypes.contains(ElementType.CLASS) || 
				requestedTypes.contains(ElementType.INSTANCE))){
			addSubTypes(classNames);
		}
		
		// locate elements, and load them into the buffer
		for(ElementType elementType : requestedTypes){
			// locate the search root based on the element type
			String root;
			switch(elementType){
			case QUALIFIERTYPE:
				root = rootDirectory + qualifierDirectory;
				break;
			case ENUMERATION:
				root = rootDirectory + enumerationDirectory;
				break;
			case STRUCTURE:
				root = rootDirectory + structureDirectory;
				break;
			case INSTANCE:
				root = rootDirectory + instanceDirectory;
				break;
			case STRUCTUREVALUE:
				root = rootDirectory + structureValueDirectory;
				break;
			case INTERFACE:
				root = rootDirectory + interfaceDirectory;
				break;
			case CLASS:
			default:
				root = rootDirectory + classDirectory;
				break;
			}
			// System.out.println("\tSearch "+root);
			if(localPaths.isEmpty()){
				// note that localPaths are empty here, and will be used as pathComponents in loadFile
				loadFile(root,localPaths,classNames,parser);
			} else {
				// Create a vector to hold name spaces, if any, and load the corresponding elements into the buffer
				Vector<String> pathComponents = new Vector<String>();
				for(String localPath : localPaths){
					pathComponents = ModelUtilities.getPathElements(localPath);
					loadFile(root+localPath,pathComponents,classNames,parser);
				}
			}
		}
		// return the desired elements from the buffer
		return cache.getElements(elementTypes, localNameSpaces, elementNames, locateSubTypes);
	}
	
	/* ********************************************
	 * Internal supporting methods
	 * ********************************************
	 */
	
	/**
	 * Locate all paths in the repository starting at file
	 * @param startIndex - top level directory path length
	 * @param file - current file or directory to check
	 * @param paths - paths saved so far
	 */
	private void locateSubDirectories(int startIndex, String file, HashSet<String> paths) {
		File f = new File(file);
		if(!f.exists()) return;
		if(f.isDirectory() && !file.contains("_")){
			// recurse...
			String [] names = f.list();
			if(names != null && names.length > 0){
				for(String n : names){
					String fName = file+"/"+n;
					locateSubDirectories(startIndex,fName,paths);
				}
			}
			return;
		}
		String path = file.substring(startIndex, file.lastIndexOf('/'));
		if(path.contains("_")) return;
		paths.add(path);
		return;
	}

	/**
	 * Load MOF files from a file or directory.
	 * @param fileOrDirectory - file or directory name. In case of directories, the hierarchy is traversed recursively to locate the mof files
	 * @param pathElements - local path elements at the file or directory
	 * @param classNames - optional class names - if given, only matching classes are loaded
	 * @param parser - parser to use for loading files
	 */
	private static void loadFile(String fileOrDirectory,Vector<String>pathElements, Set<String> classNames, Parser parser){
		File f = new File(fileOrDirectory);
		if(!f.exists()) return;
		if(f.isDirectory()){
			String [] names = f.list();
			if(names != null && names.length > 0){
				for(String n : names){
					// String fName = fileOrDirectory+File.separator+n;
					String fName = fileOrDirectory+"/"+n;
					boolean hasUnderscore = fName.contains("_");
					// check if this name is a directory that does not have underscore-- if so
					// append the name to the path. If the directory is a class directory (has underscore)
					// then we will not pass classNames down below
					File fFile = new File(fName);
					boolean appendPath = fFile.isDirectory() && !hasUnderscore;	// append path value for non-class directories
					boolean nullClassNames = fFile.isDirectory() && hasUnderscore; // null classNames for class directories	
					if(appendPath){
						pathElements.add(fFile.getName());
					}
					loadFile(fName,pathElements,nullClassNames ? null : classNames,parser);
					if(appendPath) pathElements.setSize(pathElements.size()-1);
				}
			}
		}
		if(f.getName().endsWith(mofFileSuffix)){
			String fName = f.getName().substring(0,f.getName().length()-4);	// file name without the .mof
			// if we have a mof file, parse it into the repository.
			if(classNames == null || classNames.isEmpty() || classNames.contains(fName)){	
				StringBuilder b = new StringBuilder();
				for(String p : pathElements){
					b.append("/").append(p);
				}
				NameSpacePath path = new NameSpacePath(b.toString());
				parser.parse(f.getAbsolutePath(), path);
			}
		}
	}
	
	/**
	 * check if a directory exists. If not, create it
	 * @param directory - path for the directory
	 */
	private void checkDirectory(String directory){
		// check if the directory for this key exists
		File currentDirectory = new File(directory);
		if(!currentDirectory.exists()){
			if(!currentDirectory.mkdirs())
				throw new ModelException(ExceptionReason.ACCESS_DENIED, "PersistentRepository: Unable to create directory "+currentDirectory.getAbsolutePath());
		}
		return;
	}
	
	/**
	 * Get the file path corresponding to an object path.
	 * @param objectPath - incoming objectPath for the object to be read or written.
	 * @return - file system path to the file containing the data corresponding to the objectPath
	 */
	private String getFilePath(ObjectPath objectPath){
		// Create a file name from the object name
		StringBuilder b = new StringBuilder(rootDirectory);	
		switch(objectPath.getElementType()){
		case INSTANCE:	// filePath = "/instances/localPath/schema_className/uuid.mof"
			b.append(instanceDirectory).append(objectPath.getLocalPath()).append("/").append(objectPath.getName()).append("/");
			b.append(objectPath.getUUID());
			b.append(mofFileSuffix);
			break;
		case CLASS:	// filePath = "/classes/localPath/schema_className.mof"
			b.append(classDirectory).append(objectPath.getLocalPath()).append("/").append(objectPath.getName());
			b.append(mofFileSuffix);
			break;
		case INTERFACE: // filePath = "/interfaces/localPath/schema_interfaceName.mof"
			b.append(interfaceDirectory).append(objectPath.getLocalPath()).append("/").append(objectPath.getName());
			b.append(mofFileSuffix);
			break;
		case QUALIFIERTYPE: // filePath = "/qualifiers/localPath/schema_qualifierName.mof"
			b.append(qualifierDirectory).append(objectPath.getLocalPath()).append("/").append(objectPath.getName());
			b.append(mofFileSuffix);
			break;
		case STRUCTURE:		// filePath = "/structures/localPath/schema_structureName.mof"
			b.append(structureDirectory).append(objectPath.getLocalPath()).append("/").append(objectPath.getName());
			b.append(mofFileSuffix);
			break;
		case STRUCTUREVALUE:// filePath = "/structurevalues/localPath/schema_structureName/uuid.mof"
			b.append(structureValueDirectory).append(objectPath.getLocalPath()).append("/").append(objectPath.getName()).append("/");
			b.append(objectPath.getUUID());
			b.append(mofFileSuffix);
			break;
		case ENUMERATION:	// filePath = "/enumerations/localPath/schema_enumName.mof"
			b.append(enumerationDirectory).append(objectPath.getLocalPath()).append("/").append(objectPath.getName());
			b.append(mofFileSuffix);
			break;
		default:
			throw new ModelException(objectPath.getElementType()+" not yet implemented");
		}
		return b.toString();
	}
	
	/**
	 * Scan the repository for subtypes of given classes, and add them to the list of given classes
	 * @param classNames - class names to scan
	 */
	private void addSubTypes(HashSet<String> classNames){
		HashMap<String,HashSet<String>> subTypes = new HashMap<String,HashSet<String>>();
		// Scan the repository for subTypes
		// note that only structures, classes and interfaces have superTypes
		for(String type : new String []{structureDirectory, classDirectory,interfaceDirectory}){
			addSubTypes(new File(rootDirectory+type),subTypes);
		}
		// add all subtypes for the given class names to the set
		HashSet<String> subs = new HashSet<String>();
		for(String className : classNames){
			if(subTypes.containsKey(className)) subs.addAll(subTypes.get(className));
		}
		classNames.addAll(subs);
		return;
	}
	
	/**
	 * Recursively scan a file or directory, and collect all subtypes in a map 
	 * @param fileOrDirectory - file or directory to scan
	 * @param subTypes - map containing {superType, Set<SubType>}
	 */
	private void addSubTypes(File fileOrDirectory, HashMap<String,HashSet<String>> subTypes) {
		if(!fileOrDirectory.exists()) return;
		if(fileOrDirectory.isDirectory()){
			File [] children = fileOrDirectory.listFiles();
			for(File child : children){
				addSubTypes(child,subTypes);
			}
		} else if(fileOrDirectory.getName().endsWith(superTypeFileSuffix)){
			try {
				BufferedReader reader = new BufferedReader(new FileReader(fileOrDirectory));
				String line = null;
				while((line=reader.readLine()) != null){
					String [] classes = line.split(",");
					for(int i = 0; i < classes.length-1; i++){
						for(int j = i+1; j < classes.length; j++){
							if(!subTypes.containsKey(classes[j])){
								HashSet<String> st = new HashSet<String>();
								st.add(classes[i]);
								subTypes.put(classes[j], st);
							} else {
								HashSet<String> st = subTypes.get(classes[j]);
								st.add(classes[i]);
							}
						}
					}
				}
				reader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return;
	}
	
	/**
	 * Write an element to the persistent store
	 * @param element - element to be written
	 * @return - true in case of successful write, false if write failed for some reason
	 */
	private synchronized boolean writeElementToFile(NamedElement element) {
		File file = new File(getFilePath(element.getObjectPath()));
		// System.out.println("PersistentCache: Writing "+element.getObjectPath()+" to File "+file.getAbsolutePath());
		try {
			if(!file.getParentFile().exists()){
				File parent = file.getParentFile();
				if(!parent.mkdirs()){
					return false;
				}
			}
			FileWriter writer = new FileWriter(file,false);
			// TODO: Create a lock while writing the file
			// System.out.println("Writing "+element.getElementType()+" "+element.getObjectPath());
			// System.out.println(element.toMOF());
			writer.write(element.toMOF());
			if(element.getElementType() == ElementType.STRUCTUREVALUE){
				writer.write(";");
			}
			writer.close();
			// if the element has superTypes, create the superType File
			NamedElement superType = element.getSuperType();
			if(superType != null){
				// System.out.println("Writing SuperType for "+element.toMOF());
				StringBuilder superTypeNames = new StringBuilder(element.getName()).append(",");
				superTypeNames.append(superType.getName());
				superType = superType.getSuperType();
				while(superType != null){
					superTypeNames.append(",").append(superType.getName());
					superType = superType.getSuperType();
				}
				String fileName = file.getPath();
				fileName = fileName.substring(0,fileName.length()-4).concat(superTypeFileSuffix);
				// System.out.println(fileName);
				File supFile = new File(fileName);
				writer = new FileWriter(supFile,false);
				writer.write(superTypeNames.toString());
				writer.close();
			}
		} catch(Exception ex){
			return false;
		}
		return true;
	}

	/**
	 * Read an element from the persistent store
	 * @param path - objectPath for the element to be read
	 * @return - element read from persistent store. Null if no such object found
	 */
	private synchronized NamedElement readElementFromFile(ObjectPath path) {
		String file = getFilePath(path);
		InMemoryCache cache = new InMemoryCache();
		MOFParser parser = new MOFParser(cache,this);
		parser.parse(file, path.getNameSpacePath());
		NamedElement element = cache.get(path);
		return element;
	}

}
