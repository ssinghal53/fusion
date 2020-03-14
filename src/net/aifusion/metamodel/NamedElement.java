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
 * Created Dec 31, 2013 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Vector;

/**
 * Base class for all CIM Elements that reside in a namespace. Named elements can be located via their
 * ObjectPath, and can generate events
 * @author Sharad Singhal
 */
public abstract class NamedElement extends QualifiedElement implements CimEventGenerator {
	/** object path for this element */
	private ObjectPath path;
	/** SuperType for this named element, if any */
	private NamedElement superType;
	/** event listeners for this named element */
	private HashMap<CimEventType,List<CimListener>> listeners = new HashMap<CimEventType,List<CimListener>>();
	/** Names of all interfaces implemented by this named element, if any */
	private HashSet<String> interfaces = null;

	/**
	 * Create a named element (a qualified element resident in a namespace)
	 * @param type - type of element
	 * @param name - name of the element
	 * @param superType - direct superType of this named element, if any
	 * @param qualifiers - qualifiers on the element
	 * @param path - NameSpace path (e.g., "http://user:pass@host:port/path0/path1/...") for this element.
	 * @param keys - {key,value} pairs defining keys for this named element (for instances or structureValues)
	 * @param alias - alias, if any on this element
	 */
	protected NamedElement(ElementType type, String name, NamedElement superType, List<Qualifier> qualifiers, NameSpacePath path, Map<String,DataValue> keys, String alias) {
		super(type, name, qualifiers);
		// Note that the ObjectPath constructor validates that only instances and structure values can have keys defined,
		this.path = new ObjectPath(type,name,path,keys,alias);
		// if a direct superType is given, validate the superType, and any propagated qualifiers
		if(superType != null){
			// validate that type of this element matches the type on the superType
			// The only exception is that a CLASS can inherit from a STRUCTURE
			if((type != superType.getElementType()) && 
					!(type == ElementType.CLASS && superType.getElementType() == ElementType.STRUCTURE)){
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,name+": Expected Supertype "+ type+" found "+
						superType.getElementType()+" in "+superType.getName());
			}
			// validate superType qualifiers that will propagate to us
			for(Qualifier q : superType.getAllQualifiers()){
				QualifierType qt = q.getQualifierType();
				if(qt.isRestricted()) continue;	// restricted qualifiers do not propagate
				// if a superType declares a non-overridable qualifier, we must not change its value
				// #hasQualifier() does not see the supertype yet, so it's OK to use here
				if(!qt.isOverridable() && hasQualifier(q.getName()) && !getQualifierValue(q.getName()).equals(q.getValue())){
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": Non-overridable Qualifier "+q.getName()+" is already defined in superType(s)");
				}
			}
			// note that we must set the superType in this class after the qualifier check, because the loop above uses #hasQualifier()
			this.superType = superType;
		}
		return;
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.CimEventGenerator#addListener(net.aifusion.metamodel.CimEventType, net.aifusion.metamodel.CimListener)
	 */
	@Override
	public synchronized boolean addListener(CimEventType type, CimListener listener) {
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

	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.CimEventGenerator#removeListener(net.aifusion.metamodel.CimEventType, net.aifusion.metamodel.CimListener)
	 */
	@Override
	public synchronized void removeListener(CimEventType type, CimListener listener) {
		if(listeners.containsKey(type)){
			List<CimListener> list = listeners.get(type);
			if(list.contains(listener)) list.remove(listener);
			if(list.isEmpty()) listeners.remove(type);
		}
		return;
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.aifusion.metamodel.CimEventGenerator#hasListener(net.aifusion.metamodel.CimEventType)
	 */
	@Override
	public boolean hasListener(CimEventType type, CimListener listener){
		if(!listeners.containsKey(type)) return false;
		if(listener == null) return true;
		List<CimListener> list = listeners.get(type);
		return list.contains(listener);
	}
	
	/**
	 * Raise an event to all registered listeners. This method should be used by subclasses to send events
	 * @param event - event to be sent to all interested listeners
	 */
	protected void generateEvent(CimEvent event){
		CimEventType type = event.getType();
		if(listeners.containsKey(type)){
			List<CimListener> list = listeners.get(type);
			for(CimListener l : list){
				l.notify(event);
			}
		}
		return;
	}

	/**
	 * Get the object path for this named element
	 * @return - object path for this element
	 */
	public ObjectPath getObjectPath(){
		return path;
	}
	
	/**
	 * Get the namespace path for this named element
	 * @return - namespace path for this element
	 */
	public NameSpacePath getNameSpacePath(){
		return path.getNameSpacePath();
	}

	/**
	 * Get the direct superType of this element, if any
	 * @return - direct superType of this element, null if none defined
	 */
	public NamedElement getSuperType(){
		return superType;
	}
	
	/**
	 * Check if this NamedElement is a subType of another NamedElement
	 * @param name - Name of the NamedElement to be checked 
	 * @return - true if the given name is the name of this element, or any of its superTypes. False otherwise
	 */
	public boolean isSubTypeOf(String name){
		if(name == null || name.isEmpty()) return false;
		// check against this named element name
		if(getName().equalsIgnoreCase(name)) return true;
		// check against implemented interfaces, if any 
		if(interfaces != null && interfaces.contains(name.toLowerCase())) return true;
		// check against the super type, if any
		if(superType != null && superType.isSubTypeOf(name)) return true;
		return false;
	}
	
	/**
	 * Check if this NamedElement is abstract
	 * @return - true if this NamedElement is abstract, false otherwise
	 */
	public boolean isAbstract() {
		return getElementType() == ElementType.INTERFACE || hasQualifier("ABSTRACT") && 
				(Boolean) getQualifierValue("ABSTRACT").getValue();
	}
	
	/**
	 * Return the full name of this Named element.
	 * The fully qualified name of the element is (... SupName1.SupName0.Name),
	 * where Name is the name of this element, SupName0 is the immediate superType, SupName1 is the next ancestor, and so on.
	 * If the element does not have a superType, its name is returned
	 * @return - fully qualified name of this element
	 * @see #getName()
	 */
	public String getFullName(){
		if(superType == null) return getName();
		return new StringBuilder(superType.getFullName()).append(".").append(getName()).toString();
	}
	
	/**
	 * Get the value of a qualifier declared on this element, inherited by it, or the default defined in standard qualifiers. 
	 * @param qualifierName - name of the qualifier to search
	 * @return - value of the qualifier or default from a standard qualifier. Null if none defined
	 * @see StandardQualifierType
	 */
	@Override
	public DataValue getQualifierValue(String qualifierName){
		if(qualifierName == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER, getName()+": getQualifierValue() Expected qualifier name, found null");
		// search for qualifiers defined on this element
		for(Qualifier q : getQualifiers()){
			if(!qualifierName.equalsIgnoreCase(q.getName())) continue;
			return q.getValue();
		}
		// search for qualifiers inherited by this element
		if(superType != null){
			for(Qualifier q : superType.getAllQualifiers()){
				if(q.getQualifierType().isRestricted()) continue;	// restricted qualifiers do not propagate
				// note that we will find the first matching value in the hierarchy
				if(!qualifierName.equalsIgnoreCase(q.getName())) continue;
				return q.getValue();
			}
		}
		// check standard qualifier values
		for(StandardQualifierType qt : StandardQualifierType.values()){
			if(qt.name().equalsIgnoreCase(qualifierName) && qt.appliesTo(getElementType())){
				return qt.getDefaultValue();
			}
		}
		return null;
	}

	/**
	 * Get all qualifiers declared on, or inherited by this qualified Element
	 * @return - list of all qualifiers. Empty list is returned if none are declared
	 * @see #getQualifiers()
	 */
	public List<Qualifier> getAllQualifiers(){
		// get qualifiers from our list
		List<Qualifier> q = getQualifiers();
		// if we have superType, get all qualifiers that are not restricted from the superType
		// TODO: Check what happens if the same propagated qualifier is declared multiple times
		if(superType != null){
			for(Qualifier sq : superType.getAllQualifiers()){
				if(!sq.getQualifierType().isRestricted()) q.add(sq);
			}
		}
		return q;
	}

	/**
	 * Check if this qualified element has a particular qualifier declared in it (or inherited by it)<br>
	 * @param qName - name of the qualifier
	 * @return - true if the qualified element has a qualifier declared in it (or inherited by it), false otherwise
	 */
	public boolean hasQualifier(String qName){
		if(qName == null || qName.isEmpty()) return false;
		// check qualifiers defined on this element
		for(Qualifier q : getQualifiers()){
			if(q.getName().equalsIgnoreCase(qName)) return true;
		}
		// check qualifiers inherited by this element 
		if(superType != null) {
			// note that superType.getAllQualifiers() will include restricted qualifiers in the
			// direct superType, so we need to filter them out
			for(Qualifier q : superType.getAllQualifiers()){
				if(q.getQualifierType().isRestricted()) continue;
				if(q.getName().equalsIgnoreCase(qName)) return true;
			}
		}
		return false;
	}
	/**
	 * Add an interface to this named element
	 * @param intf - interface to be added to this named element
	 */
	void addInterface(NamedElement intf){
		if(intf.getElementType() != ElementType.INTERFACE) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,getName()+": Expected Interface, found "
					+intf.getName()+":"+intf.getElementType());
		}
		if(interfaces == null) interfaces = new HashSet<String>();
		// add name of the interface
		interfaces.add(intf.getLowerCaseName());
		// add names of all supertypes of the interface
		while(intf.getSuperType() != null) {
			intf = intf.getSuperType();
			interfaces.add(intf.getLowerCaseName());
		}
		return;
	}
	
	@Override
	public boolean equals(Object obj) {
		if(!super.equals(obj) || !(obj instanceof NamedElement)) return false;
		NamedElement o = (NamedElement) obj;
		// check that both NamedElements are in the same name space
		// note that two equal NamedElements can have different ObjectPaths 
		return path.getNameSpacePath().equals(o.path.getNameSpacePath());
	}
	
}
