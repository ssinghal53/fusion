/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Apr 29, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.util.HashMap;

/**
 * Class to represent an Ast Module
 * @author Sharad Singhal
 */
public class AsnModule extends AsnNode {
	/** Known types in this module */
	private HashMap<String, AsnNode> knownTypes = new HashMap<String,AsnNode>();
	/** Known values in this module */
	private HashMap<String, AsnNode> knownValues = new HashMap<String,AsnNode>();
	
	/**
	 * Create a Module definition
	 * @param token - token containing the name of the module
	 */
	public AsnModule(Token token) {
		super(AsnProduction.MODULE_DEFINITION, token);
	}

	/**
	 * get a hashmap containing all know types in this module
	 * @return - map containing {typeName, definingAstNode} pairs
	 */
	public HashMap<String,AsnNode> getTypes(){
		return knownTypes;
	}
	
	/**
	 * Get a hashmap containing all known values in this module
	 * @return - map containing {valueName, definingAstNode} pairs
	 */
	public HashMap<String,AsnNode> getValues(){
		return knownValues;
	}
	
	/**
	 * Get the node where a type is defined
	 * @param typeName - name of the type
	 * @return - node containing type definition. Null if none currently defined
	 */
	public AsnNode getType(String typeName) {
		return knownTypes.get(typeName);
	}
	
	/**
	 * Get the node where a value is defined
	 * @param valueName - name of the value
	 * @return - node containing value definition. Null if none currently defined
	 */
	public AsnNode getValue(String valueName) {
		return knownValues.get(valueName);
	}
	
	/**
	 * Add a type to known types
	 * @param typeName - name of the type to be added
	 * @param typeNode - node at which the type is defined
	 */
	public void addType(String typeName, AsnNode typeNode) {
		knownTypes.put(typeName, typeNode);
		return;
	}
	
	/**
	 * Add a value to known values
	 * @param valueName - name of the value to be added
	 * @param valueNode - node at which the value is defined
	 */
	public void addValue(String valueName, AsnNode valueNode) {
		knownValues.put(valueName, valueNode);
		return;
	}
	
	/**
	 * Check if a given type name is known to this module
	 * @param typeName - name of the type
	 * @return true if the type is known to this module, false otherwise
	 */
	public boolean hasType(String typeName) {
		return knownTypes.containsKey(typeName);
	}
	
	/**
	 * Check if a given value is known to this module
	 * @param valueName - name of the value
	 * @return - true if the name is known to the module, false otherwise
	 */
	public boolean hasValue(String valueName) {
		return knownValues.containsKey(valueName);
	}

}
