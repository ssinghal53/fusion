/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * Created Mar 3, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import java.io.PrintStream;
import java.util.List;
import java.util.Vector;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * AST Node for ASN.1 parsing. Contains a name and/or a value
 * @author Sharad Singhal
 */
public class AsnNode {
	/** AstProduction at this node */
	private AsnProduction production;
	/** Tokens at this node, if any */
	private Vector<Token> tokens = new Vector<Token>();
	/** Parent for this node */
	private AsnNode parent;
	/** Children of this node */
	private Vector<AsnNode> children = new Vector<AsnNode>();
	/** ASN.1 value, if any */
	private AsnValue value;
	/**
	 * Create a node for a production
	 * @param production
	 */
	public AsnNode(AsnProduction production) {
		this.production = production;
		if(production == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"AstProduction should not be null");
		return;
	}
	
	/**
	 * Create a new parser node with a given token (name)
	 * @param token - Token at this node
	 */
	public AsnNode(AsnProduction production, Token token) {
		this.production = production;
		if(production == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"AstProduction should not be null");
		tokens.add(token);
		return;
	}
	
	/**
	 * Create a parser node with a given name
	 * @param production - production at this node
	 * @param token - name token at this node
	 * @param isClassField - true if this node is an ObjectClass field, false otherwise
	 */
	public AsnNode(AsnProduction production, Token token, boolean isClassField) {
		this.production = production;
		if(production == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"AstProduction should not be null");
		tokens.add(isClassField ? new Token(token.type(),"&"+token.value()) : token);
		return;
	}
	
	/**
	 * Create a new parser node with a given token pair
	 * @param production - AstProduction
	 * @param nameToken - name token
	 * @param valueToken - value token
	 */
	public AsnNode(AsnProduction production, Token nameToken, Token valueToken) {
		this.production = production;
		if(production == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"AstProduction should not be null");
		tokens.add(nameToken);
		tokens.add(valueToken);
		return;
	}
	
	/**
	 * Create a parser node with a list of tokens
	 * @param production - production
	 * @param tokens - associated tokens
	 */
	public AsnNode(AsnProduction production, List<Token> tokens) {
		this.production = production;
		if(production == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"AstProduction should not be null");
		tokens.addAll(tokens);
		return;
	}
	
	/**
	 * Add a child node to this node
	 * @param child - child node
	 * @return the child node
	 */
	public AsnNode addChild(AsnNode child) {
		if(child == null) return null;
		children.add(child);
		if(child.parent != null) throw new ModelException(ExceptionReason.ALREADY_EXISTS,"parent already exists on "+child);
		child.parent = this;
		return child;
	}
	
	/**
	 * Get the children of this node
	 * @return - children for this node
	 */
	public List<AsnNode> getChildren(){
		return children;
	}
	
	/**
	 * Get a given child
	 * @param index - index of the child
	 * @return - child at given index
	 */
	public AsnNode getChild(int index) {
		if(index < 0 || index > children.size()) {
			throw new ModelException("Index "+index+" outside range [0,"+children.size()+")");
		}
		return children.get(index);
	}
	
	/**
	 * Check if this node has any children
	 * @return - true if this node has children, false otherwise
	 */
	public boolean hasChildren() {
		return !children.isEmpty();
	}
	
	/**
	 * Get the production at this node
	 * @return - production at this node
	 */
	public AsnProduction getProduction() {
		return production;
	}
	
	/**
	 * Get the name (value of token[0]) at this node
	 * @return name of the node
	 */
	public String getName() {
		// TODO: Note that if(production == EXTERNAL_TYPE_REFERENCE) then token[0] = moduleName, token[1] = type reference
		// add the extra cases once we know how the name will be consumed.
		if(tokens.size() > 0) return tokens.elementAt(0).value();
		return null;
	}
	
	/**
	 * Get the name token from this node
	 * @return - token at element 0, if any. Null if none present
	 */
	public Token getNameToken() {
		return tokens.size() > 0 ? tokens.get(0) : null;
	}
	
	/**
	 * Get the value (value of token[1]) at this node
	 * @return value of the node
	 */
	public String getValue() {
		if(tokens.size() > 1) return tokens.elementAt(1).value();
		return null;
	}
	
	/**
	 * Get the ASN.1 value, if any
	 * @return asn value, if the node reduces to a constant value. Null otherwise
	 * @see #reduce()
	 */
	public AsnValue getAsnValue() {
		return value;
	}
	
	/**
	 * Get the value token from this node.
	 * @return - token at element 1, if any. Null if none present
	 */
	public Token getValueToken() {
		return tokens.size() > 1 ? tokens.get(1) : null;
	}
	
	/**
	 * Get the full name of the node
	 * @return full name of the node
	 */
	public String getFullName() {
		Vector<String> names = new Vector<String>();
		AsnNode n = this;
		while(n != null) {
			String s = n.getName();
			if(s != null && !s.isEmpty()) names.add(s);
			n = n.parent;
		}
		StringBuilder b = new StringBuilder("@");
		for(int i = names.size()-1; i >= 0; i--) {
			b.append(names.get(i)).append(".");
		}
		b.setLength(b.length()-1);
		return b.toString();
	}

	/**
	 * Locate the node corresponding to the type for a given name
	 * @param name - string containing the name of the node
	 * @return - node defining the corresponding type, null if none found
	 */
	public AsnNode locateType(String name) {
		AsnNode n = locate(null,name);
		// System.out.println("Located Node "+n);
		if(n == null) return null;
		for(AsnNode c : n.children) {
			// System.out.println(c+" "+c.isBuiltInType());
			switch(c.production) {
			case TAGGED_TYPE:
				return c.getChild(1);
			default:
				if(c.production.isBuiltIn()) return c;
			}
		}
		return null;
	}
	
	/**
	 * Check if this node represents a built-in type
	 * @return - true if the name of the node is a built-in type, false otherwise
	 */
	public boolean isBuiltInType() {
		return production.isBuiltIn();
		// Token nameToken = getNameToken();
		// return nameToken == null ? false : nameToken.isBuiltIn();
	}
	
	/**
	 * Locate a node corresponding to a given name and production starting at this node
	 * @param production - production to match
	 * @param name - name to match
	 * @return - Corresponding node. Null if none found
	 */
	public AsnNode locate(AsnProduction production, String name) {
		if(name == null) return null;
		// System.out.println(production+" locating "+name);
		if(name.equals(getName()) && (production == null || production.equals(this.production))) return this;
		for(AsnNode c : children) {
			AsnNode v = c.locate(production,name);
			if(v != null) return v;
		}
		return null;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return toString("");
	}
	
	/**
	 * Return a string value for this node. Subclasses should override this rather than toString()
	 * @param indent - indent used before the value of this node
	 * @return - string containing the ASN.1 definition at this node
	 */
	public String toString(String indent) {
		StringBuilder b = new StringBuilder(indent);
		// b.append(getFullName()).append(" ");
		b.append(production.astValue());
		if(!tokens.isEmpty()) {
			for(Token t : tokens) b.append(" ").append(t);
		}
		if(value != null) {
			b.append(" ");
			if(!tokens.isEmpty()) {
				b.append(tokens.get(0).value()).append(" ");
			}
			b.append(value.toAsnString(""));
			if(b.charAt(b.length()-1)== '\n') b.setLength(b.length()-1);
		}
		return b.toString();
	}
	
	public AsnNode search(String name) {
		if(tokens.size() > 0 && name.equals(tokens.get(0).value())) return this;
			for(AsnNode child : children) {
				AsnNode found = child.search(name);
				if(found == null) continue;
				return found;
			}
		return null;
	}
	
	/**
	 * Add values to this subtree for defined constants
	 * @param searchRoot - root of the tree (for locating referenced constants)
	 * @param f - print stream to dump the tree
	 */
	public void reduce(AsnNode searchRoot, PrintStream f) {
		// if this node has chidren, reduce them
		if(hasChildren()) {
			for(AsnNode n : getChildren()) {
				n.reduce(searchRoot,f);
			}
		}
		// reduce this node (if not already done)
		if(value != null) return;
		switch(production) {
		case NAME_AND_NUMBER_FORM:	// expect tokens[0] = name, tokens[1] = Integer value
			if(tokens.size() != 2) {
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+" Expected two tokens, found "+tokens.size());
			}
			value = new IntegerValue(Long.valueOf(tokens.get(1).value()));
			break;
		case NAME_FORM:
			AsnNode definingNode = searchRoot.search(tokens.get(0).value());
			if(definingNode == null) {
				throw new ModelException(toString()+" Could not find definition");
			}
			if(definingNode.getAsnValue() != null) value = definingNode.getAsnValue();
			break;
		case NUMBER_FORM:
			if(tokens.size() != 1) {
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+" Expected one token, found "+tokens.size());
			}
			value = new IntegerValue(Long.valueOf(tokens.get(0).value()));
			break;
		case OBJECT_IDENTIFIER_VALUE:
			// get all children, and concatenate their values. Note we assume that integers within the
			// OID value fit within longs
			Vector<Long> values = new Vector<Long>();
			for(int i = 0; i < children.size(); i++) {
				AsnValue v = children.get(i).getAsnValue();
				if(v == null) {
					definingNode = searchRoot.search(tokens.get(0).value());
					if(definingNode != null) {
						if(definingNode.getAsnValue() == null) { 
							definingNode.reduce(searchRoot,f);
							v = definingNode.getAsnValue();
						}
					} else {
						throw new ModelException(ExceptionReason.NOT_SUPPORTED,toString()+": Unknown child - "+children.get(i));
					}
					if(v == null) {
						throw new ModelException(ExceptionReason.NOT_SUPPORTED,toString()+": undefined value in - "+children.get(i));
					}
				} else if(v instanceof IntegerValue) {
					values.add(((IntegerValue)v).getValue());
				} else if(v instanceof OidValue) {
					for(long l : ((OidValue)(v)).getValue()) {
						values.add(l);
					}
				}
			}
			long [] oidValues = new long[values.size()];
			for(int i = 0; i < values.size(); i++) oidValues[i] = values.get(i);
			value = new OidValue(oidValues);
			break;
		case DEFINITIVE_IDENTIFIER:
			oidValues = new long[children.size()];
			for(int i = 0; i < children.size(); i++) {
				oidValues[i] = ((IntegerValue) children.get(i).getAsnValue()).getValue();
			}
			value = new OidValue(oidValues);
			break;
		case VALUE_ASSIGNMENT:	// expect 2 children, first contains type, second contains value
			if(children.size() != 2) {
				throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+" Expected two children, found "+children.size());
			}
			// we assume that the type is correct.
			value = children.get(1).getAsnValue();
		// intermediate nodes that do not take values
		default:
			break;
			// throw new ModelException(ExceptionReason.NOT_SUPPORTED,production+" reduction not yet implemented");
		}
		return;
	}
	
}
