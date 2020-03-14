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
 * Created Dec 18, 2015 by Sharad Singhal
 */
package net.aifusion.cql;

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;
import java.util.Vector;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Parser for CIMQuery. See DMTF DSP0202 (Version 1.0.0, 2007-08-13)
 * @author Sharad Singhal
 */
class QueryParser {
	/** debugging flag */
	private boolean debug = false;
	/** state maintained by the parser */
	class ParserState {
		/** CIM Query being parsed */
		String query = null;
		/** Length of the query */
		int queryLength = 0;
		/** Cursor within the query */
		int cursor = 0;
		/** Next token to be read */
		Token lookAheadToken = null;
		/** Aliases (defined using AS clause) within this query */
		HashMap<String,Node> aliases = new HashMap<String,Node>();
		/** Nodes being parsed within this query */
		Stack<Node> stack = new Stack<Node>();

		@Override
		public String toString() {
			StringBuilder b = new StringBuilder("****Parser State****\n");
			b.append(query).append("\n");
			b.append("\nCursor:").append(cursor).append("\n");
			b.append("LookAheadToken: ").append(lookAheadToken).append("\n");
			if(!stack.isEmpty()){
				for(int i = 0; i < stack.size(); i++){
					b.append(stack.get(i)).append("\n");
				}
			}
			b.append("********\n");
			return b.toString();
		}
	}

	/**
	 * Create a new instance of a parser
	 */
	QueryParser() {
		return;
	}
	
	/** 
	 * Parse a CQLQuery and return the top of the parse tree
	 * @param query - query string to be parsed
	 * @return - root of the parse tree
	 */
	Node parse(String query){
		// start = select-statement
		ParserState p = new ParserState();
		p.query = query;
		p.queryLength = query.length();
		if(debug) System.out.println(query);
		try {
			// get the first token
			nextToken(p);
			// get the select statement
			Node selectStatement = selectStatement(p);
			if(!p.lookAheadToken.is(TokenType.EOF)) throw new ModelException(ExceptionReason.INVALID_QUERY, "Incomplete parse: "+p.toString());
			return selectStatement;
		} catch (Exception e){
			if(debug) System.out.println(p.toString());
			if(e instanceof ModelException){
				throw e;
			} else {
				throw new ModelException(p.toString(),e);
			}
		}
	}
	
	/**
	 * SelectStatment = SELECT selectList FROM fromCriteria [WHERE searchCondition] 
	 * 		| "(" SELECT selectList FROM fromCriteria [WHERE searchCondition] ")" AS result_class
	 * @param p - parser state
	 * @return - select node
	 */
	private Node selectStatement(ParserState p){
		if(debug) entering(p,"selectStatement");
		// clone the old alias Values
		HashMap<String,Node> savedAlias = p.aliases;
		p.aliases = new HashMap<String,Node>();
		
		// check if result set is named
		boolean haveResultSetName = false;
		if(p.lookAheadToken.is(TokenType.LPAREN)){
			advanceOver(p,TokenType.LPAREN);
			haveResultSetName = true;
		}

		advanceOver(p,TokenType.SELECT);
		Node select = Operator.SELECT.getNode();
		
		// select-list
		select.addChild(selectList(p));
		
		// from-criteria (classList)
		advanceOver(p,TokenType.FROM);
		select.addChild(classList(p));
		
		// optional search condition
		if(p.lookAheadToken.is(TokenType.WHERE)){
			advanceOver(p,TokenType.WHERE);
			Node where = Operator.WHERE.getNode();
			select.addChild(where);
			where.addChild(searchCondition(p));
		}
		
		// named select value
		if(haveResultSetName){
			advanceOver(p,TokenType.RPAREN);
			// optional AS
			if(p.lookAheadToken.is(TokenType.AS)){
				advanceOver(p,TokenType.AS);
			}
			Node rs = classPath(p);
			select.setAlias(rs.getName());
			p.aliases.put(rs.getName(), rs);
		}
		
		// append the known alias values from the parser state
		if(!p.aliases.isEmpty()){
			Node alias = Operator.ALIAS.getNode();
			for(String key : p.aliases.keySet()){
				alias.addChild(p.aliases.get(key));
			}
			select.addChild(alias);
		}
		// restore old alias values
		p.aliases = savedAlias;
		if(debug) exiting(p,"selectStatement");
		return select;
	}

	/**
	 * SelectList = SelectedEntry *( COMMA SelectedEntry )
	 * @param p - parser state
	 * @return - selectList node
	 */
	private Node selectList(ParserState p){
		if(debug) entering(p,"selectList");
		Node selList = Operator.SELECT_LIST.getNode();
		selList.addChild(selectedEntry(p));
		while(p.lookAheadToken.is(TokenType.COMMA)){
			advanceOver(p, TokenType.COMMA);
			selList.addChild(selectedEntry(p));
		}
		if(debug) exiting(p,"selectList");
		return selList;
	}

	// TODO: validate that the selected entry represents one of the following:
	// 1. A property name
	// 2. A literal named through the AS construct
	// 3. A function named through the AS construct
	// 4. An expression named through the AS construct

	/**
	 * SelectedEntry = starExpr | expr [AS Identifier]<br>
	 * StarExpression = '*' | chain '.' '*' | chain '.' propertyScope '*'
	 * @param p - parser state
	 * @return - selectedEntry node
	 */
	private Node selectedEntry(ParserState p){
		if(debug) entering(p,"selectedEntry");
		if(p.lookAheadToken.is(TokenType.STAR)){
			advanceOver(p,TokenType.STAR);
			if(debug) exiting(p,"selectedEntry");
			return Operator.PROPERTY_NAME.getNode("*");
		}
		// Note that expr(p) also includes className '.' '*' entry
		Node expr = expr(p);
		Node selectedEntry = expr;
		boolean aliasNeeded = false;
		switch(expr.getOperator()){
		case IDENTIFIER:
			selectedEntry = Operator.PROPERTY_NAME.getNode(expr.getName());
			if(expr.hasChildren()){
				for(Node child : expr.getChildren()){
					selectedEntry.addChild(child);
				}
			}
			break;
		default:
			aliasNeeded = true;
			break;
		}
		if(p.lookAheadToken.is(TokenType.AS)){
			advanceOver(p,TokenType.AS);
			Node id = identifier(p);
			selectedEntry.setAlias(id.getName());
			p.aliases.put(id.getName(), selectedEntry);
		} else if(p.lookAheadToken.is(TokenType.IDENTIFIER)){
			Node id = identifier(p);
			selectedEntry.setAlias(id.getName());
			p.aliases.put(id.getName(), selectedEntry);
		}
		if(aliasNeeded && selectedEntry.getAlias() == null)
			error(p,"Alias required for "+selectedEntry);
		if(debug) exiting(p,"selectedEntry");
		return selectedEntry;
	}

	/**
	 * classList = fromSpecifier *(COMMA fromSpecificer) | '(' selectStatement ')' identifier
	 * @param p - parser state
	 * @return - node containing CLASS_LIST
	 */
	private Node classList(ParserState p) {
		if(debug) entering(p,"classList");
		Node classList = Operator.CLASS_LIST.getNode();
		classList.addChild(fromSpecification(p));
		while(p.lookAheadToken.is(TokenType.COMMA)){
			advanceOver(p,TokenType.COMMA);
			classList.addChild(fromSpecification(p));
		}
		if(debug) exiting(p,"classList");
		return classList;
	}

	/**
	 * FromSpecification = classPath [[AS] ALIAS] | '(' SubQuery ')' identifier
	 * @param p - parser state
	 * @return - CLASS_PATH | Select
	 */
	private Node fromSpecification(ParserState p){
		if(debug) entering(p,"fromSpecification");
		if(p.lookAheadToken.is(TokenType.LPAREN)){
			// have a subquery
			advanceOver(p,TokenType.LPAREN);
			Node select = selectStatement(p);
			advanceOver(p,TokenType.RPAREN);
			Token id = advanceOver(p,TokenType.CLASSPATH);
			select.setAlias(id.value());
			p.aliases.put(id.value(), select);
			if(debug) exiting(p,"fromSpecification");
			return select;
		} else {
			Node path = classPath(p);
			// check for optional [[AS] Alias]
			if(p.lookAheadToken.is(TokenType.AS)){
				advanceOver(p,TokenType.AS);
				String alias = advanceOver(p,TokenType.IDENTIFIER).value();
				path.setAlias(alias);
				p.aliases.put(alias, path);
			} else if(p.lookAheadToken.is(TokenType.IDENTIFIER)){
				// we can have an ALIAS, COMMA, or WHERE here
				if(!p.lookAheadToken.is(TokenType.WHERE)){
					String alias = advanceOver(p,TokenType.IDENTIFIER).value();
					path.setAlias(alias);
					p.aliases.put(alias, path);
				}
			}
			if(debug) exiting(p,"fromSpecification");
			return path;
		}
	}

	/**
	 * SearchCondition = expr
	 * @param p - Parser State
	 * @return - node containing root of expression for WHERE condition
	 */
	private Node searchCondition(ParserState p) {
		if(debug) entering(p,"searchCondition");
		Node condition = expr(p);
		if(debug) exiting(p,"searchCondition");
		return condition;
	}

	/**
	 * Expression := ORExpression
	 * ORExpression = AndExpression *( OR AndExpression)
	 * @param p - parser state
	 * @return - Node containing orExpression
	 */
	private Node expr(ParserState p) {
		if(debug) entering(p,"expr");
		Node n = andExpression(p);
		while(p.lookAheadToken.is(TokenType.OR)){
			advanceOver(p, TokenType.OR);
			if(n.getOperator() != Operator.OR){
				Node op = Operator.OR.getNode();
				op.addChild(n);
				op.addChild(andExpression(p));
				n = op;
			} else {
				n.addChild(andExpression(p));
			}
		}
		if(debug) exiting(p,"expr");
		return n;
	}

	/**
	 * AndExpression := NotExpression *(AND NotExpression)
	 * @param p - parser state
	 * @return - node containing AndExpression
	 */
	private Node andExpression(ParserState p) {
		if(debug) entering(p,"andExpression");
		Node n = notExpression(p);
		while(p.lookAheadToken.is(TokenType.AND)){
			advanceOver(p,TokenType.AND);
			if(n.getOperator() != Operator.AND){
				Node op = Operator.AND.getNode();
				op.addChild(n);
				op.addChild(notExpression(p));
				n = op;
			} else {
				n.addChild(notExpression(p));
			}
		}
		if(debug) exiting(p,"andExpression");
		return n;
	}

	/**
	 * NotExpression := [NOT] CompareExpression
	 * @param p - Parser State
	 * @return - Node containing NOT expression
	 */
	private Node notExpression(ParserState p) {
		if(debug) entering(p,"notExpression");
		Node n = null;
		if(p.lookAheadToken.is(TokenType.NOT)){
			advanceOver(p,TokenType.NOT);
			n = Operator.NOT.getNode();
		}
		if(n == null){
			n = arrayComp(p);
		} else {
			n.addChild(arrayComp(p));
		}
		if(debug) exiting(p,"notExpression");
		return n;
	}

	/**
	 * ArrayCompare := (ANY | EVERY) comp | (ANY | EVERY) Identifier IN expr SATISFIES '(' comp ')'
	 * @param p - parser state
	 * @return - return node array compare expression
	 */
	private Node arrayComp(ParserState p) {
		if(debug) entering(p,"arrayComp");
		Node n = null;
		if(p.lookAheadToken.is(TokenType.ANY) || p.lookAheadToken.is(TokenType.EVERY)){
			Token arrayOp = advanceOver(p,p.lookAheadToken.type());
			Node compOrIdentifier = comp(p);
			if(compOrIdentifier.getOperator() == Operator.IDENTIFIER && p.lookAheadToken.is(TokenType.IN)){
				// ANY | EVERY identifier IN expr SATISFIES ( comp ) 
				advanceOver(p,TokenType.IN);
				Node expr = expr(p);
				advanceOver(p,TokenType.SATISFIES);
				advanceOver(p,TokenType.LPAREN);
				Node comp = expr(p);
				advanceOver(p,TokenType.RPAREN);
				n = Operator.valueOf(arrayOp.type().toString()).getNode(compOrIdentifier.getName());
				n.addChild(expr);
				Node op = Operator.SATISFIES.getNode();
				n.addChild(op);
				op.addChild(comp);
			} else {
				n = Operator.valueOf(arrayOp.type().toString()).getNode();
				n.addChild(compOrIdentifier);
			}
		} else {
			n = comp(p);
		}
		if(debug) exiting(p,"arrayComp");
		return n;
	}

	/**
	 * Comparison operators
	 * comp := arith | arith IS [NOT] Null | arith ISA identifier
	 *         | arith (GT | GE | EQ | LE | LT | LIKE) arith
	 * @param p - parser state
	 * @return node containing comp tree
	 */
	private Node comp(ParserState p){
		if(debug) entering(p,"comp");
		Node n = arith(p);
		if(p.lookAheadToken.is(TokenType.COMPARISON)){
			Token t = advanceOver(p,TokenType.COMPARISON);
			Node op = Operator.valueOf(t.type().toString()).getNode();
			op.addChild(n);
			op.addChild(arith(p));
			n = op;
		} else if(p.lookAheadToken.is(TokenType.IS)){ // arith IS [NOT] NULL
			boolean isNot = false;
			advanceOver(p,TokenType.IS);
			if(p.lookAheadToken.is(TokenType.NOT)){
				isNot = true;
				advanceOver(p,TokenType.NOT);
			}
			advanceOver(p,TokenType.NULL);
			Node op = isNot ? Operator.ISNOTNULL.getNode() : Operator.ISNULL.getNode();
			op.addChild(n);
			n = op;
		} else if(p.lookAheadToken.is(TokenType.ISA)){ // arith ISA identifier
			advanceOver(p,TokenType.ISA);
			Token className = advanceOver(p,TokenType.IDENTIFIER);
			Node op = Operator.ISA.getNode(className.value());
			op.addChild(n);
			n = op;
		}
		if(debug) exiting(p,"comp");
		return n;
	}
	
	/**
	 * Arith := term *( ('+' | '-') term)
	 * @param p - parser state
	 * @return Node containing arith
	 */
	private Node arith(ParserState p){
		if(debug) entering(p,"arith");
		Node n = term(p);
		while(p.lookAheadToken.is(TokenType.PLUS) || p.lookAheadToken.is(TokenType.MINUS)){
			if(p.lookAheadToken.is(TokenType.PLUS)){
				advanceOver(p, TokenType.PLUS);
				Node op = Operator.ADD.getNode();
				op.addChild(n);
				n = op;
			} else {
				advanceOver(p,TokenType.MINUS);
				Node op = Operator.SUBTRACT.getNode();
				op.addChild(n);
				n = op;
			}
			n.addChild(term(p));
		}
		if(debug) exiting(p,"arith");
		return n;
	}
	
	
	/**
	 * Term := factor *( ('*'|'/') factor)
	 * @param p - parser state
	 * @return - node containing factor 
	 */
	private Node term(ParserState p){
		if(debug) entering(p,"term");
		Node n = factor(p);
		while(p.lookAheadToken.is(TokenType.STAR) || p.lookAheadToken.is(TokenType.SLASH)){
			if(p.lookAheadToken.is(TokenType.STAR)){
				advanceOver(p, TokenType.STAR);
				Node op = Operator.MULTIPLY.getNode();
				op.addChild(n);
				n = op;
			} else {
				advanceOver(p,TokenType.SLASH);
				Node op = Operator.DIVIDE.getNode();
				op.addChild(n);
				n = op;
			}
			n.addChild(factor(p));
		}
		if(debug) exiting(p,"term");
		return n;
	}
	
	/**
	 * unary '+' or '-'
	 * Factor := ['+' | '-'] concat
	 * @param p - parser state
	 * @return - node containing factor
	 */
	private Node factor(ParserState p){
		if(debug) entering(p,"factor");
		Node n = null;
		if(p.lookAheadToken.is(TokenType.SIGN)){
			Token t = advanceOver(p,TokenType.SIGN);
			n = (t.type() == TokenType.PLUS) ? Operator.SIGN.getNode("+") : Operator.SIGN.getNode("-");			
		}
		Node concat = concat(p);
		if(n == null)
			n = concat;
		else
			n.addChild(concat);
		if(debug) exiting(p,"factor");
		return n;
	}
	
	/**
	 * Concatenation 
	 * Concat := Chain | *( '||' Chain)
	 * Get a concatenated chain
	 * @param p - parser state
	 * @return - node with chain or contact
	 */
	private Node concat(ParserState p){
		if(debug) entering(p,"concat");
		Node n = null;
		if(haveChain(p)){
			Node leftChain = p.stack.pop();
			if(p.lookAheadToken.is(TokenType.CONCAT)){
				n = Operator.CONCAT.getNode();
				n.addChild(leftChain);
				while(p.lookAheadToken.is(TokenType.CONCAT)){
					advanceOver(p,TokenType.CONCAT);
					if(haveChain(p)){
						Node rightChain = p.stack.pop();
						n.addChild(rightChain);
					} else {
						error(p,"Chain",ExceptionReason.INVALID_QUERY); // error - need a chain
					}
				}
			} else {
				n = leftChain;	// only a single chain
			}
		} else {
			error(p,"Chain",ExceptionReason.INVALID_QUERY); // error - need a chain
		}
		if(debug) exiting(p,"concat");
		return n;
	}
	
	/**
	 * Check if we have a chain. If so, push it on the parser stack<br>
	 * chain := literal | '(' expr ')' | Identifier | Identifier '#' stringLiteral<br>
	 * 			| chain '.' Identifier | chain '.' identifier '#' stringLiteral<br>
	 *			| chain '[' arrayIndexList ']' | identifier PROPERTYQUALIFIER identifier<br>
	 *			| chain CLASSQUALIFIER identifier | identifier '(' arg-list ')'<br>
	 *			| propertyScope identifier | chain '.' propertyScope identifier <br>
	 *			| chain '.' propertyScope identifier '#' StringLiteral<br>
	 * @param p - parser state
	 * @return - true if we have a chain, false otherwise
	 */
	private boolean haveChain(ParserState p) {
		if(debug) entering(p,"haveChain");
		Node chain = null;
		boolean matched = false;
		/*
		 * chain = [ literal | '(' expr ')' 
		 * 		| identifier [ '.' identifier ] [ ['.' classPath ] '::' identifier [ '.' '*" ] ] [ '[' arrayIndex ']' ] ] ['#' StringLiteral]
		 * 		| identifier '.' '*'
		 * 		| identifier '(' arg-list ')'
		 */
		if(literal(p)){
			// chain := literal (note that literal will already push the item on the stack, if so)
			chain = p.stack.pop();
			if(debug) System.out.println("\t - Found Literal "+chain);
			matched = true;
		} else if(p.lookAheadToken.is(TokenType.LPAREN)){
			// chain = '(' expr ')'
			advanceOver(p,TokenType.LPAREN);
			chain = expr(p);
			advanceOver(p,TokenType.RPAREN);
			if(debug) System.out.println("\t - Found (expr) "+ (p.stack.isEmpty() ? "NULL" : p.stack.peek()));
			matched = true;
		} else if(p.lookAheadToken.is(TokenType.IDENTIFIER)){
			// chain = identifier
			chain = identifier(p);
			if(debug) System.out.println("\t - Found Identifier Cursor = "+p.cursor);
			matched = true;
			if(p.lookAheadToken.is(TokenType.LPAREN)){
				// chain = identifier '(' arg_list ')'
				List<Node> argList = argumentList(p);
				chain = Operator.FUNCTION.getNode(chain.getName());
				for(Node n : argList){
					chain.addChild(n);
				}
				p.stack.push(chain);
				if(debug) exiting(p,"haveChain");
				return true;
			} else if(p.lookAheadToken.is(TokenType.PERIOD)){
				// identifier  '.' 
				while(p.lookAheadToken.is(TokenType.PERIOD)){
					advanceOver(p,TokenType.PERIOD);
					if(p.lookAheadToken.is(TokenType.STAR)){
						// identifier '.' '*'
						advanceOver(p,TokenType.STAR);
						chain = Operator.IDENTIFIER.getNode(chain.getName()+".*");
						p.stack.push(chain);
						if(debug) exiting(p,"haveChain");
						return true;
					} else if(p.lookAheadToken.is(TokenType.IDENTIFIER)){
						// identifier '.' identifier
						Token t = advanceOver(p,TokenType.IDENTIFIER);
						chain = Operator.IDENTIFIER.getNode(chain.getName()+"."+t.value());
					} else {
						error(p,"'*' or identifier",ExceptionReason.INVALID_QUERY);
					}
				}
			}
			// check for property scope
			// (identifier | identifier(arglist) | identifier.identifier) [ ['.' classPath ] '::' identifier [ '.' '*' ] ]
			if(p.lookAheadToken.is(TokenType.SCOPE)){
				chain.addChild(propertyScope(p)); 
			}
			// check for array index
			// ((identifier | identifier(arglist) | identifier.identifier) 
			// [ ['.' classPath ] '::' identifier [ '.' '*' ] ]) [ '[' arrayIndexList ']' ]
			if(p.lookAheadToken.is(TokenType.LBRACKET)){
				// chain = identifier '[' arrayIndex ']'
				advanceOver(p,TokenType.LBRACKET);
				chain.addChild(arrayIndexList(p));
				advanceOver(p,TokenType.RBRACKET);
			}
		}
		if(p.lookAheadToken.is(TokenType.HASH)){
			// chain = [ chain ] '#' StringLiteral
			advanceOver(p,TokenType.HASH);
			Token t = advanceOver(p,TokenType.STRING_VALUE);
			Node en = Operator.ENUM.getNode(t.value());
			if(chain != null) {
				chain.addChild(en);
			} else {
				chain = en;
			}
			// if(chain != null) en.addChild(chain);
			// chain = en;
			matched = true;
		}
		if(matched) p.stack.push(chain);
		if(debug) exiting(p,"haveChain");
		return matched;
	}

	/**
	 * get a argument list
	 * @param p - parser state
	 * @return list of arguments. Empty if none defined
	 */
	private List<Node> argumentList(ParserState p){
		if(debug) entering(p,"argumentList");
		Vector<Node> args = new Vector<Node>();
		// argList := '(' *Exp *(, Exp) ')'
		advanceOver(p, TokenType.LPAREN);
		if(!p.lookAheadToken.is(TokenType.RPAREN)){
			args.add(expr(p));
			while(p.lookAheadToken.is(TokenType.COMMA)){
				advanceOver(p,TokenType.COMMA);
				args.add(expr(p));
			}
		}
		advanceOver(p,TokenType.RPAREN);
		if(debug) exiting(p,"argumentList");
		return args;
	}
	
	/**
	 * Get an array index list
	 * @param p - parser state
	 * @return - array index list
	 */
	private Node arrayIndexList(ParserState p){
		// Array-index-list	Array-index *("," Array-Index) | "*" | EMPTY
		if(debug) entering(p,"arrayIndexList");
		Node n = Operator.INDEX.getNode();
		n.addChild(arrayIndex(p));
		while(p.lookAheadToken.is(TokenType.COMMA)){
			advanceOver(p,TokenType.COMMA);
			n.addChild(arrayIndex(p));
		}
		if(debug) exiting(p,"arrayIndexList");
		return n;
	}
	
	/**
	 * arrayIndex :
	 * @param p - Parser state
	 * @return - INDEX node containing index value or '*'
	 */
	private Node arrayIndex(ParserState p){
		if(debug) entering(p,"arrayIndex");
		Node n = null;
		// Array-index-list	Array-index *("," Array-Index) | "*" | EMPTY
		// Array-index	Expr | Expr ".." [Expr] | [Expr] ".." Expr
		// Index := integer | '*' | [INTEGER] .. [INTEGER] | INTEGER *(, INTEGER)
		if(p.lookAheadToken.is(TokenType.RBRACKET)){
			n = Operator.CONSTANT.getNode(new DataValue("*"));	// empty list is treated as '*'
		} else if(p.lookAheadToken.is(TokenType.STAR)){
			advanceOver(p,TokenType.STAR);
			n = Operator.CONSTANT.getNode(new DataValue("*"));
		} else if(p.lookAheadToken.is(TokenType.RANGE)){
			// have RANGE expr ==> 0 .. expr
			advanceOver(p,TokenType.RANGE);
			n = Operator.RANGE.getNode();
			n.addChild(Operator.CONSTANT.getNode(new DataValue(0L)));
			n.addChild(expr(p));
		} else {
			// have expr [RANGE [expr]]
			Node left = expr(p);
			if(p.lookAheadToken.is(TokenType.RANGE)){
				advanceOver(p,TokenType.RANGE);
				n = Operator.RANGE.getNode();
				n.addChild(left);
				if(!p.lookAheadToken.is(TokenType.COMMA) && !p.lookAheadToken.is(TokenType.RBRACKET)){
					// have expr RANGE expr
					n.addChild(expr(p));
				} else {
					// have expr range ==> expr .. '*'
					n.addChild(Operator.CONSTANT.getNode(new DataValue("*")));
				}
			} else {
				n = left;
			}
		}
		if(n == null) error(p,"Index can only be integer value or *",ExceptionReason.INVALID_QUERY);
		if(debug) exiting(p,"arrayIndex");
		return n;
	}

	/**
	 * literal := stringLiteral | decimal-value | binary-value | hex-value | real-value | TRUE | FALSE | '{' [literal *( ',' literal )] '}'<br>
	 * If found, CONSTANT Node containing the literal value is pushed on stack
	 * @param p - parserStack
	 * @return - true if found a literal value, false otherwise
	 */
	private boolean literal(ParserState p){
		if(debug) entering(p,"literal");
		boolean matched = false;
		if(p.lookAheadToken.is(TokenType.STRING_VALUE)){
			// StringLiteral
			Token token = advanceOver(p,TokenType.STRING_VALUE);
			p.stack.push(Operator.CONSTANT.getNode(new DataValue(token.value())));
			matched = true;
		} else if(p.lookAheadToken.is(TokenType.NUMBER)){
			// decimal-value | binary-value | hex-value | real-value
			Token token = advanceOver(p,p.lookAheadToken.type());
			switch(token.type()){
			case BINARY:
				p.stack.push(Operator.CONSTANT.getNode(new DataValue(Long.parseLong(token.value(), 2))));
				break;
			case INTEGER:
				p.stack.push(Operator.CONSTANT.getNode(new DataValue(Long.parseLong(token.value()))));
				break;
			case HEX:
				p.stack.push(Operator.CONSTANT.getNode(new DataValue(Long.parseLong(token.value(), 16))));
				break;
			case REAL:
				p.stack.push(Operator.CONSTANT.getNode(new DataValue(Double.parseDouble(token.value()))));
				break;
			default:
				error(p,"Internal error - number format "+token.type()+" is not implemented");
			}
			matched = true;
		} else if(p.lookAheadToken.is(TokenType.BOOLEAN)){
			// true-value | false-value
			Token token = advanceOver(p,TokenType.BOOLEAN);
			p.stack.push(Operator.CONSTANT.getNode(new DataValue(Boolean.parseBoolean(token.value()))));
			matched = true;
		} else if(p.lookAheadToken.is(TokenType.LBRACE)){
			// arrayLiteral = '{' [literal *(,literal)] '}'
			advanceOver(p,TokenType.LBRACE);
			if(!p.lookAheadToken.is(TokenType.RBRACE)){
				Vector<String> values = new Vector<String>();
				TokenType tokenType = p.lookAheadToken.type();
				values.add(p.lookAheadToken.value());
				advanceOver(p,tokenType);
				while(p.lookAheadToken.is(TokenType.COMMA)){
					advanceOver(p,TokenType.COMMA);
					values.add(advanceOver(p,tokenType).value());
				}
				DataType dt = tokenType == TokenType.STRING_VALUE ? DataType.STRING_ARRAY : tokenType == TokenType.BOOLEAN ? DataType.BOOLEAN_ARRAY :
					tokenType == TokenType.REAL ? DataType.REAL64_ARRAY : DataType.SINT64_ARRAY;
				// System.out.println(dt+" "+dt.getClassForType());
				Object dataValues = Array.newInstance(dt.getClassForType().getComponentType(), values.size());
				for(int i = 0; i < values.size(); i++){
					switch(tokenType){
					case BOOLEAN:
						Array.set(dataValues, i, Boolean.parseBoolean(values.get(i)));
						break;
					case BINARY:
						Array.set(dataValues, i, Long.parseLong(values.get(i), 2));
						break;
					case INTEGER:
						Array.set(dataValues, i, Long.parseLong(values.get(i)));
						break;
					case HEX:
						Array.set(dataValues, i, Long.parseLong(values.get(i), 16));
						break;
					case REAL:
						Array.set(dataValues, i, Double.parseDouble(values.get(i)));
						break;
					case STRING_VALUE:
						Array.set(dataValues, i, values.get(i));
						break;
					default:
						error(p,"Internal error - literal array format "+tokenType+" is not implemented");
					}
				}
				p.stack.push(Operator.CONSTANT.getNode(new DataValue(dt,dataValues)));
			} else {
				// literal = '{' '}'
				p.stack.push(Operator.CONSTANT.getNode(new DataValue(DataType.VOID,null)));
			}
			advanceOver(p,TokenType.RBRACE);
			matched = true;
		}
		if(debug) exiting(p,"literal");
		return matched;
	}

	/**
	 * get propertyScope
	 * @param p - Parser stack
	 * @return - true if found, false otherwise
	 */
	private Node propertyScope(ParserState p){
		// '::' identifier [ '.' '*' ] ]
		if(debug) entering(p,"propertyScope");
		Node n = Operator.SCOPE.getNode();
		advanceOver(p,TokenType.SCOPE);
		Token identifier = advanceOver(p,TokenType.IDENTIFIER);
		if(p.lookAheadToken.is(TokenType.PERIOD)){
			advanceOver(p,TokenType.PERIOD);
			advanceOver(p,TokenType.STAR);
			n.addChild(Operator.SCOPE.getNode(identifier.value()+".*"));
		} else {
			n.addChild(Operator.SCOPE.getNode(identifier.value()));
		}
		if(debug) exiting(p,"propertyScope");
		return n;
	}

	/**
	 * classPath = ['/' identifier *('/' identifier) ':' ] className
	 * @param p - Parser State 
	 * @return - CLASS_PATH Node containing classpath
	 */
	private Node classPath(ParserState p){
		if(debug) entering(p,"classPath");
		Vector<String> pathNames = new Vector<String>();
		if(p.lookAheadToken.is(TokenType.SLASH)){
			while(p.lookAheadToken.is(TokenType.SLASH)){
				advanceOver(p,TokenType.SLASH);
				Token pathName = advanceOver(p,TokenType.IDENTIFIER);
				pathNames.add(pathName.value());
			}
			advanceOver(p,TokenType.COLON);
		}
		Node cp = className(p);
		if(!pathNames.isEmpty()){
			StringBuilder b = new StringBuilder();
			for(String pathName : pathNames){
				b.append("/");
				b.append(pathName);
			}
			b.append(":");
			b.append(cp.getName());
			if(debug) exiting(p,"classPath");
			return Operator.CLASS_PATH.getNode(b.toString());
		}
		if(debug) exiting(p,"classPath");
		return cp;
	}

	/**
	 * className = identifier
	 * @param p - parser state
	 * @return - CLASS_PATH node containing the class name
	 */
	private Node className(ParserState p){
		if(debug) entering(p,"className");
		Node className = identifier(p);
		String name = className.getName();
		if(!name.matches("^([a-zA-Z0-9])+_([a-zA-Z0-9])+$")){
			error(p,"Expected className of form schema_name, found "+name,ExceptionReason.TYPE_MISMATCH);
		}
		if(debug) exiting(p,"className");
		return Operator.CLASS_PATH.getNode(name);
	}

	/**
	 * identifier = identifier-start, *( identifier-subsequent )<br/>
	 * identifier-start = UNICODE-S1<br/>
	 * identifier-subsequent = identifier-start | DECIMAL-DIGIT<br>
	 * @param p - Parser state
	 * @return - IDENTIFIER | VARIABLE with name given by the incoming identifier
	 */
	private Node identifier(ParserState p){
		if(debug) entering(p,"identifier");
		Token t =  advanceOver(p,TokenType.IDENTIFIER);
		// Node id =  p.aliases.containsKey(t.value()) ? p.aliases.get(t.value()) : new Node(Operator.IDENTIFIER,t.value());
		Node id = t.value().matches("^\\$.+\\$$") ? Operator.VARIABLE.getNode(t.value()) : Operator.IDENTIFIER.getNode(t.value());
		if(debug){
			System.out.println(indent.toString()+"-- Token "+t+" returning Identifier "+id);
		}
		if(debug) exiting(p,"identifier");
		return id;
	}

	/**
	 * Advance over the lookahead token without pushing it on the stack, and read the next token from the input.
	 * Note that if the token type is a subtype of the expected type, it is NOT reset to be the expected type
	 * @param expected - Expected token type to advance over
	 * @return - Expected token, if successful
	 * @throws Exception - if the lookahead token does not match the expected token
	 */
	private Token advanceOver(ParserState p, TokenType expected) {
		Token t = p.lookAheadToken;
		if(t.is(expected)){
			if(debug) System.out.println(indent.toString()+"AdvanceOver "+expected+" [ "+t.type()+", "+t.value()+" ]");
			nextToken(p);
			return t;
		} else {
			error(p,expected.toString());
		}
		return null;
	}

	/**
	 * Throw an error. This method throws an appropriately constructed Exception
	 * @param expected - input expected by parser
	 * @param reason - reason for exception
	 * @throws Exception - Resulting Exception
	 */

	private void error(ParserState p, String expected, ExceptionReason reason) {
		String msg = "Cursor "+p.cursor+
				" Expected "+expected+" Found "+p.lookAheadToken.type()+"("+p.lookAheadToken.value()+")";
		throw new ModelException(reason,msg);
	}

	private void error(ParserState p, String message){
		throw new ModelException("Cursor "+p.cursor+":"+message);
	}

	// debugging helpers
	
	private StringBuilder indent = new StringBuilder("");

	private void entering(ParserState p, String where){
		System.out.println(indent.toString()+"Enter "+where+" cursor = "+p.cursor+" lookAhead = "+p.lookAheadToken);
		indent.append(" ");
		return;
	}

	private void exiting(ParserState p, String where){
		if(indent.length() > 0) indent.setLength(indent.length()-1);
		System.out.println(indent.toString()+"Exit "+where+" cursor = "+p.cursor+" lookAhead = "+p.lookAheadToken);
		return;
	}
	
	// Tokenizer methods

	/**
	 * Read in the next non whitespace token from the query and set the lookahead token to it
	 * @param p - parser state
	 */
	private void nextToken(ParserState p){
		// read the next Token, and skip any comments and white spaces
		readToken(p);
		while(p.lookAheadToken.is(TokenType.WHITE_SPACE) || p.lookAheadToken.is(TokenType.COMMENT)){
			// if(debug) System.out.println("// ---> "+p.lookAheadToken.value());
			readToken(p);
		}
		if(debug) System.out.println(indent.toString()+"LookAhead: "+p.lookAheadToken.type()+" ["+p.lookAheadToken.value()+"]");
		return;
	}

	/**
	 * Read in the next token from the input, and place it in the lookAheadToken. Cursor is moved beyond the token
	 * @param p - parser state
	 */
	private void readToken(ParserState p){
		// check if we are done
		if(p.cursor >= p.queryLength){
			p.lookAheadToken = new Token(TokenType.EOF, null);
			return;
		}
		// check for white space
		if(Character.isWhitespace(p.query.charAt(p.cursor))){
			// have white space at the current position
			int savedLoc = p.cursor;	// save this location and scan past white space
			while(++p.cursor < p.queryLength && Character.isWhitespace(p.query.charAt(p.cursor)));
			p.lookAheadToken = new Token(TokenType.WHITE_SPACE,p.query.substring(savedLoc,p.cursor));
			return;
		}
		// scan for next token
		char c = p.query.charAt(p.cursor++);
		switch(c){
		// single character tokens
		case ',':
			p.lookAheadToken = new Token(TokenType.COMMA);
			return;
		case '=':
			p.lookAheadToken = new Token(TokenType.EQUALS);
			return;
		case '{':
			p.lookAheadToken = new Token(TokenType.LBRACE);
			return;
		case '}':
			p.lookAheadToken = new Token(TokenType.RBRACE);
			return;
		case '(':
			p.lookAheadToken = new Token(TokenType.LPAREN);
			return;
		case ')':
			p.lookAheadToken = new Token(TokenType.RPAREN);
			return;
		case '*':
			p.lookAheadToken = new Token(TokenType.STAR);
			return;
		case '[':
			p.lookAheadToken = new Token(TokenType.LBRACKET);
			return;
		case ']':
			p.lookAheadToken = new Token(TokenType.RBRACKET);
			return;
		case ';':
			p.lookAheadToken = new Token(TokenType.SEMICOLON);
			return;
		case '#':
			p.lookAheadToken = new Token(TokenType.HASH);
			return;
		case '/':
			p.lookAheadToken = new Token(TokenType.SLASH);
			return;
		case '+':
			p.lookAheadToken = new Token(TokenType.PLUS);
			return;
		case '-':
			p.lookAheadToken = new Token(TokenType.MINUS);
			return;
		// one or two character tokens
		case '.':
			p.lookAheadToken = new Token(TokenType.PERIOD);
			if(p.cursor < p.queryLength && p.query.charAt(p.cursor) == '.'){	// check for '..'
				p.lookAheadToken = new Token(TokenType.RANGE);
				p.cursor++;
			}
			return;
			
		case ':':
			p.lookAheadToken = new Token(TokenType.COLON);
			if(p.cursor < p.queryLength && p.query.charAt(p.cursor) == ':'){	// check for '::'
				p.lookAheadToken = new Token(TokenType.SCOPE);
				p.cursor++;
			}
			return;
		case '<':
			if(p.cursor < p.queryLength){
				switch(p.query.charAt(p.cursor)){
				case '=':
					p.lookAheadToken = new Token(TokenType.LE);
					p.cursor++;
					return;
				case '>':
					p.lookAheadToken = new Token(TokenType.NE);
					p.cursor++;
					return;
				default:
					p.lookAheadToken = new Token(TokenType.LT);
					return;
				}
			}
			p.lookAheadToken = new Token(TokenType.LT);
			return;
		case '>':
			if(p.cursor < p.queryLength){
				switch(p.query.charAt(p.cursor)){
				case '=':
					p.lookAheadToken = new Token(TokenType.GE);
					p.cursor++;
					return;
				default:
					p.lookAheadToken = new Token(TokenType.GT);
					return;
				}
			}
			p.lookAheadToken = new Token(TokenType.GT);
			return;
		// two character tokens
		case '|':
			if(p.cursor < p.queryLength && p.query.charAt(p.cursor) == '|' ){
				p.lookAheadToken = new Token(TokenType.CONCAT);
				p.cursor++;
				return;
			}
			p.lookAheadToken = new Token(TokenType.ERROR);
			return;
			// string value
		case '\'':
			int savedLoc = p.cursor-1;
			boolean foundEoS = false;
			StringBuilder b = new StringBuilder();
			while(!foundEoS && p.cursor < p.queryLength){
				switch(p.query.charAt(p.cursor)){
				case '\'':
					// EOS reached
					p.lookAheadToken = new Token(TokenType.STRING_VALUE,b.toString());
					p.cursor++;
					foundEoS = true;
					break;
				case '\\':
					b.append(getQuotedCharacter(p));
					break;
				default:
					b.append(p.query.charAt(p.cursor++));		
				}
			}
			if(!foundEoS){
				// have an unterminated String
				error(p,"Unterminated string at position "+savedLoc);
			}
			break;
		default:
			p.cursor--;	// rewind cursor

			boolean found = false;
			if(c == '+' || c == '-' || Character.isDigit(c)){
				// numeric value
				found = getNumericValue(p);
			} else {
				found = getIdentifier(p);
				// check known tokens. Swap in lookahead token if found
				String value = p.lookAheadToken.value();
				for(TokenType t : TokenType.getTokenTypes()){
					if(value.equalsIgnoreCase(t.getValue())){
						p.lookAheadToken = new Token(t,value);
						break;
					}
				}
			}
			if(!found){
				p.cursor++;
				p.lookAheadToken = new Token(TokenType.CHARACTER,String.valueOf(c));
			}
			return;
		}
	}

	/**
	 * Obtain a numeric value, and update lookAheadToken<br>
	 * Numerical values include BINARY, REAL, HEX, INTEGER<br/>
	 * numeric-value = binary-value | hex-digit-value | unsigned-integer | decimal-value | real-value | exact-numeric<br/>
	 * sign = "+" | "-"<br/>
	 * binary-digit = "0" | "1"<br/>
	 * binary-value = [sign] 1*( binary-digit ) "B"<br/>
	 * decimal-digit = binary-digit | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"<br/>
	 * hex-digit = decimal-digit | "A" | "B" | "C" | "D" | "E" | "F"<br/>
	 * hex-digit-value = [sign] "0X" 1*( hex-digit )<br/>
	 * unsigned-integer = 1*( decimal-digit )<br/>
	 * decimal-value = [sign] unsigned-integer<br/>
	 * exact-numeric = unsigned-integer "." [unsigned-integer] | "." unsigned-integer<br/>
	 * real-value = [sign] exact-numeric ["E" decimal-value]<br/>
	 * @return - true if a numeric value was found, false otherwise
	 */
	private boolean getNumericValue(ParserState p){
		// System.out.println(p.query);
		boolean isHex = false, isDecimal = false, isBoolean = false;
		boolean signPossible = true, boolPossible = true, needDigit = true;
		int cursor = p.cursor;
		StringBuilder b = new StringBuilder();	
		while(cursor < p.queryLength){
			// System.out.println(b.toString() + " " + p.query.charAt(cursor) + " " + p.query.substring(cursor+1, p.queryLength));
			char c = p.query.charAt(cursor);
			if(Character.isDigit(c)){
				b.append(c);
				signPossible = needDigit = false;
				if(boolPossible && (c != '0' && c != '1')) {
					boolPossible = false;
				}
			} else if(isHex && (c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f')){
				b.append(c);
				needDigit = signPossible = boolPossible = false;
			} else if((c == 'x' || c == 'X')){
				if((b.length() == 1 && b.charAt(0) == '0') ||
						(b.length() == 2 && b.charAt(1) == '0' && 
						(b.charAt(0) == '+' || b.charAt(0) == '-'))){
					b.setLength(b.length()-1);	// remove the leading '0'
					// b.append(c);
					needDigit = isHex = true;
					signPossible = boolPossible = false;
				} else {
					break;
				}
			} else if(c == '.'){
				// check for index marker (case int..int)
				if(cursor < p.queryLength-1 && p.query.charAt(cursor+1) == '.'){
					// signPossible = boolPossible = isHex = false;
					// p.cursor--;
					break;
				}
				// check for decimal rules
				if(isHex || isDecimal) break;
				b.append(c);
				signPossible = boolPossible = isHex = false;
				needDigit = isDecimal = true;
			} else if(signPossible && (c == '+' || c == '-')){
				// sign can only be at the beginning or after (e | E)
				b.append(c);
				signPossible = false;
			} else if(!needDigit && isDecimal && (c == 'E' || c == 'e')){
				b.append(c);
				signPossible = needDigit = true;
			} else if(boolPossible && (c == 'B' || c == 'b')){
				// boolean termination reached
				if(boolPossible){
					isBoolean = true;
				// b.append(c);
					cursor++;
				}
				break;
			} else {
				// reached a non-number character
				break;
			}
			cursor++;
		}
		if(cursor > p.cursor){
			if(b.length() == 1 && b.charAt(0) == '.') return false;
			p.cursor = cursor;
			if(isBoolean){
				p.lookAheadToken = new Token(TokenType.BINARY, b.toString());
			} else if(isDecimal){
				p.lookAheadToken = new Token(TokenType.REAL, b.toString());
			} else if(isHex){
				p.lookAheadToken = new Token(TokenType.HEX, b.toString());
			} else {
				p.lookAheadToken = new Token(TokenType.INTEGER, b.toString());
			}
			return true;
		}
		return false;
	}

	/**
	 * Obtain an identifier. An identifier follows the same rules as a Java identifier. Returns
	 * true if an identifier was found (in lookAheadToken), false otherwise. Cursor is advanced to just
	 * beyond the identifier, if found. If not found, the cursor is not advanced
	 */
	private boolean getIdentifier(ParserState p){
		char c;
		StringBuilder b = new StringBuilder();
		if(Character.isJavaIdentifierStart((c = p.query.charAt(p.cursor)))){
			b.append(c);
			p.cursor++;
			while(p.cursor < p.queryLength && 
					Character.isJavaIdentifierPart(c=p.query.charAt(p.cursor))){
				b.append(c);
				p.cursor++;
			}
			p.lookAheadToken = new Token(TokenType.IDENTIFIER,b.toString());
			return true;	
		}
		return false;
	}

	/**
	 * Get a (possibly) backslash-quoted character. The sequences recognized are:<br>
	 * \b - \x0008: backspace BS<br>
	 * \t - \x0009: horizontal tab HT<br>
	 * \n - \x000A: linefeed LF<br>
	 * \f - \x000C: form feed FF<br>
	 * \r - \x000D: carriage return CR<br>
	 * \" - \x0022: double quote "<br>
	 * \' - \x0027: single quote '<br>
	 * \\ - \x005C: backslash \<br>
	 * \x<hex> // where <hex> is one to four hex digits<br>
	 * \X<hex> // where <hex> is one to four hex digits<br>
	 * In all other cases, the \ is ignored, and the subsequent character is returned.
	 * @return character value. The cursor is set just past the character (at the terminating ')
	 */
	private char getQuotedCharacter(ParserState p){
		char c = p.query.charAt(p.cursor++);	// get character value and advance
		if(c != '\\') return c;					// if no backslash, we are done	
		c = p.query.charAt(p.cursor++);		// get character after backslash and advance
		switch(c){
		case 'b':
			return '\b';
		case 't':
			return '\t';
		case 'n':
			return '\n';
		case 'f':
			return '\f';
		case 'r':
			return '\r';
		case '\\':
			return '\\';
		case '\'':
			return '\'';
		case '"':
			return '"';
		case 'x':
			if(p.cursor >= p.queryLength) return 0;	// have a \x terminated query, return \x0000
			int startPosition = p.cursor;			// startPosition is first character after \x
			for(int i=0; i<4; i++){					// pick up to four hex characters
				c = p.query.charAt(p.cursor++);		// next character
				// if we reached a non-Hex character, break
				if(!(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F')) break;
				if(p.cursor >= p.queryLength) break;	// went past the end of line
			}
			return (char) Integer.parseInt(p.query.substring(startPosition,p.cursor), 16);
		default:
			return c;		// for all others, a '\c' is simply a 'c'
		}
	}
}
