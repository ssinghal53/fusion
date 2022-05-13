/**
 * Copyright 2021, Sharad Singhal, All Rights Reserved
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
 * Created Dec 4, 2021 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.reflect.Array;
import java.util.List;
import java.util.Stack;
import java.util.Vector;

/**
 * Parser for Filter Query Language. See DMTF DSP0212
 * @author Sharad Singhal
 */
class FqlParser {
	/**
	 * Enumeration to manage lexical token types for FQL
	 */
	enum TokenType {
		COLON(":"),
		COMMA(","), 
		EOF(null),
		EQ("="),
		HASH("#"),
		LBRACE("{"),
		LBRACKET("["),
		LPAREN("("),
		MINUS("-"),
		NE("<>"),
		NOT("not"),
		NULL("null"),
		PERIOD("."),
		PLUS("+"),
		RANGE(".."),
		RPAREN(")"),
		RBRACE("}"),
		RBRACKET("]"),
		SCOPE("::"), 
		SEMICOLON(";"),
		SLASH("/"),		
		GT(">"),
		LT("<"),
		GE(">="),
		LE("<="),
		LIKE("like"),
		IDENTIFIER(null),
		STAR("*"),
		CONCAT("||"),
		ERROR(null),
		STRING_VALUE(null),
		OR("or"),
		AND("and"),
		ANY("any"),
		EVERY("every"), 
		BINARY(null),
		REAL(null),
		HEX(null),
		INTEGER(null),
		CHARACTER(null),
		NUMBER(null),
		BOOLEAN(null),
		SIGN(null),
		COMPARISON(null),
		ISA("isa"),
		IS("is"), 
		IN("in"),
		SATISFIES("satisfies"),
		FALSE("false"),
		TRUE("true");
		/** Token value */
		private final String value;
		/**
		 * Create Token type
		 * @param value - string value of token
		 */
		TokenType(String value) {
			this.value = value;
			return;
		}
		/**
		 * Get all token types
		 * @return array containing all token types
		 */
		static TokenType[] getTokenTypes() {
			return TokenType.class.getEnumConstants();
		}
		/**
		 * Get the String value of the token type
		 * @return value of this token type
		 */
		String getValue() {
			return value;
		}
	}
	/**
	 * Class to manage lexical tokens
	 */
	class Token {
		/** Type of the token */
		private TokenType type;
		/** value of the token, if any */
		private String value;
		/**
		 * Create a token with the given type
		 * @param type - type of the token
		 */
		public Token(TokenType type) {
			this.type = type;
			return;
		}
		/**
		 * Create a token with the given type and value
		 * @param type - token type
		 * @param value - token value
		 */
		public Token(TokenType type, String value) {
			this.type = type;
			this.value = value;
			return;
		}
		/**
		 * Check if this token matches an expected token
		 * @param expected - type of the token expected
		 * @return - true if this token matches the expected token, false otherwise
		 */
		public boolean is(TokenType expected) {
			switch(expected) {
			case NUMBER:
				return isInteger() || isReal();
			case SIGN:
				return type == TokenType.PLUS || type == TokenType.MINUS;
			case COMPARISON:
				return type == TokenType.EQ || type == TokenType.NE || type == TokenType.GT || type == TokenType.GE ||
				type == TokenType.LT || type == TokenType.LE || type == TokenType.LIKE;
			case BOOLEAN:
				return type == TokenType.TRUE || type == TokenType.FALSE;
			default:
				return type == expected;
			}
		}
		/**
		 * Check if the token represents a real number
		 * @return true if the token represents a real
		 */
		public boolean isReal() {
			return type == TokenType.REAL;
		}
		/**
		 * Check if the token represents an integer
		 * @return true if the token represents an integer
		 */
		public boolean isInteger() {
			return type == TokenType.INTEGER | type == TokenType.HEX || type == TokenType.BINARY;
		}
		/**
		 * Get the type of the token
		 * @return - type of the token
		 */
		public TokenType getType() {
			return type;
		}
		/**
		 * Get the value of the token
		 * @return - value of the token (may be null)
		 */
		public String getValue() {
			return value;
		}
		/**
		 * Reset the type of the token
		 * @param type - new type for the token
		 */
		public void setType(TokenType type) {
			this.type = type;
			return;
		}
		@Override
		public String toString() {
			StringBuilder b = new StringBuilder();
			b.append(type);
			if(value != null) {
				b.append(" [").append(value).append("]");
			}
			return b.toString();
		}
		
	}

	/** debugging flag */
	private boolean debug = false;
	/** CQL Query being parsed */
	String query = null;
	/** Length of the query */
	int queryLength = 0;
	/** Character cursor within the query */
	int cursor = 0;
	/** List of tokens in the query */
	private Vector<Token> tokens = new Vector<Token>();
	/** cursor for the next token in the query */
	private int currentToken = 0;
	/** Stack to hold tokens during parsing */
	private Stack<FqlNode> stack = new Stack<FqlNode>();
	/** Look ahead token for methods which have alternatives */
	private Token lookAheadToken;
	/** root node for the parsed tree */
	private FqlNode statement = null;

	/**
	 * Create a new instance of a Filter Query Language parser
	 */
	FqlParser(String query) {
		// start = select-statement
		this.query = query;
		queryLength = query.length();
		if(debug) System.out.println("*** Parsing ***\n"+query+"\n****");
		try {
			Token t;
			do {
				t = nextToken();
				tokens.add(t);
			} while(!t.is(TokenType.EOF));
			if(debug) System.out.println(tokens);
			if(tokens.size() > 1) statement = expr();
		} catch (Exception e){
			if(e instanceof ModelException){
				throw e;
			} else {
				throw new ModelException(ExceptionReason.INVALID_QUERY, "Invalid query: "+query,e);
			}
		}
		if(!lookAhead().is(TokenType.EOF)) throw new ModelException(ExceptionReason.INVALID_QUERY, "Incomplete parse: "+query);
		return;
	}

	/**
	 * Get the parse tree from the parser
	 * @return - root node for the parse tree. Null for an empty tree
	 */
	public FqlNode getParseTree() {
		return statement;
	}

	/**
	 * Get the look-ahead token (do not advance cursor)
	 * @return - look ahead token
	 */
	private Token lookAhead() {
		return tokens.elementAt(currentToken);
	}
	
	/**
	 * Advance over the lookahead token without pushing it on the stack and read next token.
	 * Note that if the token type is a subtype of the expected type, it is NOT reset to be the expected type
	 * @param expected - Expected token type to advance over
	 * @return - Expected token, if successful
	 * @throws Exception - if the lookahead token does not match the expected token
	 */
	private Token advanceOver(TokenType expected) {
		Token t = lookAhead();
		if(t.is(expected)){
			if(debug) System.out.println("AdvanceOver "+expected+" found [ "+t.getType()+", "+t.getValue()+" ]");
			currentToken++;
			return t;
		}
		throw new ModelException("AdvanceOver Failed: Expected "+expected+" at Token "+currentToken+" found [ "+t.getType()+", "+t.getValue()+" ]");
	}
	
	/**
	 * Debug method called at method entry
	 * @param s - Method name
	 */
	private void enter(String s) {
		System.out.println("Enter- "+s+" LookAhead "+lookAhead());
	}
	/**
	 * Debug method called at method exit
	 * @param s - method name
	 * @param n - root of the subtree at to be shown
	 */
	private void exit(String s, FqlNode n) {
		System.out.println("Exit-"+s+" LookAhead "+lookAhead());
		System.out.println(n.toTree(s+" "));
	}

	/**
	 * Expression := ORExpression
	 * ORExpression = AndExpression *( OR AndExpression)
	 * @return - FqlNode containing orExpression
	 */
	private FqlNode expr() {
		if(debug) enter("ORExpr");
		FqlNode n = andExpression();
		// Optimize OR(A, OR(B,C)) --> OR(A,B,C)
		while(lookAhead().is(TokenType.OR)){
			advanceOver(TokenType.OR);
			if(n.getOperator() != FqlOperator.OR){
				FqlNode op = FqlOperator.OR.getFqlNode();
				op.addChild(n);
				op.addChild(andExpression());
				n = op;
			} else {
				n.addChild(andExpression());
			}
		}
		if(debug) exit("ORExpr",n);
		return n;
	}

	/**
	 * AndExpression := NotExpression *(AND NotExpression)
	 * @return - FqlNode containing AndExpression
	 */
	private FqlNode andExpression() {
		if(debug) enter("ANDExpr");
		FqlNode n = notExpression();
		// Optimize AND(A,AND(B,C)) --> AND(A,B,C)
		while(lookAhead().is(TokenType.AND)){
			advanceOver(TokenType.AND);
			if(n.getOperator() != FqlOperator.AND){
				FqlNode op = FqlOperator.AND.getFqlNode();
				op.addChild(n);
				op.addChild(notExpression());
				n = op;
			} else {
				n.addChild(notExpression());
			}
		}
		if(debug) exit("AndExpr",n);
		return n;
	}

	/**
	 * NotExpression := [NOT] CompareExpression
	 * @param p - Parser State
	 * @return - FqlNode containing NOT expression
	 */
	private FqlNode notExpression() {
		if(debug) enter("NOTExpr");
		FqlNode n = null;
		if(lookAhead().is(TokenType.NOT)){
			advanceOver(TokenType.NOT);
			n = FqlOperator.NOT.getFqlNode();
		}
		if(n == null){
			n = arrayComp();
		} else {
			n.addChild(arrayComp());
		}
		if(debug) exit("NotExpr",n);
		return n;
	}

	/**
	 * ArrayCompare := (ANY | EVERY) comp | (ANY | EVERY) Identifier IN expr SATISFIES '(' comp ')'
	 * @return - return FqlNode array compare expression
	 */
	private FqlNode arrayComp() {
		if(debug) enter("AComp");
		FqlNode n = null;
		if(lookAhead().is(TokenType.ANY) || lookAhead().is(TokenType.EVERY)){
			Token arrayOp = advanceOver(lookAhead().getType());
			FqlNode compOrIdentifier = comp();
			if(compOrIdentifier.getOperator() == FqlOperator.IDENTIFIER && lookAhead().is(TokenType.IN)){
				// ANY | EVERY identifier IN expr SATISFIES ( comp ) 
				advanceOver(TokenType.IN);
				FqlNode expr = expr();
				advanceOver(TokenType.SATISFIES);
				advanceOver(TokenType.LPAREN);
				FqlNode comp = expr();
				advanceOver(TokenType.RPAREN);
				n = FqlOperator.valueOf(arrayOp.getType().toString()).getFqlNode(compOrIdentifier.getName());
				n.addChild(expr);
				FqlNode op = n;
				n = FqlOperator.SATISFIES.getFqlNode();
				n.addChild(op);
				n.addChild(comp);
			} else {
				n = FqlOperator.valueOf(arrayOp.getType().toString()).getFqlNode();
				n.addChild(compOrIdentifier);
			}
		} else {
			n = comp();
		}
		if(debug) exit("Acomp",n);
		return n;
	}

	/**
	 * Comparison FqlOperators
	 * comp := arith | arith IS [NOT] Null | arith ISA identifier
	 *         | arith (GT | GE | EQ | NE | LE | LT | [NOT] LIKE) arith
	 * @return FqlNode containing comp tree
	 */
	private FqlNode comp(){
		if(debug) enter("Comp");
		// Note: DSP0212 defines [NOT] LIKE, but NOT is not in CQL
		FqlNode n = arith();
		if(lookAhead().is(TokenType.NOT)) {
			advanceOver(TokenType.NOT);
			advanceOver(TokenType.LIKE);
			FqlNode op = FqlOperator.NOTLIKE.getFqlNode();
			op.addChild(n);
			op.addChild(arith());
			n = op;
		} else if(lookAhead().is(TokenType.COMPARISON)){
			Token t = advanceOver(TokenType.COMPARISON);
			FqlNode op = FqlOperator.valueOf(t.getType().toString()).getFqlNode();
			op.addChild(n);
			op.addChild(arith());
			n = op;
		} else if(lookAhead().is(TokenType.IS)){ // arith IS [NOT] NULL
			boolean isNot = false;
			advanceOver(TokenType.IS);
			if(lookAhead().is(TokenType.NOT)){
				isNot = true;
				advanceOver(TokenType.NOT);
			}
			advanceOver(TokenType.NULL);
			FqlNode op = isNot ? FqlOperator.ISNOTNULL.getFqlNode() : FqlOperator.ISNULL.getFqlNode();
			op.addChild(n);
			n = op;
		} else if(lookAhead().is(TokenType.ISA)){ // arith ISA identifier
			advanceOver(TokenType.ISA);
			Token className = advanceOver(TokenType.IDENTIFIER);
			FqlNode op = FqlOperator.ISA.getFqlNode(className.getValue());
			op.addChild(n);
			n = op;
		}
		if(debug) exit("Comp",n);
		return n;
	}

	/**
	 * Arith := term *( ('+' | '-') term)
	 * @return FqlNode containing arith
	 */
	private FqlNode arith(){
		if(debug) enter("Arith");
		FqlNode n = term();
		while(lookAhead().is(TokenType.PLUS) || lookAhead().is(TokenType.MINUS)){
			if(lookAhead().is(TokenType.PLUS)){
				advanceOver(TokenType.PLUS);
				FqlNode op = FqlOperator.ADD.getFqlNode();
				op.addChild(n);
				n = op;
			} else {
				advanceOver(TokenType.MINUS);
				FqlNode op = FqlOperator.SUBTRACT.getFqlNode();
				op.addChild(n);
				n = op;
			}
			n.addChild(term());
		}
		if(debug) exit("Arith",n);
		return n;
	}


	/**
	 * Term := factor *( ('*'|'/') factor)
	 * @return - FqlNode containing factor 
	 */
	private FqlNode term(){
		if(debug) enter("Term");
		FqlNode n = factor();
		while(lookAhead().is(TokenType.STAR) || lookAhead().is(TokenType.SLASH)){
			if(lookAhead().is(TokenType.STAR)){
				advanceOver(TokenType.STAR);
				FqlNode op = FqlOperator.MULTIPLY.getFqlNode();
				op.addChild(n);
				n = op;
			} else {
				advanceOver(TokenType.SLASH);
				FqlNode op = FqlOperator.DIVIDE.getFqlNode();
				op.addChild(n);
				n = op;
			}
			n.addChild(factor());
		}
		if(debug) exit("Term",n);
		return n;
	}

	/**
	 * unary '+' or '-'
	 * Factor := ['+' | '-'] concat
	 * @return - FqlNode containing factor
	 */
	private FqlNode factor(){
		if(debug) enter("Factor");
		FqlNode n = null;
		if(lookAhead().is(TokenType.SIGN)){
			Token t = advanceOver(TokenType.SIGN);
			n = (t.getType() == TokenType.PLUS) ? FqlOperator.SIGN.getFqlNode("+") : FqlOperator.SIGN.getFqlNode("-");			
		}
		FqlNode concat = concat();
		if(n == null)
			n = concat;
		else
			n.addChild(concat);
		if(debug) exit("Factor",n);
		return n;
	}
	
	/**
	 * Concatenation 
	 * Concat := Chain | *( '||' Chain)
	 * Get a concatenated chain
	 * @return - FqlNode with chain or concat
	 */
	private FqlNode concat() {
		if(debug) enter("Concat");
		FqlNode left = chain();
		if(lookAhead().is(TokenType.CONCAT)) {
			FqlNode n = FqlOperator.CONCAT.getFqlNode();
			n.addChild(left);
			while(lookAhead().is(TokenType.CONCAT)) {
				advanceOver(TokenType.CONCAT);
				n.addChild(chain());
			}
			if(debug) exit("Concat",n);
			return n;
		}
		if(debug) exit("Concat",left);
		return left;
	}
	
	/**
	 * Property identifier
	 * Property := literal<br>
	 * 	| '(' expr ')'
	 *  | classPath ('.' propertyIdentifier )+
	 *  | propertyIdentifier ('.' chain)*
	 * 
	 * @return FqlNode with property
	 */
	private FqlNode chain() {
		if(debug) enter("Chain");
		FqlNode chain = null;
		if(literal()) {
			chain = stack.pop();
			if(debug) exit("Chain",chain);
			return chain;
		} else if(lookAhead().is(TokenType.LPAREN)) {
			advanceOver(TokenType.LPAREN);
			chain = expr();
			advanceOver(TokenType.RPAREN);
			if(debug) exit("Chain",chain);
			return chain;
		} else if(lookAhead().is(TokenType.SLASH)) {
			chain = classPath();
			advanceOver(TokenType.PERIOD);
			chain.addChild(propertyIdentifier());
		} else {
			chain = propertyIdentifier();
		}
		while(lookAhead().is(TokenType.PERIOD)) {
			advanceOver(TokenType.PERIOD);
			FqlNode n = FqlOperator.PERIOD.getFqlNode();
			n.addChild(propertyIdentifier());
			chain.addChild(n);
		}
		if(debug) exit("Chain",chain);
		return chain;
	}
	
	/**
	 * Property Identifier
	 * propIdent := identifier
	 * 		| identifier '[' index range ']'
	 * 		| identifier '(' argList ')'
	 * @return identifier
	 */
	private FqlNode propertyIdentifier() {
		if(debug) enter("PropIdentifier");
		FqlNode propIdentifier = identifier();
		if(lookAhead().is(TokenType.LBRACKET)) {
			advanceOver(TokenType.LBRACKET);
			propIdentifier.addChild(arrayIndexList());
			advanceOver(TokenType.RBRACKET);
		} else if(lookAhead().is(TokenType.LPAREN)) {
			FqlNode n = FqlOperator.FUNCTION.getFqlNode(propIdentifier.getName());
			propIdentifier = n;
			List<FqlNode> argList = argumentList();
			for(FqlNode a : argList) {
				n.addChild(a);
			}
		}
		if(debug) exit("propIdentifier",propIdentifier);
		return propIdentifier;
	}

	/**
	 * get a argument list
	 * @return list of arguments. Empty if none defined
	 */
	private List<FqlNode> argumentList(){
		if(debug) enter("ArgList");
		Vector<FqlNode> args = new Vector<FqlNode>();
		// argList := '(' *Exp *(, Exp) ')'
		advanceOver(TokenType.LPAREN);
		if(!lookAhead().is(TokenType.RPAREN)){
			args.add(expr());
			while(lookAhead().is(TokenType.COMMA)){
				advanceOver(TokenType.COMMA);
				args.add(expr());
			}
		}
		advanceOver(TokenType.RPAREN);
		return args;
	}

	/**
	 * Get an array index list
	 * @return - array index list
	 */
	private FqlNode arrayIndexList(){
		if(debug) enter("ArrayIndexList");
		// Array-index-list	Array-index *("," Array-Index) | "*" | EMPTY
		FqlNode n = FqlOperator.INDEX.getFqlNode();
		n.addChild(arrayIndex());
		while(lookAhead().is(TokenType.COMMA)){
			advanceOver(TokenType.COMMA);
			n.addChild(arrayIndex());
		}
		if(debug) exit("ArrayIndexList",n);
		return n;
	}

	/**
	 * get an array index
	 * @return - INDEX FqlNode containing index value or '*'
	 */
	private FqlNode arrayIndex(){
		if(debug) enter("ArrayIndex");
		FqlNode n = null;
		// Array-index-list	Array-index *("," Array-Index) | "*" | EMPTY
		// Array-index	Expr | Expr ".." [Expr] | [Expr] ".." Expr
		// Index := integer | '*' | [INTEGER] .. [INTEGER] | INTEGER *(, INTEGER)
		if(lookAhead().is(TokenType.RBRACKET)){
			n = FqlOperator.CONSTANT.getFqlNode(new DataValue("*"));	// empty list is treated as '*'
		} else if(lookAhead().is(TokenType.STAR)){
			advanceOver(TokenType.STAR);
			n = FqlOperator.CONSTANT.getFqlNode(new DataValue("*"));
		} else if(lookAhead().is(TokenType.RANGE)){
			// have RANGE expr ==> 0 .. expr
			advanceOver(TokenType.RANGE);
			n = FqlOperator.RANGE.getFqlNode();
			n.addChild(FqlOperator.CONSTANT.getFqlNode(new DataValue(0L)));
			n.addChild(expr());
		} else {
			// have expr [RANGE [expr]]
			FqlNode left = expr();
			if(lookAhead().is(TokenType.RANGE)){
				advanceOver(TokenType.RANGE);
				n = FqlOperator.RANGE.getFqlNode();
				n.addChild(left);
				if(!lookAhead().is(TokenType.COMMA) && !lookAhead().is(TokenType.RBRACKET)){
					// have expr RANGE expr
					n.addChild(expr());
				} else {
					// have expr range ==> expr .. '*'
					n.addChild(FqlOperator.CONSTANT.getFqlNode(new DataValue("*")));
				}
			} else {
				n = left;
			}
		}
		if(n == null) throw new ModelException(ExceptionReason.INVALID_QUERY,"Index can only be integer value or *");
		if(debug) exit("ArrayIndex",n);
		return n;
	}

	/**
	 * Get Literal node<br>
	 * literal := stringLiteral | decimal-value | binary-value | hex-value | real-value | TRUE | FALSE | '{' [literal *( ',' literal )] '}'<br>
	 * If found, CONSTANT FqlNode containing the literal value is pushed on stack
	 * @return - true if found a literal value, false otherwise. Value(s) are on the stack if true
	 */
	private boolean literal(){
		if(debug) enter("Literal");
		boolean matched = false;
		if(lookAhead().is(TokenType.STRING_VALUE)){
			// StringLiteral
			Token token = advanceOver(TokenType.STRING_VALUE);
			stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(DataType.STRING,token.getValue())));
			matched = true;
		} else if(lookAhead().is(TokenType.NUMBER)){
			// decimal-value | binary-value | hex-value | real-value
			Token token = advanceOver(lookAhead().getType());
			switch(token.getType()){
			case BINARY:
				stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(Long.parseLong(token.getValue(), 2))));
				break;
			case INTEGER:
				stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(Long.parseLong(token.getValue()))));
				break;
			case HEX:
				stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(Long.parseLong(token.getValue(), 16))));
				break;
			case REAL:
				stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(Double.parseDouble(token.getValue()))));
				break;
			default:
				throw new ModelException("Internal error - number format "+token.getType()+" is not implemented");
			}
			matched = true;
		} else if(lookAhead().is(TokenType.BOOLEAN)){
			// true-value | false-value
			Token token = advanceOver(TokenType.BOOLEAN);
			stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(Boolean.parseBoolean(token.getValue()))));
			matched = true;
		} else if(lookAhead().is(TokenType.LBRACE)){
			// arrayLiteral = '{' [literal *(,literal)] '}'
			advanceOver(TokenType.LBRACE);
			if(!lookAhead().is(TokenType.RBRACE)){
				Vector<String> values = new Vector<String>();
				TokenType tokenType = lookAhead().getType();
				values.add(lookAhead().getValue());
				advanceOver(tokenType);
				while(lookAhead().is(TokenType.COMMA)){
					advanceOver(TokenType.COMMA);
					values.add(advanceOver(tokenType).getValue());
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
						throw new ModelException("Internal error - literal array format "+tokenType+" is not implemented");
					}
				}
				stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(dt,dataValues)));
			} else {
				// literal = '{' '}'
				stack.push(FqlOperator.CONSTANT.getFqlNode(new DataValue(DataType.VOID,null)));
			}
			advanceOver(TokenType.RBRACE);
			matched = true;
		}
		return matched;
	}

	/**
	 * classPath = ['/' identifier *('/' identifier) ':' ] className
	 * @return - CLASS_PATH FqlNode containing classpath
	 */
	private FqlNode classPath(){
		if(debug) enter("ClassPath");
		Vector<String> pathNames = new Vector<String>();
		if(lookAhead().is(TokenType.SLASH)){
			while(lookAhead().is(TokenType.SLASH)){
				advanceOver(TokenType.SLASH);
				Token pathName = advanceOver(TokenType.IDENTIFIER);
				pathNames.add(pathName.getValue());
			}
			advanceOver(TokenType.COLON);
		}
		FqlNode cp = className();
		if(!pathNames.isEmpty()){
			StringBuilder b = new StringBuilder();
			for(String pathName : pathNames){
				b.append("/");
				b.append(pathName);
			}
			b.append(":");
			b.append(cp.getName());
			if(debug){
				System.out.println("-- returning classPath "+b.toString());
			}
			return FqlOperator.CLASS_PATH.getFqlNode(b.toString());
		}
		if(debug){
			System.out.println("-- returning classPath "+cp);
		}
		return cp;
	}

	/**
	 * Get className node<br>
	 * className = identifier
	 * @return - CLASS_PATH FqlNode containing the class name
	 */
	private FqlNode className(){
		if(debug) enter("ClassName");
		FqlNode className = identifier();
		String name = className.getName();
		if(!name.matches("^([a-zA-Z0-9])+_([a-zA-Z0-9_])+$")){
			throw new ModelException(ExceptionReason.TYPE_MISMATCH,"Expected className of form schema_name, found "+name);
		}
		return FqlOperator.CLASS_PATH.getFqlNode(name);
	}

	/**
	 * get Identifier (or variable) node<br>
	 * identifier = identifier-start, *( identifier-subsequent )<br/>
	 * identifier-start = UNICODE-S1<br/>
	 * identifier-subsequent = identifier-start | DECIMAL-DIGIT<br>
	 * @return - IDENTIFIER | VARIABLE with name given by the incoming identifier
	 */
	private FqlNode identifier(){
		if(debug) enter("Identifier");
		Token t =  advanceOver(TokenType.IDENTIFIER);
		// FqlNode id =  p.aliases.containsKey(t.value()) ? p.aliases.get(t.value()) : new FqlNode(FqlOperator.IDENTIFIER,t.value());
		FqlNode id = t.getValue().matches("^\\$.+\\$$") ? FqlOperator.VARIABLE.getFqlNode(t.getValue()) : FqlOperator.IDENTIFIER.getFqlNode(t.getValue());
		if(debug){
			System.out.println("-- Token "+t+" returning Identifier "+id);
		}
		if(debug) exit("identifier",id);
		return id;
	}

	// Tokenizer methods

	/**
	 * Read in the next non whitespace token from the query and set the lookahead token to it
	 * @return next input token
	 */
	private Token nextToken(){
		if(debug) System.out.println(cursor+": "+query.substring(cursor));
		// read the next Token, and skip any comments and white spaces
		if(cursor >= queryLength){
			return new Token(TokenType.EOF);
		}
		// ignore white space
		if(Character.isWhitespace(query.charAt(cursor))){
			// scan past white space
			while(++cursor < queryLength && Character.isWhitespace(query.charAt(cursor)));
			if(cursor >= queryLength) return new Token(TokenType.EOF);

		}
		// scan for next token
		char c = query.charAt(cursor++);
		switch(c){
		// single character tokens
		case ',':
			return new Token(TokenType.COMMA);
		case '=':
			return new Token(TokenType.EQ);
		case '{':
			return new Token(TokenType.LBRACE);
		case '}':
			return new Token(TokenType.RBRACE);
		case '(':
			return new Token(TokenType.LPAREN);
		case ')':
			return new Token(TokenType.RPAREN);
		case '*':
			return new Token(TokenType.STAR);
		case '[':
			return new Token(TokenType.LBRACKET);
		case ']':
			return new Token(TokenType.RBRACKET);
		case ';':
			return new Token(TokenType.SEMICOLON);
		case '#':
			return new Token(TokenType.HASH);
		case '/':
			return new Token(TokenType.SLASH);
		case '+':
			return new Token(TokenType.PLUS);
		case '-':
			return new Token(TokenType.MINUS);
			// one or two character tokens
		case '.':
			if(cursor < queryLength && query.charAt(cursor) == '.'){	// check for '..'
				cursor++;
				return new Token(TokenType.RANGE);
			}
			return new Token(TokenType.PERIOD);
		case ':':

			if(cursor < queryLength && query.charAt(cursor) == ':'){	// check for '::'
				cursor++;
				return new Token(TokenType.SCOPE);

			}
			return new Token(TokenType.COLON);
		case '<':
			if(cursor < queryLength){
				switch(query.charAt(cursor)){
				case '=':
					cursor++;
					return new Token(TokenType.LE);
				case '>':
					cursor++;
					return new Token(TokenType.NE);
				default:
					break;
				}
			}
			return new Token(TokenType.LT);
		case '>':
			if(cursor < queryLength){
				switch(query.charAt(cursor)){
				case '=':
					cursor++;
					return new Token(TokenType.GE);
				default:
					break;
				}
			}
			return new Token(TokenType.GT);
			// two character tokens
		case '|':
			if(cursor < queryLength && query.charAt(cursor) == '|' ){
				cursor++;
				return new Token(TokenType.CONCAT);
			}
			return new Token(TokenType.ERROR);
			// string value
		case '\'':
			int savedLoc = cursor-1;
			StringBuilder b = new StringBuilder();
			while(cursor < queryLength){	// while cursor is in range
				switch(query.charAt(cursor)){
				case '\'':
					// EOS reached
					cursor++;
					return new Token(TokenType.STRING_VALUE,b.toString());
				case '\\':
					b.append(getQuotedCharacter());
					break;
				default:
					b.append(query.charAt(cursor++));		
				}
			}
			// have an unterminated String
			throw new ModelException("Unterminated string at position "+savedLoc);
		default:
			cursor--;	// rewind cursor
			boolean found = false;
			if(c == '+' || c == '-' || Character.isDigit(c)){
				// numeric value, token is in lookAheadToken
				found = getNumericValue();
			} else {
				found = getIdentifier();	// found an identifier in lookAheadToken
				// check known tokens. Swap in lookahead token if found
				String value = lookAheadToken.getValue();
				for(TokenType t : TokenType.getTokenTypes()){
					if(value.equalsIgnoreCase(t.getValue())){
						lookAheadToken.setType(t);
						break;
					}
				}
			}
			if(!found){
				cursor++;
				lookAheadToken = new Token(TokenType.CHARACTER,String.valueOf(c));
			}
			return lookAheadToken;
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
	private boolean getNumericValue(){
		// System.out.println(p.query);
		boolean isHex = false, isDecimal = false, isBoolean = false;
		boolean signPossible = true, boolPossible = true, needDigit = true;
		int cursor = this.cursor;
		StringBuilder b = new StringBuilder();	
		while(cursor < queryLength){
			// System.out.println(b.toString() + " " + p.query.charAt(cursor) + " " + p.query.substring(cursor+1, p.queryLength));
			char c = query.charAt(cursor);
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
				if(cursor < queryLength-1 && query.charAt(cursor+1) == '.'){
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
		if(cursor > this.cursor){
			if(b.length() == 1 && b.charAt(0) == '.') return false;
			this.cursor = cursor;
			if(isBoolean){
				lookAheadToken = new Token(TokenType.BINARY, b.toString());
			} else if(isDecimal){
				lookAheadToken = new Token(TokenType.REAL, b.toString());
			} else if(isHex){
				lookAheadToken = new Token(TokenType.HEX, b.toString());
			} else {
				lookAheadToken = new Token(TokenType.INTEGER, b.toString());
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
	private boolean getIdentifier(){
		char c;
		StringBuilder b = new StringBuilder();
		if(Character.isJavaIdentifierStart((c = query.charAt(cursor)))){
			b.append(c);
			cursor++;
			while(cursor < queryLength && 
					Character.isJavaIdentifierPart(c=query.charAt(cursor))){
				b.append(c);
				cursor++;
			}
			lookAheadToken = new Token(TokenType.IDENTIFIER,b.toString());
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
	private char getQuotedCharacter(){
		char c = query.charAt(cursor++);	// get character value and advance
		if(c != '\\') return c;				// if no backslash, we are done	
		c = query.charAt(cursor++);			// get character after backslash and advance
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
		case 'X':
			if(cursor >= queryLength) return 0;	// have a \x terminated query, return \x0000
			int startPosition = cursor;			// startPosition is first character after \x
			for(int i=0; i<4; i++){					// pick up to four hex characters
				c = query.charAt(cursor++);		// next character
				// if we reached a non-Hex character, break
				if(!(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F')) break;
				if(cursor >= queryLength) break;	// went past the end of line
			}
			return (char) Integer.parseInt(query.substring(startPosition,cursor), 16);
		default:
			return c;		// for all others, a '\c' is simply a 'c'
		}
	}
}
