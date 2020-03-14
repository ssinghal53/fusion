/**
 * Copyright 2016, Sharad Singhal, All Rights Reserved
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
 * Created Oct 9, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import java.util.List;

import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.DateTime;
import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;
import net.aifusion.metamodel.ObjectPath;

/**
 * Class to manage Functions
 * @author Sharad Singhal
 */
class Function extends Node {
	Functions f;
	DataType fType;
	/**
	 * Create an IDENTIFIER node
	 * @param name - name of the identifier
	 */
	Function(String name) {
		super(Operator.FUNCTION, name,null);
		// OBJECTPATH, DATETIME,DateTimeToMicrosecond,CurrentDateTime, MicroSecondToTimeStamp, MicroSecondToInterval
		// stringToUint, StringToSint, StringtoReal, uppercase, lowercase, numericToString, ReferenceToString, InstanceOf, 
		// Classpath, ObjectPath,
		// count, min, max, sum, mean, median
		f = Functions.lookup(getName());
		if(f == null){
			throw new ModelException(ExceptionReason.INVALID_QUERY,"Function not supported: "+getName());
		}
		fType = f.getType();
		return;
	}

	@Override
	void evaluate() {
		if(debug) System.out.println(toString()+"() - Enter");
		List<Node> children = getChildren();
		int size = children.size();
		switch(f){
		case DateTimeToMicroSeconds:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isDateTime()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected DateTime argument, found "+children.get(0).getType());
			DateTime dt = (DateTime) children.get(0).getValue().getValue();
			setValue(new DataValue(fType,dt.getLowerBound()));
			break;
		case StringToUint:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,children.get(0).getStringValue()));
			break;
		case StringToSint:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,children.get(0).getStringValue()));
			break;
		case StringToReal:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,children.get(0).getStringValue()));
			break;
		case UpperCase:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,children.get(0).getStringValue().toUpperCase()));
			break;
		case LowerCase:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,children.get(0).getStringValue().toLowerCase()));
			break;
		case NumericToString:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isNumeric()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected numeric argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,children.get(0).getStringValue()));
			break;
		case ReferenceToString:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isReference()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected numeric argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,children.get(0).getStringValue()));
			break;
		case ClassPath:
		case ObjectPath:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,new ObjectPath(children.get(0).getStringValue())));
			break;
		case CurrentDateTime:
			setValue(new DataValue(new DateTime()));
			break;
		case DateTime:
			if(size != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+size);
			if(!children.get(0).getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,new DateTime(children.get(0).getStringValue())));
			break;
		case MicroSecondToInterval:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isInteger()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected integer argument, found "+children.get(0).getType());
			setValue(new DataValue(fType,new DateTime(children.get(0).getLongValue())));
			break;
		case MicroSecondToTimeStamp:
			if(children.size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+children.size());
			if(!children.get(0).getType().isInteger()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected integer argument, found "+children.get(0).getType());
			long timeStamp = children.get(0).getLongValue();
			setValue(new DataValue(fType,new DateTime(timeStamp,timeStamp,false)));
			break;
		default:
			throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Function "+getName()+" is not supported");
		}
		if(debug) System.out.println(toString()+"() - Exit "+getValue());
		return;
	}

	@Override
	DataType getType() {
		return f.getType();
	}
	
	/**
	 * Get the class name from the objectPath/classPath created by this function.
	 * @return - name of the referenced class if the function is an objectPath. Null otherwise
	 */
	String getRefClass(){
		switch(f){
		case ClassPath:
		case ObjectPath:
			if(getChildren().size() != 1) throw new ModelException(ExceptionReason.INVALID_QUERY,toString()+": Only one child expected, found "+getChildren().size());
			Node child = getChildren().get(0);
			if(!child.getType().isString()) throw new ModelException(ExceptionReason.INVALID_PARAMETER,toString()+": expected String argument, found "+child.getType());
			ObjectPath path = new ObjectPath(child.getStringValue());
			return path.getName();
		default:
			return null;
		}
	}
}
