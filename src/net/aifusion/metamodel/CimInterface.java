/**
 * Copyright 2019 Sharad Singhal, All Rights Reserved
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
 * Created Jul 20, 2019 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.util.List;

/**
 * Class to represent a CIM Interface. Interfaces provide the ability to support multiple inheritance in class definitions. They cannot be directly
 * instantiated.
 * @see StandardQualifierType#IMPLEMENTS
 * @author Sharad Singhal
 */
public class CimInterface extends CimClass {
	/**
	 * @param name - name of the interface
	 * @param superType - supertype for the interface
	 * @param qualifiers - Qualifiers for this interface
	 * @param path - namespace path for this interface
	 * @param classFeatures - class features for the interface
	 */
	protected CimInterface(String name, CimStructure superType, List<Qualifier> qualifiers,
			NameSpacePath path, List<? extends QualifiedElement> classFeatures) {
		super(ElementType.INTERFACE, name, superType, qualifiers, path, classFeatures);
	}
	
	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.CimClass#invokeMethod(java.lang.String, java.util.List)
	 */
	@Override
	public DataValue invokeMethod(String methodName, List<CimParameter> cimParameters) {
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,getName()+": Interfaces do not support method invocation");
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.CimStructure#setPropertyValue(java.lang.String, net.aifusion.metamodel.DataValue)
	 */
	@Override
	public void setPropertyValue(String propertyName, DataValue propertyValue) {
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,getName()+": Interfaces do not support setting properties");
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.NamedElement#addListener(net.aifusion.metamodel.CimEventType, net.aifusion.metamodel.CimListener)
	 */
	@Override
	public synchronized boolean addListener(CimEventType type, CimListener listener) {
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,getName()+": Interfaces do not support listeners");
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.NamedElement#removeListener(net.aifusion.metamodel.CimEventType, net.aifusion.metamodel.CimListener)
	 */
	@Override
	public synchronized void removeListener(CimEventType type, CimListener listener) {
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,getName()+": Interfaces do not support listeners");
	}
}
