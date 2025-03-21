/**
 * Copyright 2015,2025 Sharad Singhal, All Rights Reserved
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
 * Created Nov 16, 2015 by Sharad Singhal
 * Last Modified March 9, 2025
 */
package net.aifusion.providers;

import java.net.URI;
import java.util.List;

import net.aifusion.metamodel.CimEventGenerator;
import net.aifusion.metamodel.CimParameter;
import net.aifusion.metamodel.DataType;
import net.aifusion.metamodel.DataValue;
import net.aifusion.metamodel.ObjectPath;
import net.aifusion.metamodel.Repository;
import net.aifusion.metamodel.StructureValue;

/**
 * Interface to define a CIM Provider. A Provider implements methods to get and set properties, invoke methods and
 * execute queries on objects held within it.
 * @author Sharad Singhal
 */
public interface Provider extends Repository, CimEventGenerator {
	
	/**
	 * Register a child provider with this provider. This enables this provider to dispatch queries to the child provider
	 * and and delegate selected queries to the child provider.
	 * @param child - child provider to be registered with this provider
	 */
	public void registerChildProvider(Provider child);
	
	/**
	 * Unregister a child provider. This disconnects the child from this provider
	 * @param child - child provider to be unregistered
	 */
	public void unregisterChildProvider(Provider child);
	
	/**
	 * Get the property names for a given named element. In case the objectPath is an enumeration, the key names are returned
	 * @param path - object path for the CIM element from which property names are desired
	 * @return - list containing names of the properties. Empty if none defined
	 */
	public List<String> getPropertyNames(ObjectPath path);
	
	/**
	 * Get the data type corresponding to a particular property (or enumeration key)
	 * @param path - object path of the CIM element from which the property is selected
	 * @param propertyName - name of the property or key
	 * @return - DataType of the corresponding property or key
	 */
	public DataType getPropertyType(ObjectPath path, String propertyName);
	
	/**
	 * Get a property value from this provider, or one of its children.
	 * @param path - object path of the CIM Element from which the property is being read
	 * @param propertyName - case insensitive name of the property being read
	 * @return - data value containing the property or key value
	 */
	public DataValue getPropertyValue(ObjectPath path, String propertyName);
	
	/**
	 * Set a property value within this provider, or one of its children
	 * @param path - object path of the CIM Element in which the property is being set.
	 * Note that only Classes and Instances have settable properties
	 * @param propertyName - case insensitive name of the property being set
	 * @param propertyValue - value of the property being set
	 */
	public void setPropertyValue(ObjectPath path, String propertyName, DataValue propertyValue);
	
	/**
	 * Get the method names defined in a given CIM class
	 * @param path - object path for the CIM class from which method names are desired
	 * @return - list containing method names. Empty if no methods are defined.
	 */
	public List<String> getMethodNames(ObjectPath path);
	
	/**
	 * Get the return type associated with a method
	 * @param path - object path for the CIM class from which the method is selected
	 * @param methodName - name of the method
	 * @return - data type associated with the method. Null if no such method exists
	 */
	public DataType getMethodReturnType(ObjectPath path, String methodName);
	
	/**
	 * Get the parameter list associated with a method
	 * @param path - object path of the CIM class from which the method is desired
	 * @param methodName - name of the method desired
	 * @return - list of parameters. Empty if the method does not take parameters.
	 */
	public List<CimParameter> getMethodParameters(ObjectPath path, String methodName);
	
	/**
	 * Invoke an extrinsic method on a class or instance within this provider, or one of its children
	 * @param path - object path of the CIM element on which the method is being invoked
	 * @param methodName - case insensitive name of the method being invoked
	 * @param methodParameters - list of parameters to be passed to the method
	 * @return - value returned from the method invocation
	 */
	public DataValue invokeMethod(ObjectPath path, String methodName, List<CimParameter> methodParameters);
	
	/**
	 * Execute a query against this provider, and return the result set
	 * @param query - query to be executed
	 * @return - set of results
	 */
	public List<StructureValue> executeQuery(String query);
	
	/**
	 * Get the underlying repository for this provider
	 * @return - reference to the underlying repository
	 */
	public Repository getRepository();
	
	/**
	 * Get the URI for this provider
	 * @return - Endpoint  where this provider can be reached.
	 */
	public URI getProviderEndpoint();

}
