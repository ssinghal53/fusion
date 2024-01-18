/**
 * Copyright 2021 Hewlett Packard Enterprise Development LP
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

/**
 * A CimFilter is a query on the properties of some structure value
 * @author Sharad Singhal
 */
public interface CimFilter {
	/**
	 * Check if the filter is satisfied by the given structure value
	 * @param value - StructureValue to test
	 * @param repository - Repository used to obtain transitive objects (if needed)
	 * @return - true if value satisfies the filter, false otherwise
	 */
	public boolean satisfies(StructureValue value, Repository repository);
	
	/**
	 * Set a delayed constant value in this filter
	 * @param variableName - name of the variable (must be in form '$' name '$')
	 * @param value - data value to set for this variable
	 */
	public void setVariable(String variableName, DataValue value);
	
	/**
	 * Get the query string defined in the filter
	 * @return - filter query string
	 */
	public String getFilterQuery();
	
}
