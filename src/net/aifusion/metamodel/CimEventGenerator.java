/**
 * Copyright 2017, Sharad Singhal, All Rights Reserved
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
 * Created Jan 8, 2017 by Sharad Singhal
 */
package net.aifusion.metamodel;

/**
 * Interface implemented by all CimEvent generators
 * @author Sharad Singhal
 */
public interface CimEventGenerator {
	
	/**
	 * Add a listener to this generator
	 * @param type - type of event the listener wants
	 * @param listener - CimListener interested in events
	 * @return - true if listener was successfully added, false otherwise
	 */
	public boolean addListener(CimEventType type, CimListener listener);
	
	/**
	 * Remove a listener from this generator
	 * @param type of event the listener wants
	 * @param listener - CimListener to be removed
	 */
	public void removeListener(CimEventType type, CimListener listener);
	
	/**
	 * Check if this event generator has a listener for an event type
	 * @param type - type of event
	 * @param listener - listener to check. If null, check if any listener is registered
	 * @return - true if the generator has at least one registered listener for the event type, false otherwise
	 */
	public boolean hasListener(CimEventType type, CimListener listener);
	
}
