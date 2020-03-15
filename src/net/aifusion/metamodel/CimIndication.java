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
 * Base class to implement Cim Events. CimIndications provide the capability to send asynchronous events from
 * CIM Event Generators to CIM Listeners.
 * @author Sharad Singhal
 */
public class CimIndication implements CimEvent {
	/** Type of this event */
	private CimEventType type;
	/** Event Generator that created this event */
	private CimEventGenerator generator;
	/** Description provided for this event */
	private String eventDescription;
	
	/**
	 * Create a CIM Indication
	 * @param type - type of event within this indication
	 * @param generator - element that generated the indication
	 * @param eventDescription - Optional subject associated with the indication
	 */
	public CimIndication(CimEventType type, CimEventGenerator generator, String eventDescription){
		this.type = type;
		this.generator = generator;
		this.eventDescription = eventDescription;
		return;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.CimEvent#getType()
	 */
	@Override
	public CimEventType getType() {
		return type;
	}

	/* (non-Javadoc)
	 * @see net.aifusion.metamodel.CimEvent#getGenerator()
	 */
	@Override
	public CimEventGenerator getGenerator() {
		return generator;
	}
	
	@Override
	public String getDescription() {
		return eventDescription;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append("CimIndication {\n");
		b.append("\tType = \"").append(type).append("\";\n");
		if(eventDescription != null) b.append("\tDescription = \"").append(ModelUtilities.escape(eventDescription)).append("\";\n");
		b.append("};\n");
		return b.toString();
	}
}
