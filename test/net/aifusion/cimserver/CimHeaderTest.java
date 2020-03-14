/**
 * Copyright 2017 Sharad Singhal, All Rights Reserved
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
 * Created Sep 16, 2017 by sharad
 */
package net.aifusion.cimserver;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.cimserver.CimHeader;
import net.aifusion.cimserver.HttpMethod;

/**
 * Class to test CIM Headers
 * @author Sharad Singhal
 */
public class CimHeaderTest {
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("done.");
	}

	@Before
	public void setUp() throws Exception {
		System.out.print("-");
	}
	
	@After
	public void tearDown() throws Exception {
		System.out.print(".");
		return;
	}
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("CimHeader ");
		assertEquals(22,CimHeader.values().length);
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimHeader#getHttpMethod()}.
	 */
	@Test
	public void testGetHttpMethod() {
		for(CimHeader h : CimHeader.values()){
			switch(h){
			case PUT_ELEMENT:
				assertEquals(HttpMethod.PUT,h.getHttpMethod());
				break;
			case HAS_ELEMENT:
			case HAS_LISTENER:
			case SHUT_DOWN:
				assertEquals(HttpMethod.HEAD,h.getHttpMethod());
				break;
			case DELETE_ELEMENT:
				assertEquals(HttpMethod.DELETE,h.getHttpMethod());
				break;
			case ADD_LISTENER:
			case REMOVE_LISTENER:
			case REGISTER_PROVIDER:
			case UNREGISTER_PROVIDER:
			case SET_PROPERTY_VALUE:
			case INVOKE_METHOD:
			case EXECUTE_QUERY:
			case SEND_EVENT:
				assertEquals(HttpMethod.POST,h.getHttpMethod());
				break;
			case GET_ELEMENT:
			case GET_ELEMENTS:
			case GET_METHOD_NAMES:
			case GET_METHOD_PARAMETERS:
			case GET_METHOD_TYPE:
			case GET_NAMESPACES:
			case GET_PROPERTY_NAMES:
			case GET_PROPERTY_TYPE:
			case GET_PROPERTY_VALUE:
				assertEquals(HttpMethod.GET,h.getHttpMethod());
				break;
			}
			
		}
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimHeader#toString()}.
	 * Test method for {@link net.aifusion.cimserver.CimHeader#lookup(java.lang.String)}.
	 */
	@Test
	public void testLookup() {
		for(CimHeader h : CimHeader.values()){
			assertEquals(h,CimHeader.lookup(h.toString()));
		}
	}

}
