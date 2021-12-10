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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.cimserver.CimHeader;
import net.aifusion.cimserver.CimXHeader;

/**
 * Class to check Http extension headers
 * @author Sharad Singhal
 */
public class HttpXHeaderTest {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("HttpXHeader ");
		assertEquals(15,CimXHeader.values().length);
	}
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

	/**
	 * Test method for {@link net.aifusion.cimserver.CimXHeader#appliesTo(net.aifusion.cimserver.CimHeader)}.
	 */
	@Test
	public void testAppliesTo() {
		for(CimXHeader x : CimXHeader.values()){
			for(CimHeader h : CimHeader.values()){
				switch(x){
				case METHOD_NAME:	// method name is required for the following
					switch(h){
					case GET_METHOD_TYPE:
					case GET_METHOD_PARAMETERS:
					case INVOKE_METHOD:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
					}
					break;
				case PROPERTY_NAME:
					switch(h){
					case GET_PROPERTY_TYPE:
					case GET_PROPERTY_VALUE:
					case SET_PROPERTY_VALUE:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
					}
					break;
				case PROPERTY_TYPE:
					switch(h){
					case SET_PROPERTY_VALUE:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));	
					}
					break;
				case ELEMENT_TYPES:
				case NAME_SPACES:
				case ELEMENT_NAMES:
				case LOCATE_SUBCLASS:
					switch(h){
					case GET_ELEMENTS:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
					}
					break;
				case EVENT_TYPE:
					switch(h){
					case ADD_LISTENER:
					case REMOVE_LISTENER:
					case HAS_LISTENER:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
					}
					break;
				case CIM_URL:
					switch(h){
					case ADD_LISTENER:
					case REMOVE_LISTENER:
					case HAS_LISTENER:
					case REGISTER_PROVIDER:
					case UNREGISTER_PROVIDER:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
					}
					break;
				case OBJECT_PATH:
					switch(h) {
					case DELETE_ELEMENT:
					case GET_ELEMENT:
					case GET_METHOD_NAMES:
					case GET_METHOD_PARAMETERS:
					case GET_METHOD_TYPE:
					case GET_PROPERTY_NAMES:
					case GET_PROPERTY_TYPE:
					case GET_PROPERTY_VALUE:
					case HAS_ELEMENT:
					case INVOKE_METHOD:
					case SET_PROPERTY_VALUE:
					case FILTER:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
						break;
					}
					break;
				case NAMESPACE_PATH:
					switch(h) {
					case PUT_ELEMENT:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
						break;
					}
					break;
				case FILTER_STRING:
					switch(h) {
					case FILTER:
						assertTrue(x.appliesTo(h));
						break;
					default:
						assertFalse(x.appliesTo(h));
					}
					break;
				case METHOD_TYPE:
				case PARAMETER_NAME:
				case INTRINSIC:
				default:
					assertFalse(x.appliesTo(h));
					break;
				}
			}
			
		}
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimXHeader#getXHeaders(net.aifusion.cimserver.CimHeader)}.
	 */
	@Test
	public void testGetXHeaders() {
		for(CimHeader h : CimHeader.values()){
			List<CimXHeader> xHeaders = CimXHeader.getXHeaders(h);
			assertNotNull(xHeaders);
			switch(h){
			case GET_ELEMENT:
			case HAS_ELEMENT:
			case DELETE_ELEMENT:
			case GET_PROPERTY_NAMES:
			case GET_METHOD_NAMES:
				assertEquals(1,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.OBJECT_PATH));
				break;
			case GET_ELEMENTS:
				assertEquals(4,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.ELEMENT_TYPES) && 
						xHeaders.contains(CimXHeader.NAME_SPACES) && 
						xHeaders.contains(CimXHeader.ELEMENT_NAMES) &&
						xHeaders.contains(CimXHeader.LOCATE_SUBCLASS));
				break;
			case FILTER:
				assertEquals(2,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.FILTER_STRING) && 
						xHeaders.contains(CimXHeader.OBJECT_PATH));
				break;
			case ADD_LISTENER:
			case REMOVE_LISTENER:
			case HAS_LISTENER:
				assertEquals(2,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.EVENT_TYPE) && 
						xHeaders.contains(CimXHeader.CIM_URL));
				break;
			case REGISTER_PROVIDER:
			case UNREGISTER_PROVIDER:
				assertEquals(1,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.CIM_URL));
				break;
			case GET_PROPERTY_TYPE:
			case GET_PROPERTY_VALUE:
				assertEquals(2,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.PROPERTY_NAME) &&
						xHeaders.contains(CimXHeader.OBJECT_PATH));
				break;
			case SET_PROPERTY_VALUE:
				assertEquals(3,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.PROPERTY_NAME) &&
						xHeaders.contains(CimXHeader.PROPERTY_TYPE) &&
						xHeaders.contains(CimXHeader.OBJECT_PATH));
				break;
			case GET_METHOD_TYPE:
			case GET_METHOD_PARAMETERS:
			case INVOKE_METHOD:
				assertEquals(2,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.METHOD_NAME) &&
						xHeaders.contains(CimXHeader.OBJECT_PATH));
				break;
			case PUT_ELEMENT:
				assertEquals(1,xHeaders.size());
				assertTrue(xHeaders.contains(CimXHeader.NAMESPACE_PATH));
				break;
			default:
				assertEquals(0,xHeaders.size());
				break;
			
			}
		}
	}

	/**
	 * Test method for {@link net.aifusion.cimserver.CimXHeader#lookup(java.lang.String)}.
	 * Test method for {@link net.aifusion.cimserver.CimXHeader#toString()}.
	 */
	@Test
	public void testLookup() {
		for(CimXHeader h : CimXHeader.values()){
			assertEquals(h,CimXHeader.lookup(h.toString()));
		}
	}
}
