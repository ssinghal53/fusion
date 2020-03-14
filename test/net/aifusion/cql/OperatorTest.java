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
 * Created Oct 2, 2016 by Sharad Singhal
 */
package net.aifusion.cql;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import net.aifusion.metamodel.DataValue;

/**
 * Class to test Operators
 * @author Sharad Singhal
 */
public class OperatorTest {
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("Operator ");
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
	 * Test method for {@link net.aifusion.cql.Operator#getNode()}.
	 */
	@Test
	public final void testGetNode() {
		for(Operator o : Operator.values()){
			Node n = o.getNode();
			assertNotNull(n);
			assertEquals(o,n.getOperator());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Operator#getNode(java.lang.String)}.
	 */
	@Test
	public final void testGetNodeString() {
		for(Operator o : Operator.values()){
			Node n = Operator.FUNCTION.equals(o) ? o.getNode(Functions.CurrentDateTime.toString()) : o.getNode("foo");
			assertNotNull(n);
			assertEquals(o,n.getOperator());
			assertEquals(Operator.FUNCTION.equals(o) ? Functions.CurrentDateTime.toString() : "foo",n.getName());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Operator#getNode(net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testGetNodeDataValue() {
		for(Operator o : Operator.values()){
			Node n = o.getNode(new DataValue(54));
			assertNotNull(n);
			assertEquals(o,n.getOperator());
			assertEquals(null,n.getName());
			assertEquals(new DataValue(54),n.getValue());
		}
	}

	/**
	 * Test method for {@link net.aifusion.cql.Operator#getNode(java.lang.String, net.aifusion.metamodel.DataValue)}.
	 */
	@Test
	public final void testGetNodeStringDataValue() {
		for(Operator o : Operator.values()){
			Node n = o.getNode("foo",new DataValue(54));
			assertNotNull(n);
			assertEquals(o,n.getOperator());
			assertEquals("foo",n.getName());
			assertEquals(new DataValue(54),n.getValue());
		}
	}

}
