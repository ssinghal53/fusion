/**
 * Copyright 2013, Sharad Singhal, All Rights Reserved
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
 * Created December 31, 2013 by Sharad Singhal
 * Last Modified January 19, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.*;

import java.lang.reflect.InvocationTargetException;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Sharad Singhal
 *
 */
public class ModelExceptionTest {
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("ModelException ");
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
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException()}.
	 */
	@Test
	public final void testModelException() {
		ModelException e = new ModelException();
		assertEquals(ExceptionReason.FAILED,e.getReason());
		assertEquals("A general error occurred that is not covered by a more specific error code",
				e.getMessage());
		assertNull(e.getCause());
		assertEquals("1 FAILED [A general error occurred that is not covered by a more specific error code]",e.toString());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException(java.lang.String)}.
	 */
	@Test
	public final void testModelExceptionString() {
		ModelException e = new ModelException("Some Message");
		assertEquals(ExceptionReason.FAILED,e.getReason());
		assertEquals("Some Message",e.getMessage());
		assertNull(e.getCause());
		assertEquals(
				"1 FAILED [A general error occurred that is not covered by a more specific error code]: Some Message",
				e.toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException(java.lang.Throwable)}.
	 */
	@Test
	public final void testModelExceptionThrowable() {
		Exception wrappedException = new Exception("Wrapped Message");
		ModelException e = new ModelException(wrappedException);
		assertEquals(ExceptionReason.FAILED,e.getReason());
		assertEquals(wrappedException, e.getCause());
		assertEquals("Wrapped Message",e.getMessage());
		assertEquals(
				"1 FAILED [A general error occurred that is not covered by a more specific error code]\njava.lang.Exception: Wrapped Message",
				e.toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException(java.lang.String, java.lang.Throwable)}.
	 */
	@Test
	public final void testModelExceptionStringThrowable() {
		Exception wrappedException1 = new Exception("Wrapped Message 1");
		InvocationTargetException wrappedException2 = new InvocationTargetException(wrappedException1);
		ModelException e = new ModelException("Local Message",wrappedException2);
		assertEquals(ExceptionReason.FAILED,e.getReason());
		assertEquals(wrappedException2, e.getCause());
		assertEquals("Local Message",e.getMessage());
		assertEquals(
				"1 FAILED [A general error occurred that is not covered by a more specific error code]"+
				": Local Message"+
				"\njava.lang.reflect.InvocationTargetException: null"+
				"\njava.lang.Exception: Wrapped Message 1",
				e.toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException(net.aifusion.metamodel.ExceptionReason)}.
	 */
	@Test
	public final void testModelExceptionModelExceptionReason() {
		ModelException e = new ModelException(ExceptionReason.ACCESS_DENIED);
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		assertNull(e.getCause());
		assertEquals("Access to a resource is not available to the client",e.getMessage());
		assertEquals(
				"2 ACCESS_DENIED [Access to a resource is not available to the client]",
				e.toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException(net.aifusion.metamodel.ExceptionReason, java.lang.String)}.
	 */
	@Test
	public final void testModelExceptionModelExceptionReasonString() {
		ModelException e = new ModelException(ExceptionReason.ACCESS_DENIED,"Local Message");
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		assertNull(e.getCause());
		assertEquals("Local Message",e.getMessage());
		assertEquals(
				"2 ACCESS_DENIED [Access to a resource is not available to the client]"+
				": Local Message",
				e.toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException(net.aifusion.metamodel.ExceptionReason, java.lang.Throwable)}.
	 */
	@Test
	public final void testModelExceptionModelExceptionReasonThrowable() {
		Exception wrappedException1 = new Exception("Wrapped Message 1");
		InvocationTargetException wrappedException2 = new InvocationTargetException(wrappedException1);
		ModelException e = new ModelException(ExceptionReason.ACCESS_DENIED,wrappedException2);
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		assertEquals(wrappedException2, e.getCause());
		assertEquals("Access to a resource is not available to the client",e.getMessage());
		assertEquals(
				"2 ACCESS_DENIED [Access to a resource is not available to the client]"+
				"\njava.lang.reflect.InvocationTargetException: null"+
				"\njava.lang.Exception: Wrapped Message 1",
				e.toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#ModelException(net.aifusion.metamodel.ExceptionReason, java.lang.String, java.lang.Throwable)}.
	 */
	@Test
	public final void testModelExceptionModelExceptionReasonStringThrowable() {
		Exception wrappedException1 = new Exception("Wrapped Message 1");
		InvocationTargetException wrappedException2 = new InvocationTargetException(wrappedException1);
		ModelException e = new ModelException(ExceptionReason.ACCESS_DENIED,"Local Message",wrappedException2);
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		assertEquals(wrappedException2, e.getCause());
		assertEquals("Local Message",e.getMessage());
		assertEquals(
				"2 ACCESS_DENIED [Access to a resource is not available to the client]"+
				": Local Message"+
				"\njava.lang.reflect.InvocationTargetException: null"+
				"\njava.lang.Exception: Wrapped Message 1",
				e.toString());
		return;
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#getReason()}.
	 */
	@Test
	public final void testGetReason() {
		// check cases where no reason is defined
		ModelException e = new ModelException();
		assertEquals(ExceptionReason.FAILED,e.getReason());
		e = new ModelException("Some message");
		assertEquals(ExceptionReason.FAILED,e.getReason());
		e = new ModelException(new Exception());
		assertEquals(ExceptionReason.FAILED,e.getReason());
		e = new ModelException("Some message", new Exception("Underlying message"));
		assertEquals(ExceptionReason.FAILED,e.getReason());

		// check cases where the reason is defined
		e = new ModelException(ExceptionReason.ACCESS_DENIED);
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		e = new ModelException(ExceptionReason.ACCESS_DENIED,new Exception());
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		e = new ModelException(ExceptionReason.ACCESS_DENIED, "Some message",new Exception("Underlying message"));
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		return;

	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#getMessage()}.
	 */
	@Test
	public final void testGetMessage() {
		// check cases where no reason is defined
		
		// case 1: no message, cause or reason given. Expect the default ExceptionReason description
		ModelException e = new ModelException();
		assertEquals("A general error occurred that is not covered by a more specific error code",e.getMessage());
		
		// case 2: if message is given, return it
		e = new ModelException("Some message");
		assertEquals("Some message",e.getMessage());
		
		// case 3: if an underlying cause with no reason is given, return the ExceptionReason description
		e = new ModelException(new Exception());
		assertEquals("A general error occurred that is not covered by a more specific error code",e.getMessage());
		
		// case 4: An underlying cause with a message is given, return the message from the underlying cause
		e = new ModelException(new Exception("Underlying message"));
		assertEquals("Underlying message",e.getMessage());
		
		// case 5: A message with some underlying cause. Return the message
		e = new ModelException("Some message", new Exception());
		assertEquals("Some message",e.getMessage());
		
		// case 6: A message with some underlying cause (containing a message). Return message
		e = new ModelException("Some message", new Exception("Underlying message"));
		assertEquals("Some message",e.getMessage());

		// check cases where the reason is defined. Same as above, except in cases 1 and 3, the message comes from
		// the given ExceptionReason
		// case 1: no message, cause or reason given. Expect the default ExceptionReason description
		e = new ModelException(ExceptionReason.ACCESS_DENIED);
		assertEquals("Access to a resource is not available to the client",e.getMessage());

		// case 2: if message is given, return it
		e = new ModelException(ExceptionReason.ACCESS_DENIED, "Some message");
		assertEquals("Some message",e.getMessage());

		// case 3: if an underlying cause with no reason is given, return the ExceptionReason description
		e = new ModelException(ExceptionReason.ACCESS_DENIED, new Exception());
		assertEquals("Access to a resource is not available to the client",e.getMessage());

		// case 4: An underlying cause with a message is given, return the message from the underlying cause
		e = new ModelException(ExceptionReason.ACCESS_DENIED, new Exception("Underlying message"));
		assertEquals("Underlying message",e.getMessage());

		// case 5: A message with some underlying cause. Return the message
		e = new ModelException(ExceptionReason.ACCESS_DENIED, "Some message", new Exception());
		assertEquals("Some message",e.getMessage());

		// case 6: A message with some underlying cause (containing a message). Return message
		e = new ModelException(ExceptionReason.ACCESS_DENIED, "Some message", new Exception("Underlying message"));
		assertEquals("Some message",e.getMessage());

	}

	/**
	 * Test method for {@link net.aifusion.metamodel.ModelException#getRootMessage()}.
	 */
	@Test
	public final void testGetRootMessage() {
		// check root message on deepest node
		Exception wrappedException1 = new Exception("Root Message");
		InvocationTargetException wrappedException2 = new InvocationTargetException(wrappedException1);
		ModelException e = new ModelException(ExceptionReason.ACCESS_DENIED,"Local Message",wrappedException2);
		
		assertEquals(ExceptionReason.ACCESS_DENIED,e.getReason());
		assertEquals(wrappedException2, e.getCause());
		assertEquals("Local Message",e.getMessage());
		assertEquals("Root Message",e.getRootMessage());
		assertEquals(
				"2 ACCESS_DENIED [Access to a resource is not available to the client]"+
						": Local Message"+
						"\njava.lang.reflect.InvocationTargetException: null"+
						"\njava.lang.Exception: Root Message",
						e.toString());

		// check root message on intermediate node
		Exception wrappedException0 = new Exception();
		wrappedException1 = new Exception("Root Message",wrappedException0);
		wrappedException2 = new InvocationTargetException(wrappedException1);
		e = new ModelException("Local Message",wrappedException2);
		assertEquals("Root Message",e.getRootMessage());

		// check root message on top node
		wrappedException1 = new Exception(wrappedException0);
		wrappedException2 = new InvocationTargetException(wrappedException1);
		e = new ModelException("Local Message",wrappedException2);
		assertEquals("Local Message",e.getRootMessage());

		// check no message at all
		wrappedException1 = new Exception(wrappedException0);
		wrappedException2 = new InvocationTargetException(wrappedException1);
		e = new ModelException(wrappedException2);
		assertEquals("A general error occurred that is not covered by a more specific error code",e.getRootMessage());
		return;

	}

}
