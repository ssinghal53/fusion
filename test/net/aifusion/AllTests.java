/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
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
 * Created Jan 19, 2014 by Sharad Singhal
 */
package net.aifusion;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.RunWith;
import org.junit.runner.notification.Failure;
import org.junit.runners.Suite;

/**
 * Test suite to run all cimFusion unit tests
 * @author Sharad Singhal
 */

@RunWith(Suite.class)
@Suite.SuiteClasses( {
	// meta-model tests
	net.aifusion.metamodel.AllTests.class,
	
	// x.509 tests
	// net.aifusion.x509.AllTests.class,
	
	// provider tests
	net.aifusion.providers.AllTests.class,
	
	// utils tests
	net.aifusion.utils.AllTests.class,
	
	// CQL tests
	net.aifusion.cql.AllTests.class,
	
	// HttpServer tests
	net.aifusion.cimserver.AllTests.class,
	
	// ASN.1 tests
	// net.aifusion.asn.AllTests.class
	
})

public class AllTests {
	public static void main(String[] args) {
		if(args.length == 0) {
			Result result = JUnitCore.runClasses(AllTests.class);
			if(!result.wasSuccessful()) {
				System.out.println(result.getFailureCount()+" tests failed");
				for (Failure failure : result.getFailures()) {							
					System.out.println(failure.toString());					
				}
			} else {
				System.out.println("All Tests passed");
			}
		} else {
			for(String arg : args) {
				try {
					Class<?> cls = AllTests.class.getClassLoader().loadClass("net.aifusion."+arg);
					System.out.println("Running "+cls.getName()+" ... ");
					Result result = JUnitCore.runClasses(cls);
					if(!result.wasSuccessful()) {
						System.out.println(" failed");
						for (Failure failure : result.getFailures()) {							
							System.out.println(failure.toString());					
						}
					} else {
						System.out.println(" passed");
					}
				} catch (ClassNotFoundException e) {
					System.out.println("Could not load class "+arg);
				}
			}
		}
		System.exit(0);
	}

}
