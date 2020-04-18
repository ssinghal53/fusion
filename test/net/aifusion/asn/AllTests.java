/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
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
 * Created April 8, 2018 by Sharad Singhal
 */
package net.aifusion.asn;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Test suite to run all cimFusion unit tests
 * @author Sharad Singhal
 */

@RunWith(Suite.class)
@Suite.SuiteClasses( {
	net.aifusion.asn.TagClassTest.class,
	net.aifusion.asn.TagEncodingTest.class,
	net.aifusion.asn.TagTest.class,
	net.aifusion.asn.AsnValueTest.class,
	net.aifusion.asn.BooleanValueTest.class,
	net.aifusion.asn.IntegerValueTest.class,
	net.aifusion.asn.LargeIntegerTest.class,
	net.aifusion.asn.BitStringValueTest.class,
	net.aifusion.asn.OctetStringValueTest.class,
	net.aifusion.asn.NullValueTest.class,
	net.aifusion.asn.RealValueTest.class,
	net.aifusion.asn.BMPStringValueTest.class,
	net.aifusion.asn.GeneralizedTimeValueTest.class,
	net.aifusion.asn.IA5StringValueTest.class,
	net.aifusion.asn.UTCTimeValueTest.class,
	net.aifusion.asn.EnumeratedValueTest.class,
	net.aifusion.asn.VisibleStringValueTest.class,
	net.aifusion.asn.PrintableStringValueTest.class,
	net.aifusion.asn.NumericStringValueTest.class,
	net.aifusion.asn.OidValueTest.class,
	net.aifusion.asn.RelativeOidValueTest.class,
	net.aifusion.asn.SequenceValueTest.class,
	net.aifusion.asn.SetValueTest.class,
	// net.aifusion.ast.TaggedValueTest.class,
	net.aifusion.asn.AsnParserTest.class
})

public class AllTests {

}
