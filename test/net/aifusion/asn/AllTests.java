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
