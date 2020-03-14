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
 * Created Jul 20, 2014 by Sharad Singhal
 */
package net.aifusion.metamodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Calendar;
import java.util.Date;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test cases for DateTime
 * @author Sharad  Singhal
 */
public class DateTimeTest {
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.print("DateTime ");
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
	 * Test method for {@link net.aifusion.metamodel.DateTime#DateTime()}.
	 */
	@Test
	public final void testDateTime() {
		DateTime d = new DateTime();
		assertNotNull(d);
		assertFalse(d.isInterval());
		assertEquals(1000L,d.getResolution());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#DateTime(java.util.Date)}.
	 */
	@Test
	public final void testDateTimeDate() {
		Date date = new Date();
		DateTime d = new DateTime(date);
		assertNotNull(d);
		assertFalse(d.isInterval());
		assertEquals(1000L,d.getResolution());
		Calendar dateCalendar = d.getCalendar();
		Calendar expectedCalendar = Calendar.getInstance();
		expectedCalendar.setTime(date);
		assertEquals(expectedCalendar.getTimeInMillis(),dateCalendar.getTimeInMillis());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#DateTime(java.util.Calendar)}.
	 */
	@Test
	public final void testDateTimeCalendar() {
		Calendar c = Calendar.getInstance();
		DateTime d = new DateTime(c);
		assertNotNull(d);
		// System.out.println(c.getTimeInMillis());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#DateTime(long)}.
	 */
	@Test
	public final void testDateTimeLong() {
		DateTime d = new DateTime(5000L);
		assertNotNull(d);
		assertTrue(d.isInterval());
		assertEquals(1L,d.getResolution());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#DateTime(long, long, boolean)}.
	 */
	@Test
	public final void testDateTimeLongLongBoolean() {
		DateTime d = new DateTime(100L,199L, true);
		assertNotNull(d);
		assertTrue(d.isInterval());
		assertEquals(100L,d.getResolution());
		d = new DateTime(100000L,100199L,false);
		assertNotNull(d);
		assertFalse(d.isInterval());
		assertEquals(200L,d.getResolution());
		assertEquals("19700101000000.100***+000",d.toString());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#DateTime(java.lang.String)}.
	 */
	@Test
	public final void testDateTimeString() {
		String validInterval[] = {"20140720212527.245***:000"};
		String invalidInterval[] = {"20140720212568.245***:000"};
		String validTimeStamp[] = {"20140720212527.245***+000"};
		String invalidTimeStamp[] = {"20140720212568.245***+000", "20150230212568.245***+000"}; // Feb 30, 2015 should fail
		
		// try some valid intervals
		for(String s : validInterval){
			DateTime d = new DateTime(s);
			assertNotNull(d);
			assertEquals(s,d.toString());
		}
		// try some valid time stamps
		for(String s : validTimeStamp){
			DateTime d = new DateTime(s);
			assertNotNull(d);
			assertEquals(s,d.toString());
		}
		// try some invalid intervals
		for(String s : invalidInterval){
			try {
				new DateTime(s);
				fail("Illegal format succeeded "+ s);
			} catch (Exception ex){
				// no-op
			}
		}
		// try some invalid timestamps
		for(String s : invalidTimeStamp){
			try {
				new DateTime(s);
				fail("Illegal format succeeded "+s);
			} catch(Exception ex){
				// no-op
			}
		}
		
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#isInterval()}.
	 */
	@Test
	public final void testIsInterval() {
		DateTime d1 = new DateTime("20140720231121.688145:000");
		DateTime d2 = new DateTime("20140720231121.688146+000");
		assertTrue(d1.isInterval());
		assertFalse(d2.isInterval());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#getCalendar()}.
	 */
	@Test
	public final void testGetCalendar() {
		DateTime d1 = new DateTime("20140720231121.688145+000");
		Calendar c = d1.getCalendar();
		assertEquals(1405897881688L,c.getTimeInMillis());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#isAfter(net.aifusion.metamodel.DateTime)}.
	 */
	@Test
	public final void testAfter() {
		DateTime d1 = new DateTime("20140720231121.688145+000");
		DateTime d2 = new DateTime("20140720231121.688146+000");
		assertFalse(d1.isAfter(d2));
		assertTrue(d2.isAfter(d1));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#isBefore(net.aifusion.metamodel.DateTime)}.
	 */
	@Test
	public final void testBefore() {
		DateTime d1 = new DateTime("20140720231121.688145+000");
		DateTime d2 = new DateTime("20140720231121.688146+000");
		assertTrue(d1.isBefore(d2));
		assertFalse(d2.isBefore(d1));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#compareTo(net.aifusion.metamodel.DateTime)}.
	 */
	@Test
	public final void testCompareTo() {
		Date d = new Date();
		DateTime d1 = new DateTime(d);
		DateTime d2 = new DateTime(d);
		
		// note that normal date values cannot be compared equal using precision arithmetic
		// unless they are precise to microsecond time 
		assertEquals(null, d1.compareTo(d2));
		// however, they will compare equal, since the underlying values are the same
		assertEquals(d1,d2);
		
		d1 = new DateTime("20140720231121.688145+000");
		d2 = new DateTime("20140720231121.688145+000");
		assertEquals(Integer.valueOf(0),d1.compareTo(d2));
		
		d2 = new DateTime("20140720231121.688146+000");
		assertEquals(Integer.valueOf(-1),d1.compareTo(d2));
		
		d2 = new DateTime("20140720231121.688144+000");
		assertEquals(Integer.valueOf(1),d1.compareTo(d2));
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#getResolution()}.
	 */
	@Test
	public final void testGetResolution() {
		assertEquals(1000L,(new DateTime("00016271230058.008***:000")).getResolution());
		assertEquals(10L,(new DateTime("00016271230058.00834*:000")).getResolution());
		assertEquals(1L,(new DateTime("00016271230058.008341:000")).getResolution());
		assertEquals(60000000L,(new DateTime("000162712300**.******:000")).getResolution());
		
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#equals(java.lang.Object)}.
	 */
	@Test
	public final void testEqualsObject() {
		assertEquals(new DateTime(1405897258008000L,1405897258008999L, true),new DateTime("00016271230058.008***:000"));
		assertEquals(new DateTime(new Date(1405897881688L)),new DateTime("20140720231121.688***+000"));
		assertNotEquals(new DateTime(new Date(1405897881688L)),new DateTime("20140720231121.6881**+000"));
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#toString()}.
	 */
	@Test
	public final void testToString() {
		DateTime interval = new DateTime(1405897258008000L,1405897258008999L, true);
		assertEquals("00016271230058.008***:000",interval.toString());
		DateTime timeStamp = new DateTime(new Date(1405897881688L));
		assertEquals("20140720231121.688***+000",timeStamp.toString());
	}

	/**
	 * Test method for {@link net.aifusion.metamodel.DateTime#toMOF()}.
	 */
	@Test
	public final void testToMOF() {
		DateTime interval = new DateTime(1405897258008000L,1405897258008999L, true);
		assertEquals("\"00016271230058.008***:000\"",interval.toMOF());
		DateTime timeStamp = new DateTime(new Date(1405897881688L));
		assertEquals("\"20140720231121.688***+000\"",timeStamp.toMOF());
		
	}
	
	@Test
	public final void testArithmetic(){
		/*
		 * DateTime tests From DSP0004 Version 3.0.1
		 *
		 */
		DateTime d1 = null;
		DateTime d2 = null;
		
		// "000000000011**.******:000" * 60 = "0000000011****.******:000"
		d1 = new DateTime("000000000011**.******:000");
		d2 = d1.multiply(60);
		assertEquals("0000000011****.******:000",d2.toString());
		
		// 60 times adding up "000000000011**.******:000" = "0000000011****.******:000"
		d2 = d1;
		for(int i= 1; i < 60; i++){
			d2 = d2.add(d1);
		}
		assertEquals("0000000011****.******:000",d2.toString());
		
		// division of an interval
		d1 = new DateTime("000000000011**.******:000");
		d2 = d1.divide(60);
		assertEquals("00000000000011.******:000",d2.toString());
		
		d1 = new DateTime("20051003110000.******+000");
		d2 = new DateTime("20051003000000.******+000");
		DateTime i1 = new DateTime("000000001100**.******:000");
		DateTime i2 = new DateTime("000000000011**.******:000");
		
		// add an interval to an interval
		DateTime res = i1.add(i2);
		assertTrue(res.isInterval());
		assertEquals("0000000011****.******:000",res.toString());
		
		// add an interval to a timestamp
		res = d1.add(i1);
		assertFalse(res.isInterval());
		assertEquals("2005100322****.******+000",res.toString());
		
		// add a timestamp to an interval
		res = i1.add(d1);
		assertFalse(res.isInterval());
		assertEquals("2005100322****.******+000",res.toString());
		
		// add a time stamp to a timestamp
		try {
			res = d1.add(d2);
			fail("This should not succeed");
		} catch (ModelException e){
			assertEquals(7,e.getReason().getCode());
		}
		
		// subtract an interval from an interval
		res = i1.subtract(i2);
		assertTrue(res.isInterval());
		assertEquals("0000000010****.******:000",res.toString());
		
		// subtract an interval from a timestamp
		res = d1.subtract(i1);
		assertFalse(res.isInterval());
		assertEquals("2005100223****.******+000",res.toString());
		
		// subtract a timestamp from an interval
		try {
			res = i1.subtract(d1);
			fail("This should not succeed");
		} catch (ModelException e){
			assertEquals(7,e.getReason().getCode());
		}
		
		// subtract a timestamp from a timestamp
		
		res = d1.subtract(d2);
		assertTrue(res.isInterval());
		assertEquals("000000001059**.******:000",res.toString());
		
		
		// NOTE: The DSP0004 values seem to be off by 1 microseconds, thus reducing the precision in many cases (or maybe we are off by 1 microsecond)
		
		// "20051003110000.******+000" + "00000000005959.******:000" = "20051003******.******+000"
		d1 = new DateTime("20051003110000.******+000");
		d2 = new DateTime("00000000005959.******:000");
		assertEquals("200510031159**.******+000",d1.add(d2).toString());
		
		// "20051003112233.******+000" - "00000000002233.******:000" = "20051003******.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003110000.******+000" + "000000000022**.******:000" = "2005100311****.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003112233.******+000" - "00000000002232.******:000" = "200510031100**.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003112233.******+000" - "00000000002233.00000*:000" = "20051003110000.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003112233.******+000" - "00000000002233.000000:000" = "20051003110000.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003112233.000000+000" - "00000000002233.000000:000" = "20051003110000.000000+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003110000.******+000" + "00000000002233.******:000" = "200510031122**.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003110000.******+000" + "00000000002233.00000*:000" = "200510031122**.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003060000.******-300" + "00000000002233.000000:000" = "20051003112233.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003110000.******+000" + "00000000002233.000000:000" = "20051003112233.******+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003060000.000000-300" + "00000000002233.000000:000" = "20051003112233.000000+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003110000.000000+000" + "00000000002233.000000:000" = "20051003112233.000000+000"
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("00000000002233.******:000");
		assertEquals("200510031059**.******+000",d1.subtract(d2).toString());
		
		// "20051003112233.******+000" = "200510031122**.******+000" = Null(uncertain)
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("200510031122**.******+000");
		assertFalse(d1.equals(d2));						// d1 equals d2 returns true because the representations are identical
		assertEquals(null,d1.compareTo(d2));			// d1 compareTo d2 returns null because truth cannot be determined
		assertFalse(d1.isBefore(d2));					// d1 < d2 returns false because truth cannot be determined
		assertFalse(d1.isAfter(d2));					// d1 > d2 returns false because truth cannot be determined	
		
		
		// "20051003112233.******+000" = "20051003112233.******+000" = Null(uncertain)
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("20051003112233.******+000");
		assertTrue(d1.equals(d2));						// d1 equals d2 returns true because the representations are identical
		assertEquals(null,d1.compareTo(d2));			// d1 compareTo d2 returns null because truth cannot be determined
		assertFalse(d1.isBefore(d2));					// d1 < d2 returns false because truth cannot be determined
		assertFalse(d1.isAfter(d2));					// d1 > d2 returns false because truth cannot be determined	
		
		
		// "20051003112233.5*****+000" < "20051003112233.******+000" = Null(uncertain)
		d1 = new DateTime("20051003112233.5*****+000");
		d2 = new DateTime("20051003112233.******+000");
		assertFalse(d1.equals(d2));						// d1 equals d2 returns false because the representations are different
		assertEquals(null,d1.compareTo(d2));			// d1 compareTo d2 returns null as specified in DSP0004 because truth cannot be determined
		assertFalse(d1.isBefore(d2));					// d1 < d2 returns false because truth cannot be determined
		assertFalse(d1.isAfter(d2));					// d1 > d2 returns false because truth cannot be determined
		
		
		// "20051003112233.******+000" = "20051003112234.******+000" = FALSE
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("20051003112234.******+000");
		assertFalse(d1.equals(d2));						// d1 equals d2 returns false
		assertEquals(Integer.valueOf(-1),d1.compareTo(d2));	// d1 compareTo d2 returns -1 (d1 < d2) as in DSP0004
		assertTrue(d1.isBefore(d2));					// d1 < d2 returns true
		assertFalse(d1.isAfter(d2));					// d1 > d2 returns false
		
		// "20051003112233.******+000" < "20051003112234.******+000" = TRUE
		d1 = new DateTime("20051003112233.******+000");
		d2 = new DateTime("20051003112234.******+000");
		assertFalse(d1.equals(d2));						// d1 equals d2 returns false
		assertEquals(Integer.valueOf(-1),d1.compareTo(d2));	// d1 compareTo d2 returns -1 (d1 < d2) as in DSP0004
		assertTrue(d1.isBefore(d2));					// d1 < d2 returns true
		assertFalse(d1.isAfter(d2));					// d1 > d2 returns false
		
		// "20051003112233.000000+000" = "20051003112233.000000+000" = TRUE
		d1 = new DateTime("20051003112233.000000+000");
		d2 = new DateTime("20051003112233.000000+000");
		assertEquals(d1,d2);							// d1 equals d2 returns true because the representations are identical
		assertEquals(Integer.valueOf(0),d1.compareTo(d2));	// d1 compareTo d2 returns 0 as in DSP0004 because values are same
		assertFalse(d1.isBefore(d2));					// d1 < d2 returns false
		assertFalse(d1.isAfter(d2));					// d1 > d2 returns false
		
		// "20051003122233.000000+060" = "20051003112233.000000+000" = TRUE
		d1 = new DateTime("20051003122233.000000+060");
		d2 = new DateTime("20051003112233.000000+000");
		assertEquals(d1,d1);							// d1 equals d2 returns true
		assertEquals(Integer.valueOf(0),d1.compareTo(d2));	// d1 compareTo d2 returns 0 as in DSP0004 because values are same
		assertFalse(d1.isBefore(d2));					// d1 < d2 returns false
		assertFalse(d1.isAfter(d2));					// d1 > d2 returns false
	}
}
