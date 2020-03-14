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
 * Created Dec 29, 2013 by Sharad Singhal
 */

package net.aifusion.metamodel;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

/**
 * Class to represent a CIM Datetime (Time stamps and intervals)
 * @author Sharad Singhal
 */
public class DateTime {

	/** true if this DateTime represents an interval, false if it is a time stamp */
	private boolean isInterval;
	/** time stamp dataValue (up to millisecond range) */
	private Calendar timeStamp;
	/** microseconds in timeStamp  (if any) */
	private int microSecondsInTimeStamp = 0;
	/** upper bound (inclusive) of this interval or timeStamp */
	private long upperBound = 0L;
	/** lower bound (inclusive) of this interval or timeStamp */
	private long lowerBound = 0L;
	/** string representation of this dataValue */
	private String dateTimeString;
	// private constants needed for conversions
	private static final long microSecondsPerSecond = 1000000;
	private static final long microSecondsPerMinute = microSecondsPerSecond * 60;
	private static final long microSecondsPerHour = microSecondsPerMinute * 60;
	private static final long microSecondsPerDay = microSecondsPerHour * 24;
	// assume that one year is 365.2422 days (see http://hypertextbook.com/facts/1999/MitchellKrasnerman.shtml)
	// and that a year contains exactly 12 equal months for our purposes
	private static final long microSecondsPerMonth = 2629743840000L; // (long) (microSecondsPerDay * 30.43685); 
	private static final long microSecondsPerYear = 31556926080000L; // (long)(microSecondsPerDay * 365.2422);
	/** Number of microseconds representing the Epoc date (00:00 on Jan 1, 1970) */
	// TODO: for debugging. Change to correct dataValue later
	private static final long microSecondsAtEpoc = 0;
	// private static final long microSecondsAtEpoc = 62135740800000000L;

	/**
	 * Create a TimeStamp using the current time
	 */
	public DateTime() {
		this(new Date());
		return;
	}

	/**
	 * Create a TimeStamp with the given date as its dataValue
	 * @param date - date to be used for this dateTime
	 */
	public DateTime(Date date) {
		Calendar c = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		c.clear();
		c.setLenient(false);
		c.setTime(date);
		initTimeStamp(c);
		return;
	}

	/**
	 * Create a TimeStamp from the given calendar dataValue
	 * @param c - calendar dataValue representing the dataValue
	 */
	public DateTime(Calendar c) {
		initTimeStamp(c);
		return;
	}

	/**
	 * Create a duration (CIM Interval) with the given length, i.e., interval is [duration,duration+1)
	 * @param duration - duration in microseconds
	 * @throws ModelException if duration is negative
	 */
	public DateTime(long duration){
		if(duration < 0) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected positive duration, found "+duration);
		upperBound = lowerBound = duration;
		dateTimeString = getDurationString();
		isInterval = true;
		// System.out.println(dateTimeString+" ["+lowerBound+","+upperBound+"] "+getResolution());
		return;
	}
	
	/**
	 * Create a duration or TimeStamp with the given precision. It is assumed that the timestamp or duration lies within the 
	 * closed interval [minValue,maxValue] microseconds
	 * @param minValue - minimum value of the duration or timestamp in microseconds
	 * @param maxValue - maximum value of the duration or timestamp in microseconds
	 * @param isDuration - true if this datetime represents a duration, false if this datetime is a timestamp
	 * @throws ModelException if minValue &lt; 0, or if maxValue &lt; minValue
	 */
	public DateTime(long minValue,long maxValue, boolean isDuration){
		if(minValue < (isDuration ? 0 : microSecondsAtEpoc) || maxValue < minValue) 
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Illegal duration ["+minValue+","+maxValue+"]");
		upperBound = maxValue;
		lowerBound = minValue;
		if(isDuration){
			dateTimeString = getDurationString();
			isInterval = true;
		} else {
			timeStamp = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
			timeStamp.clear();
			timeStamp.setLenient(false);
			timeStamp.setTime(new Date((minValue-microSecondsAtEpoc)/1000));
			dateTimeString = getDateString(timeStamp,getResolution());
			isInterval = false;
		}
		return;
	}

	/**
	 * Create a DateTime object from the given string value
	 * @param s - string vlue containing either a TimeStamp specification or an interval (duration) specification
	 * <p>The dateValue can be a timestamp represented in 25 characters formatted as<br>
	 * yyyyMMddhhmmss.mmmmmmsutc<br>
	 * where yyyy - years, MM - months (1-12), dd - day (1-31), hh - hours (0 - 23), mm - minutes (0 - 59), ss - seconds (0-59),
	 * mmmmmm - microseconds within the second (starting with 0)<br>
	 * s is a sign (+ or -) indicating offset from UTC with + meaning east of UTC, and - meaning west of UTC<br>
	 * utc is the offset from utc expressed in minutes</p>
	 * <p>It can also be an interval represented as<br>
	 * ddddddddhhmmss.mmmmmm:000<br>
	 * here dddd... represents days, hh hours, mm  minutes, ss seconds, and mmmmmm is microseconds within the Interval. The UTC offset
	 * is always 000, and : represents an interval</p>
	 * <p>Values are left-padded, so that the string is always 25 characters long. If the resolution of the measurement is greater than
	 * one microsecond, the corresponding fields are represented by '*', starting from the lowest digit. The measurement granularity is 
	 * always the entire field, except the microsecond field, where the granularity is single digits.</p>
	 */
	public DateTime(String s) {
		// null strings are invalid
		if(s == null) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"DateTime(String) requires non-null dataValue");
		//strip any leading or trailing quote marks
		if (s.startsWith("\"") && s.endsWith("\"")) {
				s = s.substring(1,s.length()-1);
		}
		
		// s must be exactly 25 characters long, with a "." at index 14
		if(s.length() != 25 || s.charAt(14) != '.'){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected DateTime format, found "+s);
		}
		
		// check  to avoid extraneous characters (e.g., {+,-}) that are possible in the Integer.valueOf() method
		for(int i=0; i < 25; i++){
			if(i == 14 || i == 21) continue;
			char c = s.charAt(i);
			if(c >= '0' && c <= '9' || c == '*') continue;
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected DateTime format, found "+s);
		}
		long resolution = 0;
		// obtain the character at position 21
		switch(s.charAt(21)){
		case ':':
			// have an interval. Validate the utc field
			isInterval = true;
			if(!s.substring(22, 25).equals("000")) throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Interval offset must be 0, found "+s.substring(22, 25));
			
			// compute the duration and resolution of the interval
			String element = s.substring(0,8);	// days
			resolution = microSecondsPerDay;
			lowerBound = Integer.valueOf(element).intValue() * microSecondsPerDay;
			
			element = s.substring(8, 10); // hours
			if(!element.equals("**")){
				resolution = microSecondsPerHour;
				int elementValue = Integer.valueOf(element).intValue();
				if(elementValue < 24)
					lowerBound += elementValue * microSecondsPerHour;
				else
					throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Hours field > 23 in "+s);
				
				element = s.substring(10, 12);	// minutes
				if(!element.equals("**")){
					resolution = microSecondsPerMinute;
					elementValue = Integer.valueOf(element).intValue();
					if(elementValue < 60)
						lowerBound +=  elementValue * microSecondsPerMinute;
					else
						throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Minute field > 59 in "+s);
					
					element = s.substring(12, 14);	// seconds
					if(!element.equals("**")){
						resolution = microSecondsPerSecond;
						elementValue = Integer.valueOf(element).intValue() ;
						if(elementValue < 60)
							lowerBound += elementValue * microSecondsPerSecond;
						else 
							throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Second field > 59 in "+s);
						
						element = s.substring(15, 21);	// microseconds
						for(int i = 0; i < element.length(); i++){
							char c = element.charAt(i);
							if(c == '*') {
								isStars(s,i+15);
								break;
							}
							if(c < '0' || c > '9') throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Non-numeric characters in microsecond field "+s);
							resolution /= 10;
							lowerBound += (element.charAt(i) - '0')*resolution;
						}
					} else isStars(s,12);
				} else isStars(s,10);
			} else isStars(s,8);
			break;
		case '+':
		case '-':
			// have a time stamp
			isInterval = false;
			timeStamp = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
			timeStamp.setLenient(false);
			timeStamp.clear();
			timeStamp.set(1,0,1,0,0,0);
			element = s.substring(21, 25);
			// System.out.println("Offset: "+element);
			timeStamp.set(Calendar.ZONE_OFFSET, Integer.valueOf(element)*60000);	// timezone offset
			resolution = microSecondsPerYear;
			element = s.substring(0, 4);
			timeStamp.set(Calendar.YEAR, Integer.valueOf(s.substring(0, 4)));	// year
			
			element = s.substring(4, 6);	// month
			if(!element.equals("**")){
				resolution = microSecondsPerMonth;
				timeStamp.set(Calendar.MONTH, Integer.valueOf(element)-1);
				element = s.substring(6,8);	// day of month
				if(!element.equals("**")){
					resolution = microSecondsPerDay;
					timeStamp.set(Calendar.DAY_OF_MONTH, Integer.valueOf(element));	
					element = s.substring(8,10); 	// hour
					if(!element.equals("**")){
						resolution = microSecondsPerHour;
						timeStamp.set(Calendar.HOUR_OF_DAY, Integer.valueOf(element));
						element = s.substring(10,12);	// minute
						if(!element.equals("**")){
							resolution = microSecondsPerMinute;
							timeStamp.set(Calendar.MINUTE, Integer.valueOf(element));
							element = s.substring(12,14); // seconds
							if(!element.equals("**")){
								timeStamp.set(Calendar.SECOND, Integer.valueOf(element));
								resolution = microSecondsPerSecond;
								element = s.substring(15,21);
								for(int i = 0; i < 6; i++){
									char c = element.charAt(i);
									if(c == '*') {
										isStars(s,15+i);
										break;
									}
									if(c < '0' || c > '9') throw new ModelException(ExceptionReason.INVALID_PARAMETER,"non-numeric characters in microsecond field "+s);
									resolution /= 10;
									microSecondsInTimeStamp += (element.charAt(i) - '0')*resolution;
								}
								if(microSecondsInTimeStamp > 1000){
									timeStamp.set(Calendar.MILLISECOND, microSecondsInTimeStamp / 1000);
									microSecondsInTimeStamp = microSecondsInTimeStamp % 1000;
								}
							} else isStars(s,12);
						} else isStars(s,10);
					} else isStars(s,8);
				} else isStars(s,6);
			} else isStars(s,4);
			lowerBound = (timeStamp.getTimeInMillis()* 1000) + microSecondsInTimeStamp + microSecondsAtEpoc;
			break;
		default:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected DateTime string, found "+s);
		}
		upperBound = lowerBound + resolution - 1;
		dateTimeString = s;
		// System.out.println(dateTimeString+" ["+lowerBound+","+upperBound+"] "+resolution);
		return;
	}
	
	/**
	 * Validate that the timeStamp contains '*' characters starting at index i
	 * @param s - string to validate
	 * @param index - starting index
	 * @throws ModelException if string is not valid
	 */
	private void isStars(String s, int index){
		for(int i = index; i < 21; i++){
			char c = s.charAt(i);
			if(i == 14 && c != '.' || i != 14 && c != '*') throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Expected dateTime String found "+s);
		}
		return;
	}
	
	/**
	 * Initialize the DateTime dataValue with the given timestamp
	 * @param c - calendar representing the time stamp
	 */
	private void initTimeStamp(Calendar c){
		// save the given timestamp dataValue
		timeStamp = c;
		
		// get the UTC time dataValue in microseconds from the epoc
		long timeInMuSec = c.getTimeInMillis() * 1000 + microSecondsAtEpoc;
		
		// NOTE that calendars do not contain microsecond level information
		// so we assume that the date value is truncated to the nearest millisecond, thus
		// the actual time is in the range (ms, ms+0.999muSec)
		lowerBound = timeInMuSec;
		upperBound = timeInMuSec+999;
		dateTimeString = getDateString(c,getResolution());
		isInterval = false;
		// System.out.println(dateTimeString+" ["+lowerBound+","+upperBound+"] "+getResolution());
		return;		
	}
	
	/**
	 * Get a CIM DateTime string representation for a timestamp with the given resolution
	 * @param c - Calendar containing the timestamp
	 * @param resolution - resolution in microseconds
	 * @return - string formatted with the timestamp
	 */
	private String getDateString(Calendar c, long resolution){
		StringBuilder b = new StringBuilder(25);	
		b.append(String.format("%04d", c.get(Calendar.YEAR)));
		// Note: We assume that if resolution is > 30 days, then we do not know the month
		if(resolution <= microSecondsPerMonth){
			b.append(String.format("%02d", c.get(Calendar.MONTH)+1));
			if(resolution <= microSecondsPerDay){
				b.append(String.format("%02d", c.get(Calendar.DAY_OF_MONTH)));
				if(resolution <= microSecondsPerHour){
					b.append(String.format("%02d", c.get(Calendar.HOUR_OF_DAY)));
					if(resolution <= microSecondsPerMinute){
						b.append(String.format("%02d", c.get(Calendar.MINUTE)));
						if(resolution <= microSecondsPerSecond){
							b.append(String.format("%02d", c.get(Calendar.SECOND)));
							b.append(".");
							int muSec = c.get(Calendar.MILLISECOND)* 1000 + microSecondsInTimeStamp;
							int divisor = 100000;
							for(int i = 0; i < 6; i++){
								int digit = muSec / divisor;
								if(resolution <= divisor){
									b.append(digit);
								} else {
									b.append("*");
								}
								muSec %= divisor;
								divisor /= 10;
							}
						} else {
							b.append("**.******");	// seconds are not known
						}
					} else {
						b.append("****.******");	// minutes and seconds are not known
					}
				} else {
					b.append("******.******");	// minutes, seconds and hours are not known
				}
			} else {
				b.append("********.******");	// everything but year and month is unknown
			}
		} else {
			b.append("**********.******");	// everything but year is unknown
		}
		int offset = c.get(Calendar.ZONE_OFFSET)/60000;
		b.append(offset >= 0 ? String.format("+%03d",offset) : String.format("-%03d",-offset));
		return b.toString();
	}

	/**
	 * Check if this dateTime dataValue represents an interval
	 * @return - true if the dataValue is an interval, false if it is a timestamp
	 */
	public boolean isInterval(){
		return isInterval;
	}
	
	/**
	 * Get a calendar dataValue containing this time stamp dataValue. Note that the microsecond dataValue, if any, is truncated
	 * in the returned calendar
	 * @return - calendar containing the timestamp dataValue
	 * @throws ModelException if this DateTime dataValue is an interval
	 */
	public Calendar getCalendar() {
		if(isInterval){
			throw new RuntimeException("Calendar is not available for Intervals");
		}
		Calendar c = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		c.setTime(timeStamp.getTime());
		return c;
	}

	/**
	 * check if this timestamp comes after the prescribed timestamp
	 * @param prescribedDate - timestamp for comparison
	 * @return - true if the given timestamp is later than this timestamp. False if the given
	 * timestamp is before this timestamp, or if the difference in timestamps is smaller than the
	 * resolution of the timestamps
	 */
	public boolean isAfter(DateTime prescribedDate) {
		Integer i = compareTo(prescribedDate);
		return  i != null && i > 0 ? true : false;
	}
	
	/**
	 * Check if this timestamp is earlier than the prescribed timestamp
	 * @param prescribedDate - timestamp for comparison 
	 * @return - true if the given timestamp is before than this timestamp. False if the given
	 * timestamp is after this timestamp, or if the difference in timestamps is smaller than the
	 * resolution of the timestamps
	 */
	public boolean isBefore(DateTime prescribedDate){
		Integer i = compareTo(prescribedDate);
		// System.out.println("Before: CompareTo returned "+i);
		return i != null && i < 0 ? true : false;
	}
	
	/**
	 * Compare two timestamps, or two intervals based on their precision
	 * @param otherDateTime - interval or timestamp to compare
	 * @return - returns an integer {&lt; 0, 0, or &gt;0} if this dataValue is {&lt;, = or &gt;} the other dataValue. If
	 * the answer cannot be determined given the precision in the values, a null dataValue is returned<br>
	 * <b>Note: Since calendar values are precise only to 1 ms, compareTo() may return null, even if
	 * the calendar values are identical in the two dateTime values</b> and {@link #equals(Object)} returns true.
	 * @throws ModelException if an interval is compared to a timestamp or vice versa
	 */
	public Integer compareTo(DateTime otherDateTime){
		// System.out.println("Compare : "+dateTimeString+" ["+lowerBound+","+upperBound+"] "+
		//		otherDateTime.dateTimeString +" ["+otherDateTime.lower+","+otherDateTime.upper+"] ");
		if(otherDateTime == null || isInterval != otherDateTime.isInterval)
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Null parameter or attempt to compare intervals with timestamps");
		if(upperBound < otherDateTime.lowerBound) return -1;
		if(lowerBound > otherDateTime.upperBound) return 1;
		if(lowerBound == upperBound && otherDateTime.lowerBound == otherDateTime.upperBound && lowerBound == otherDateTime.lowerBound) return 0;
		return null;
	}
	
	/**
	 * Get the resolution of this timeStamp (or interval)
	 * @return - resolution of the timestamp or interval in microseconds
	 */
	public long getResolution(){
		return upperBound-lowerBound+1;
	}
	
	/**
	 * Get the lowerbound (in musec) for this date or interval
	 * @return - lower bound for this date or interval
	 */
	public long getLowerBound(){
		return lowerBound;
	}
	
	/**
	 * Add an interval to this time stamp or interval
	 * @param interval - interval to be added
	 * @return - DateTime value with the interval added to this time stamp or interval
	 * @throws ModelException if a time stamp is added to a timestamp
	 */
	public DateTime add(DateTime interval){
		/*
		 * I(a,b) + I(c,d) = I(a+c,b+d)
		 * T(a,b) + I(c,d) = T(a+c,b+d)
		 */
		if(interval == null) return this;
		if(isInterval){
			// we are an interval, return an interval or timestamp depending on incoming datetime value
			return interval.isInterval ?  new DateTime(lowerBound+interval.lowerBound,upperBound+interval.upperBound, true) : interval.add(this);
		} else if(interval.isInterval){
			// construct a date value with T(a,b) + I(c,d) = T(a+c,b+d)
			return new DateTime(lowerBound+interval.lowerBound,upperBound+interval.upperBound,false);
		}
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Timestamp "+interval+" cannot be added to timestamp "+toString());
	}
	
	/**
	 * Subtract an interval from this time stamp or interval, or a time stamp from this time stamp
	 * @param durationOrTime - interval / timestamp to be subtracted
	 * @return - DateTime value with the interval or time stamp representing the difference
	 * @throws ModelException if a time stamp is subtracted from a time stamp
	 */
	public DateTime subtract(DateTime durationOrTime){
		if(durationOrTime == null) return this;
		if(durationOrTime.isInterval){
			// I(a,b) - I(c,d) = I(a-d,b-c)
			// T(a,b) - I(c,d) = T(a-d,b-c)
			return new DateTime(lowerBound-durationOrTime.upperBound,upperBound-durationOrTime.lowerBound, isInterval);
		} else if(!isInterval){
			// T(a,b) - T(c,d) = I(a-d,b-c)
			return new DateTime(lowerBound-durationOrTime.upperBound,upperBound-durationOrTime.lowerBound, true);
		}
		throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Timestamp "+durationOrTime+" cannot be subtracted from duration "+toString());
	}
	
	/**
	 * Multiply this interval by a factor
	 * @param factor - factor to use for multiplication
	 * @return - DateTime with scaled interval in it
	 * @throws ModelException if this DateTime does not represent an interval
	 * @see #isInterval()
	 */
	public DateTime multiply(double factor){
		if(!isInterval()) throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Multiplication can only be done on intervals");
		return new DateTime((long)(lowerBound * factor), (long)(upperBound * factor), true);
	}
	
	/**
	 * Divide this interval by a factor
	 * @param factor - factor to use for division
	 * @return - DateTime with scaled interval in it
	 * @throws ModelException if this DateTime does not represent an interval
	 * @see #isInterval()
	 */
	public DateTime divide(double factor){
		if(!isInterval()) throw new ModelException(ExceptionReason.NOT_SUPPORTED,"Division can only be done on intervals");
		return new DateTime((long)(lowerBound / factor), (long)(upperBound / factor), true);
	}
	
	/**
	 * Check if this DateTime object is equal to another DateTime object<br>
	 * Note that this method compares the java objects by comparing that the underlying dateTime <b>representations</b>
	 * are identical, i.e., have identical bounds and precision. However, for dateTime values with greater than 1 
	 * microsecond precision, this does not imply that the underlying dateTime <b>values</b> are equal (it may not be possible
	 * to decide equality for the given precision). Use {@link #compareTo(DateTime)} to check for equality of underlying values
	 * using interval arithmetic.
	 * @return true if the given DateTime object is equal to this DateTime object, false otherwise
	 * @see #compareTo(DateTime)
	 */
	public boolean equals(Object o){
		if(o == null || !(o instanceof DateTime)) return false;
		DateTime other = (DateTime)o;
		if(!(this.isInterval == other.isInterval)) return false;
		if(isInterval) return this.dateTimeString.equals(other.dateTimeString);
		if(timeStamp.compareTo(other.timeStamp) != 0) return false;
		return microSecondsInTimeStamp == other.microSecondsInTimeStamp;
	}
	
	/**
	 * Return the dataValue of this duration as a DateTime formatted string
	 */
	private String getDurationString(){
		long lb = lowerBound;
		long resolution = getResolution();
		// System.out.println("Lower: "+l+" Upper: "+u+" Resolution: "+resolution);
		StringBuilder b = new StringBuilder(25);
		// days are always present in the string
		b.append(String.format("%08d", lb / microSecondsPerDay));
		lb = lb % microSecondsPerDay;
		if(resolution <= microSecondsPerHour){
			// show number of hours
			b.append(String.format("%02d", lb / microSecondsPerHour));
			lb = lb % microSecondsPerHour;
			if(resolution <= microSecondsPerMinute){
				// show number of minutes
				b.append(String.format("%02d", lb / microSecondsPerMinute));
				lb = lb % microSecondsPerMinute;
				if(resolution <= microSecondsPerSecond){
					// show number of seconds
					b.append(String.format("%02d", lb / microSecondsPerSecond));
					lb = lb % microSecondsPerSecond;
					b.append(".");
					// System.out.println("MicroSeconds: "+l);
					// now show the microseconds
					long r = 100000;
					for(int i = 0; i < 6; i++){
						if(resolution <= r){
							b.append(String.format("%1d", lb / r));
						} else {
							b.append("*");
						}
						lb = lb % r;
						r = r/10;
					}
				} else {
					b.append("**.******");
				}
			} else {
				b.append("****.******");
			}
		} else {
			b.append("******.******");
		}
		// zone offset is always :000
		b.append(":000");
		return b.toString();
	}
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return dateTimeString;
	}
	
	/**
	 * Returns a MOF representation of the DateTime 
	 * @return String - quoted date dataValue to be used as MOF string
	 */
	public String toMOF() {
		return "\"" + dateTimeString + "\"";
	}
}
