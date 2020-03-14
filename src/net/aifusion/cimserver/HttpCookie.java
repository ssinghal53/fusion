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
 * Created Jul 5, 2017 by Sharad Singhal
 */
package net.aifusion.cimserver;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.TimeZone;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.aifusion.metamodel.Constants;


/**
 * Class to represent an HTTP Cookie.
 * @author Sharad Singhal
 */
public class HttpCookie {
	/** Logger for the class */
	private static final Logger logger = Logger.getLogger(HttpCookie.class.getName());
	/** Date formatter */
	private static final SimpleDateFormat gmtFormat = new SimpleDateFormat("E, d MMM yyyy HH:mm:ss 'GMT'", Locale.UK);
	/** pattern to match time values */
	private static final Pattern timePattern = Pattern.compile("(\\d{2}):(\\d{2}):(\\d{2})");
	/** pattern to match day of month values */
	private static final Pattern dayOfMonthPattern = Pattern.compile("(\\d{1,2})");
	/** pattern to match month values */
	private static final Pattern monthPattern = Pattern.compile("(?i)(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)");
	/** pattern to match year values */
	private static final Pattern yearPattern = Pattern.compile("(\\d{2,4})");
	/** indexed month values */
	private static final String months[] = {"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"};
	/** Top level domains file managed by IANA */
	private static final String ianaTLDFile = "http://data.iana.org/TLD/tlds-alpha-by-domain.txt";
	/** Local cached copy of IANA TLD file */
	private static final String tldFileName = "resources/tlds-alpha-by-domain.txt";
	/** Public Suffix list maintained by the Mozilla Foundation */
	private static final String publicSuffixFile ="https://publicsuffix.org/list/public_suffix_list.dat";
	/** Locally cached copy of Public Suffix List */
	private static final String publicSuffixFileName = "resources/public_suffix_list.txt";
	/** Set of all top level domains defined by IANA */
	private static final HashSet<String> tld = new HashSet<String>();
	/** Set of all Public Suffix Definitions */
	private static final HashSet<String> publicSuffix = new HashSet<String>();

	/** Name of the Cookie */
	private String name;
	/** Value assigned to the Cookie */
	private String value = null;
	/** Creation date for the Cookie */
	private long createdAt;
	/** Valid paths for the Cookie */
	private String path;
	/** Valid path elements for the cookie */
	private String [] pathElements;
	/** Valid domain elements for the Cookie (reversed) */
	private String [] domainElements;
	/** Valid domains String */
	private String domain;
	/** True if the Cookie is to be marked Secure */
	private boolean secure;
	/** True if the Cookie is to be marked HttpOnly */
	private boolean httpOnly;
	/** Maximum age of the Cookie (in milliseconds) */
	private long maxAge;
	/** Expiry date for the Cookie (in milliseconds since epoc) */
	private long expires;
	/** Origin Server for this Cookie */
	private String originServer;
	/** Origin URI Path for this Cookie */
	private String originPath;

	/** Initialize static variables */
	static {
		// set the time zone format to GMT
		gmtFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
		// locate all top-level domains defined by IANA
		try {
			File tldFile = new File(tldFileName);
			// if no TLD file is cached or is more than 30 days old, try to retrieve it from IANA
			if(!tldFile.exists() /* || (tldFile.lastModified()+30*24*60*60*1000 < new Date().getTime()) */){
				URL ianaURL = new URL(ianaTLDFile);
				// TODO: This fails if a proxy is not set correctly.
				InputStream input = ianaURL.openStream();	
				FileOutputStream output = new FileOutputStream(tldFile);
				byte [] buffer = new byte[8192];
				int read = 0;
				while((read = input.read(buffer, 0, 8192)) > 0){
					output.write(buffer, 0, read);
				}
				output.close();
				input.close();
			}
			// read the TLD file if it is readable
			if(tldFile.canRead()) {	
				BufferedReader reader = new BufferedReader(new FileReader(tldFile));
				String line;
				while((line = reader.readLine()) != null){
					// TODO: We still need to handle internationalized strings (those starting with XN--) see RFC 3492
					if(line.startsWith("#") || line.startsWith("XN--") || line.trim().isEmpty()) continue;
					tld.add(line.trim());
				}
				reader.close();
			}
			tldFile = new File(publicSuffixFileName);
			// if no public suffix file is cached or is more than 30 days old, try to retrieve it from public_suffix.org
			if(!tldFile.exists() /* || (tldFile.lastModified()+30*24*60*60*1000 < new Date().getTime()) */){
				URL pubURL = new URL(publicSuffixFile);
				InputStream input = pubURL.openStream();
				FileOutputStream output = new FileOutputStream(tldFile);
				byte [] buffer = new byte[8192];
				int read = 0;
				while((read = input.read(buffer, 0, 8192)) > 0){
					output.write(buffer, 0, read);
				}
				output.close();
				input.close();
			}
			// read the Public Suffix file if it is readable
			if(tldFile.canRead()) {	
				BufferedReader reader = new BufferedReader(new FileReader(tldFile));
				String line;
				while((line = reader.readLine()) != null){
					if(line.startsWith("//") || line.trim().isEmpty()) continue;
					String elements[] = line.split(" ");	
					publicSuffix.add(elements[0]);
				}
				reader.close();
			}
			// System.out.println("Read "+tld.size()+" domains and "+publicSuffix.size()+" suffixes");
		} catch (IOException e) {
			logger.log(Level.WARNING,"HttpCookie - unable to load top level domains for validation - ",e);
		}
	}

	/**
	 * Create a cookie at the server side
	 * @param name - name of the cookie (must be non-null)
	 * @param value - value of the cookie (may be null)
	 * @param path - valid paths (may be null)
	 * @param domain - valid domains (may be null)
	 * @param maxAge - maximum age of cookie in seconds (=0 for session cookies)
	 * @param expires - expiration date of the cookie (maybe null if maxAge is defined, or for session cookies)
	 * @param isSecure - true if this cookie should be marked secure
	 * @param isHttpOnly - true if this cookie should be marked httpOnly
	 */
	public HttpCookie(String name, String value, String path, String domain, long maxAge, Date expires, boolean isSecure, boolean isHttpOnly){
		// validate cookie name
		if(name == null || name.trim().isEmpty()) 
			throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"HttpCookie name null or empty");
		name = name.trim();
		for(int i = 0; i < name.length(); i++){
			char c = name.charAt(i);
			if(c <= ' ' || c == '(' || c == ')' || c== '<' || c== '>' || c== '@' || c== ',' || c== ';' || c== ':' 
					|| c== '\\' || c== '"' || c== '/' || c== '[' || c== ']' || c==  '?' || c==  '='
					|| c==  '{' || c==  '}' ){
				throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"HttpCookie expected token, found "+name);
			}
		}
		this.name = name;
		// validate cookie value
		if(value != null && !value.trim().isEmpty()){
			try {
				byte[] octets = value.trim().getBytes(Constants.byteEncoding);
				for(byte b : octets){
					if(b == 0x21 || b >= 0x23 && b <= 0x2B || b >= 0x2D && b <= 0x3A || b >= 0x3C && b <= 0x5B || b >= 0x5D && b <= 0x7E) continue;
					throw new UnsupportedEncodingException();
				}
			} catch (UnsupportedEncodingException e) {
				throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"HttpCookie expected octetString value, found "+value);
			}
			this.value = value.trim();
		}
		// get Path String
		pathElements = getPathElements(path);
		if(path != null) this.path = normalizePath(path);
		// validate cookie domain
		if(!domainIsValid(domain)) 
			throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"HttpCookie expected non-TLD domain component, found "+domain);
		// obtain the domain elements in reverse order
		if(domain != null){			
			domain = domain.trim();
			String [] elements = domain.split("\\.");
			domainElements = new String[elements.length];
			for(int j = 0; j < elements.length; j++){
				domainElements[j] = elements[elements.length-1-j];
			}
		}
		// other Cookie parameters
		if(maxAge >= 0) this.maxAge = maxAge * 1000;
		if(expires != null) this.expires = expires.getTime();
		this.secure = isSecure;
		this.httpOnly = isHttpOnly;
		createdAt = System.currentTimeMillis();	// creation time
		return;
	}
	
	/*
	 * **********************************************************
	 * Static methods to obtain cookies by parsing cookie strings
	 * **********************************************************
	 */

	/**
	 * Obtain list of cookies sent by a client to the server using the Cookie header
	 * @param cookieString - Cookie string value
	 * @return list of cookies sent by the client
	 */
	public static HttpCookie[] parseCookieValues(String cookieString){
		if(cookieString == null || cookieString.trim().isEmpty()) return null;
		String [] elements = cookieString.split(";");
		if(elements.length < 1) return null;
		HttpCookie [] received = new HttpCookie[elements.length];
		int i = 0;
		for(String element : elements){
			String [] nameValue = element.split("=");
			String name = nameValue[0].trim();
			String value = nameValue.length < 2 ? null : nameValue[1].trim();
			received[i++] = new HttpCookie(name,value,null,null,0,null,false,false);
		}
		return received;		
	}

	/**
	 * Obtain a cookie value sent to the client using Set-Cookie header by an origin server
	 * @param originURI - Server URI which returned the cookie
	 * @param cookieString - cookie string value
	 * @return - cookie, or null if the cookie is invalid
	 */
	public static HttpCookie parseSetCookieValue(URI originURI, String cookieString){
		if(originURI == null || cookieString == null){
			throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"Either Origin URI or cookie String is null");
		}
		String [] elements = cookieString.split(";");
		if(elements.length < 1) return null;
		String [] nameValue = elements[0].split("=");
		String cookieName = nameValue[0].trim();	// name of the cookie
		String cookieValue = nameValue[1].trim();	// value of the cookie
		String path = null;
		String domain = null;
		long maxAge = 0;
		Date expires = null;
		boolean isSecure = false;
		boolean isHttpOnly = false;
		for(int i = 1; i < elements.length; i++){
			nameValue = elements[i].split("=");
			String name = nameValue[0].trim().toLowerCase();
			switch(name){
			case "path":
				if(nameValue.length < 2) return null;
				path = nameValue[1].trim();
				break;
			case "domain":
				if(nameValue.length < 2) return null;
				domain = nameValue[1].trim();
				break;
			case "max-age":
				if(nameValue.length < 2) return null;
				maxAge = Long.parseLong(nameValue[1].trim());
				break;
			case "expires":
				if(nameValue.length < 2) return null;
				expires = parseDate(nameValue[1].trim());
				if(expires == null) return null;
				break;
			case "secure":
				isSecure = true;
				break;
			case "httponly":
				isHttpOnly = true;
				break;
			default:
				break;
			}
		}
		HttpCookie cookie = new HttpCookie(cookieName,cookieValue,path,domain,maxAge,expires,isSecure,isHttpOnly);
		cookie.originServer = originURI.getHost();
		cookie.originPath = normalizePath(originURI.getPath());
		// ensure that the serverPath and originServer match any path/domain attributes specified in the cookie
		return cookie.matchesPath(cookie.originPath) && cookie.matchesDomain(cookie.originServer) ? cookie : null ;
	}
	
	/*
	 * ******************************************
	 * Public methods to get state of this cookie
	 * ******************************************
	 */
	
	/**
	 * Get the name for this cookie
	 * @return - string containing name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Get the value of this cookie
	 * @return - cookie value (may be null)
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Test if this Cookie has expired
	 * @return - true if this cookie has expired, false otherwise
	 */
	public boolean isExpired(){
		long now = System.currentTimeMillis();
		// max-age is in milliseconds. MaxAge takes precedence over Expires.
		if(maxAge > 0){
			return maxAge < now-createdAt;
		} else if(expires > 0){
			return expires < now;
		}
		return true;
	}

	/**
	 * Test if this Cookie is persistent
	 * @return - true if the cookie is persistent, false otherwise
	 */
	public boolean isPersistent(){
		return maxAge > 0 || expires > 0;
	}

	/**
	 * Test if this cookie is marked secure 
	 * @return - true if the Secure attribute is set on the Cookie
	 */
	public boolean isSecure(){
		return secure;
	}

	/**
	 * Test if this cookie is marked HttpOnly
	 * @return - true if this cookie is marked HttpOnly
	 */
	public boolean isHttpOnly(){
		return httpOnly;
	}

	/**
	 * Test if this cookie matches the given path
	 * @param path - path to match
	 * @return - true if the cookie path matches the given path, false otherwise
	 */
	public boolean matchesPath(String path){
		// Paths restrict to subdirectory of path ('/' separated). If omitted, use path for the requested URI at set-cookie
		if(path == null || path.isEmpty()) return false;
		// path was set in the cookie
		if(this.path != null){
			String [] givenPathElements = getPathElements(path);
			boolean isValid = true;
			if(givenPathElements.length >= pathElements.length){
				for(int i=0; i < pathElements.length; i++){
					if(!pathElements[i].equalsIgnoreCase(givenPathElements[i])){
						isValid = false;
						break;
					}
				}
				return isValid;
			} else {
				return false;
			}
		}
		// origin path was set in the cookie
		if(this.originPath != null){
			return this.originPath.equalsIgnoreCase(path);
		}
		return false;
	}

	/**
	 * Test if this cookie matches the given domain
	 * @param domain - domain to match
	 * @return - true if the cookie domain matches the given domain, false otherwise
	 */
	public boolean matchesDomain(String domain){
		if(domain == null || domain.isEmpty()) return false;
		if(domainElements != null){
			// Cookie contains a domain element, match with the given domain
			// Domains: x.y.com is valid for *.x.y.com but not for y.com. 
			String [] elements = domain.split("\\.");
			boolean isValid = false;
			if(elements.length >= domainElements.length){
				for(int i = 0, j = elements.length-1; i < domainElements.length; j--,i++){
					if(!domainElements[i].equalsIgnoreCase(elements[j])) return false;
					if(!tld.contains(elements[j].toUpperCase())) isValid = true;
				}
				return isValid;
			}
		}
		// if no domain was defined in the cookie, only the origin server will match
		if(originServer != null && originServer.equalsIgnoreCase(domain)) return true;
		return false;
	}

	/**
	 * Check if this cookie matches another cookie. Cookies match if they have the same name and value
	 * @param other - other cookie to match
	 * @return - true if the cookies have the same name and value, false otherwise
	 */
	public boolean matches(HttpCookie other){
		return name.equals(other.name) && value.equals(other.value);
	}
	
	/**
	 * Get the value of this cookie appropriate for insertion at the client
	 * @return - name=value pair for this cookie
	 * @see #toString()
	 */
	public String clientValue(){
		StringBuilder sb = new StringBuilder(name).append("=");
		if(value != null) sb.append(value);
		return sb.toString();
	}

	/**
	 * Get the value of this cookie appropriate for insertion at the server
	 * @return - name=value [cookie options]
	 * @see #clientValue()
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(name).append("=");
		if(value != null) sb.append(value);
		if(path != null) sb.append("; Path=").append(path);
		if(domain != null) sb.append("; Domain=").append(domain);
		if(maxAge != 0) sb.append("; Max-Age=").append(maxAge/1000);
		if(expires != 0) sb.append("; Expires=").append(gmtFormat.format(new Date(expires)));
		if(httpOnly) sb.append("; HttpOnly");
		if(secure) sb.append("; Secure");
		return sb.toString();
	}
	
	/* 
	 * ********************************************
	 * Private helper methods used in the class
	 * ********************************************
	 */
	
	/**
	 * Parse a date string from the Cookie using algorithm defined in RFC 6265
	 * @param dateString - date string from the cookie
	 * @return date value, or null if illegal
	 */
	private static Date parseDate(String dateString) {
		boolean foundTime=false, foundDayOfMonth=false,foundMonth=false,foundYear=false;
		int hour = 0,minute = 0,second = 0,day = 0,year = 0,month = 0;
		String [] tokens = dateString.split("[, ]");
		for(String token : tokens){
			if(!foundTime){
				Matcher m = timePattern.matcher(token);
				if(m.matches() && m.groupCount() == 3){
					hour = Integer.parseInt(m.group(1));
					minute = Integer.parseInt(m.group(2));
					second = Integer.parseInt(m.group(3));
					System.out.println("hh:mm:ss = "+hour+":"+minute+":"+second);
					if(hour > 23 || minute > 59 || second > 59) return null;
					foundTime = true;
					continue;
				}
			}
			if(!foundDayOfMonth){
				Matcher m = dayOfMonthPattern.matcher(token);
				if(m.matches() && m.groupCount() == 1){
					day = Integer.parseInt(m.group(1));
					System.out.println("day = "+day);
					if(day < 1 || day > 31) return null;
					foundDayOfMonth = true;
					continue;
				}
			}
			if(!foundMonth){
				Matcher m = monthPattern.matcher(token);
				if(m.matches() && m.groupCount() == 1){
					String mon = m.group(1).toLowerCase();
					System.out.println("month = "+mon);
					for(int i = 0; i < months.length; i++){
						if(months[i].equals(mon)){
							month = i;
							break;
						}
					}
					foundMonth = true;
					continue;
				}
			}
			if(!foundYear){
				Matcher m = yearPattern.matcher(token);
				if(m.matches() && m.groupCount() == 1){
					year = Integer.parseInt(m.group(1));
					if(year>=70 && year<=99){
						year += 1900;
					}else if(year >= 0 && year <= 69){
						year += 2000;
					}
					System.out.println("year = "+year);
					if(year <= 1601) return null;
					foundYear = true;
					continue;
				}
			}
		}
		if(!foundTime || !foundDayOfMonth || !foundMonth || !foundYear ) return null;
		Calendar c = Calendar.getInstance(TimeZone.getTimeZone("UTC"), Locale.US);
		c.setLenient(false);
		c.set(year, month, day, hour, minute,second);
		return c.getTime();
	}
	
	/**
	 * Normalize a given path by removing all empty, '.' and '..' segments. A null path will resolve to "/"
	 * @param path - given path
	 * @return - normalized value of the path
	 */
	private static String normalizePath(String path){
		// get Path String
		if(path != null){
			String [] pathElements = getPathElements(path);
			if(pathElements.length > 0){
				StringBuilder b = new StringBuilder();
				for(String s : pathElements){
					b.append("/");
					b.append(s);
				}
				return b.toString();
			}
		}
		return "/";
	}

	/**
	 * Get path elements from a local path definition. The path is split using "/" or "\" characters.
	 * All '.' and empty segments are ignored, and all ".." segments move up the hierarchy without going past 
	 * the initial level. All path elements are converted to lower case
	 * @param localPath - local path to be broken
	 * @return - vector containing path elements
	 */
	private static String [] getPathElements(String localPath){
		Vector<String> pathElements = new Vector<String>();
		if (localPath != null) {
			String[] elements = localPath.split("[/\\\\]");
			for (int i = 0; i < elements.length; i++) {
				if(elements[i] == null) continue;
				String element = elements[i].trim().toLowerCase();
				if (element.length() == 0 || element.equals(".")) continue;
				if(element.equals("..")){
					if(pathElements.size() > 0){
						pathElements.setSize(pathElements.size()-1);
						continue;
					} else {
						// attempt to go above the path root. Throw an exception
						throw new HttpException(HttpStatus.NOT_ACCEPTABLE,"Path "+localPath+"resolves above /");
					}
				}
				pathElements.add(element);
			}
		}
		String [] elements = new String[pathElements.size()];
		for(int i=0; i<elements.length;i++) elements[i] = pathElements.get(i);
		return elements;
	}
	
	/**
	 * Check if a domain is valid (is not a tld or public suffix)
	 * @param domain - domain string to check
	 * @return - true if the domain is valid, false otherwise
	 */
	private boolean domainIsValid(String domain){
		if(domain == null || domain.trim().isEmpty()) return true;
		boolean isValid = false;
		domain = domain.trim();
		String [] elements = domain.split("\\.");
		for(String dom : elements){
			if(dom == null || dom.isEmpty()) continue;
			if(!tld.contains(dom.toUpperCase())){
				isValid = true; // at least one element of domain is not TLD
				break;
			}
		}
		// if no TLD match, look at public suffix match
		return isValid ? isNotPublicSuffix(domain) : false;
	}

	/**
	 * Check if a domain is part of the public suffix list list
	 * @param domain - domain to check
	 * @return - true if the domain does not match a public suffix, false otherwise
	 */
	private boolean isNotPublicSuffix(String domain) {
		domain = domain.toLowerCase();
		for(String ps : publicSuffix){
			if(ps.startsWith("*")){
				// System.out.println("matching "+domain+" to "+ps);
				String domain0 = domain.substring(domain.indexOf(".")+1);	// remove first component in domain
				String ps0 = ps.substring(2);	// remove the '*.' in public suffix
				// if(ps.equals("*.ck")) System.out.println("\tmatching "+domain0+" to "+ps0);
				if(ps0.equals(domain0)){ // have a match
					// System.out.println("Found match with "+ps+" check for exceptions");
					// check if there are exceptions to the rule
					for(String ps1 : publicSuffix){
						if(!ps1.startsWith("!")) continue;
						// System.out.println("Checking "+ps1+" against ");
						if(domain.equals(ps1.substring(1))) return true;
					}
					return false;
				}
				// TODO: We currently do not yet deal with *.* forms, but we don't see them in the list
			} else if(ps.startsWith("!")){
				// System.out.println("matching "+domain+" to "+ps);
				if(domain.equals(ps.substring(1))) return true;
			} else if(ps.equals(domain)) return false;
		}
		return true;
	}
}
