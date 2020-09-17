/**
 * Copyright 2015, Sharad Singhal, All Rights Reserved
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
 * Created Feb 17, 2015 by Sharad Singhal
 */
package net.aifusion.metamodel;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation used for mapping java to CIM. The presence of this annotation on a java class, method, or method parameter provides
 * information to allow the run time to bind the java element to the corresponding CIM definition.
 * @author Sharad Singhal
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({java.lang.annotation.ElementType.METHOD,java.lang.annotation.ElementType.TYPE,java.lang.annotation.ElementType.PARAMETER})
@Documented
public @interface Export {
	/**
	 * Name of the CIM model class. By default, the name of the underlying java class or method is used
	 * @return name of the model class
	 */
	String name() default "";
	
	/**
	 * CIM Namespace within which this element resides. 
	 * @return - namespace within which the element resides.
	 */
	String nameSpace() default Constants.defaultLocalPath;
	
    /**
     * CIM Model Schema used when annotating classes. Default is "CimFusion". Schema names must be of the form "[a-zA-Z]\\w*_\\w+"
     * @return schema associated with the model class
     * */
    String schema() default Constants.defaultSchema;
    
    /**
     * Forces a java class to be defined as a MOF class, even if it only exports properties. Default is false
     * @return true if this class should be modeled as a CIM class, even if it does not have methods
     */
    boolean forceClass() default false;
    
    /**
     * Referenced Java class, used to the name of the corresponding NamedElement for methods/properties that return
     * ObjectPath/EnumValue/StructureValue/CimInstance etc. Empty by default.
     * @return fully qualified name of referenced Java class. Empty by default
     */
    String refClass() default "";
    
    /**
     * Qualifiers, if any on this element. Default is an empty string
     * @return - string containing MOF qualifers, if any.
     */
    String qualifiers() default "";
    /** default value, if any, for the annotated property, parameter or reference. Empty string implies no default value. Ignored for methods.
     * Note that arrays can be specified by enclosing comma separated list in braces, e.g. "{1,2,3}". Also note that string values
     * must contain double quotes, so must be explicitly included e.g., defaultValue="\"stringValue\""
     * @return default value for the element
     */
    public String defaultValue() default "";
    
    /**
     * default version of the class, if any, for annotated enumerations, classes, and structures
     * @return version for the class
     */
    public String version() default Constants.defaultVersion;

}
