/**
<p>This package contains an implementation of a metamodel largely based on the 
<a href ="https://www.dmtf.org/sites/default/files/standards/documents/DSP0004_3.0.1.pdf">DMTF Common Information Model (CIM) Metamodel (version 3)</a>,
and language bindings to Java 1.8.
Note, however, that the implementation differs from the CIM metamodel in the following ways:</p>
<ul>
<li>ASSOCIATION classes are not implemented. The functionality of Associations can be implemented using standard CLASS constructs</li>
<li>OUT parameters are not implemented. OUT parameters were used in CIM version 2 to return multiple values. Given that the version 3 
metamodel now contains structures, which allow multiple values to be returned, OUT parameters
are unnecessary, and cause complexity when mapping to Java</li>
<li>INTERFACE classes have been added to the metamodel. They enable multiple inheritence in the model, and map to java interface classes.</li>
</ul>
<h2>Package Specification</h2>
This package contains the basic implementation classes needed for the CIM metamodel, as well as a MOF parser.
It also contains a number of CIM repository implementations.
<ul>
<li>Primary Java classes corresponding to CIM elements
 <ul>
 <li>CimQualifierType - Defines a qualifier type. Standard qualifier types are "built-in" and do not need
 	to be separately defined</li>
 <li>CimQualifier - defines qualifiers on individual CIM elements</li>
 <li>CimEnumeration - defines enumerations (a type containing integer or string enumerated values)</li>
  <li>CimStructure - defines a CIM structure (a type containing only properties)</li>
  <li>StructureValue - defines a CIM Structure value (instantiates values for a CIM Structure)</li>
  <li>CimClass - defines a CIM class (a type containing both properties and methods</li>
  <li>CimInstance - represents a CIM instance. Instantiates a CIM class</li>
  <li>CimInterface - represents a CIM interface. Interfaces are an extension to the standard metamodel</li>
 </ul></li>
<li>Metamodel Elements used within the primary classes
 <ul>
  <li>CimProperty - defines a CIM property</li>
  <li>CimMethod - defines CIM Methods</li>
  <li>CimParameter - defines method parameter</li>
 </ul></li>
<li>Other metamodel items
 <ul>
  <li>CimFlavor (Enum) - Cim Qualifier Flavors</li>
  <li>CimScope (Enum) - Cim Qualifier Scopes</li>
  <li>CimElementType (Enum) - Cim element types</li>
 </ul></li>
 <li>Other elements used as base classes
 <ul>
 <li>CimQualifedElement - base class representing a qualified element (class, instance, property, method, parameter)</li>
  <li>CimNamedElement - base class representing all CIM named elements</li>
  <li>CimElement - base class that represents all CIM elements</li>
  <li>NameSpacePath - class representing a CIM namespace</li>
  </ul></li>
<li>CIM data types
 <ul>
  <li>CimDataType (Enum) - known CIM data types</li>
 </ul></li>
<li>CIM data values
 <ul>
  <li>DataValue - encapsulating class for all CIM data value types</li>
  <li>DateTime - CIM dateTime values</li>
  <li>UInt8 - CIM UINT8 values</li>
  <li>UInt16 - CIM UINT16 values</li>
  <li>UInt32 - CIM UINT32 values </li>
  <li>UInt64 - CIM UINT64 values</li>
  <li>OctetString - CIM byte array values</li>
  <li>ObjectPath - class representing a CIM object name (reference)</li>
 </ul></li>	
<li>Exceptions
 <ul>
  <li>ModelException - RuntimeException used by the implementation</li>
  <li>ExceptionReason - Reason for Exception (used by ModelException)</li>
 </ul></li>	
 <li>Miscellaneous
 <ul>
  <li>StandardQualifierTypes - provides static methods to initialize the repository with known Qualifiers (DSP0004, Cim Version 3.0) as well as the following
  extensions:
  <dl>
   <dt>Implements</dt><dd> Implements is used to declare that a class implements the methods, properties, and references declared in the corresponding interface class</dd>
   </dl>
   </li>
 </ul>
<li>Parser and Repository Interfaces
 <ul>
  <li>Parser - interface implemented by all parsers</li>
  <li>Repository - interface implemented by all repositories</li>
 </ul></li>	
<li>MOF Parser
 <ul>
  <li>MOFParser - Recursive descent MOF parser</li>
 </ul></li>
<li>CIM Repositories
 <ul>
  <li>InMemoryRepository - a lightweight in-memory repository for named elements. Supports a single namespace, and no persistence</li>
  <li>InMemoryCache - an in-memory repository that supports namespaces</li>
  <li>BufferedCache - an in-memory repository with a backing store. Used for parsing CIM messages.</li>
  <li>PersistentCache - a file-system based persistent repository. Supports namespaces and persistence</li>
 </ul></li>
 <li>Tools
 <ul>
  <li>Java2Cim - provides the ability to introspect annotated java code and generate the corresponding metamodel classes from them
  (which can in turn generate the corresponding MOF). This enables the CIM definitions to be generated directly from java code, and
  prevents divergence of the model from its language bindings. The factory relies on the following java annotations
  <dl>
   <dt>Export</dt><dd> Marks the java element (class/method) as an element that can be exported to the Cim Environment</dd>
   </dl>
   </li>
 </ul></li>
 <li>The implementation supports a rudimentary event system supported by the following classes:
 <ul>
  <li>CimEvent - interface defining a CIM Event</li>
  <li>CimEventType - interface defining a Cim Event type</li>
  <li>CimEventGenerator - interface implemented by elements that can generate events</li>
  <li>CimListener - interface implemented by elements that can receive events</li>
   <li>CimIndication - represents a CIM Indication (an event instance)</li>
 </ul></li>
</ul>
<h2>Related Documentation</h2>
Please see the <a href="http://dmtf.org/sites/default/files/standards/documents/DSP0004_3.0.0.pdf">
Common Information Model (CIM) Metamdel</a> (DSP0004) Version 3.0.1, Date 2014-08-30 for the CIM metamodel specification.
 */
package net.aifusion.metamodel;