/**
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
  <li>Policy (Enum) - Cim Qualifier propagation policies</li>
  <li>CimScope (Enum) - Cim Qualifier scopes</li>
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
<li>CIM data values (not mapped to standard java values)
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
  <li>StandardQualifierTypes - provides static methods to initialize the repository with known Qualifiers (DSP0004, Cim Version 3.0) as well as
  extensions to declare that a class implements the methods, properties, and references declared in the corresponding interface class.
   </li>
 </ul>
<li>CIM Repositories
 <ul>
  <li>Repository - interface implemented by all repositories</li>
  <li>InMemoryRepository - a lightweight in-memory repository for named elements. Supports a single namespace, and no persistence</li>
  <li>InMemoryCache - an in-memory repository that supports namespaces</li>
  <li>BufferedCache - an in-memory repository with a backing store. Used for parsing CIM messages.</li>
  <li>PersistentCache - a file-system based persistent repository. Supports namespaces and persistence</li>
 </ul></li>
 <li>MOF Parsers
 <ul>
  <li>Parser - interface implemented by all parsers</li>
  <li>MOFParser - Recursive descent MOF parser</li>
 </ul></li>
 <li>The implementation supports a rudimentary event system supported by the following classes:
 <ul>
  <li>CimEvent - interface defining a CIM Event</li>
  <li>CimEventType - interface defining a Cim Event type</li>
  <li>CimEventGenerator - interface implemented by elements that can generate events</li>
  <li>CimListener - interface implemented by elements that can receive events</li>
   <li>CimIndication - represents a CIM Indication (an event instance)</li>
 </ul></li>
 <li>Language bindings to Java
 <ul>
 <li>Export - annotations used in java to map to CIM elements</li>
 <li>JavaModelMapper - Helper class to map java to CIM</li>
 </ul>
 </li>
</ul>
 */
package net.aifusion.metamodel;