/**
This package contains an implementation of a CIM Client and CIM Server based on a lightweight HTTP server.
<p>Note that unlike <a href="https://www.dmtf.org/sites/default/files/standards/documents/DSP0200_1.4.0.pdf">
CIM Operations over HTTP (DSP0200)</a>, the server uses MOF and text files directly over HTTP to avoid a second layer of translation to XML.</p>
<p>All underlying data is saved in MOF form by the server using a {@link net.aifusion.metamodel.Repository}. The CIM Client presents a 
{@link net.aifusion.providers.Provider} interface to the application, and communicates to the server over HTTP to retrieve information from
the server-side repository.</p>
<p>The server is configured using {@link net.aifusion.cimserver.HttpConfiguration} that is also stored in MOF format by the server. Both the CIM server
and the HTTPConfiguration classes can be used from the command line (they have <code>main()</code> methods) to manage the server and its configuration,
respectively.</p> 
*/
package net.aifusion.cimserver;