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
 * Created Aug 13, 2017 by sharad
 */
package net.aifusion.cimserver;

import java.io.IOException;

/**
 * Class to create a CimServer. A CIM Server is a long-running service that manages CIM objects
 * @author Sharad Singhal
 */
public class CimServer extends HttpServer {
	public CimServer(CimServerConfiguration config) throws IOException {
		super(config);
	}
	
	/**
	 * Start the Cim Server.
	 * @param args - the program takes the following arguments
	 * <dl>
	 * <dt>-h | -h</dt><dd>(must be only argument) print a help message and exit</dd>
	 * <dt>-c configId</dt><dd>use configuration identified by ID [defaultConfig]</dd>
	 * <dt>-cp path</dt><dd>use specified directory to locate the configuration [resources/config]</dd>
	 * <dt>-n nameSpace</dt><dd>use specified namespace to locate the configuration [aifusion]</dd>
	 * </dl>
	 */
	public static void main(String[] args) {
		// check if we need to print out help
		if(args.length == 1 && (args[0].startsWith("-h") || args[0].startsWith("-H"))){
			System.out.println("Use:\n$ CimServer options\nwhere options can be:");
			System.out.println("\t-help | -Help				# show help (this message) (must be only argument)");
			System.out.println("\t-n namespacePath			# NameSpacePath to use [aifusion]");
			System.out.println("\t-c configurationId		# ConfigurationID to use [defaultConfig]");
			System.out.println("\t-cp configPath			# Path to the configuration directory [resources/config]");
			return;
		}
		String id = null, path = null, directory = null;
		for(int i = 0; i < args.length-1; i += 2){
			switch(args[i]){
			case "-n":
				path = args[i+1].trim();
				break;
			case "-c":
				id = args[i+1].trim();
				break;
			case "-cp":
				directory = args[i+1].trim();
			default:
				System.out.println("Unknown argument (ignored) "+args[i]+" "+args[i+1]);
				break;
			}
		}
		CimServerConfiguration config = CimServerConfiguration.getConfiguration(id, path, directory);
		if(config == null){
			System.out.println("CimServer: No configuration found at ("+id+","+path+","+directory+")");
			return;
		}
		CimServer server = null;
		try {
			server = new CimServer(config);
			server.startServer();
			Thread.currentThread().join();
		} catch(IOException | InterruptedException ex){
			ex.printStackTrace();
		} finally {
			if(server != null) server.stopServer();
			System.out.println("Stopped");
		}
		return;
	}
}
