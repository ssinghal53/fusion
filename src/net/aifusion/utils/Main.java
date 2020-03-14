/**
 * Copyright 2012, Sharad Singhal, All Rights Reserved
 * Copyright 2020, Hewlett Packard Enterprise Development LP
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
 * Last Modified Jan 17, 2020 by Sharad Singhal
 */
package net.aifusion.utils;

import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;

import net.aifusion.AllTests;
import net.aifusion.cimserver.CimServer;
import net.aifusion.cimserver.HttpConfiguration;
import net.aifusion.metamodel.MOFParser;

/**
 * The Main (dispatcher) entry point for all CimFusion tools<br>
 * This program is the main entry point for all tools and provides a command line interface that dispatches
 * the request to the different. The following tools are currently handled:
 * <dl>
 * <dt>Cim Server</dt><dd>Run the cim Server</dd>
 * <dt>AllTests</dt><dd>Run the unit tests</dd>
 * <dt>Configure</dt><dd>Create configuration directories</dd>
 * <dt>MofParser</dt><dd>load mof files into the repository</dd>
 * <dt>Cim2Java</dt><dd>Create java source stubs from MOF</dd>
 * <dt>Java2Cim</dt><dd>Create mof files from java</dd>
 * </dl>
 * 
 * @author Sharad Singhal
 */
public class Main {
	/** commands known to the main dispatcher */
	private static HashMap<String,String> knownCommands;
	static {
		knownCommands = new HashMap<String,String>();
		knownCommands.put("server", "Run the CimServer");
		knownCommands.put("config", "Generate a CimServer configuration");
		knownCommands.put("parse", "Parse a MOF File");
		knownCommands.put("test", "Run unit tests");
		knownCommands.put("j2c", "Java -> Cim generator");
		knownCommands.put("c2j", "Cim -> Java stub generator");
	}
	
	/**
	 * The Main (dispatcher) entry point for all AIFusion tools<br>
	 * This program is the main entry point for all tools and provides a command line interface that dispatches
	 * the request to the different. The following tools are currently handled:
	 * <dl>
	 * <dt>server</dt><dd>Run the cim Server</dd>
	 * <dt>test</dt><dd>Run the unit tests</dd>
	 * <dt>config</dt><dd>Create configuration directories</dd>
	 * <dt>parse</dt><dd>load mof files into the repository</dd>
	 * <dt>c2j</dt><dd>Create java source stubs from MOF</dd>
	 * <dt>j2c</dt><dd>Create mof files from java</dd>
	 * </dl>
	 * @param args - program arguments.
	 */
	public static void main(String[] args) {
		if(args.length == 0){
			showHelp(null,args);
			return;
		} else {
			runCommand(args);
		}
		return;
	}
	/**
	 * Show help for a command
	 * @param javaCommand - the java command that needs to be run. May be null if using the dispatcher or a jar file where the dispatcher
	 * is the default
	 * @param args - command line arguments given
	 */
	public static void showHelp(String javaCommand,String [] args){
		// if no java command given, we will either use the dispatcher, or "java -jar xxx"
		if(javaCommand == null){
			// get name of source jar, if any
			URL url = Main.class.getProtectionDomain().getCodeSource().getLocation();
			if(url != null){
				String sourceLocation = url.getFile();
				if(sourceLocation.endsWith(".jar")){
					javaCommand = "-jar "+sourceLocation.substring(sourceLocation.lastIndexOf("/")+1);
				} else {
					javaCommand = Main.class.getName();
				}
			}
		}	
		if(args.length == 0){
			System.out.println("use: java "+javaCommand+" commandName [options] [file...]\nwhere commandName is one of");
			System.out.println("\twhere commandName is one of");
			for(String cn : knownCommands.keySet()){
				System.out.println("\t\t"+cn+"\t- "+knownCommands.get(cn));
			}
		} else if(javaCommand.equalsIgnoreCase("server")){
			CimServer.main(new String[] {"-help"});
		}
	}
	
	/**
	 * Dispatch a command
	 * @param args - command line arguments. First item in args is the command name
	 */
	private static void runCommand(String [] args){
		String command = args[0].toLowerCase();
		String [] args1 = Arrays.copyOfRange(args,1,args.length);
		switch(command) {
		case "server":
			CimServer.main(args1);
			break;
		case "config":
			HttpConfiguration.main(args1);
			// TODO: Create resource directory
			break;
		case "parse":
			MOFParser.main(args1);
			break;
		case "test":
			AllTests.main(args1);
			break;
		case "j2c":
			Java2Cim.main(args1);
			break;
		case "c2j":
			Cim2Java.main(args1);
			break;
		default:
			showHelp(null,args);
		}
		return;
	}
}
