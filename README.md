# FBFarm

A Simple text based farm game written in FreeBASIC. For x-platform compatability (Linux and Windows) the text is printed to a graphics Window (not a console app). Linux terminals (AFAIK) don't support the extended ASCII character by default.

# How to build? 

To compile this program you need the FreeBASIC compiler which is available for Linux and Windows (And yes DOS). This game was tested with version 0.24 of the compiler which can be downloaded from [here](http://www.freebasic.net/get). 

## Windows
1. Open the file farm.bas in a FreeBASIC IDE and use the built in compiler.
2. Navigate inside the console cmd.exe to the folder, now type 
	fbc farm.bas 

## Linux
Navigate to the folder of FBFarm and type
	fbc farm.bas

If you are using Ubuntu 12.10 (Quantal) 64-bit you need to add -l stdc++ so the line becomes:
	fbc -l stdc++ farm.bas
	
	
 
