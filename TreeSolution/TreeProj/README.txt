README for running our Project 3

Authors: 
Andreas David Lauritzen, s134849
Silas Sebastian Pihl, s134860

1. Files
All files beside the project is included in the "TreeSolution" folder.
- Code files:
		- AST.fs		(Project 2)
		- Parser.fsi	(Project 2)
		- Parser.fs		(Project 2)
		- Lexer.fs 		(Project 2)
		- Util.fs		(Project 2)
		- Translate.fs
		- Design.fs
		- Draw.fs
- PDF of .ps and them self sample files in folder "Chosen ps files and pdf"
- .gc files for convenience in "gc_files" folder

2. RUN THE CODE
First steps before running the "TreeScript.fsx" file
	- Change the "localPath" in top of file to same path as all code files (working directory)
	- Optional: Change the "gcfiles" (whereever your gcfiles are placed, you can use the "gc_files" folder)
	- RUN THE HOLE "TreeScript.fsx" file (no build needed)
	
A "example.gc" file has been used to create "example.ps" in working directory, take a look (only if you didn't change the "gcfiles").

Second steps to get some more results
	- You can change "example" to any other filename with extension .gc (write only the raw filename)
	  Now try to run the "draw" command again (line 56)
	- More options are available in the bottom of the "TreeScript.fsx" file. 