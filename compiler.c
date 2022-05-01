/************************************************************************
University of Leeds
School of Computing
COMP2932- Compiler Design and Construction
The Compiler Module

I confirm that the following code has been developed and written by me and it is entirely
the result of my own work. I also confirm that I have not copied any parts of this program
from another person or any other source or facilitated someone to copy this program from
me. I confirm that I will not publish the program online or share it with anyone without
permission of the module leader.

Student Name:
Student ID:
Email:
Date Work Commenced:
*************************************************************************/

#include "compiler.h"

#include <dirent.h>
#include <stdio.h>
#include <string.h>

int InitCompiler() { return InitSymbolTable(); }

ParserInfo parse_dir(char *dir_name) {
  ParserInfo p;
  DIR *directory;
  struct dirent *entry;
  directory = opendir(dir_name);
  while ((entry = readdir(directory)) != NULL) {
    // Make sure file ends with .JACK.
    char *ext = strrchr(entry->d_name, '.');
    if (strcmp(ext, ".jack") != 0 && strcmp(ext, ".JACK") != 0) continue;

    char path[1024];
    sprintf(path, "./%s/%s", dir_name, entry->d_name);

    InitParser(path);
    p = Parse();
    StopParser();

    if (p.er != none) return p;
  }
  closedir(directory);
  return p;
}

ParserInfo compile_libs() {
  char *libs[] = {"Array.jack",  "Keyboard.jack", "Math.jack", "Output.jack",
                  "Screen.jack", "String.jack",   "Sys.jack",  "Memory.jack"};

  ParserInfo p;
  for (int i = 0; i < 2; i++) {
    SetPassMode(i == 0 ? FIRST : SECOND);
    for (int j = 0; j < 8; j++) {
      InitParser(libs[j]);
      p = Parse();
      StopParser();
      if (p.er != none) return p;
    }
  }

  return p;
}

ParserInfo compile(char *dir_name) {
  ParserInfo p;
  p.er = none;

  // First compile the JACK libs.
  p = compile_libs();
  if (p.er != none) return p;

  // Compile source code.

  // FIRST pass.
  SetPassMode(FIRST);
  p = parse_dir(dir_name);
  if (p.er != none) return p;

  // SECOND pass.
  SetPassMode(SECOND);
  p = parse_dir(dir_name);

  return p;
}

int StopCompiler() { return StopSymbolTable(); }

#ifndef TEST_COMPILER
// int main ()
//{
//	InitCompiler ();
//	ParserInfo p = compile ("Pong");
////	PrintError (p);
//	StopCompiler ();
//	return 1;
//}
#endif
