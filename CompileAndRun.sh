#!/bin/bash
cc -std=c99 compiler.c lexer.c symbols.c parser.c vm_writer.c CodeGrader.c -o CodeGrader;
./CodeGrader;