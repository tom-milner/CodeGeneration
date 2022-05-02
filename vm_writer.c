//
// Created by Tom Milner on 19/04/2022.
//

#include "vm_writer.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

static FILE *output_file;

static const char *segment_strings[] = {
    [SEGMENT_CONST] = "constant",  [SEGMENT_ARG] = "argument",
    [SEGMENT_LOCAL] = "local",     [SEGMENT_STATIC] = "static",
    [SEGMENT_THIS] = "this",       [SEGMENT_THAT] = "that",
    [SEGMENT_POINTER] = "pointer", [SEGMENT_TEMP] = "temp"};

static bool is_init = false;

int VM_init(char *filename) {
  // Create and open a .VM file for the source file.

  // Get the ".JACK" extension.
  char *ext = strrchr(filename, '.');
  if (strcmp(ext, ".jack") != 0 && strcmp(ext, ".JACK") != 0) return 1;

  // Overwrite the .JACK with .VM
  int name_length = ext - filename;
  char vm_file_name[1024] = {0};
  strncpy(vm_file_name, filename, name_length);
  strcat(vm_file_name, ".VM");

  // Open the file.
  output_file = fopen(vm_file_name, "w");
  if (output_file == NULL) return 1;

  is_init = true;
  return 0;
}

void VM_write_push(VM_Data_Segment segment, int index) {
  if (!is_init) return;
  fprintf(output_file, "push %s %d\n", segment_strings[segment], index);
}

void VM_write_pop(VM_Data_Segment segment, int index) {
  if (!is_init) return;
  fprintf(output_file, "pop %s %d\n", segment_strings[segment], index);
}

void VM_write(char *command) {
  if (!is_init) return;
  fprintf(output_file, "%s\n", command);
}

void VM_call_function(Symbol *parent, Symbol *subroutine, int num_args) {
  if (!is_init) return;

  fprintf(output_file, "call %s.%s %d\n", parent->type, subroutine->identifier,
          num_args);
}

void VM_array_item(Symbol *array) {
  if (!is_init) return;

  // The result of the index expression is already in the stack! (k)

  // We need to turn something like bar[k] into *(bar+k).
  VM_write_push(sym_to_seg(array->kind), array->num);
  VM_write("add");
}

void VM_start_function(Symbol *function) {
  if (!is_init) return;
  //  printf("function %s.%s %d\n", function->parent,
  //          function->identifier, function->num_locals);
  fprintf(output_file, "function %s.%s %d\n", function->parent,
          function->identifier, function->num_locals);

  if (function->kind == METHOD) {
    // If the subroutine is a method, we need to get the this pointer.
    VM_write_push(SEGMENT_ARG, 0);
    VM_write_pop(SEGMENT_POINTER, 0);
  }
}

int VM_stop() {
  is_init = false;
  fclose(output_file);
  return 0;
}

VM_Data_Segment sym_to_seg(SymbolKind symbol_kind) {
  switch (symbol_kind) {
    case STATIC: return SEGMENT_STATIC;
    case ARG: return SEGMENT_ARG;
    case VAR: return SEGMENT_LOCAL;
    case FIELD: return SEGMENT_THIS;
    default: printf("Invalid symbol to turn into segment."); return -1;
  }
}
void VM_write_string_literal(char *literal) {
  // String literals are handled as string objects.

  // The string length is required as an argument.
  int length = strlen(literal);
  VM_write_push(SEGMENT_CONST, length);

  // Create the string object.
  Symbol *class = LookupSymbol("String");
  Symbol *new = LookupSubroutine("new", class->identifier);
  VM_call_function(class, new, 1);

  // Add the characters of the string.
  Symbol *append = LookupSubroutine("appendChar", class->identifier);
  for (int i = 0; i < length; i++) {
    VM_write_push(SEGMENT_CONST, literal[i]);
    VM_call_function(class, append, 2);
  }
}

void VM_start_if(int serial_no) {
  if (!is_init) return;
  fprintf(output_file, "if-goto IF_TRUE%d\n", serial_no);
  fprintf(output_file, "goto IF_FALSE%d\n", serial_no);
  fprintf(output_file, "label IF_TRUE%d\n", serial_no);
}

void VM_start_else(int serial_no){
  if (!is_init) return;
  fprintf(output_file, "goto IF_END%d\n", serial_no);
  fprintf(output_file, "label IF_FALSE%d\n", serial_no);
}
void VM_stop_if(int serial_no) {
  if (!is_init) return;
  fprintf(output_file, "label IF_END%d\n", serial_no);
}
