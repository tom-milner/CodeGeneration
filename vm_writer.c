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
  /*
   * Possible combinations:
   * 1. CLASS, METHOD: Calling a local method.
   * 2. FIELD/VAR/LOCAL/STATIC, METHOD: Calling an instance function.
   * 3. *, FUNCTION: Calling a static function.
   */

  // If the subroutine is a method, we'll need to provide a reference to the
  // object to use as "this".
  if (subroutine->kind == METHOD) {
    num_args++;
    if (parent->kind == CLASS) {
      // Set to "this".
      VM_write_push(SEGMENT_POINTER, 0);
    } else {
      VM_write_push(sym_to_seg(parent->kind), parent->num);
    }
  }
  fprintf(output_file, "call %s.%s %d\n", parent->type, subroutine->identifier,
          num_args);
}

void VM_get_array_item(Symbol *array) {
  if (!is_init) return;

  // We need to turn something like bar[k] into *(bar+k).
  VM_write_push(sym_to_seg(array->kind), array->num);
  VM_write("add");

  // Access the element using the "that' pointer.
  VM_write_pop(SEGMENT_POINTER, 1);
  // Push the element to the stack.
  VM_write_push(SEGMENT_THAT, 0);
}

void VM_start_function(Symbol *function) {
  if (!is_init) return;
  fprintf(output_file, "function %s.%s %d\n", function->parent,
          function->identifier, function->num_locals);
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
