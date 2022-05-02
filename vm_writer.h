//
// Created by Tom Milner on 19/04/2022.
//

#ifndef SYMBOLTABLEGRADER__VM_WRITER_H_
#define SYMBOLTABLEGRADER__VM_WRITER_H_

#include "symbols.h"

typedef enum {
  SEGMENT_ARG,
  SEGMENT_CONST,
  SEGMENT_LOCAL,
  SEGMENT_STATIC,
  SEGMENT_THIS,
  SEGMENT_THAT,
  SEGMENT_POINTER,
  SEGMENT_TEMP
} VM_Data_Segment;

int VM_init(char *filename);

void VM_call_function(Symbol *parent, Symbol *subroutine, int num_args);
void VM_array_item(Symbol *array);
void VM_start_function(Symbol *function);
void VM_write_push(VM_Data_Segment segment, int index);
void VM_write_string_literal(char *literal);
void VM_write(char *command);
void VM_write_pop(VM_Data_Segment segment, int index);
void VM_start_if(int serial_no);
void VM_start_else(int serial_no);
void VM_stop_if(int serial_no);

int VM_stop();

VM_Data_Segment sym_to_seg(SymbolKind symbol_kind);

#endif  // SYMBOLTABLEGRADER__VM_WRITER_H_