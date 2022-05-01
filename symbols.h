#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "lexer.h"
#include "parser.h"

// Symbol Table.

// The possible kinds of each symbol.
typedef enum {
  // These can be directly mapped to the VM.
  STATIC,
  ARG,
  VAR,
  FIELD,

  // These can't.
  FUNCTION,
  METHOD,

  CLASS
} SymbolKind;

// An entry in a symbol table.
typedef struct Symbol {
  // The name of the symbol.
  char identifier[128];

  // The type of the symbol.
  char type[128];

  // The kind/scope of the symbol.
  SymbolKind kind;

  // The number of this kind of symbol.
  int num;

  //  struct Symbol *parent;  // The parent scope.
  char parent[128];

  int num_locals;  // Only used by FUNCTION and METHOD.
  int num_fields;  // Only used by CLASS.

} Symbol;

typedef struct {
  char context[128];  // The context the symbol table is for.
  Symbol *table;      // The list of symbols.
  int max_size;       // The amount of symbols we can fit in the table (memory).
  int size;           // How many symbols are currently in the table.
} SymbolTable;

// Setup the symbol table.
int InitSymbolTable();

void StartNewSubroutine(char *name);

void StartNewClass(char *name);

// Destroy the symbol table.
int StopSymbolTable();

char *GetCurrSubroutine();
char *GetCurrClass();

typedef enum { CTX_PROG, CTX_CLASS, CTX_SUBROUTINE } Context;
Context curr_context;

char *get_context_string();

typedef enum { FIRST, SECOND } PassMode;
void SetPassMode(PassMode mode);
PassMode GetPassMode();

// Append to the symbol table.
Symbol *AppendSymbol(Symbol symbol);

Symbol *LookupSubroutine(char *identifier, char *class_name);

// Lookup a symbol in the symbol table, using it's identifier.
Symbol *LookupSymbol(char *identifier);

#endif
