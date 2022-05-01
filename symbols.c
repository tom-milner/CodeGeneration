
/************************************************************************
University of Leeds
School of Computing
COMP2932- Compiler Design and Construction
The Symbol Tables Module

I confirm that the following code has been developed and written by me and it is
entirely the result of my own work. I also confirm that I have not copied any
parts of this program from another person or any other source or facilitated
someone to copy this program from me. I confirm that I will not publish the
program online or share it with anyone without permission of the module leader.

Student Name:
Student ID:
Email:
Date Work Commenced:
*************************************************************************/

#include "symbols.h"

#include "stdio.h"
#include "stdlib.h"
#include "string.h"

// A list of the available classes.
static SymbolTable program;

static SymbolTable accessible_subroutines;

// The current class and subroutine.
static SymbolTable curr_class;
static SymbolTable curr_subroutine;

int count[7];

PassMode pass_mode;

void print_symbol(Symbol *symbol) {
  char *symbol_kind_strings[] = {
      [STATIC] = "STATIC", [FIELD] = "FIELD", [ARG] = "ARG",
      [VAR] = "VAR",       [CLASS] = "CLASS", [FUNCTION] = "FUNCTION",
      [METHOD] = "METHOD"};
  printf("{ %s, %s, %s, %d, %s }\n", symbol->identifier, symbol->type,
         symbol_kind_strings[symbol->kind], symbol->num, symbol->parent);
}
void print_table(SymbolTable *table) {
  for (int i = 0; i < table->size; i++) {
    Symbol *symbol = &table->table[i];
    print_symbol(symbol);
  }
}

int InitSymbolTable() {
  // Start the tables off small.
  int start_size = 5;

  program.table = malloc(start_size * sizeof(Symbol));
  program.max_size = start_size;
  program.size = 0;

  accessible_subroutines.table = malloc(start_size * sizeof(Symbol));
  accessible_subroutines.max_size = start_size;
  accessible_subroutines.size = 0;

  curr_class.table = malloc(start_size * sizeof(Symbol));
  curr_class.max_size = start_size;
  curr_class.size = 0;

  curr_subroutine.table = malloc(start_size * sizeof(Symbol));
  curr_subroutine.max_size = start_size;
  curr_subroutine.size = 0;

  count[0] = count[1] = count[2] = count[3] = count[4] = count[5] = count[6] =
      0;

  return 0;
}

Symbol *AppendSymbol(Symbol symbol) {
  // Determine which table to add the symbol to. (scope)
  SymbolTable *scope;
  switch (symbol.kind) {
    case STATIC:
    case FIELD: scope = &curr_class; break;
    case ARG:
    case VAR: scope = &curr_subroutine; break;
    case CLASS: scope = &program; break;
    case FUNCTION:
    case METHOD: scope = &accessible_subroutines; break;
  }

  // If the table is full, we have to expand it.
  if (scope->size == scope->max_size) {
    // Double the size of the table.
    //    printf("Changing table size from %d to %d.\n", scope->max_size,
    //    scope->max_size * 2);
    scope->max_size *= 2;
    scope->table = realloc(scope->table, scope->max_size * sizeof(Symbol));
    if (scope->table == NULL) {
      printf("Error resizing table\n");
      return NULL;
    }
  }

  if (symbol.kind == METHOD || symbol.kind == FUNCTION ||
      symbol.kind == STATIC || symbol.kind == FIELD) {
    strcpy(symbol.parent, curr_class.context);
  } else if (symbol.kind == VAR || symbol.kind == ARG) {
    strcpy(symbol.parent, curr_subroutine.context);
  }

  // Get the number of this kind of symbol.
  symbol.num = count[symbol.kind]++;

  if (symbol.kind == FIELD) {
        LookupSymbol(symbol.parent)->num_fields = count[FIELD];
  }

  if (symbol.kind == VAR) {
    LookupSymbol(symbol.parent)->num_locals = count[VAR];
  }

  symbol.num_locals = symbol.num_fields = 0;

  // Now we can add the symbol to the table.
  int index = scope->size++;
  scope->table[index] = symbol;

  //  printf("\nClass table:\n");
  //  print_table(&class);
  //  printf("\nSubroutine table:\n");
  //  print_table(&subroutine);

  return &(scope->table[index]);
}

char *GetCurrSubroutine() { return curr_subroutine.context; }
char *GetCurrClass() { return curr_class.context; }

char *get_context_string() {
  switch (curr_context) {
    case CTX_CLASS: return curr_class.context;
    case CTX_SUBROUTINE: return curr_subroutine.context;
    case CTX_PROG: return "";
  }
}

Symbol *LookupSymbol(char *identifier) {
  //  printf("Looking for: %s\n", identifier);
  // The symbol is more likely to be in the subroutine table, so start
  // there.
  SymbolTable *tables[] = {&curr_subroutine, &curr_class,
                           &accessible_subroutines, &program};

  for (int i = 0; i < 4; i++) {
    SymbolTable *table = tables[i];
    for (int j = 0; j < table->size; j++) {
      Symbol *item = &(table->table[j]);
      if (strcmp(identifier, item->identifier) == 0) {
        return item;
      }
    }
  }

  //  printf("\nProgram table:\n");
  //  print_table(&program);

  return NULL;
}

Symbol *LookupSubroutine(char *identifier, char *class_name) {
  // Only need to search the accessible_subroutines table.
  for (int i = 0; i < accessible_subroutines.size; i++) {
    Symbol *item = &(accessible_subroutines.table[i]);
    if (strcmp(identifier, item->identifier) == 0 &&
        strcmp(class_name, item->parent) == 0) {
      return item;
    }
  }

  // If the symbol is not found, return an empty symbol.
  return NULL;
}

int StopSymbolTable() {
  free(program.table);
  free(accessible_subroutines.table);
  free(curr_class.table);
  free(curr_subroutine.table);
  return 0;
}

void StartNewClass(char *name) {
  // Reset the "class" table in preparation for a new class.
  curr_class.size = 0;

  // Reset the count for all the internal class variables.
  count[STATIC] = count[FIELD] = count[FUNCTION] = count[METHOD] = 0;

  strcpy(curr_class.context, name);
}

void StartNewSubroutine(char *name) {
  // Reset the "subroutine" table in preparation for a new subroutine scope.
  // Instead of fully reallocating the table, we can just reset the "size"
  // parameter and overwrite everything in the table.
  curr_subroutine.size = 0;

  // Reset the count for all the internal subroutine variables.
  count[ARG] = count[VAR] = 0;

  strcpy(curr_subroutine.context, name);
}
void SetPassMode(PassMode pass) { pass_mode = pass; }

PassMode GetPassMode() { return pass_mode; }

// int main() {
//   InitSymbolTable();
//
//   // Test symbol table.
//   Symbol symbols[] = {
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", FIELD},
//       {"", "string", ARG},
//       {"", "string", STATIC},
//       {"", "string", STATIC},
//       {"", "string", STATIC},
//       {"", "string", STATIC},
//       {"", "string", STATIC},
//       {"", "string", VAR},
//       {"", "string", VAR},
//       {"", "string", VAR},
//   };
//   int num_symbols = sizeof(symbols) / sizeof(Symbol);
//
//   for (int i = 0; i < num_symbols; i++) {
//     sprintf(symbols[i].identifier, "symbol_%d", i);
//     AppendSymbol(symbols[i]);
//   }
//
//   printf("Class table:\n");
//   print_table(&class);
//   printf("\nSubroutine table:\n");
//   print_table(&subroutine);
//
//   char *sym_name = "symbol_8";
//   Symbol returned_symbol = LookupSymbol(sym_name);
//   printf("\nLookup on %s:\n", sym_name);
//   print_symbol(&returned_symbol);
//
//   return 0;
// }

// Every time there is an identifier instance, we need to make sure the
// identifier exists. If the identifier doesn't exist, we need to return
// undecIdentifier.