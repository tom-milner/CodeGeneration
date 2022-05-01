#include "parser.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "symbols.h"
#include "vm_writer.h"

#define s_cmp(s1, s2) ((int)strcmp(s1, s2) == 0)
#define p_err_check(p_info) \
  if (p_info.er != none) return p_info

// you can declare prototypes of parser functions below
ParserInfo classDeclar();
ParserInfo memberDeclar();
ParserInfo classVarDeclar();
ParserInfo type();
ParserInfo subroutineDeclar();
ParserInfo paramList();
ParserInfo subroutineBody();
ParserInfo statement();
ParserInfo varDeclarStatement();
ParserInfo letStatement();
ParserInfo ifStatement();
ParserInfo whileStatement();
ParserInfo doStatement();
ParserInfo subroutineCall();
ParserInfo secondHalfSubroutineCall(char *token_name);
ParserInfo expressionList(int *args_count);
ParserInfo returnStatement();
ParserInfo expression();
ParserInfo relationalExpression();
ParserInfo arithmeticExpression();
ParserInfo term();
ParserInfo factor();
ParserInfo operand();

// The possible checks to perform on an identifier.
typedef enum { ID_NO_CHECK, ID_EXISTS, ID_NEW } IdentifierStatus;
ParserInfo identifier(IdentifierStatus check);

int InitParser(char *file_name) {
  int status = InitLexer(file_name);
  if (status != 0 || GetPassMode() == FIRST) return status;

  curr_context = CTX_PROG;

  // Don't run the VM writer on the first pass.
  return VM_init(file_name);
}

ParserInfo Parse() {
  ParserInfo pi;
  pi = classDeclar();
  if (pi.tk.ec != NoLexErr) pi.er = lexerErr;
  return pi;
}

int StopParser() {
  int status = VM_stop();
  if (status != 0) return status;
  return StopLexer();
}

void print_parse_info(ParserInfo pi) {
  char *err_strings[16] = {"none",
                           "lexerErr",
                           "classExpected",
                           "idExpected",
                           "openBraceExpected",
                           "closeBraceExpected",
                           "memberDeclarErr",
                           "classVarErr",
                           "illegalType",
                           "semicolonExpected",
                           "subroutineDeclarErr",
                           "openParenExpected",
                           "closeParenExpected",
                           "closeBracketExpected",
                           "equalExpected",
                           "syntaxError"};

  printf("Error: %s\n", err_strings[pi.er]);
  if (pi.er != none) {
    printf("Token: %s\n", pi.tk.lx);
    printf("Line No.: %d\n", pi.tk.ln);
  }
}

#ifndef TEST_PARSER
// int main() {
//   int status = InitParser("SquareGame.jack");
//   if (status != 0) {
//     printf("Parser init error.\n");
//     return 1;
//   }
//   ParserInfo pi = Parse();
//   print_parse_info(pi);
//
//   return pi.er == none ? 0 : 1;
// }
#endif

/**
 * Read/Parse an identifier.
 * @param check The check to perform on the identifier.45
 * @return
 */
ParserInfo identifier(IdentifierStatus check) {
  Token next_token = GetNextToken();
  ParserInfo p_info;
  p_info.tk = next_token;
  p_info.er = none;

  // Make sure the token is a valid ID.
  if (next_token.tp != ID) {
    p_info.er = idExpected;
    return p_info;
  }

  if (check == ID_NO_CHECK) return p_info;

  Symbol *sym = LookupSymbol(next_token.lx);
  if (check == ID_EXISTS) {
    // Make sure the symbol exists.
    if (sym == NULL) p_info.er = undecIdentifier;

  } else if (check == ID_NEW) {
    // Make sure the symbol is new in the current scope.
    if (sym != NULL && (s_cmp(sym->parent, get_context_string())))
      p_info.er = redecIdentifier;
  }

  return p_info;
}

/**
 * Check the next token against a given lexeme and TokenType.
 * @param lexeme
 * @param token_type
 * @param error The error to return if there isn't a match.
 * @return
 */
ParserInfo lex_check(char lexeme[128], TokenType token_type,
                     SyntaxErrors error) {
  Token next_token = GetNextToken();
  ParserInfo p_info;
  p_info.tk = next_token;
  if (next_token.tp != token_type || !s_cmp(next_token.lx, lexeme)) {
    p_info.er = error;
  } else {
    p_info.er = none;
  }
  return p_info;
}

// classDeclar -> class identifier { {memberDeclar} }
ParserInfo classDeclar() {
  // Starts with "class".
  ParserInfo p_info = lex_check("class", RESWORD, classExpected);
  p_err_check(p_info);

  // identifier.
  p_info = identifier(GetPassMode() == FIRST ? ID_NEW : ID_EXISTS);
  p_err_check(p_info);

  // Add this class to the symbol table.
  Symbol *class;
  if (GetPassMode() == FIRST) {
    Symbol new_class = {
        .kind = CLASS,
    };
    strcpy(new_class.identifier, p_info.tk.lx);
    strcpy(new_class.type, p_info.tk.lx);

    class = AppendSymbol(new_class);
  } else {
    class = LookupSymbol(p_info.tk.lx);
  }

  StartNewClass(class->identifier);
  curr_context = CTX_CLASS;

  p_info = lex_check("{", SYMBOL, openBraceExpected);
  p_err_check(p_info);

  // Read memberDeclars until we reach a "}".
  Token next_token;
  while (1) {
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL || s_cmp(next_token.lx, "}")) {
      GetNextToken();
      break;
    }

    // Read a memberDeclar
    p_info = memberDeclar();
    p_err_check(p_info);
  }

  curr_context = CTX_PROG;

  return p_info;
}

// memberDeclar -> classVarDeclar | subroutineDeclar
ParserInfo memberDeclar() {
  // classVarDeclr starts with "static" or "field", subroutineDeclar starts with
  // "constructor", "function", "method".

  Token next_token = PeekNextToken();
  ParserInfo p_info;

  if (s_cmp(next_token.lx, "static") || s_cmp(next_token.lx, "field")) {
    p_info = classVarDeclar();
  } else if (s_cmp(next_token.lx, "constructor") ||
             s_cmp(next_token.lx, "function") ||
             s_cmp(next_token.lx, "method")) {
    p_info = subroutineDeclar();
  } else {
    p_info.er = memberDeclarErr;
    p_info.tk = next_token;
  }
  return p_info;
}

// These are the arguments for when a subroutine is called.
// expressionList -> expression { , expression } | ε
ParserInfo expressionList(int *args_count) {
  ParserInfo p_info;
  Token next_token = PeekNextToken();
  p_info.er = none;
  p_info.tk = next_token;

  // The number of arguments in the subroutine call.
  *args_count = 0;

  // An expression list is always followed by a ")". Therefore we can check for
  // "ε" by checking for a ")".
  if (s_cmp(next_token.lx, ")")) {
    return p_info;
  }

  // We don't need to consume the token; it will be part of the expression.
  do {
    // We need an expression.
    p_info = expression();
    p_err_check(p_info);
    (*args_count)++;

    // If the next token isn't a comma, we've processed all the expressions.
    next_token = PeekNextToken();
    if (!s_cmp(next_token.lx, ",")) break;
    GetNextToken();  // Consume the ",".
  } while (1);

  //  The expressions (arguments) for subroutine call will now be on the stack,
  //  we just have to call the function.

  p_info.tk = next_token;
  p_info.er = none;
  return p_info;
}

// operand -> integerConstant | identifier [.identifier ] [ [ expression ] |
// (expressionList ) ] | (expression) | stringLiteral | true | false | null |
// this
ParserInfo operand() {
  ParserInfo p_info;
  Token next_token = PeekNextToken();
  p_info.tk = next_token;
  p_info.er = none;

  // First check for integerConstant.
  if (next_token.tp == INT) {
    GetNextToken();  // Consume the token.
    // Write VM code.
    VM_write_push(SEGMENT_CONST, strtol(next_token.lx, NULL, 10));

    return p_info;
  }

  // stringLiteral
  if (next_token.tp == STRING) {
    GetNextToken();

    // Create a new string using the JACK string class.

    // We have to create a new JACK local variable.

    return p_info;
  }
  // true | false | null | this
  if (next_token.tp == RESWORD) {
    GetNextToken();
    p_info.er = none;
    // null and false are mapped to 0.
    if (s_cmp(next_token.lx, "false") || s_cmp(next_token.lx, "null")) {
      VM_write_push(SEGMENT_CONST, 0);

      // True is mapped to -1.
    } else if (s_cmp(next_token.lx, "true")) {
      VM_write_push(SEGMENT_CONST, 1);
      VM_write("neg");

      // This is pointer 0.
    } else if (s_cmp(next_token.lx, "this")) {
      VM_write_push(SEGMENT_POINTER, 0);

      // Invalid resword.
    } else {
      p_info.er = syntaxError;
    }
    return p_info;
  }

  // If the next token is a "(", the operand is (expression).
  // (expression)
  if (s_cmp(next_token.lx, "(")) {
    GetNextToken();
    // Expression.
    p_info = expression();
    p_err_check(p_info);
    // Closing ")".
    next_token = GetNextToken();
    if (!s_cmp(next_token.lx, ")")) {
      p_info.er = closeParenExpected;
      p_info.tk = next_token;
      return p_info;
    }

    p_info.tk = next_token;
    return p_info;
  }

  // Lastly, check for the identifier.
  // identifier [.identifier ] [ [ expression ] | (expressionList ) ]

  // UPDATE: I don't think this grammar is correct. All variables are private,
  // so there will never be an expression of the form:
  //      identifier.identifier[expression]

  // New expression:
  // identifier ([.identifier](expressionList) | [expression])

  // Get the identifier.
  p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_EXISTS);
  p_err_check(p_info);

  next_token = PeekNextToken();

  // Check if we are accessing an array.
  // identifier[expression]
  if (s_cmp(next_token.lx, "[")) {
    GetNextToken();  // Consume "[".

    char array_name[128];
    strcpy(array_name, p_info.tk.lx);

    // Process the expression.
    p_info = expression();
    p_err_check(p_info);

    // Closing "]".
    next_token = GetNextToken();
    p_info.er = s_cmp(next_token.lx, "]") ? none : closeBracketExpected;

    // Generate the VM code.
    if (GetPassMode() == SECOND) {
      Symbol *array_sym = LookupSymbol(array_name);
      VM_get_array_item(array_sym);
    }

    p_info.tk = next_token;
  }

  // We are accessing a subroutine.
  // [ .identifier ] ( expressionList )
  else if (s_cmp(next_token.lx, ".") || s_cmp(next_token.lx, "(")) {
    p_info = secondHalfSubroutineCall(p_info.tk.lx);
  }
  return p_info;
}

// factor -> ( - | ~ | ε ) operand
ParserInfo factor() {
  ParserInfo p_info;
  Token next_token = PeekNextToken();
  if (next_token.tp == SYMBOL &&
      (s_cmp(next_token.lx, "-") || s_cmp(next_token.lx, "~"))) {
    // Consume the symbol.
    GetNextToken();
  }

  // Now get the operand.
  p_info = operand();
  p_err_check(p_info);

  p_info.er = none;
  return p_info;
}

// term -> factor { ( * | / ) factor }
ParserInfo term() {
  // Get the first factor.
  ParserInfo p_info = factor();
  p_err_check(p_info);

  Token next_token;
  while (1) {
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL &&
        (s_cmp(next_token.lx, "*") || s_cmp(next_token.lx, "/"))) {
      // Consume the token.
      GetNextToken();

      // Must be followed by a factor.
      p_info = factor();
      p_err_check(p_info);

    } else {
      break;
    }
  }

  return p_info;
}

// ArithmeticExpression -> term { ( + | - ) term }
ParserInfo arithmeticExpression() {
  ParserInfo p_info = term();
  p_err_check(p_info);

  Token next_token;
  while (1) {
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL &&
        (s_cmp(next_token.lx, "+") || s_cmp(next_token.lx, "-"))) {
      GetNextToken();

      p_info = term();
      p_err_check(p_info);
    } else {
      break;
    }
  }

  return p_info;
}

// relationalExpression -> ArithmeticExpression { ( = | > | < )
// ArithmeticExpression }
ParserInfo relationalExpression() {
  ParserInfo p_info = arithmeticExpression();
  p_err_check(p_info);

  Token next_token;
  while (1) {
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL &&
        (s_cmp(next_token.lx, "=") || s_cmp(next_token.lx, "<") ||
         s_cmp(next_token.lx, ">"))) {
      GetNextToken();

      // Must be followed by another ArithmeticExpression.
      p_info = arithmeticExpression();
      p_err_check(p_info);
    } else {
      break;
    }
  }

  return p_info;
}

// expression -> relationalExpression { ( & | | ) relationalExpression }
ParserInfo expression() {
  ParserInfo p_info = relationalExpression();
  p_err_check(p_info);

  Token next_token;
  while (1) {
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL &&
        (s_cmp(next_token.lx, "&") || s_cmp(next_token.lx, "|"))) {
      GetNextToken();
      p_info = relationalExpression();
      p_err_check(p_info);
    } else {
      break;
    }
  }
  return p_info;
}

// subroutineCall -> identifier [ .identifier ] ( expressionList )
ParserInfo subroutineCall() {
  ParserInfo p_info;

  // This is either the class name or the subroutine name.
  p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_EXISTS);
  p_err_check(p_info);

  p_info = secondHalfSubroutineCall(p_info.tk.lx);
  return p_info;
}

// This is for the following grammar snippet:
// [ .identifier ] ( expressionList )
ParserInfo secondHalfSubroutineCall(char *token_name) {
  char subroutine_name[128], subroutine_parent_name[128];
  strcpy(subroutine_name, token_name);

  ParserInfo p_info;

  // Check to see if the subroutine is in another class/instance.
  Token next_token = PeekNextToken();
  bool is_external = s_cmp(next_token.lx, ".");

  if (is_external) {
    GetNextToken();  // Consume the '.'.

    // Get the subroutine.
    p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_EXISTS);
    p_err_check(p_info);

    // What we thought was the subroutine is actually a class/instance name.
    strcpy(subroutine_parent_name, subroutine_name);
    subroutine_name[0] = '\0';
    strcpy(subroutine_name, p_info.tk.lx);

  }
  // We are accessing a subroutine in the current class (NOT EXTERNAL).
  else {
    strcpy(subroutine_parent_name, GetCurrClass());
  }

  Symbol *parent_symbol, *subr_symbol;
  if (GetPassMode() == SECOND) {
    // Get the class of the subroutine parent.
    parent_symbol = LookupSymbol(subroutine_parent_name);
    // Make sure the subroutine exists in the class.
    subr_symbol = LookupSubroutine(subroutine_name, parent_symbol->type);
    if (subr_symbol == NULL) {
      p_info.er = undecIdentifier;
      return p_info;
    }

    // If we are accessing an external subroutine, make sure we are
    // accessing it from a valid context.
    if (is_external) {
      bool is_class = s_cmp(subroutine_parent_name, parent_symbol->type);
      bool invalid_context = is_class && subr_symbol->kind == METHOD;
      if (invalid_context) {
        p_info.er = undecIdentifier;
        return p_info;
      }
    }
  }

  // Opening "(".
  p_info = lex_check("(", SYMBOL, openParenExpected);
  p_err_check(p_info);

  // expressionList.
  int arg_count = 0;
  p_info = expressionList(&arg_count);
  p_err_check(p_info);

  // Now we can generate the VM code!
  VM_call_function(parent_symbol, subr_symbol, arg_count);

  // Closing ")".
  p_info = lex_check(")", SYMBOL, closeParenExpected);
  return p_info;
}

// returnStatement -> return [ expression ] ;
ParserInfo returnStatement() {
  // Starts with "return".
  ParserInfo p_info = lex_check("return", RESWORD, syntaxError);
  p_err_check(p_info);

  // If the next token is a ";", there is no expression.
  Token next_token = PeekNextToken();
  if (next_token.tp == SYMBOL && s_cmp(next_token.lx, ";")) {
    GetNextToken();
    p_info.tk = next_token;
    return p_info;
    // SYNTAX_OK
  }

  // If the next token is a "}", we skipped a semicolon.
  if (next_token.tp == SYMBOL && s_cmp(next_token.lx, "}")) {
    p_info.tk = next_token;
    p_info.er = semicolonExpected;
    return p_info;
  }

  // There must be an expression.
  p_info = expression();
  p_err_check(p_info);

  // Semicolon.
  p_info = lex_check(";", SYMBOL, semicolonExpected);

  return p_info;
}

// doStatement -> do subroutineCall ;
ParserInfo doStatement() {
  ParserInfo p_info;

  // "do" keyword comes first.
  p_info = lex_check("do", RESWORD, syntaxError);
  p_err_check(p_info);

  p_info = subroutineCall();

  p_err_check(p_info);

  // Semicolon.
  p_info = lex_check(";", SYMBOL, semicolonExpected);

  return p_info;
}

// whileStatement -> while ( expression ) { {statement} }
ParserInfo whileStatement() {
  // "while" keyword comes first.
  ParserInfo p_info = lex_check("while", RESWORD, syntaxError);
  p_err_check(p_info);

  // Opening "(".
  p_info = lex_check("(", SYMBOL, openParenExpected);
  p_err_check(p_info);

  // Expression.
  p_info = expression();
  p_err_check(p_info);

  // Closing ")".
  p_info = lex_check(")", SYMBOL, closeParenExpected);
  p_err_check(p_info);

  // Opening "{".
  p_info = lex_check("{", SYMBOL, openBraceExpected);
  p_err_check(p_info);

  // 0 or many statements.
  Token next_token;
  while (1) {
    // Keep reading statements until we reach a "}".
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL && s_cmp(next_token.lx, "}")) {
      // Consume the symbol.
      GetNextToken();
      break;
    }

    // This has to be a statement.
    p_info = statement();
    p_err_check(p_info);
  }

  return p_info;
}

// ifStatement -> if ( expression ) { {statement} } [else { {statement} }]
ParserInfo ifStatement() {
  ParserInfo p_info;
  // "if" keywords comes first.
  p_info = lex_check("if", RESWORD, syntaxError);
  p_err_check(p_info);

  // Opening "(".
  p_info = lex_check("(", SYMBOL, openParenExpected);
  p_err_check(p_info);

  // expression.
  p_info = expression();
  p_err_check(p_info);

  // Closing ")".
  p_info = lex_check(")", SYMBOL, closeParenExpected);
  p_err_check(p_info);

  // Opening "{".
  p_info = lex_check("{", SYMBOL, openBraceExpected);
  p_err_check(p_info);

  // 0 or many statements.
  Token next_token;
  while (1) {
    // Keep reading statements until we reach a "}".
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL && s_cmp(next_token.lx, "}")) {
      // Consume the symbol.
      GetNextToken();
      break;
    }

    // This has to be a statement.
    p_info = statement();
    p_err_check(p_info);
  }

  // There might be an "else" statement attached.
  next_token = PeekNextToken();
  if (next_token.tp == RESWORD && s_cmp(next_token.lx, "else")) {
    // Consume "else".
    GetNextToken();

    // Opening "{".
    p_info = lex_check("{", SYMBOL, openBraceExpected);
    p_err_check(p_info);

    // 0 or many statements.
    while (1) {
      // Keep reading statements until we reach a "}".
      next_token = PeekNextToken();
      if (next_token.tp == SYMBOL && s_cmp(next_token.lx, "}")) {
        // Consume the symbol.
        GetNextToken();
        break;
      }

      // This has to be a statement.
      p_info = statement();
      p_err_check(p_info);
    }
  }

  p_info.er = none;
  return p_info;
}

// letStatement -> let identifier [ [ expression ] ] = expression ;
ParserInfo letStatement() {
  ParserInfo p_info;
  Token next_token;

  // "let" keyword comes first.
  p_info = lex_check("let", RESWORD, syntaxError);
  p_err_check(p_info);

  // identifier.
  // Symbols in let statements need to already exist.
  p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_EXISTS);
  p_err_check(p_info);

  // If the variable is an array, we can access it's elements using "[x]".
  // The next token should either be a "[" or a "=".
  next_token = PeekNextToken();
  if (next_token.tp == SYMBOL && s_cmp("[", next_token.lx)) {
    GetNextToken();
    // expression.
    p_info = expression();
    p_err_check(p_info);

    // Closing "]".
    p_info = lex_check("]", SYMBOL, closeBracketExpected);
    p_err_check(p_info);
  }
  // "="
  p_info = lex_check("=", SYMBOL, equalExpected);
  p_err_check(p_info);

  // expression.
  p_info = expression();
  p_err_check(p_info);

  // Semicolon.
  p_info = lex_check(";", SYMBOL, semicolonExpected);
  p_err_check(p_info);

  p_info.er = none;
  return p_info;
}

// varDeclarStatement -> var type identifier { , identifier } ;
ParserInfo varDeclarStatement() {
  ParserInfo p_info = lex_check("var", RESWORD, syntaxError);
  p_err_check(p_info);

  Symbol new_symbol = {.kind = VAR};

  // Now we need a type.
  p_info = type();
  p_err_check(p_info);
  strcpy(new_symbol.type, p_info.tk.lx);  // Store the symbol type.

  // Identifier.
  p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_NEW);
  p_err_check(p_info);
  strcpy(new_symbol.identifier, p_info.tk.lx);  // Store the symbol identifier.

  if (GetPassMode() == SECOND) AppendSymbol(new_symbol);

  // List of identifiers.
  // TODO: This is duplicated in classVarDecl. Move to separate function?
  Token next_token;
  while (1) {
    // First we need a comma.
    // Peek so we don't consume the token (just in case it isn't a comma).
    next_token = PeekNextToken();
    if (!s_cmp(next_token.lx, ",")) {
      // Not a comma, so no more identifiers are coming.
      break;
    }
    GetNextToken();  // Consume the comma.

    // The next token has to be an identifier.
    p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_NEW);
    p_err_check(p_info);
    strcpy(new_symbol.identifier,
           p_info.tk.lx);  // Store the symbol identifier.

    if (GetPassMode() == SECOND) AppendSymbol(new_symbol);
  }

  // Now we need to check for the final ";".
  p_info = lex_check(";", SYMBOL, semicolonExpected);
  p_err_check(p_info);

  p_info.er = none;
  return p_info;
}

// statement -> varDeclarStatement | letStatement | ifStatement | whileStatement
// | doStatement | returnStatement
ParserInfo statement() {
  // Peek next token so we know which checks to run.
  Token next_token = PeekNextToken();
  ParserInfo p_info;

  if (next_token.tp != RESWORD) {
    p_info.er = syntaxError;
    p_info.tk = next_token;
    return p_info;
  }

  // varDeclarStatement
  if (s_cmp("var", next_token.lx)) {
    return varDeclarStatement();
  }

  // letStatement
  if (s_cmp("let", next_token.lx)) {
    return letStatement();
  }

  // ifStatement
  if (s_cmp("if", next_token.lx)) {
    return ifStatement();
  }

  // whileStatement
  if (s_cmp("while", next_token.lx)) {
    return whileStatement();
  }

  // doStatement
  if (s_cmp("do", next_token.lx)) {
    return doStatement();
  }

  // returnStatement
  if (s_cmp("return", next_token.lx)) {
    return returnStatement();
  }

  p_info.er = syntaxError;
  return p_info;
}

// subroutineBody -> { {statement} }
ParserInfo subroutineBody() {
  // Opening brace.
  ParserInfo p_info = lex_check("{", SYMBOL, openBraceExpected);
  p_err_check(p_info);

  // Read statements until we encounter a closing brace.
  Token next_token;
  while (1) {
    next_token = PeekNextToken();
    if (next_token.tp == SYMBOL && s_cmp(next_token.lx, "}")) {
      GetNextToken();
      break;
    }

    // Must be a statement.
    p_info = statement();

    p_err_check(p_info);
  }

  return p_info;
}

// paramList -> type identifier {, type identifier} | ε
ParserInfo paramList() {
  ParserInfo p_info;
  p_info.er = none;

  // If the next token is a ")", paramList is ε and we can return.
  Token next_token = PeekNextToken();
  if (s_cmp(next_token.lx, ")")) {
    p_info.tk = next_token;
    return p_info;
  }

  // If the next token is "{", we have missed a close bracket.
  if (s_cmp(next_token.lx, "{")) {
    p_info.tk = next_token;
    p_info.er = closeParenExpected;
    return p_info;
  }

  // Start parsing the symbol.
  Symbol new_symbol = {.kind = ARG};

  // Type
  p_info = type();
  p_err_check(p_info);
  strcpy(new_symbol.type, p_info.tk.lx);  // Store the symbol type.

  // Identifier
  p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_NEW);
  p_err_check(p_info);
  strcpy(new_symbol.identifier, p_info.tk.lx);  // Store the symbol identifier.

  if (GetPassMode() == SECOND) AppendSymbol(new_symbol);

  while (1) {
    // Comma.
    // Peek so that we don't consume the token, just in case there are no more
    // parameters.
    next_token = PeekNextToken();
    if (!s_cmp(next_token.lx, ",")) {
      // No more parameters.
      break;
    }
    GetNextToken();

    // Type
    p_info = type();
    p_err_check(p_info);
    strcpy(new_symbol.type, p_info.tk.lx);  // Store the symbol type.

    // Identifier
    p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_NEW);
    p_err_check(p_info);
    strcpy(new_symbol.identifier,
           p_info.tk.lx);  // Store the symbol identifier.

    if (GetPassMode() == SECOND) AppendSymbol(new_symbol);
  }
  return p_info;
}

// subroutineDeclar -> (constructor | function | method) (type|void) identifier
// (paramList) subroutineBody
ParserInfo subroutineDeclar() {
  ParserInfo p_info;
  // Must start with either "constructor", "function" or "method".
  Token next_token = GetNextToken();
  Symbol new_subroutine;
  if (s_cmp(next_token.lx, "constructor") || s_cmp(next_token.lx, "function")) {
    new_subroutine.kind = FUNCTION;
  } else if (s_cmp(next_token.lx, "method")) {
    new_subroutine.kind = METHOD;
  } else {
    p_info.er = subroutineDeclarErr;
    p_info.tk = next_token;
    return p_info;
  }

  // Must be either a type or "void".
  next_token = PeekNextToken();
  if (!s_cmp(next_token.lx, "void")) {
    // Not "void" so needs to be a type.
    p_info = type();
    p_err_check(p_info);
  } else {
    GetNextToken();
  }
  strcpy(new_subroutine.type, next_token.lx);

  // Now we need an identifier.
  p_info = identifier(GetPassMode() == FIRST ? ID_NEW : ID_EXISTS);
  p_err_check(p_info);
  strcpy(new_subroutine.identifier, p_info.tk.lx);

  Symbol *curr_sub;
  if (GetPassMode() == FIRST) {
    curr_sub = AppendSymbol(new_subroutine);
  } else {
    curr_sub = LookupSubroutine(new_subroutine.identifier, GetCurrClass());
  }

  // Reset the symbol table for the new subroutine scope.
  StartNewSubroutine(curr_sub->identifier);
  curr_context = CTX_SUBROUTINE;

  // Generate the VM code.
  VM_start_function(curr_sub);

  // Open bracket.
  p_info = lex_check("(", SYMBOL, openParenExpected);
  p_err_check(p_info);

  // List of parameters.
  p_info = paramList();
  p_err_check(p_info);

  // Closing bracket.
  p_info = lex_check(")", SYMBOL, closeParenExpected);
  p_err_check(p_info);

  // Subroutine body.
  p_info = subroutineBody();
  p_err_check(p_info);

  curr_context = CTX_CLASS;

  return p_info;
}

// classVarDeclar -> (static | field) type identifier {, identifier} ;
ParserInfo classVarDeclar() {
  Token next_token = GetNextToken();
  ParserInfo p_info;
  p_info.er = none;
  p_info.tk = next_token;

  Symbol new_symbol;
  // Read either "static" or "field".
  if (s_cmp(next_token.lx, "static")) {
    new_symbol.kind = STATIC;
  } else if s_cmp (next_token.lx, "field") {
    new_symbol.kind = FIELD;
  } else {
    p_info.er = classVarErr;
    return p_info;
  }

  // Now we need a type.
  p_info = type();
  p_err_check(p_info);
  strcpy(new_symbol.type, p_info.tk.lx);

  // Identifier.
  p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_NEW);
  p_err_check(p_info);
  strcpy(new_symbol.identifier, p_info.tk.lx);

  if (GetPassMode() == SECOND) AppendSymbol(new_symbol);

  // Now check for the ", identifier"s that may remain.
  while (1) {
    // First we need a comma.
    // Peek so we don't consume the token (just in case it isn't a comma).
    next_token = PeekNextToken();
    if (!s_cmp(next_token.lx, ",")) {
      // Not a comma, so no more identifiers are coming.
      break;
    }
    GetNextToken();  // Consume the comma.

    // Identifier.
    p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_NEW);
    p_err_check(p_info);
    strcpy(new_symbol.identifier, p_info.tk.lx);

    if (GetPassMode() == SECOND) AppendSymbol(new_symbol);
  }

  // Now we need to check for the final ";".
  p_info = lex_check(";", SYMBOL, semicolonExpected);
  p_err_check(p_info);

  // Class variable declaration is valid.
  return p_info;
}

// type -> int | char | boolean | identifier
ParserInfo type() {
  Token next_token = PeekNextToken();
  ParserInfo p_info;
  p_info.tk = next_token;
  p_info.er = none;

  // Allow token if it's a reserved word that represents a type.
  char valid_type = s_cmp("int", next_token.lx) ||
                    s_cmp("char", next_token.lx) ||
                    s_cmp("boolean", next_token.lx);
  if (next_token.tp == RESWORD && valid_type) {
    // Make sure we consume the token.
    GetNextToken();
    return p_info;
  }

  // Allow token if it's an ID.
  p_info = identifier(GetPassMode() == FIRST ? ID_NO_CHECK : ID_EXISTS);
  p_err_check(p_info);

  // Make sure the symbol is a class.
  if (GetPassMode() == SECOND) {
    // Make sure ID is for a class.
    Symbol *sym = LookupSymbol(p_info.tk.lx);
    if (sym->kind != CLASS) {
      p_info.er = illegalType;
      return p_info;
    }
  }

  return p_info;
}