/************************************************************************
University of Leeds
School of Computing
COMP2932- Compiler Design and Construction
Lexer Module

I confirm that the following code has been developed and written by me and it is
entirely the result of my own work. I also confirm that I have not copied any
parts of this program from another person or any other source or facilitated
someone to copy this program from me. I confirm that I will not publish the
program online or share it with anyone without permission of the module leader.

Student Name: Thomas Milner
Student ID: 201937578
Email: el20tm@leeds.ac.uk
Date Work Commenced: 31/01/2022
*************************************************************************/

#include "lexer.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// YOU CAN ADD YOUR OWN FUNCTIONS, DECLARATIONS AND VARIABLES HERE

#define LEXER_SUCCESS 0
#define LEXER_FAILURE 1

static FILE *source_file;
static char source_file_name[32];
static int line_num = 1;

#define NEW_LINE '\n'

static const char WHITESPACE[] = {'\t',  // tab
                                  NEW_LINE,
                                  ' ',  // space,
                                  '\r'};
static const int N_WHITESPACE = 4;

static const char SYMBOLS[] = {'(', ')', '[', ']', '{', '}', ',', ';', '=', '.',
                               '+', '-', '*', '/', '&', '|', '~', '<', '>'};
static const int N_SYMBOLS = 19;

static const char *KEYWORDS[] = {
    "class", "constructor", "method", "function", "int",   "boolean", "char",
    "void",  "var",         "static", "field",    "let",   "do",      "if",
    "else",  "while",       "return", "true",     "false", "null",    "this"};
static const int N_KEYWORDS = 21;

static bool char_arr_contains(const char *char_set, const int n, const char c) {
  for (int i = 0; i < n; i++) {
    if (char_set[i] == c) return true;
  }
  return false;
}

static bool string_arr_contains(const char **string_set, const int n, const char *str) {
  for (int i = 0; i < n; i++) {
    if (strcmp(string_set[i], str) == 0) return true;
  }
  return false;
}

static void print_token(Token *token, FILE *file) {
  char *tp_string[] = {"RESWORD", "ID", "INT", "SYMBOL", "STRING", "EOFile", "ERR"};
  // < Main.jack, 7, class, RESWORD >
  fprintf(file, "< %s, %d, %s, %s >\n", token->fl, token->ln, token->lx,
          tp_string[token->tp]);
}

// IMPLEMENT THE FOLLOWING functions
//***********************************

// Initialise the lexer to read from source file
// file_name is the name of the source file
// This requires opening the file and making any necessary initialisations
// of the lexer If an error occurs, the function should return 0 if
// everything goes well the function should return 1
int InitLexer(char *file_name) {
  source_file = fopen(file_name, "r");
  if (source_file == NULL) return LEXER_FAILURE;
  strcpy(source_file_name, file_name);
  line_num = 1;
  //  printf("Lexer initialised.\n");
  return LEXER_SUCCESS;
}

// Get the next token from the source file
Token GetNextToken() {
  // Set up the token.
  Token token;
  token.ec = NoLexErr;
  strcpy(token.fl, source_file_name);

  // Main Lexical analysis.

  // First we need to skip through any whitespace and comments.
  // NOTE: These can come in any order (e.g. comments, whitespace, comments).

  int curr_char;
  while (true) {
    // Get the next character.
    curr_char = fgetc(source_file);

    // Skip through any whitespace.
    if (char_arr_contains(WHITESPACE, N_WHITESPACE, curr_char)) {
      if (curr_char == NEW_LINE) {
        line_num++;
      }
      continue;
    }

    // If this definitely isn't a comment, we can move to the tokenizing phase.
    if (curr_char != '/') break;

    int next_char = fgetc(source_file);
    // Line Comment.
    if (next_char == '/') {
      // Continue to the end of the line.
      line_num++;
      while ((curr_char = fgetc(source_file)) != NEW_LINE) {
        // TODO: This code is duplicated - there must be a way to make it
        // better.
        if (curr_char == EOF) {
          token.tp = ERR;
          token.ec = EofInCom;
          strcpy(token.lx, "Error: unexpected eof in comment");
          token.ln = line_num;
          return token;
        }
      }

      // Inline Comment.
    } else if (next_char == '*') {
      while ((curr_char = fgetc(source_file))) {
        if (curr_char == EOF) {
          token.tp = ERR;
          token.ec = EofInCom;
          strcpy(token.lx, "Error: unexpected eof in comment");
          token.ln = line_num;
          return token;
        }

        // TODO: These new line increments are in seemingly random places. There
        // must be a way to make them cleaner/simpler; maybe by implementing
        //  a `get_next_char` function that will automatically increment the
        //  line count if it encounters a new line?

        if (curr_char == NEW_LINE) line_num++;

        // Continue until "*/" reached.
        if (curr_char != '*') continue;

        // Once we've found the "*", there needs to be a '/' after it.
        next_char = fgetc(source_file);
        if (next_char == '/') break;

        // The character wasn't what we wanted, but might be needed by the next
        // iteration, so we put it back.
        ungetc(next_char, source_file);  // TODO: I may not be right here!
      }
    } else {
      // This isn't a comment!
      ungetc(next_char, source_file);  // TODO: Check this line of logic.
      break;
    }
  }
  token.ln = line_num;

  // ==== EOF ====
  if (curr_char == EOF) {
    token.tp = EOFile;
    strcpy(token.lx, "End of File");
    return token;
  }

  // ==== STRING LITERAL ====
  if (curr_char == '"') {
    int literal_size = 0;
    token.tp = STRING;

    while ((curr_char = fgetc(source_file))) {
      if (curr_char == EOF) {
        // TODO: Abstract away creating new tokens into factory function.
        token.tp = ERR;
        token.ec = EofInStr;
        strcpy(token.lx, "Error: unexpected eof in string constant");
        return token;
      }

      if (curr_char == NEW_LINE) {
        token.tp = ERR;
        token.ec = NewLnInStr;
        strcpy(token.lx, "Error: new line in string constant");
        return token;
      }

      if (curr_char == '"') {
        // We've reached the end of the string literal!
        token.lx[literal_size] = 0;
        return token;
      }

      token.lx[literal_size++] = curr_char;
    }
  }

  // ==== KEYWORD/IDENTIFIER ====
  if (isalpha(curr_char) || curr_char == '_') {
    int length = 0;
    token.lx[length++] = curr_char;
    while ((curr_char = fgetc(source_file))) {
      if (!isalnum(curr_char) && curr_char != '_') {
        // We've reached the end of the string.
        token.lx[length] = 0;
        // Make sure we don't end up skipping the token we've just read.
        ungetc(curr_char, source_file);
        break;
      }
      token.lx[length++] = curr_char;
    }

    // Check if the string is a keyword or identifier.
    bool is_resword = string_arr_contains(KEYWORDS, N_KEYWORDS, token.lx);
    token.tp = is_resword ? RESWORD : ID;

    return token;
  }

  // ==== NUMBER ====
  if (isdigit(curr_char)) {
    int length = 0;
    token.lx[length++] = curr_char;
    while ((curr_char = fgetc(source_file))) {
      if (!isdigit(curr_char)) {
        // We've reached the end of the number.
        token.lx[length] = 0;
        token.tp = INT;
        // Make sure we don't end up skipping the token we've just read.
        ungetc(curr_char, source_file);
        return token;
      }
      token.lx[length++] = curr_char;
    }
  }

  // ==== SYMBOL ====

  // Make sure it's an allowed symbol.
  if (!char_arr_contains(SYMBOLS, N_SYMBOLS, curr_char)) {
    token.tp = ERR;
    token.ec = IllSym;
    strcpy(token.lx, "Error: illegal symbol in source file");
  } else {
    token.tp = SYMBOL;
    token.lx[0] = curr_char;
    token.lx[1] = 0;
  }
  return token;
}

// peek (look) at the next token in the source file without removing it from the
// stream
Token PeekNextToken() {
  int curr_pos = ftell(source_file);
  int curr_line_num = line_num;
  Token t = GetNextToken();
  fseek(source_file, curr_pos, SEEK_SET);
  line_num = curr_line_num;

  return t;
}

// clean out at end, e.g. close files, free memory, ... etc
int StopLexer() {
  fclose(source_file);
  return LEXER_SUCCESS;
}

// do not remove the next line
#ifndef TEST
// int main() {
//   // implement your main function here
//   // NOTE: the autograder will not use your main function
//
//   int status = InitLexer("OnlyComments.jack");
//   if (status != LEXER_SUCCESS) {
//     printf("Lexer init error.\n");
//     return 1;
//   }
//
//   FILE *out = fopen("out_tokens.txt", "w");
//
//   Token token;
//   while (true) {
//     token = GetNextToken();
//     print_token(&token, out);
//     if (token.tp == EOFile) break;
//   }
//
//   fclose(out);
//
//   StopLexer();
//
//   return 0;
// }
//// do not remove the next line
#endif