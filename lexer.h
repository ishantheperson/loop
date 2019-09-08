#pragma once 

#include <stdbool.h>

// static char buffer[256];

typedef int_t long;

typedef enum {
  // come with additional data
  CONSTANT,
  IDENTIFIER,

  EQUALS,
  INCREMENT,
  SEMICOLON,
  DO,
  COLON,
  OD,
} token_t;

//const char* token_to_string(token_t token);

const char* get_string();
int_t get_int();

const char* read_token(token_t token, const char* source);
const char* read_tokens(token_t* tokens, int len, const char* source);