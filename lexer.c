#include "lexer.h"

#include <stdlib.h>
#include <ctype.h>

#include "util.h"

static char string_buffer[256] = { 0 };
static long int_buffer = 0;

static 
const char* skip_whitespace(const char* source) {
  REQUIRES(source != NULL, "empty input string");

  while (isspace(*source)) source++;
  return source;
}

static 
const char* match_string(const char* token, const char* source) {
  REQUIRES(token != NULL, "null string to match")
  REQUIRES(source != NULL, "null input string");

  while (*token) {
    if (*source == '\0' || *source != *token) return NULL;

    token++; source++;
  }

  return skip_whitespace(source);
}

static 
const char* read_string(const char* source) {
  REQUIRES(source != NULL, "empty input string");

  int num_chars = 0;

  while (isalpha(*source)) { 
    ASSERT(num_chars < sizeof(string_buffer), "string too long");
    string_buffer[num_chars++] = *source++;
  }

  string_buffer[num_chars] = '\0';

  return num_chars > 0 ? skip_whitespace(source) : NULL;
}

static 
const char* read_int(const char* source) {
  REQUIRES(source != NULL, "empty input string");

  const char* end; 
  int_buffer = strtol(source, &end, 10);

  return source != end ? skip_whitespace(end) : NULL;
}

static const char* token_map[] = {
  [EQUALS] = "=",
  [INCREMENT] = "++",
  [SEMICOLON] = ";",
  [DO] = "do",
  [COLON] = ":",
  [OD] = "od",
};

const char* read_token(token_t token, const char* source) {
  REQUIRES(source != NULL, "empty input string");

  switch (token) {
    case DO: 
    case OD: 
    case EQUALS: 
    case INCREMENT:
    case COLON:
    case SEMICOLON:
      return match_string(token_map[token], source);

    case IDENTIFIER:
      return read_string(source);

    case CONSTANT:
      return read_int(source);

    default:
      ASSERT(false, "Unknown token type received");
  }
}
