#include <wctype.h>
#include <tree_sitter/parser.h>

#include "array.h"

typedef enum {
  LISTSTART,
  LISTEND,
  LISTITEMEND,
  BULLET,
  HLSTARS,
  SECTIONEND,
  ENDOFFILE,
} TokenType;

typedef enum {
  NOTABULLET,
  DASH,
  PLUS,
  STAR,
  LOWERDOT,
  UPPERDOT,
  LOWERPAREN,
  UPPERPAREN,
  NUMDOT,
  NUMPAREN,
} Bullet;

typedef struct {
  Array indent_length_stack;
  Array bullet_stack;
  Array section_stack;
} Scanner;

static void scanner_stack_init(Array *array)
{
    array_init_sized(array, sizeof(int16_t));
}

static int16_t scanner_stack_get(Array *array, size_t idx)
{
    int16_t *val = array_get(array, idx);
    return val ? *val : -1;
}

static void scanner_stack_push(Array *array, int16_t item)
{
    array_push(array, &item);
}

static int16_t scanner_stack_pop(Array *array)
{
    int16_t *val = array_pop(array);
    return val ? *val : -1;
}

static int16_t scanner_stack_peek(Array *array)
{
    int16_t *val = array_peek(array);
    return val ? *val : -1;
}

static size_t scanner_stack_size(Array *array)
{
    return array_length(array);
}

static size_t scanner_stack_clear(Array *array)
{
    array_clear(array);
}

static void scanner_deserialize(Scanner *scanner, const char *buffer, unsigned length);

static void scanner_ctor(Scanner *scanner)
{
  scanner_stack_init(&scanner->indent_length_stack);
  scanner_stack_init(&scanner->bullet_stack);
  scanner_stack_init(&scanner->section_stack);

  scanner_deserialize(scanner, NULL, 0);
}

static void scanner_dtor(Scanner *scanner)
{
  array_release(&scanner->indent_length_stack);
  array_release(&scanner->bullet_stack);
  array_release(&scanner->section_stack);
}

static unsigned scanner_serialize(Scanner *scanner, char *buffer)
{
    size_t iter_idx, end_idx;
    size_t i = 0;

    size_t indent_count = scanner_stack_size(&scanner->indent_length_stack) - 1;
    if (indent_count > UINT8_MAX) indent_count = UINT8_MAX;
    buffer[i++] = indent_count;

    iter_idx = 1;
    end_idx = scanner_stack_size(&scanner->indent_length_stack);

    for (; iter_idx <= end_idx && i < TREE_SITTER_SERIALIZATION_BUFFER_SIZE; iter_idx++) {
      buffer[i++] = scanner_stack_get(&scanner->indent_length_stack, iter_idx);
    }

    iter_idx = 1;
    end_idx = scanner_stack_size(&scanner->bullet_stack);

    for (; iter_idx <= end_idx && i < TREE_SITTER_SERIALIZATION_BUFFER_SIZE; iter_idx++) {
      buffer[i++] = scanner_stack_get(&scanner->bullet_stack, iter_idx);
    }

    iter_idx = 1;
    end_idx = scanner_stack_size(&scanner->section_stack);

    for (; iter_idx <= end_idx && i < TREE_SITTER_SERIALIZATION_BUFFER_SIZE; iter_idx++) {
      buffer[i++] = scanner_stack_get(&scanner->section_stack, iter_idx);
    }

    return i;
}

static void scanner_deserialize(Scanner *scanner, const char *buffer, unsigned length) {
    scanner_stack_clear(&scanner->section_stack);
    scanner_stack_push(&scanner->section_stack, 0);

    scanner_stack_clear(&scanner->indent_length_stack);
    scanner_stack_push(&scanner->indent_length_stack, -1);

    scanner_stack_clear(&scanner->bullet_stack);
    scanner_stack_push(&scanner->bullet_stack, NOTABULLET);

    if (length == 0) return;

    size_t i = 0;

    size_t indent_count = (uint8_t)buffer[i++];

    for (; i <= indent_count    ; i++) scanner_stack_push(&scanner->indent_length_stack, buffer[i]);
    for (; i <= 2 * indent_count; i++) scanner_stack_push(&scanner->bullet_stack, buffer[i]);
    for (; i < length           ; i++) scanner_stack_push(&scanner->section_stack, buffer[i]);
}

static void scanner_advance(Scanner *scanner, TSLexer *lexer) {
    lexer->advance(lexer, false);
}

static void scanner_skip(Scanner *scanner, TSLexer *lexer) {
    lexer->advance(lexer, true);
}

static bool scanner_dedent(Scanner *scanner, TSLexer *lexer) {
    scanner_stack_pop(&scanner->indent_length_stack);
    scanner_stack_pop(&scanner->bullet_stack);
    lexer->result_symbol = LISTEND;
    return true;
}

static Bullet getbullet(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead == '-') {
      scanner_advance(scanner, lexer);
      if (iswspace(lexer->lookahead)) return DASH;
    } else if (lexer->lookahead == '+') {
      scanner_advance(scanner, lexer);
      if (iswspace(lexer->lookahead)) return PLUS;
    } else if (lexer->lookahead == '*') {
      scanner_advance(scanner, lexer);
      if (iswspace(lexer->lookahead)) return STAR;
    } else if ('a' <= lexer->lookahead && lexer->lookahead <= 'z') {
      scanner_advance(scanner, lexer);
      if (lexer->lookahead == '.') {
        scanner_advance(scanner, lexer);
        if (iswspace(lexer->lookahead)) return LOWERDOT;
      } else if (lexer->lookahead == ')') {
        scanner_advance(scanner, lexer);
        if (iswspace(lexer->lookahead)) return LOWERPAREN;
      }
    } else if ('A' <= lexer->lookahead && lexer->lookahead <= 'Z') {
      scanner_advance(scanner, lexer);
      if (lexer->lookahead == '.') {
        scanner_advance(scanner, lexer);
        if (iswspace(lexer->lookahead)) return UPPERDOT;
      } else if (lexer->lookahead == ')') {
        scanner_advance(scanner, lexer);
        if (iswspace(lexer->lookahead)) return UPPERPAREN;
      }
    } else if ('0' <= lexer->lookahead && lexer->lookahead <= '9') {
      do {
        scanner_advance(scanner, lexer);
      } while ('0' <= lexer->lookahead && lexer->lookahead <= '9');
      if (lexer->lookahead == '.') {
        scanner_advance(scanner, lexer);
        if (iswspace(lexer->lookahead)) return NUMDOT;
      } else if (lexer->lookahead == ')') {
        scanner_advance(scanner, lexer);
        if (iswspace(lexer->lookahead)) return NUMPAREN;
      }
    }
    return NOTABULLET;
}

static bool scanner_scan(Scanner *scanner, TSLexer *lexer, const bool *valid_symbols) {

  // - Section ends
  int16_t indent_length = 0;
  lexer->mark_end(lexer);
  for (;;) {
    if (lexer->lookahead == ' ') {
      indent_length++;
    } else if (lexer->lookahead == '\t') {
      indent_length += 8;
    } else if (lexer->lookahead == '\0') {

      if (valid_symbols[LISTEND])    { lexer->result_symbol = LISTEND; }
      else if (valid_symbols[SECTIONEND]) { lexer->result_symbol = SECTIONEND; }
      else if (valid_symbols[ENDOFFILE])  { lexer->result_symbol = ENDOFFILE; }
      else return false;

      return true;
    } else {
      break;
    }
    scanner_skip(scanner, lexer);
  }

  // - Listiem ends
  // Listend -> end of a line, looking for:
  // 1. dedent
  // 2. same indent, not a bullet
  // 3. two eols
  int16_t newlines = 0;
  if (valid_symbols[LISTEND] || valid_symbols[LISTITEMEND]) {
    for (;;) {
      if (lexer->lookahead == ' ') {
        indent_length++;
      } else if (lexer->lookahead == '\t') {
        indent_length += 8;
      } else if (lexer->lookahead == '\0') {
        return scanner_dedent(scanner, lexer);
      } else if (lexer->lookahead == '\n') {
        if (++newlines > 1) return scanner_dedent(scanner, lexer);
        indent_length = 0;
      } else {
        break;
      }
      scanner_skip(scanner, lexer);
    }

    if (indent_length < scanner_stack_peek(&scanner->indent_length_stack)) {
      return scanner_dedent(scanner, lexer);
    } else if (indent_length == scanner_stack_peek(&scanner->indent_length_stack)) {
      if (getbullet(scanner, lexer) == scanner_stack_peek(&scanner->bullet_stack)) {
        lexer->result_symbol = LISTITEMEND;
        return true;
      }
      return scanner_dedent(scanner, lexer);
    }
  }

  // - Col=0 star
  if (indent_length == 0 && lexer->lookahead == '*') {
    lexer->mark_end(lexer);
    int16_t stars = 1;
    scanner_skip(scanner, lexer);
    while (lexer->lookahead == '*') {
      stars++;
      scanner_skip(scanner, lexer);
    }

    if (valid_symbols[SECTIONEND] && iswspace(lexer->lookahead) && stars > 0 && stars <= scanner_stack_peek(&scanner->section_stack)) {
      scanner_stack_pop(&scanner->section_stack);
      lexer->result_symbol = SECTIONEND;
      return true;
    } else if (valid_symbols[HLSTARS] && iswspace(lexer->lookahead)) {
      scanner_stack_push(&scanner->section_stack, stars);
      lexer->result_symbol = HLSTARS;
      return true;
    }
    return false;
  }

  // - Liststart and bullets
  if ((valid_symbols[LISTSTART] || valid_symbols[BULLET]) && newlines == 0) {
    Bullet bullet = getbullet(scanner, lexer);

    if (valid_symbols[BULLET] &&
           bullet == scanner_stack_peek(&scanner->bullet_stack) &&
           indent_length == scanner_stack_peek(&scanner->indent_length_stack)) {
      lexer->mark_end(lexer);
      lexer->result_symbol = BULLET;
      return true;
    } else if (valid_symbols[LISTSTART] && bullet != NOTABULLET && indent_length > scanner_stack_peek(&scanner->indent_length_stack)) {
      scanner_stack_push(&scanner->indent_length_stack, indent_length);
      scanner_stack_push(&scanner->bullet_stack, bullet);
      lexer->result_symbol = LISTSTART;
      return true;
    }
  }

  return false; // default
}

void *tree_sitter_org_external_scanner_create() {
    Scanner *scanner = calloc(1, sizeof(*scanner));
    if (scanner)
      scanner_ctor(scanner);
    return scanner;
}

bool tree_sitter_org_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = payload;
    return scanner_scan(scanner, lexer, valid_symbols);
}

unsigned tree_sitter_org_external_scanner_serialize(void *payload, char *buffer) {
    Scanner *scanner = payload;
    return scanner_serialize(scanner, buffer);
}

void tree_sitter_org_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    Scanner *scanner = payload;
    scanner_deserialize(scanner, buffer, length);
}

void tree_sitter_org_external_scanner_destroy(void *payload) {
    Scanner *scanner = payload;
    scanner_dtor(scanner);
    free(scanner);
}
