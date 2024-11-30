#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 32
#define LARGE_STATE_COUNT 4
#define SYMBOL_COUNT 40
#define ALIAS_COUNT 0
#define TOKEN_COUNT 25
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 1

enum ts_symbol_identifiers {
  sym_identifier = 1,
  anon_sym_SEMI = 2,
  anon_sym_print = 3,
  anon_sym_DASH = 4,
  anon_sym_BANG = 5,
  anon_sym_SLASH = 6,
  anon_sym_STAR = 7,
  anon_sym_PLUS = 8,
  anon_sym_DOT = 9,
  anon_sym_LT = 10,
  anon_sym_LT_EQ = 11,
  anon_sym_GT_EQ = 12,
  anon_sym_GT = 13,
  anon_sym_BANG_EQ = 14,
  anon_sym_EQ_EQ = 15,
  anon_sym_and = 16,
  anon_sym_or = 17,
  sym_number = 18,
  sym_string = 19,
  anon_sym_true = 20,
  anon_sym_false = 21,
  sym_null = 22,
  anon_sym_SLASH_SLASH = 23,
  aux_sym_comment_token1 = 24,
  sym_source_file = 25,
  sym__declaration = 26,
  sym__statement = 27,
  sym_expressionStatement = 28,
  sym_printStatement = 29,
  sym__expression = 30,
  sym_unary = 31,
  sym_binary = 32,
  sym_comparison = 33,
  sym_equality = 34,
  sym_logical = 35,
  sym__primary = 36,
  sym_bool = 37,
  sym_comment = 38,
  aux_sym_source_file_repeat1 = 39,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_identifier] = "identifier",
  [anon_sym_SEMI] = ";",
  [anon_sym_print] = "print",
  [anon_sym_DASH] = "-",
  [anon_sym_BANG] = "!",
  [anon_sym_SLASH] = "/",
  [anon_sym_STAR] = "*",
  [anon_sym_PLUS] = "+",
  [anon_sym_DOT] = ".",
  [anon_sym_LT] = "<",
  [anon_sym_LT_EQ] = "<=",
  [anon_sym_GT_EQ] = ">=",
  [anon_sym_GT] = ">",
  [anon_sym_BANG_EQ] = "!=",
  [anon_sym_EQ_EQ] = "==",
  [anon_sym_and] = "and",
  [anon_sym_or] = "or",
  [sym_number] = "number",
  [sym_string] = "string",
  [anon_sym_true] = "true",
  [anon_sym_false] = "false",
  [sym_null] = "null",
  [anon_sym_SLASH_SLASH] = "//",
  [aux_sym_comment_token1] = "comment_token1",
  [sym_source_file] = "source_file",
  [sym__declaration] = "_declaration",
  [sym__statement] = "_statement",
  [sym_expressionStatement] = "expressionStatement",
  [sym_printStatement] = "printStatement",
  [sym__expression] = "_expression",
  [sym_unary] = "unary",
  [sym_binary] = "binary",
  [sym_comparison] = "comparison",
  [sym_equality] = "equality",
  [sym_logical] = "logical",
  [sym__primary] = "_primary",
  [sym_bool] = "bool",
  [sym_comment] = "comment",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_identifier] = sym_identifier,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_print] = anon_sym_print,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_LT] = anon_sym_LT,
  [anon_sym_LT_EQ] = anon_sym_LT_EQ,
  [anon_sym_GT_EQ] = anon_sym_GT_EQ,
  [anon_sym_GT] = anon_sym_GT,
  [anon_sym_BANG_EQ] = anon_sym_BANG_EQ,
  [anon_sym_EQ_EQ] = anon_sym_EQ_EQ,
  [anon_sym_and] = anon_sym_and,
  [anon_sym_or] = anon_sym_or,
  [sym_number] = sym_number,
  [sym_string] = sym_string,
  [anon_sym_true] = anon_sym_true,
  [anon_sym_false] = anon_sym_false,
  [sym_null] = sym_null,
  [anon_sym_SLASH_SLASH] = anon_sym_SLASH_SLASH,
  [aux_sym_comment_token1] = aux_sym_comment_token1,
  [sym_source_file] = sym_source_file,
  [sym__declaration] = sym__declaration,
  [sym__statement] = sym__statement,
  [sym_expressionStatement] = sym_expressionStatement,
  [sym_printStatement] = sym_printStatement,
  [sym__expression] = sym__expression,
  [sym_unary] = sym_unary,
  [sym_binary] = sym_binary,
  [sym_comparison] = sym_comparison,
  [sym_equality] = sym_equality,
  [sym_logical] = sym_logical,
  [sym__primary] = sym__primary,
  [sym_bool] = sym_bool,
  [sym_comment] = sym_comment,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_print] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_and] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_or] = {
    .visible = true,
    .named = false,
  },
  [sym_number] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_true] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_false] = {
    .visible = true,
    .named = false,
  },
  [sym_null] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SLASH_SLASH] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_comment_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__declaration] = {
    .visible = false,
    .named = true,
  },
  [sym__statement] = {
    .visible = false,
    .named = true,
  },
  [sym_expressionStatement] = {
    .visible = true,
    .named = true,
  },
  [sym_printStatement] = {
    .visible = true,
    .named = true,
  },
  [sym__expression] = {
    .visible = false,
    .named = true,
  },
  [sym_unary] = {
    .visible = true,
    .named = true,
  },
  [sym_binary] = {
    .visible = true,
    .named = true,
  },
  [sym_comparison] = {
    .visible = true,
    .named = true,
  },
  [sym_equality] = {
    .visible = true,
    .named = true,
  },
  [sym_logical] = {
    .visible = true,
    .named = true,
  },
  [sym__primary] = {
    .visible = false,
    .named = true,
  },
  [sym_bool] = {
    .visible = true,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(8);
      ADVANCE_MAP(
        '!', 12,
        '"', 2,
        '*', 14,
        '+', 15,
        '-', 10,
        '.', 16,
        '/', 13,
        ';', 9,
        '<', 17,
        '=', 5,
        '>', 20,
      );
      if (lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(23);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(26);
      END_STATE();
    case 1:
      ADVANCE_MAP(
        '!', 4,
        '*', 14,
        '+', 15,
        '-', 10,
        '.', 16,
        '/', 13,
        ';', 9,
        '<', 17,
        '=', 5,
        '>', 20,
      );
      if (lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(26);
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(25);
      if (lookahead != 0) ADVANCE(2);
      END_STATE();
    case 3:
      if (lookahead == '/') ADVANCE(27);
      END_STATE();
    case 4:
      if (lookahead == '=') ADVANCE(21);
      END_STATE();
    case 5:
      if (lookahead == '=') ADVANCE(22);
      END_STATE();
    case 6:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(24);
      END_STATE();
    case 7:
      if (eof) ADVANCE(8);
      if (lookahead == '!') ADVANCE(11);
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '-') ADVANCE(10);
      if (lookahead == '/') ADVANCE(3);
      if (lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(7);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(23);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(26);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(21);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_SLASH);
      if (lookahead == '/') ADVANCE(27);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '=') ADVANCE(18);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '=') ADVANCE(19);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_BANG_EQ);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(6);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(23);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(24);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_string);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(26);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_SLASH_SLASH);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_SLASH_SLASH);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(31);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(aux_sym_comment_token1);
      if (lookahead == '/') ADVANCE(30);
      if (lookahead == '\r' ||
          lookahead == ' ') ADVANCE(29);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(31);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym_comment_token1);
      if (lookahead == '/') ADVANCE(28);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(31);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(aux_sym_comment_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(31);
      END_STATE();
    default:
      return false;
  }
}

static bool ts_lex_keywords(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (lookahead == 'a') ADVANCE(1);
      if (lookahead == 'f') ADVANCE(2);
      if (lookahead == 'n') ADVANCE(3);
      if (lookahead == 'o') ADVANCE(4);
      if (lookahead == 'p') ADVANCE(5);
      if (lookahead == 't') ADVANCE(6);
      if (lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0);
      END_STATE();
    case 1:
      if (lookahead == 'n') ADVANCE(7);
      END_STATE();
    case 2:
      if (lookahead == 'a') ADVANCE(8);
      END_STATE();
    case 3:
      if (lookahead == 'u') ADVANCE(9);
      END_STATE();
    case 4:
      if (lookahead == 'r') ADVANCE(10);
      END_STATE();
    case 5:
      if (lookahead == 'r') ADVANCE(11);
      END_STATE();
    case 6:
      if (lookahead == 'r') ADVANCE(12);
      END_STATE();
    case 7:
      if (lookahead == 'd') ADVANCE(13);
      END_STATE();
    case 8:
      if (lookahead == 'l') ADVANCE(14);
      END_STATE();
    case 9:
      if (lookahead == 'l') ADVANCE(15);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_or);
      END_STATE();
    case 11:
      if (lookahead == 'i') ADVANCE(16);
      END_STATE();
    case 12:
      if (lookahead == 'u') ADVANCE(17);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_and);
      END_STATE();
    case 14:
      if (lookahead == 's') ADVANCE(18);
      END_STATE();
    case 15:
      if (lookahead == 'l') ADVANCE(19);
      END_STATE();
    case 16:
      if (lookahead == 'n') ADVANCE(20);
      END_STATE();
    case 17:
      if (lookahead == 'e') ADVANCE(21);
      END_STATE();
    case 18:
      if (lookahead == 'e') ADVANCE(22);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(sym_null);
      END_STATE();
    case 20:
      if (lookahead == 't') ADVANCE(23);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_true);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_false);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_print);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 7},
  [2] = {.lex_state = 7},
  [3] = {.lex_state = 7},
  [4] = {.lex_state = 7},
  [5] = {.lex_state = 7},
  [6] = {.lex_state = 7},
  [7] = {.lex_state = 7},
  [8] = {.lex_state = 7},
  [9] = {.lex_state = 7},
  [10] = {.lex_state = 7},
  [11] = {.lex_state = 7},
  [12] = {.lex_state = 1},
  [13] = {.lex_state = 1},
  [14] = {.lex_state = 1},
  [15] = {.lex_state = 1},
  [16] = {.lex_state = 1},
  [17] = {.lex_state = 1},
  [18] = {.lex_state = 1},
  [19] = {.lex_state = 1},
  [20] = {.lex_state = 1},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 1},
  [23] = {.lex_state = 1},
  [24] = {.lex_state = 7},
  [25] = {.lex_state = 7},
  [26] = {.lex_state = 7},
  [27] = {.lex_state = 7},
  [28] = {.lex_state = 7},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 29},
  [31] = {(TSStateId)(-1)},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [sym_comment] = STATE(0),
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_identifier] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_print] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_LT] = ACTIONS(1),
    [anon_sym_LT_EQ] = ACTIONS(1),
    [anon_sym_GT_EQ] = ACTIONS(1),
    [anon_sym_GT] = ACTIONS(1),
    [anon_sym_BANG_EQ] = ACTIONS(1),
    [anon_sym_EQ_EQ] = ACTIONS(1),
    [anon_sym_and] = ACTIONS(1),
    [anon_sym_or] = ACTIONS(1),
    [sym_number] = ACTIONS(1),
    [sym_string] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [sym_null] = ACTIONS(1),
    [anon_sym_SLASH_SLASH] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(29),
    [sym__declaration] = STATE(24),
    [sym__statement] = STATE(25),
    [sym_expressionStatement] = STATE(26),
    [sym_printStatement] = STATE(26),
    [sym__expression] = STATE(15),
    [sym_unary] = STATE(14),
    [sym_binary] = STATE(14),
    [sym_comparison] = STATE(14),
    [sym_equality] = STATE(14),
    [sym_logical] = STATE(14),
    [sym__primary] = STATE(14),
    [sym_bool] = STATE(13),
    [sym_comment] = STATE(1),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(5),
    [sym_identifier] = ACTIONS(7),
    [anon_sym_print] = ACTIONS(9),
    [anon_sym_DASH] = ACTIONS(11),
    [anon_sym_BANG] = ACTIONS(11),
    [sym_number] = ACTIONS(13),
    [sym_string] = ACTIONS(13),
    [anon_sym_true] = ACTIONS(15),
    [anon_sym_false] = ACTIONS(15),
    [sym_null] = ACTIONS(7),
    [anon_sym_SLASH_SLASH] = ACTIONS(3),
  },
  [2] = {
    [sym__declaration] = STATE(24),
    [sym__statement] = STATE(25),
    [sym_expressionStatement] = STATE(26),
    [sym_printStatement] = STATE(26),
    [sym__expression] = STATE(15),
    [sym_unary] = STATE(14),
    [sym_binary] = STATE(14),
    [sym_comparison] = STATE(14),
    [sym_equality] = STATE(14),
    [sym_logical] = STATE(14),
    [sym__primary] = STATE(14),
    [sym_bool] = STATE(13),
    [sym_comment] = STATE(2),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(17),
    [sym_identifier] = ACTIONS(7),
    [anon_sym_print] = ACTIONS(9),
    [anon_sym_DASH] = ACTIONS(11),
    [anon_sym_BANG] = ACTIONS(11),
    [sym_number] = ACTIONS(13),
    [sym_string] = ACTIONS(13),
    [anon_sym_true] = ACTIONS(15),
    [anon_sym_false] = ACTIONS(15),
    [sym_null] = ACTIONS(7),
    [anon_sym_SLASH_SLASH] = ACTIONS(3),
  },
  [3] = {
    [sym__declaration] = STATE(24),
    [sym__statement] = STATE(25),
    [sym_expressionStatement] = STATE(26),
    [sym_printStatement] = STATE(26),
    [sym__expression] = STATE(15),
    [sym_unary] = STATE(14),
    [sym_binary] = STATE(14),
    [sym_comparison] = STATE(14),
    [sym_equality] = STATE(14),
    [sym_logical] = STATE(14),
    [sym__primary] = STATE(14),
    [sym_bool] = STATE(13),
    [sym_comment] = STATE(3),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(19),
    [sym_identifier] = ACTIONS(21),
    [anon_sym_print] = ACTIONS(24),
    [anon_sym_DASH] = ACTIONS(27),
    [anon_sym_BANG] = ACTIONS(27),
    [sym_number] = ACTIONS(30),
    [sym_string] = ACTIONS(30),
    [anon_sym_true] = ACTIONS(33),
    [anon_sym_false] = ACTIONS(33),
    [sym_null] = ACTIONS(21),
    [anon_sym_SLASH_SLASH] = ACTIONS(3),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(4), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(16), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [37] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(5), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(18), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [74] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(6), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(19), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [111] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(7), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(17), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [148] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(8), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(20), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [185] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(9), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(23), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [222] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(10), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(21), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [259] = 9,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(11), 1,
      sym_comment,
    STATE(13), 1,
      sym_bool,
    STATE(22), 1,
      sym__expression,
    ACTIONS(7), 2,
      sym_null,
      sym_identifier,
    ACTIONS(11), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      sym_number,
      sym_string,
    ACTIONS(15), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(14), 6,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
  [296] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(12), 1,
      sym_comment,
    ACTIONS(38), 3,
      anon_sym_SLASH,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(36), 11,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [321] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(13), 1,
      sym_comment,
    ACTIONS(42), 3,
      anon_sym_SLASH,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(40), 11,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [346] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(14), 1,
      sym_comment,
    ACTIONS(46), 3,
      anon_sym_SLASH,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(44), 11,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [371] = 11,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(48), 1,
      anon_sym_SEMI,
    ACTIONS(52), 1,
      anon_sym_SLASH,
    ACTIONS(54), 1,
      anon_sym_STAR,
    ACTIONS(62), 1,
      anon_sym_and,
    ACTIONS(64), 1,
      anon_sym_or,
    STATE(15), 1,
      sym_comment,
    ACTIONS(56), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(58), 2,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(60), 2,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
    ACTIONS(50), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
  [410] = 11,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(52), 1,
      anon_sym_SLASH,
    ACTIONS(54), 1,
      anon_sym_STAR,
    ACTIONS(62), 1,
      anon_sym_and,
    ACTIONS(64), 1,
      anon_sym_or,
    ACTIONS(66), 1,
      anon_sym_SEMI,
    STATE(16), 1,
      sym_comment,
    ACTIONS(56), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(58), 2,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(60), 2,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
    ACTIONS(50), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
  [449] = 7,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(52), 1,
      anon_sym_SLASH,
    ACTIONS(54), 1,
      anon_sym_STAR,
    ACTIONS(62), 1,
      anon_sym_and,
    STATE(17), 1,
      sym_comment,
    ACTIONS(70), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(68), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [480] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(18), 1,
      sym_comment,
    ACTIONS(74), 3,
      anon_sym_SLASH,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(72), 11,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [505] = 7,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(52), 1,
      anon_sym_SLASH,
    ACTIONS(54), 1,
      anon_sym_STAR,
    ACTIONS(62), 1,
      anon_sym_and,
    STATE(19), 1,
      sym_comment,
    ACTIONS(78), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(76), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [536] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(20), 1,
      sym_comment,
    ACTIONS(70), 3,
      anon_sym_SLASH,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(68), 11,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [561] = 7,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(52), 1,
      anon_sym_SLASH,
    ACTIONS(54), 1,
      anon_sym_STAR,
    ACTIONS(62), 1,
      anon_sym_and,
    STATE(21), 1,
      sym_comment,
    ACTIONS(82), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(80), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [592] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(22), 1,
      sym_comment,
    ACTIONS(86), 3,
      anon_sym_SLASH,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(84), 11,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [617] = 7,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(52), 1,
      anon_sym_SLASH,
    ACTIONS(54), 1,
      anon_sym_STAR,
    ACTIONS(62), 1,
      anon_sym_and,
    STATE(23), 1,
      sym_comment,
    ACTIONS(86), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(84), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [648] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(24), 1,
      sym_comment,
    ACTIONS(88), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_number,
      sym_string,
    ACTIONS(90), 5,
      anon_sym_print,
      anon_sym_true,
      anon_sym_false,
      sym_null,
      sym_identifier,
  [669] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(25), 1,
      sym_comment,
    ACTIONS(92), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_number,
      sym_string,
    ACTIONS(94), 5,
      anon_sym_print,
      anon_sym_true,
      anon_sym_false,
      sym_null,
      sym_identifier,
  [690] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(26), 1,
      sym_comment,
    ACTIONS(96), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_number,
      sym_string,
    ACTIONS(98), 5,
      anon_sym_print,
      anon_sym_true,
      anon_sym_false,
      sym_null,
      sym_identifier,
  [711] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(27), 1,
      sym_comment,
    ACTIONS(100), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_number,
      sym_string,
    ACTIONS(102), 5,
      anon_sym_print,
      anon_sym_true,
      anon_sym_false,
      sym_null,
      sym_identifier,
  [732] = 4,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    STATE(28), 1,
      sym_comment,
    ACTIONS(104), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_number,
      sym_string,
    ACTIONS(106), 5,
      anon_sym_print,
      anon_sym_true,
      anon_sym_false,
      sym_null,
      sym_identifier,
  [753] = 3,
    ACTIONS(3), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(108), 1,
      ts_builtin_sym_end,
    STATE(29), 1,
      sym_comment,
  [763] = 3,
    ACTIONS(110), 1,
      anon_sym_SLASH_SLASH,
    ACTIONS(112), 1,
      aux_sym_comment_token1,
    STATE(30), 1,
      sym_comment,
  [773] = 1,
    ACTIONS(114), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(4)] = 0,
  [SMALL_STATE(5)] = 37,
  [SMALL_STATE(6)] = 74,
  [SMALL_STATE(7)] = 111,
  [SMALL_STATE(8)] = 148,
  [SMALL_STATE(9)] = 185,
  [SMALL_STATE(10)] = 222,
  [SMALL_STATE(11)] = 259,
  [SMALL_STATE(12)] = 296,
  [SMALL_STATE(13)] = 321,
  [SMALL_STATE(14)] = 346,
  [SMALL_STATE(15)] = 371,
  [SMALL_STATE(16)] = 410,
  [SMALL_STATE(17)] = 449,
  [SMALL_STATE(18)] = 480,
  [SMALL_STATE(19)] = 505,
  [SMALL_STATE(20)] = 536,
  [SMALL_STATE(21)] = 561,
  [SMALL_STATE(22)] = 592,
  [SMALL_STATE(23)] = 617,
  [SMALL_STATE(24)] = 648,
  [SMALL_STATE(25)] = 669,
  [SMALL_STATE(26)] = 690,
  [SMALL_STATE(27)] = 711,
  [SMALL_STATE(28)] = 732,
  [SMALL_STATE(29)] = 753,
  [SMALL_STATE(30)] = 763,
  [SMALL_STATE(31)] = 773,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0, 0, 0),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(4),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [21] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(13),
  [24] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(4),
  [27] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(5),
  [30] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(13),
  [33] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(12),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bool, 1, 0, 0),
  [38] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_bool, 1, 0, 0),
  [40] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__primary, 1, 0, 0),
  [42] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__primary, 1, 0, 0),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expression, 1, 0, 0),
  [46] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expression, 1, 0, 0),
  [48] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [50] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [52] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [54] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [56] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [58] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [60] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [62] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [64] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [66] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary, 3, 0, 0),
  [70] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary, 3, 0, 0),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary, 2, 0, 0),
  [74] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary, 2, 0, 0),
  [76] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comparison, 3, 0, 0),
  [78] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comparison, 3, 0, 0),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_equality, 3, 0, 0),
  [82] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_equality, 3, 0, 0),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_logical, 3, 0, 0),
  [86] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_logical, 3, 0, 0),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 1, 0, 0),
  [90] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 1, 0, 0),
  [92] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__declaration, 1, 0, 0),
  [94] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__declaration, 1, 0, 0),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__statement, 1, 0, 0),
  [98] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__statement, 1, 0, 0),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_printStatement, 3, 0, 0),
  [102] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_printStatement, 3, 0, 0),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expressionStatement, 2, 0, 0),
  [106] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expressionStatement, 2, 0, 0),
  [108] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [110] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [112] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [114] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment, 2, 0, 0),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_flow(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .keyword_lex_fn = ts_lex_keywords,
    .keyword_capture_token = sym_identifier,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
