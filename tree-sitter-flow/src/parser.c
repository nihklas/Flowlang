#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 25
#define LARGE_STATE_COUNT 4
#define SYMBOL_COUNT 37
#define ALIAS_COUNT 0
#define TOKEN_COUNT 23
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
  sym_source_file = 23,
  sym__declaration = 24,
  sym__statement = 25,
  sym_expressionStatement = 26,
  sym_printStatement = 27,
  sym__expression = 28,
  sym_unary = 29,
  sym_binary = 30,
  sym_comparison = 31,
  sym_equality = 32,
  sym_logical = 33,
  sym__primary = 34,
  sym_bool = 35,
  aux_sym_source_file_repeat1 = 36,
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
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(7);
      ADVANCE_MAP(
        '!', 11,
        '"', 2,
        '*', 13,
        '+', 14,
        '-', 9,
        '.', 15,
        '/', 12,
        ';', 8,
        '<', 16,
        '=', 4,
        '>', 19,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(0);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 1:
      ADVANCE_MAP(
        '!', 3,
        '*', 13,
        '+', 14,
        '-', 9,
        '.', 15,
        '/', 12,
        ';', 8,
        '<', 16,
        '=', 4,
        '>', 19,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(1);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(24);
      if (lookahead != 0) ADVANCE(2);
      END_STATE();
    case 3:
      if (lookahead == '=') ADVANCE(20);
      END_STATE();
    case 4:
      if (lookahead == '=') ADVANCE(21);
      END_STATE();
    case 5:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(23);
      END_STATE();
    case 6:
      if (eof) ADVANCE(7);
      if (lookahead == '!') ADVANCE(10);
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '-') ADVANCE(9);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(6);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(20);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '=') ADVANCE(17);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '=') ADVANCE(18);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_BANG_EQ);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(5);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(23);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_string);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
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
      if (('\t' <= lookahead && lookahead <= '\r') ||
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
  [1] = {.lex_state = 6},
  [2] = {.lex_state = 6},
  [3] = {.lex_state = 6},
  [4] = {.lex_state = 6},
  [5] = {.lex_state = 6},
  [6] = {.lex_state = 6},
  [7] = {.lex_state = 6},
  [8] = {.lex_state = 6},
  [9] = {.lex_state = 6},
  [10] = {.lex_state = 6},
  [11] = {.lex_state = 6},
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
  [22] = {.lex_state = 6},
  [23] = {.lex_state = 6},
  [24] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
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
  },
  [1] = {
    [sym_source_file] = STATE(24),
    [sym__declaration] = STATE(2),
    [sym__statement] = STATE(2),
    [sym_expressionStatement] = STATE(2),
    [sym_printStatement] = STATE(2),
    [sym__expression] = STATE(14),
    [sym_unary] = STATE(14),
    [sym_binary] = STATE(14),
    [sym_comparison] = STATE(14),
    [sym_equality] = STATE(14),
    [sym_logical] = STATE(14),
    [sym__primary] = STATE(14),
    [sym_bool] = STATE(14),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(3),
    [sym_identifier] = ACTIONS(5),
    [anon_sym_print] = ACTIONS(7),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_number] = ACTIONS(11),
    [sym_string] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_null] = ACTIONS(5),
  },
  [2] = {
    [sym__declaration] = STATE(3),
    [sym__statement] = STATE(3),
    [sym_expressionStatement] = STATE(3),
    [sym_printStatement] = STATE(3),
    [sym__expression] = STATE(14),
    [sym_unary] = STATE(14),
    [sym_binary] = STATE(14),
    [sym_comparison] = STATE(14),
    [sym_equality] = STATE(14),
    [sym_logical] = STATE(14),
    [sym__primary] = STATE(14),
    [sym_bool] = STATE(14),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(15),
    [sym_identifier] = ACTIONS(5),
    [anon_sym_print] = ACTIONS(7),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_number] = ACTIONS(11),
    [sym_string] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_null] = ACTIONS(5),
  },
  [3] = {
    [sym__declaration] = STATE(3),
    [sym__statement] = STATE(3),
    [sym_expressionStatement] = STATE(3),
    [sym_printStatement] = STATE(3),
    [sym__expression] = STATE(14),
    [sym_unary] = STATE(14),
    [sym_binary] = STATE(14),
    [sym_comparison] = STATE(14),
    [sym_equality] = STATE(14),
    [sym_logical] = STATE(14),
    [sym__primary] = STATE(14),
    [sym_bool] = STATE(14),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(17),
    [sym_identifier] = ACTIONS(19),
    [anon_sym_print] = ACTIONS(22),
    [anon_sym_DASH] = ACTIONS(25),
    [anon_sym_BANG] = ACTIONS(25),
    [sym_number] = ACTIONS(28),
    [sym_string] = ACTIONS(28),
    [anon_sym_true] = ACTIONS(31),
    [anon_sym_false] = ACTIONS(31),
    [sym_null] = ACTIONS(19),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(34), 2,
      sym_null,
      sym_identifier,
    ACTIONS(36), 2,
      sym_number,
      sym_string,
    STATE(13), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [27] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(38), 2,
      sym_null,
      sym_identifier,
    ACTIONS(40), 2,
      sym_number,
      sym_string,
    STATE(15), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [54] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(42), 2,
      sym_null,
      sym_identifier,
    ACTIONS(44), 2,
      sym_number,
      sym_string,
    STATE(17), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [81] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(46), 2,
      sym_null,
      sym_identifier,
    ACTIONS(48), 2,
      sym_number,
      sym_string,
    STATE(19), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [108] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(50), 2,
      sym_null,
      sym_identifier,
    ACTIONS(52), 2,
      sym_number,
      sym_string,
    STATE(18), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [135] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(54), 2,
      sym_null,
      sym_identifier,
    ACTIONS(56), 2,
      sym_number,
      sym_string,
    STATE(21), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [162] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(58), 2,
      sym_null,
      sym_identifier,
    ACTIONS(60), 2,
      sym_number,
      sym_string,
    STATE(20), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [189] = 5,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(62), 2,
      sym_null,
      sym_identifier,
    ACTIONS(64), 2,
      sym_number,
      sym_string,
    STATE(16), 8,
      sym__expression,
      sym_unary,
      sym_binary,
      sym_comparison,
      sym_equality,
      sym_logical,
      sym__primary,
      sym_bool,
  [216] = 2,
    ACTIONS(68), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(66), 12,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [235] = 8,
    ACTIONS(70), 1,
      anon_sym_SEMI,
    ACTIONS(82), 1,
      anon_sym_and,
    ACTIONS(84), 1,
      anon_sym_or,
    ACTIONS(74), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(76), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(78), 2,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(80), 2,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
    ACTIONS(72), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
  [266] = 8,
    ACTIONS(82), 1,
      anon_sym_and,
    ACTIONS(84), 1,
      anon_sym_or,
    ACTIONS(86), 1,
      anon_sym_SEMI,
    ACTIONS(74), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(76), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(78), 2,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(80), 2,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
    ACTIONS(72), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
  [297] = 2,
    ACTIONS(90), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(88), 12,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [316] = 2,
    ACTIONS(94), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(92), 12,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [335] = 4,
    ACTIONS(82), 1,
      anon_sym_and,
    ACTIONS(74), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(94), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(92), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [358] = 4,
    ACTIONS(82), 1,
      anon_sym_and,
    ACTIONS(74), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(98), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(96), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [381] = 4,
    ACTIONS(82), 1,
      anon_sym_and,
    ACTIONS(74), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(102), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(100), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [404] = 4,
    ACTIONS(82), 1,
      anon_sym_and,
    ACTIONS(74), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(106), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(104), 9,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_or,
  [427] = 2,
    ACTIONS(106), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(104), 12,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_DOT,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_BANG_EQ,
      anon_sym_EQ_EQ,
      anon_sym_and,
      anon_sym_or,
  [446] = 2,
    ACTIONS(108), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_number,
      sym_string,
    ACTIONS(110), 5,
      anon_sym_print,
      anon_sym_true,
      anon_sym_false,
      sym_null,
      sym_identifier,
  [461] = 2,
    ACTIONS(112), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_number,
      sym_string,
    ACTIONS(114), 5,
      anon_sym_print,
      anon_sym_true,
      anon_sym_false,
      sym_null,
      sym_identifier,
  [476] = 1,
    ACTIONS(116), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(4)] = 0,
  [SMALL_STATE(5)] = 27,
  [SMALL_STATE(6)] = 54,
  [SMALL_STATE(7)] = 81,
  [SMALL_STATE(8)] = 108,
  [SMALL_STATE(9)] = 135,
  [SMALL_STATE(10)] = 162,
  [SMALL_STATE(11)] = 189,
  [SMALL_STATE(12)] = 216,
  [SMALL_STATE(13)] = 235,
  [SMALL_STATE(14)] = 266,
  [SMALL_STATE(15)] = 297,
  [SMALL_STATE(16)] = 316,
  [SMALL_STATE(17)] = 335,
  [SMALL_STATE(18)] = 358,
  [SMALL_STATE(19)] = 381,
  [SMALL_STATE(20)] = 404,
  [SMALL_STATE(21)] = 427,
  [SMALL_STATE(22)] = 446,
  [SMALL_STATE(23)] = 461,
  [SMALL_STATE(24)] = 476,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0, 0, 0),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(4),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [19] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(14),
  [22] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(4),
  [25] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(5),
  [28] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(14),
  [31] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(12),
  [34] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [36] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [38] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [40] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [42] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [44] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [46] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [48] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [50] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [52] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [54] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [56] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [58] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [60] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [62] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [64] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bool, 1, 0, 0),
  [68] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_bool, 1, 0, 0),
  [70] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [72] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [74] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [76] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary, 2, 0, 0),
  [90] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary, 2, 0, 0),
  [92] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary, 3, 0, 0),
  [94] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary, 3, 0, 0),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_equality, 3, 0, 0),
  [98] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_equality, 3, 0, 0),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comparison, 3, 0, 0),
  [102] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comparison, 3, 0, 0),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_logical, 3, 0, 0),
  [106] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_logical, 3, 0, 0),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expressionStatement, 2, 0, 0),
  [110] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expressionStatement, 2, 0, 0),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_printStatement, 3, 0, 0),
  [114] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_printStatement, 3, 0, 0),
  [116] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
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
