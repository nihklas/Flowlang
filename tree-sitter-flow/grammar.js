/**
 * @file A toy language, inspired by Go's concurrency model
 * @author Niklas Koll <niklas@nkoll.de>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "flow",
  word: $ => $.identifier,
  extras: $ => [' ', '\n', '\r', $.comment],
  rules: {
    source_file: $ => repeat($._declaration),

    _declaration: $ => choice(
      $.varDeclaration,
      // $.functionDeclaration,
      // $.channelDeclaration,
      $._statement,
    ),

    varDeclaration: $ => seq(
      choice('var', 'const'),
      $.identifier,
      optional($._typeHint),
      optional(seq('=', $._expression)),
      ';'
    ),

    _statement: $ => choice(
      $.expressionStatement,
      $.printStatement,
      $._block,
      $.ifStatement,
      // $.forStatement,
      // $.returnStatement,
      // $.chnReadStatement,
      // $.chnWriteStatement,
    ),

    _block: $ => seq(
      '{',
      repeat($._statement),
      '}'
    ),

    ifStatement: $ => prec.left(seq(
      'if',
      $._expression,
      $._statement,
      optional(seq('else', $._statement)),
    )),

    expressionStatement: $ => seq(
      $._expression,
      ';'
    ),

    printStatement: $ => seq(
      'print',
      $._expression,
      ';'
    ),

    _expression: $ => choice(
      $._primary,
      $.unary,
      $.binary,
      $.comparison,
      $.equality,
      $.logical,
      $.assignment,
      // TODO: Call
    ),

    unary: $ => prec(8, seq(
      choice('-', '!'),
      $._expression,
    )),

    binary: $ => choice(
      prec.left(7, seq($._expression, '/', $._expression)),
      prec.left(7, seq($._expression, '*', $._expression)),
      prec.left(6, seq($._expression, '-', $._expression)),
      prec.left(6, seq($._expression, '+', $._expression)),
      prec.left(1, seq($._expression, '.', $._expression)),
    ),

    comparison: $ => choice(
      prec.left(5, seq($._expression, '<', $._expression)),
      prec.left(5, seq($._expression, '<=', $._expression)),
      prec.left(5, seq($._expression, '>=', $._expression)),
      prec.left(5, seq($._expression, '>', $._expression)),
    ),

    equality: $ => choice(
      prec.left(4, seq($._expression, '!=', $._expression)),
      prec.left(4, seq($._expression, '==', $._expression)),
    ),

    logical: $ => choice(
      prec.left(3, seq($._expression, 'and', $._expression)),
      prec.left(2, seq($._expression, 'or', $._expression)),
    ),

    assignment: $ => seq(
      $.identifier,
      '=',
      $._expression
    ),

    _primary: $ => choice(
      $.number,
      $.string,
      $.bool,
      $.null,
      $.identifier,
    ),

    number: _ => /\d+(\.\d+)?/,
    string: _ => /"[^"]*"/,
    bool: _ => choice('true', 'false'),
    null: _ => 'null',
    identifier: _ => /[a-zA-Z]([a-zA-Z0-9_])*/,

    _typeHint: $ => seq(':', $.identifier),

    comment: _ => seq(
      '//',
      /.*/
    )
  }
});
