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
  rules: {
    source_file: $ => repeat($._declaration),

    _declaration: $ => choice(
      $._statement,
      // $.varDeclaration,
      // TODO:
    ),

    _statement: $ => choice(
      $.expressionStatement,
      $.printStatement,
      // TODO:
    ),

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
      // TODO:
    ),

    unary: $ => prec(3, seq(
      choice('-', '!'),
      $._expression,
    )),

    binary: $ => choice(
      prec.left(2, seq($._expression, '/', $._expression)),
      prec.left(2, seq($._expression, '*', $._expression)),
      prec.left(1, seq($._expression, '-', $._expression)),
      prec.left(1, seq($._expression, '+', $._expression)),
      prec.left(1, seq($._expression, '.', $._expression)),
    ),

    comparison: $ => choice(
      prec.left(1, seq($._expression, '<', $._expression)),
      prec.left(1, seq($._expression, '<=', $._expression)),
      prec.left(1, seq($._expression, '>=', $._expression)),
      prec.left(1, seq($._expression, '>', $._expression)),
    ),

    equality: $ => choice(
      prec.left(1, seq($._expression, '!=', $._expression)),
      prec.left(1, seq($._expression, '==', $._expression)),
    ),

    logical: $ => choice(
      prec.left(2, seq($._expression, 'and', $._expression)),
      prec.left(1, seq($._expression, 'or', $._expression)),
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
  }
});
