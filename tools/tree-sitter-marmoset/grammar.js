/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  OR_PATTERN: 1,
  UNION_TYPE: 2,
  LOGICAL_OR: 3,
  LOGICAL_AND: 4,
  EQUALS: 5,
  LESS_GREATER: 6,
  SUM: 7,
  PRODUCT: 8,
  IS: 9,
  PREFIX: 10,
  CALL: 11,
  INDEX: 12,
  DOT: 13,
};

module.exports = grammar({
  name: "marmoset",

  extras: ($) => [/\s/, $.comment],

  word: ($) => $.identifier,

  inline: ($) => [$.trait_sig_param],

  conflicts: ($) => [
    [$._expression, $.lambda_parameter],
    [$.block, $.object_literal],
  ],

  rules: {
    source_file: ($) => repeat($._statement),

    // ── Statements ──────────────────────────────────────────────

    _statement: ($) =>
      choice(
        $.fn_declaration,
        $.let_statement,
        $.return_statement,
        $.enum_definition,
        $.trait_definition,
        $.impl_block,
        $.type_alias,
        $.expression_statement,
      ),

    let_statement: ($) =>
      seq(
        "let",
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
        "=",
        field("value", $._expression),
        optional(";"),
      ),

    return_statement: ($) =>
      seq("return", field("value", $._expression), optional(";")),

    expression_statement: ($) => seq($._expression, optional(";")),

    block: ($) => seq("{", repeat($._statement), "}"),

    expr_or_block: ($) => choice($.block, $._expression),

    // ── Top-level definitions ───────────────────────────────────

    fn_declaration: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        "(",
        commaSep($.parameter),
        ")",
        optional(seq(choice("->", "=>"), field("return_type", $._type))),
        "=",
        field("body", $.expr_or_block),
        optional(";"),
      ),

    enum_definition: ($) =>
      prec.right(seq(
        "enum",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        "=",
        "{",
        repeat(choice($.enum_variant, ",")),
        "}",
        optional(field("derive", $.derive_clause)),
      )),

    enum_variant: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq("(", commaSep1($._type), ")")),
      ),

    trait_definition: ($) =>
      seq(
        "trait",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        optional(seq(":", field("supertraits", $.supertrait_list))),
        "=",
        "{",
        repeat(choice($.trait_method_signature, $.trait_field)),
        "}",
      ),

    trait_method_signature: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        "(",
        commaSep($.trait_sig_param),
        ")",
        optional(seq(choice("->", "=>"), field("return_type", $._type))),
        optional(seq("=", field("body", $.expr_or_block))),
      ),

    trait_sig_param: ($) => choice($.trait_named_param, $._type),

    trait_named_param: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("type", $._type),
      ),

    trait_field: ($) =>
      seq(field("name", $.identifier), ":", field("type", $._type), optional(",")),

    supertrait_list: ($) => seq($.identifier, repeat(seq("&", $.identifier))),

    impl_block: ($) =>
      seq(
        "impl",
        optional(field("type_params", $.impl_type_parameter_list)),
        field("target", $._type),
        "=",
        "{",
        repeat($.method_definition),
        "}",
      ),

    impl_type_parameter_list: ($) =>
      seq("[", commaSep1($._impl_type_param), "]"),

    _impl_type_param: ($) =>
      choice($.constrained_type_param, $.identifier),

    method_definition: ($) =>
      seq(
        optional("override"),
        "fn",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        "(",
        commaSep($.parameter),
        ")",
        optional(seq(choice("->", "=>"), field("return_type", $._type))),
        "=",
        field("body", $.expr_or_block),
      ),

    derive_clause: ($) =>
      prec.right(seq(
        "derive",
        commaSep1(field("trait", $.identifier)),
      )),

    type_alias: ($) =>
      prec.right(seq(
        "type",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        "=",
        field("type", $._type),
        optional(field("derive", $.derive_clause)),
      )),

    // ── Types ───────────────────────────────────────────────────

    _type: ($) => choice($.union_type, $._non_union_type),

    _non_union_type: ($) =>
      choice(
        $.trait_object_type,
        $.generic_type,
        $._simple_type,
        $.function_type,
        $.record_type,
        $.parenthesized_type,
      ),

    _simple_type: ($) => choice($.type_identifier, $.type_variable),

    type_identifier: ($) =>
      token(choice("Int", "Str", "Bool", "Float", "Unit", "List", "Map")),

    type_variable: ($) => $.identifier,

    generic_type: ($) =>
      prec(1, seq(
        field("name", choice($.type_identifier, $.identifier)),
        "[",
        commaSep1(field("arg", $._type)),
        "]",
      )),

    trait_object_type: ($) =>
      seq(
        "Dyn",
        "[",
        field("constraints", $.supertrait_list),
        "]",
      ),

    function_type: ($) =>
      seq("(", commaSep($._type), ")", choice("->", "=>"), field("return_type", $._type)),

    parenthesized_type: ($) => seq("(", $._type, ")"),

    union_type: ($) =>
      prec.left(
        PREC.UNION_TYPE,
        seq($._non_union_type, repeat1(seq("|", $._non_union_type))),
      ),

    record_type: ($) =>
      seq(
        "{",
        commaSep1(
          choice($.record_type_field, $.row_variable),
        ),
        "}",
      ),

    record_type_field: ($) =>
      seq(field("name", $.identifier), ":", field("type", $._type)),

    row_variable: ($) => seq("...", $.identifier),

    // ── Type parameters ─────────────────────────────────────────

    type_parameter_list: ($) =>
      seq("[", commaSep1($._type_parameter), "]"),

    _type_parameter: ($) =>
      choice($.constrained_type_param, $.identifier),

    constrained_type_param: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("constraints", $.constraint_list),
      ),

    constraint_list: ($) => seq($.identifier, repeat(seq("&", $.identifier))),

    // ── Expressions ─────────────────────────────────────────────

    _expression: ($) =>
      choice(
        $.placeholder,
        $.identifier,
        $.integer_literal,
        $.float_literal,
        $.string_literal,
        $.boolean_literal,
        $.array_literal,
        $.object_literal,
        $.prefix_expression,
        $.infix_expression,
        $.is_expression,
        $.lambda_expression,
        $.parenthesized_expression,
        $.if_expression,
        $.match_expression,
        $.call_expression,
        $.field_access,
        $.index_expression,
      ),

    integer_literal: ($) => /[0-9]+/,

    float_literal: ($) => /[0-9]+\.[0-9]+/,

    string_literal: ($) => seq('"', optional($.string_content), '"'),

    string_content: ($) => /[^"]+/,

    boolean_literal: ($) => choice("true", "false"),

    array_literal: ($) => seq("[", commaSep($._expression), "]"),

    // Unified brace literal for both records and hashes.
    // Records: { x: 1, y: 2 } or { ...p, x: 10 }
    // Hashes:  { "key": val }  or { 1: "one" }
    // The distinction is semantic, not syntactic when keys are identifiers.
    object_literal: ($) =>
      seq("{", commaSep($.object_entry), "}"),

    object_entry: ($) =>
      choice(
        // key: value  (record field or hash entry)
        // value is optional for punning: { x:, y: }
        seq(field("key", $._expression), ":", optional(field("value", $._expression))),
        // spread:  { ...expr }
        $.spread_entry,
      ),

    spread_entry: ($) => seq("...", $._expression),

    prefix_expression: ($) =>
      prec(
        PREC.PREFIX,
        seq(
          field("operator", choice("-", "!")),
          field("operand", $._expression),
        ),
      ),

    infix_expression: ($) =>
      choice(
        ...[
          ["||", PREC.LOGICAL_OR],
          ["&&", PREC.LOGICAL_AND],
          ["+", PREC.SUM],
          ["-", PREC.SUM],
          ["*", PREC.PRODUCT],
          ["/", PREC.PRODUCT],
          ["%", PREC.PRODUCT],
          ["==", PREC.EQUALS],
          ["!=", PREC.EQUALS],
          ["<", PREC.LESS_GREATER],
          ["<=", PREC.LESS_GREATER],
          [">", PREC.LESS_GREATER],
          [">=", PREC.LESS_GREATER],
        ].map(([op, prec_val]) =>
          prec.left(
            prec_val,
            seq(
              field("left", $._expression),
              field("operator", op),
              field("right", $._expression),
            ),
          ),
        ),
      ),

    is_expression: ($) =>
      prec.left(
        PREC.IS,
        seq(field("value", $._expression), "is", field("type", $._non_union_type)),
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    if_expression: ($) =>
      prec.right(seq(
        "if",
        "(",
        field("condition", $._expression),
        ")",
        field("consequence", choice($.expr_or_block, $.return_shorthand)),
        optional(seq("else", field("alternative", choice($.expr_or_block, $.return_shorthand)))),
      )),

    // Early return without braces: if (cond) return expr else ...
    return_shorthand: ($) => seq("return", $._expression),

    match_expression: ($) =>
      seq(
        "match",
        field("value", $._expression),
        "{",
        repeat($.match_arm),
        "}",
      ),

    match_arm: ($) =>
      seq("case", field("pattern", $._pattern), ":", field("body", $.expr_or_block)),

    lambda_expression: ($) =>
      prec.right(
        seq(
          "(",
          commaSep($.lambda_parameter),
          ")",
          choice("->", "=>"),
          field("body", $.expr_or_block),
        ),
      ),

    lambda_parameter: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
      ),

    parameter: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
      ),

    call_expression: ($) =>
      prec(
        PREC.CALL,
        seq(
          field("function", $._expression),
          token.immediate("("),
          commaSep($._expression),
          ")",
        ),
      ),

    field_access: ($) =>
      prec.left(
        PREC.DOT,
        seq(field("object", $._expression), ".", field("field", $.identifier)),
      ),

    index_expression: ($) =>
      prec(
        PREC.INDEX,
        seq(
          field("object", $._expression),
          token.immediate("["),
          field("index", $._expression),
          "]",
        ),
      ),

    // ── Patterns ────────────────────────────────────────────────

    _pattern: ($) =>
      choice(
        $.alternative_pattern,
        $._primary_pattern,
      ),

    _primary_pattern: ($) =>
      choice(
        $.wildcard_pattern,
        $.constructor_pattern,
        $.record_pattern,
        $.literal_pattern,
        $.variable_pattern,
      ),

    alternative_pattern: ($) =>
      prec.left(
        PREC.OR_PATTERN,
        seq($._primary_pattern, repeat1(seq("|", $._primary_pattern))),
      ),

    wildcard_pattern: ($) => "_",

    variable_pattern: ($) => $.identifier,

    literal_pattern: ($) =>
      choice($.integer_literal, $.float_literal, $.string_literal, $.boolean_literal),

    constructor_pattern: ($) =>
      seq(
        field("enum", $.identifier),
        ".",
        field("variant", $.identifier),
        optional(seq("(", commaSep1($._pattern), ")")),
      ),

    record_pattern: ($) =>
      seq(
        "{",
        commaSep1(
          choice($.record_pattern_field, $.record_pattern_punned, $.spread_pattern),
        ),
        "}",
      ),

    record_pattern_field: ($) =>
      seq(field("name", $.identifier), ":", field("pattern", $._pattern)),

    record_pattern_punned: ($) =>
      seq(field("name", $.identifier), ":"),

    spread_pattern: ($) => seq("...", optional($.identifier)),

    // ── Atoms ───────────────────────────────────────────────────

    placeholder: ($) => "_",

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*[!?]?/,

    comment: ($) => token(seq("#", /.*/)),
  },
});

// Helpers

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)), optional(","));
}

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
