; Marmoset highlight queries for Neovim
; Uses standard Neovim capture names (see :h treesitter-highlight-groups)

; ── Keywords ──────────────────────────────────────────────────────

"let" @keyword
"return" @keyword.return
"if" @keyword.conditional
"else" @keyword.conditional
"match" @keyword.conditional
"case" @keyword.conditional
"fn" @keyword.function
"enum" @keyword.type
"trait" @keyword.type
"impl" @keyword.type
"derive" @keyword.type
"override" @keyword.modifier
"type" @keyword.type
"for" @keyword
"is" @keyword.operator

; ── Literals ──────────────────────────────────────────────────────

(integer_literal) @number
(float_literal) @number.float
(string_literal) @string
(string_content) @string
(boolean_literal) @boolean

; ── Comments ──────────────────────────────────────────────────────

(comment) @comment @spell

; ── Operators ─────────────────────────────────────────────────────

(infix_expression
  operator: _ @operator)

(prefix_expression
  operator: _ @operator)

"=" @operator
"->" @operator
"=>" @operator
"|" @operator
"&" @operator
"." @punctuation.delimiter
"..." @operator
"%" @operator
"&&" @operator
"||" @operator
"<=" @operator
">=" @operator

; ── Punctuation ───────────────────────────────────────────────────

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"," @punctuation.delimiter
";" @punctuation.delimiter
":" @punctuation.delimiter

; ── Builtin types ─────────────────────────────────────────────────

(type_identifier) @type.builtin

; ── Type variables in annotations ─────────────────────────────────

(type_variable
  (identifier) @type)

; Generic type name (e.g., list in list[int])
(generic_type
  name: (identifier) @type)

; Record type field names
(record_type_field
  name: (identifier) @property)

; Row variable
(row_variable
  (identifier) @type)

; ── Function definitions ──────────────────────────────────────────

; Parameters in function literals
(fn_declaration
  name: (identifier) @function)

(fn_declaration
  (parameter
    name: (identifier) @variable.parameter))

(function_literal
  (parameter
    name: (identifier) @variable.parameter))

(lambda_expression
  (lambda_parameter
    name: (identifier) @variable.parameter))

; Parameters in method definitions
(method_definition
  name: (identifier) @function.method)

(method_definition
  (parameter
    name: (identifier) @variable.parameter))

; Parameters in trait method signatures
(trait_method_signature
  name: (identifier) @function.method)

(trait_method_signature
  (parameter
    name: (identifier) @variable.parameter))

; ── Function calls ────────────────────────────────────────────────

; Direct function call: foo(...)
(call_expression
  function: (identifier) @function.call)

; Method call: x.method(...)
(call_expression
  function: (field_access
    field: (identifier) @function.method.call))

; ── Builtin functions ─────────────────────────────────────────────

((identifier) @function.builtin
  (#any-of? @function.builtin "puts" "len" "first" "rest" "push"))

; ── Field access ──────────────────────────────────────────────────

(field_access
  field: (identifier) @property)

; ── Let binding ───────────────────────────────────────────────────

(let_statement
  name: (identifier) @variable)

; ── Enum definition ───────────────────────────────────────────────

(enum_definition
  name: (identifier) @type)

(enum_variant
  name: (identifier) @constructor)

; ── Trait definition ──────────────────────────────────────────────

(trait_definition
  name: (identifier) @type)

(trait_field
  name: (identifier) @property)

; Supertrait names
(supertrait_list
  (identifier) @type)

; ── Impl block ────────────────────────────────────────────────────

(impl_block
  trait: (identifier) @type)

(impl_block
  target: (generic_type
    name: (identifier) @type))

; ── Derive statement ──────────────────────────────────────────────

(derive_clause
  trait: (identifier) @type)

(derive_statement
  trait: (identifier) @type)

; ── Type alias ────────────────────────────────────────────────────

(type_alias
  name: (identifier) @type.definition)

; ── Type parameters ───────────────────────────────────────────────

(type_parameter_list
  (identifier) @type)

(impl_type_parameter_list
  (identifier) @type)

(constrained_type_param
  name: (identifier) @type)

(constraint_list
  (identifier) @type)

; ── Pattern matching ──────────────────────────────────────────────

(placeholder) @variable.builtin

(wildcard_pattern) @variable.builtin

(variable_pattern
  (identifier) @variable)

(constructor_pattern
  enum: (identifier) @type
  variant: (identifier) @constructor)

(record_pattern_field
  name: (identifier) @property)

(record_pattern_punned
  name: (identifier) @variable)

; ── Record/hash entries ───────────────────────────────────────────

(object_entry
  key: (identifier) @property)

; ── Return shorthand ──────────────────────────────────────────────

(return_shorthand
  "return" @keyword.return)
