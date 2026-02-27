; Keywords
"let" @keyword
"return" @keyword.return
"if" @keyword.conditional
"else" @keyword.conditional
"match" @keyword.conditional
"fn" @keyword.function
"enum" @keyword.type
"trait" @keyword.type
"impl" @keyword.type
"derive" @keyword.type
"type" @keyword.type
"for" @keyword
"is" @keyword.operator

; Literals
(integer_literal) @number
(float_literal) @number.float
(string_literal) @string
(string_content) @string
(boolean_literal) @boolean
(comment) @comment

; Operators
"+" @operator
"-" @operator
"*" @operator
"/" @operator
"==" @operator
"!=" @operator
"<" @operator
">" @operator
"!" @operator
"=" @operator
"->" @operator
"|" @operator
"." @operator
"..." @operator

; Punctuation
"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"," @punctuation.delimiter
";" @punctuation.delimiter
":" @punctuation.delimiter

; Type identifiers
(type_identifier) @type.builtin

; Type variables in type annotations
(type_variable
  (identifier) @type)

; Generic type name
(generic_type
  name: (identifier) @type)

; Function definitions
(function_literal
  (parameter
    name: (identifier) @variable.parameter))

; Method definitions
(method_definition
  name: (identifier) @function.method)

(method_definition
  (parameter
    name: (identifier) @variable.parameter))

; Trait method signatures
(trait_method_signature
  name: (identifier) @function.method)

(trait_method_signature
  (parameter
    name: (identifier) @variable.parameter))

; Function calls
(call_expression
  function: (identifier) @function.call)

; Method calls (field_access used as callee)
(call_expression
  function: (field_access
    field: (identifier) @function.method.call))

; Field access
(field_access
  field: (identifier) @property)

; Let binding name
(let_statement
  name: (identifier) @variable)

; Enum definition
(enum_definition
  name: (identifier) @type)

(enum_variant
  name: (identifier) @constructor)

; Trait definition
(trait_definition
  name: (identifier) @type)

(trait_field
  name: (identifier) @property)

; Impl block
(impl_block
  trait: (identifier) @type)

; Derive
(derive_statement
  trait: (identifier) @type)

; Type alias
(type_alias
  name: (identifier) @type)

; Type parameters
(type_parameter_list
  (identifier) @type)

(constrained_type_param
  name: (identifier) @type)

(constraint_list
  (identifier) @type)

; Supertrait
(supertrait_list
  (identifier) @type)

; Pattern matching
(wildcard_pattern) @variable.builtin

(constructor_pattern
  enum: (identifier) @type
  variant: (identifier) @constructor)

(record_pattern_field
  name: (identifier) @property)

(record_pattern_punned
  name: (identifier) @variable)

; Record/hash entry keys
(object_entry
  key: (identifier) @property)

; Builtin functions (common ones)
((identifier) @function.builtin
  (#any-of? @function.builtin "puts" "len" "first" "rest" "push"))
