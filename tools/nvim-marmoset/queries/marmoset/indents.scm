; Marmoset indent queries for Neovim tree-sitter indentation

; Indent inside blocks
(block) @indent.begin

; Indent inside enum bodies
(enum_definition
  "{" @indent.begin
  "}" @indent.end)

; Indent inside shape bodies
(shape_definition
  "{" @indent.begin
  "}" @indent.end)

; Indent inside trait bodies
(trait_definition
  "{" @indent.begin
  "}" @indent.end)

; Indent inside impl bodies
(impl_block
  "{" @indent.begin
  "}" @indent.end)

; Indent inside match expressions
(match_expression
  "{" @indent.begin
  "}" @indent.end)

; Indent inside array literals
(array_literal
  "[" @indent.begin
  "]" @indent.end)

; Indent inside object literals
(object_literal
  "{" @indent.begin
  "}" @indent.end)

; Indent inside record types
(record_type
  "{" @indent.begin
  "}" @indent.end)

; Indent inside canonical sum-type bodies
(constructor_type_body
  "{" @indent.begin
  "}" @indent.end)

; Indent inside parenthesized expressions
(parenthesized_expression
  "(" @indent.begin
  ")" @indent.end)

; Indent inside function call arguments
(call_expression
  "(" @indent.begin
  ")" @indent.end)

; Closing brackets and braces should dedent
"}" @indent.branch
"]" @indent.branch
")" @indent.branch
