; Marmoset fold queries for Neovim

; Blocks (general)
(block) @fold

; Top-level fn declarations
(fn_declaration) @fold

(fn_declaration
  body: (expr_or_block
    (block) @fold))

; Enum definitions
(enum_definition) @fold

; Trait definitions
(trait_definition) @fold

; Impl blocks
(impl_block) @fold

; Method definitions
(method_definition) @fold

(method_definition
  body: (expr_or_block
    (block) @fold))

; Match expressions
(match_expression) @fold

; If expressions with block bodies
(if_expression
  consequence: (block) @fold)

(if_expression
  alternative: (block) @fold)

(if_expression
  consequence: (expr_or_block
    (block) @fold))

(if_expression
  alternative: (expr_or_block
    (block) @fold))

; Function literals
(function_literal) @fold

; Lambda expressions with block bodies
(lambda_expression
  body: (block) @fold)

(lambda_expression
  body: (expr_or_block
    (block) @fold))

; Trait default methods
(trait_method_signature
  body: (expr_or_block
    (block) @fold))

; Array literals (multi-line)
(array_literal) @fold

; Object/record literals (multi-line)
(object_literal) @fold

; Record types
(record_type) @fold
