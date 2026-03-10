; Marmoset textobjects for nvim-treesitter-textobjects

; ── Functions ─────────────────────────────────────────────────────

; Function literal: outer is the whole fn expression, inner is the body block
(fn_declaration) @function.outer

(fn_declaration
  body: (block) @function.inner)

(fn_declaration
  body: (expr_or_block
    (block) @function.inner))

(function_literal) @function.outer

(function_literal
  body: (block) @function.inner)

(lambda_expression) @function.outer

(lambda_expression
  body: (block) @function.inner)

(lambda_expression
  body: (expr_or_block
    (block) @function.inner))

; Method definitions in impl blocks
(method_definition) @function.outer

(method_definition
  body: (block) @function.inner)

(method_definition
  body: (expr_or_block
    (block) @function.inner))

; Trait method signatures and default methods
(trait_method_signature) @function.outer

(trait_method_signature
  body: (block) @function.inner)

(trait_method_signature
  body: (expr_or_block
    (block) @function.inner))

; ── Blocks ────────────────────────────────────────────────────────

(block) @block.outer

; Inner block: the statements inside the braces
(block
  "{" @_start
  "}" @_end) @block.inner

; ── Parameters ────────────────────────────────────────────────────

(parameter) @parameter.inner
(parameter) @parameter.outer

; ── Comments ──────────────────────────────────────────────────────

(comment) @comment.outer

; ── Conditionals ──────────────────────────────────────────────────

(if_expression) @conditional.outer

(if_expression
  consequence: (_) @conditional.inner)

; ── Match / switch ────────────────────────────────────────────────

(match_expression) @conditional.outer

(match_arm) @conditional.inner

; ── Loops / iteration (for future loop support) ───────────────────

; ── Classes / types ───────────────────────────────────────────────

(enum_definition) @class.outer

(enum_definition
  "{" @_start
  "}" @_end) @class.inner

(trait_definition) @class.outer

(trait_definition
  "{" @_start
  "}" @_end) @class.inner

(impl_block) @class.outer

(impl_block
  "{" @_start
  "}" @_end) @class.inner

; ── Calls ─────────────────────────────────────────────────────────

(call_expression) @call.outer

(call_expression
  "(" @_start
  ")" @_end) @call.inner
