(fn_declaration
  "fn" @context
  name: (identifier) @name) @item

(let_statement
  "let" @context
  name: (identifier) @name) @item

(enum_definition
  "enum" @context
  name: (identifier) @name) @item

(shape_definition
  "shape" @context
  name: (identifier) @name) @item

(trait_definition
  "trait" @context
  name: (identifier) @name) @item

(impl_block
  "impl" @context
  target: (_) @name) @item

(method_definition
  "fn" @context
  name: (identifier) @name) @item

(type_definition
  "type" @context
  name: (identifier) @name) @item
