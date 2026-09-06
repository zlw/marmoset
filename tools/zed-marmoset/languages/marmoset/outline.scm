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

(extern_type_definition
  "extern" @context
  name: (identifier) @name) @item

(extern_block
  "extern" @context
  alias: (identifier) @name) @item

(extern_fn_signature
  "fn" @context
  name: (identifier) @name) @item
