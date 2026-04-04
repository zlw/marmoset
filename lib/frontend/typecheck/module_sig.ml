type definition_site = {
  file_path : string;
  start_pos : int;
  end_pos : int;
}

type member_binding = {
  internal_name : string;
  value_type : Types.poly_type option;
  value_definition : definition_site option;
  enum_def : Enum_registry.enum_def option;
  enum_definition : definition_site option;
  named_type_def : Type_registry.named_type_def option;
  named_type_definition : definition_site option;
  transparent_type : Annotation.type_alias_info option;
  transparent_type_definition : definition_site option;
  shape_def : Type_registry.shape_def option;
  shape_definition : definition_site option;
  trait_def : Trait_registry.trait_def option;
  trait_definition : definition_site option;
}

type type_impl_entry = {
  for_type : Types.mono_type;
  methods : Trait_registry.method_sig list;
}

type module_signature = {
  module_id : string;
  exports : (string, member_binding) Hashtbl.t;
  trait_impls : Trait_registry.impl_def list;
  type_impls : type_impl_entry list;
}

type module_locals = {
  enums : Enum_registry.enum_def list;
  named_types : Type_registry.named_type_def list;
  transparent_types : (string * Annotation.type_alias_info) list;
  shapes : Type_registry.shape_def list;
  traits : Trait_registry.trait_def list;
  trait_impls : Trait_registry.impl_def list;
  type_impls : type_impl_entry list;
}

let empty_signature ~(module_id : string) () =
  { module_id; exports = Hashtbl.create 16; trait_impls = []; type_impls = [] }

let find_export (signature : module_signature) (name : string) : member_binding option =
  Hashtbl.find_opt signature.exports name
