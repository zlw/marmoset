type severity =
  | Error
  | Warning
  | Info

type span =
  | NoSpan
  | Span of {
      file_id : string;
      start_pos : int;
      end_pos : int option;
    }

type label = {
  span : span;
  message : string option;
  primary : bool;
}

type t = {
  code : string;
  severity : severity;
  message : string;
  labels : label list;
  notes : string list;
}

val severity_to_string : severity -> string

val make :
  code:string -> severity:severity -> message:string -> ?labels:label list -> ?notes:string list -> unit -> t

val primary_label : ?message:string -> span -> label
val secondary_label : ?message:string -> span -> label
val error_no_span : code:string -> message:string -> t

val error_with_span :
  code:string -> message:string -> file_id:string -> start_pos:int -> ?end_pos:int -> unit -> t

val with_note : t -> string -> t
val with_secondary_label : t -> label -> t
val pick_primary_span : label list -> span option
val render_cli : source_lookup:(string -> string option) -> t -> string
val render_many_cli : source_lookup:(string -> string option) -> t list -> string
