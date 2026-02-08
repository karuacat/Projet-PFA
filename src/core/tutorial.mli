type tutorial_message = {
  id : string;
  text : string;
  mutable shown : bool;
  mutable active : bool;
}

type tutorial_state = {
  messages : (string, tutorial_message) Hashtbl.t;
  mutable current : tutorial_message option;
}

val create_state : unit -> tutorial_state

val register_message : tutorial_state -> string -> string -> unit

val show_message : tutorial_state -> string -> unit

val hide_current : tutorial_state -> unit

val complete_message : tutorial_state -> string -> unit

val is_active : tutorial_state -> bool

val current_message : tutorial_state -> string option

val reset_all : tutorial_state -> unit
