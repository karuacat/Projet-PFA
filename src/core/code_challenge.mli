type challenge_type = 
  | StringVariable of string

type state = {
  mutable active : bool;
  mutable challenge : challenge_type option;
  mutable code : string;
  mutable cursor_pos : int;
  mutable on_success : (unit -> unit) option;
  mutable on_failure : (unit -> unit) option;
}

val create_state : unit -> state

val start_challenge : state -> challenge_type -> (unit -> unit) -> (unit -> unit) -> unit

val add_char : state -> char -> unit

val remove_char : state -> unit

val validate_code : state -> bool

val submit_code : state -> unit

val close_challenge : state -> unit

val get_prompt : state -> string
