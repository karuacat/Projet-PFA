type challenge_type = 
  | BoolVariable of string * bool
  | StringVariable of string * string
  | PowerCalculation
  | TypeDefinition

type failure_reason =
  | SyntaxError
  | TypeError
  | GenericError

type state = {
  mutable active : bool;
  mutable challenge : challenge_type option;
  mutable code : string;
  mutable cursor_pos : int;
  mutable last_failure_reason : failure_reason option;
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

val get_last_failure_reason : state -> failure_reason option
