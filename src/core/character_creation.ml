type gender = Male | Female

type state = {
  mutable gender : gender option;
  mutable name : string;
  mutable input_focused : bool;
  max_name_length : int;
}

let create () = {
  gender = None;
  name = "";
  input_focused = false;
  max_name_length = 10;
}

let set_gender state gender =
  state.gender <- Some gender

let add_character state ch =
  if String.length state.name < state.max_name_length then
    state.name <- state.name ^ String.make 1 ch

let remove_character state =
  if String.length state.name > 0 then
    state.name <- String.sub state.name 0 (String.length state.name - 1)

let clear_name state =
  state.name <- ""

let get_gender state =
  state.gender

let get_name state =
  state.name

let is_complete state =
  match state.gender with
  | None -> false
  | Some _ -> String.length state.name > 0

let set_input_focused state focused =
  state.input_focused <- focused
