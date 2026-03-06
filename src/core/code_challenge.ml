type challenge_type = 
  | BoolVariable of string * bool
  | StringVariable of string * string
  | TypeDefinition

type state = {
  mutable active : bool;
  mutable challenge : challenge_type option;
  mutable code : string;
  mutable cursor_pos : int;
  mutable on_success : (unit -> unit) option;
  mutable on_failure : (unit -> unit) option;
}

let create_state () = {
  active = false;
  challenge = None;
  code = "";
  cursor_pos = 0;
  on_success = None;
  on_failure = None;
}

let start_challenge state challenge_type on_success on_failure =
  state.active <- true;
  state.challenge <- Some challenge_type;
  state.code <- "";
  state.cursor_pos <- 0;
  state.on_success <- Some on_success;
  state.on_failure <- Some on_failure

let add_char state c =
  if state.active && String.length state.code < 100 then begin
    let before = String.sub state.code 0 state.cursor_pos in
    let after = String.sub state.code state.cursor_pos (String.length state.code - state.cursor_pos) in
    state.code <- before ^ (String.make 1 c) ^ after;
    state.cursor_pos <- state.cursor_pos + 1
  end

let remove_char state =
  if state.active && state.cursor_pos > 0 then begin
    let before = String.sub state.code 0 (state.cursor_pos - 1) in
    let after = String.sub state.code state.cursor_pos (String.length state.code - state.cursor_pos) in
    state.code <- before ^ after;
    state.cursor_pos <- state.cursor_pos - 1
  end

let validate_bool_variable code expected_name expected_value =
  let trimmed = String.trim code in
  let pattern =
    Str.regexp
      (Printf.sprintf
         "^let[ \t]+%s[ \t]*=[ \t]*\\(true\\|false\\)[ \t]*;;?$"
         (Str.quote expected_name))
  in
  if Str.string_match pattern trimmed 0 then
    let found = Str.matched_group 1 trimmed in
    Bool.equal (String.equal found "true") expected_value
  else
    false

let validate_string_variable code expected_name expected_value =
  let trimmed = String.trim code in
  let pattern =
    Str.regexp
      (Printf.sprintf
         "^let[ \t]+%s[ \t]*=[ \t]*\"\\(.*\\)\"[ \t]*;;?$"
         (Str.quote expected_name))
  in
  if Str.string_match pattern trimmed 0 then
    let found = Str.matched_group 1 trimmed in
    String.equal found expected_value
  else
    false

let validate_type_definition code =
  let trimmed = String.trim code in
  let normalize s =
    String.trim (Str.global_replace (Str.regexp "[ \n\t]+") " " s)
  in
  let normalized = normalize trimmed in
  String.length normalized > 0 && 
  (String.contains normalized '=' || String.contains normalized '{') &&
  (String.length normalized > 10)

let validate_code state =
  match state.challenge with
  | None -> false
    | Some (BoolVariable (expected_name, expected_value)) ->
      validate_bool_variable state.code expected_name expected_value
    | Some (StringVariable (expected_name, expected_value)) ->
      validate_string_variable state.code expected_name expected_value
  | Some TypeDefinition ->
      validate_type_definition state.code

let submit_code state =
  if state.active then begin
    let success = validate_code state in
    if success then begin
      state.active <- false;
      match state.on_success with
      | Some callback -> callback ()
      | None -> ()
    end else begin
      match state.on_failure with
      | Some callback -> callback ()
      | None -> ()
    end
  end

let close_challenge state =
  state.active <- false;
  state.code <- "";
  state.cursor_pos <- 0

let get_prompt state =
  match state.challenge with
  | None -> "Aucun défi"
  | Some (BoolVariable (name, value)) ->
      Printf.sprintf "Déclare une variable booléenne: let %s = %b;;" name value
  | Some (StringVariable (name, value)) ->
    Printf.sprintf "Quel est ton nom ?"
  | Some TypeDefinition -> "Définis un type eleve avec ses propriétés:"
