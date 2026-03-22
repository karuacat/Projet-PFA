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

let create_state () = {
  active = false;
  challenge = None;
  code = "";
  cursor_pos = 0;
  last_failure_reason = None;
  on_success = None;
  on_failure = None;
}

let start_challenge state challenge_type on_success on_failure =
  state.active <- true;
  state.challenge <- Some challenge_type;
  state.code <- "";
  state.cursor_pos <- 0;
  state.last_failure_reason <- None;
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

let validate_power_calculation code =
  let trimmed = String.trim code in
  let sum_pattern =
    Str.regexp "^let[ \t]+puissance[ \t]*=[ \t]*\\([0-9]+\\)[ \t]*\\+[ \t]*\\([0-9]+\\)[ \t]*;;?$"
  in
  let value_pattern =
    Str.regexp "^let[ \t]+puissance[ \t]*=[ \t]*\\([0-9]+\\)[ \t]*;;?$"
  in
  if Str.string_match sum_pattern trimmed 0 then
    let a = int_of_string (Str.matched_group 1 trimmed) in
    let b = int_of_string (Str.matched_group 2 trimmed) in
    a + b = 15
  else if Str.string_match value_pattern trimmed 0 then
    int_of_string (Str.matched_group 1 trimmed) = 15
  else
    false

let failure_reason_for_power code =
  let trimmed = String.trim code in
  let quoted_ten = Str.regexp "\"10\"" in
  let plus_number = Str.regexp "\\+[ \t]*[0-9]+" in
  if Str.string_match quoted_ten trimmed 0 || (try ignore (Str.search_forward quoted_ten trimmed 0); true with Not_found -> false) then
    TypeError
  else if (try ignore (Str.search_forward plus_number trimmed 0); true with Not_found -> false) then
    SyntaxError
  else
    GenericError

let validate_code state =
  match state.challenge with
  | None -> false
  | Some (BoolVariable (expected_name, expected_value)) ->
      validate_bool_variable state.code expected_name expected_value
  | Some (StringVariable (expected_name, expected_value)) ->
      validate_string_variable state.code expected_name expected_value
  | Some PowerCalculation ->
      validate_power_calculation state.code
  | Some TypeDefinition ->
      validate_type_definition state.code

let submit_code state =
  if state.active then begin
    let success = validate_code state in
    if success then begin
      state.last_failure_reason <- None;
      state.active <- false;
      match state.on_success with
      | Some callback -> callback ()
      | None -> ()
    end else begin
      let reason =
        match state.challenge with
        | Some PowerCalculation -> failure_reason_for_power state.code
        | _ -> GenericError
      in
      state.last_failure_reason <- Some reason;
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
  | Some PowerCalculation ->
      "Créer une variable puissance contenant la valeur 10 + 5."
  | Some TypeDefinition -> "Définis un type eleve avec ses propriétés:"

let get_last_failure_reason state = state.last_failure_reason
