type challenge_type = 
  | BoolVariable of string * bool
  | StringVariable of string * string
  | PowerCalculation
  | GolemActivateHp
  | GolemDealDamage
  | TypeExercise of int
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

let move_cursor_left state =
  if state.active && state.cursor_pos > 0 then
    state.cursor_pos <- state.cursor_pos - 1

let move_cursor_right state =
  if state.active && state.cursor_pos < String.length state.code then
    state.cursor_pos <- state.cursor_pos + 1

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

let validate_type_exercise index code =
  let trimmed = String.trim code in
  match index with
  | 1 ->
      Str.string_match
        (Str.regexp "^let[ \t]+niveau[ \t]*=[ \t]*1[ \t]*;;?$")
        trimmed 0
  | 2 ->
      Str.string_match
        (Str.regexp "^let[ \t]+est_pret[ \t]*=[ \t]*true[ \t]*;;?$")
        trimmed 0
  | _ ->
      let pattern =
        Str.regexp "^let[ \t]+salle[ \t]*=[ \t]*\"\\([^\"]+\\)\"[ \t]*;;?$"
      in
      if Str.string_match pattern trimmed 0 then
        let value = String.lowercase_ascii (Str.matched_group 1 trimmed) in
        String.equal value "bibliotheque"
      else
        false

let type_exercise_hint index =
  match index with
  | 1 ->
    "Indice : le nom attendu est niveau. int sert pour un nombre sans guillemets."
  | 2 ->
    "Indice : le nom attendu est est_pret. bool sert pour vrai/faux sans guillemets."
  | _ ->
    "Indice : le nom attendu est salle. Ecris un texte entre guillemets, par ex. \"Bibliotheque\"."

let validate_golem_activate_hp code =
  let trimmed = String.trim code in
  let pattern =
    Str.regexp "^let[ \t]+pv[ \t]*=[ \t]*true[ \t]*;;?$"
  in
  Str.string_match pattern trimmed 0

let extract_golem_damage code =
  let trimmed = String.trim code in
  let patterns = [
    Str.regexp "^let[ \t]+d[eé]g[aâ]ts[ \t]*=[ \t]*\\([0-9]+\\)[ \t]*;;?$";
    Str.regexp "^let[ \t]+degats[ \t]*=[ \t]*\\([0-9]+\\)[ \t]*;;?$"
  ] in
  let rec find_match = function
    | [] -> None
    | p :: rest ->
        if Str.string_match p trimmed 0 then
          Some (int_of_string (Str.matched_group 1 trimmed))
        else
          find_match rest
  in
  find_match patterns

let validate_golem_deal_damage code =
  match extract_golem_damage code with
  | Some dmg -> dmg > 0
  | None -> false

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
    | Some GolemActivateHp ->
      validate_golem_activate_hp state.code
    | Some GolemDealDamage ->
      validate_golem_deal_damage state.code
  | Some (TypeExercise idx) ->
      validate_type_exercise idx state.code
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
        | Some GolemActivateHp
        | Some GolemDealDamage -> GenericError
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
    | Some GolemActivateHp ->
      "Active les points de vie du golem: let pv = true;;"
    | Some GolemDealDamage ->
        "Inflige des degats au golem: let degats = <valeur>;;"
    | Some (TypeExercise 1) ->
      "Enigme 1: Tu commences ton aventure a la premiere marche du parcours. Donne cette valeur a niveau."
    | Some (TypeExercise 2) ->
      "Enigme 2: Tu es devant le livre, pret a etudier. Traduis cet etat dans la variable est_pret."
    | Some (TypeExercise _) ->
      "Enigme 3: Associer a la variable salle le nom de la salle dans laquelle tu es."
  | Some TypeDefinition -> "Définis un type eleve avec ses propriétés:"

let get_last_failure_reason state = state.last_failure_reason

let get_type_exercise_hint state =
  match state.challenge with
  | Some (TypeExercise idx) -> Some (type_exercise_hint idx)
  | _ -> None
