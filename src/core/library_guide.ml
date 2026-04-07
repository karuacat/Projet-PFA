type page =
  | Summary
  | Types

type action =
  | No_action
  | Open_types
  | Go_summary
  | Start_training
  | Close_panel

type state = {
  mutable active : bool;
  mutable page : page;
  mutable intro_shown : bool;
  mutable training_index : int;
  training_done : bool array;
  mutable current_training : int option;
}

let create_state () = {
  active = false;
  page = Summary;
  intro_shown = false;
  training_index = 0;
  training_done = [| false; false; false |];
  current_training = None;
}

let open_panel state =
  state.active <- true;
  state.page <- Summary

let close_panel state =
  state.active <- false

let mark_intro_shown state =
  state.intro_shown <- true

let should_show_intro state =
  not state.intro_shown

let panel_rect () =
  (70, 58, Cst.window_width - 140, Cst.window_height - 116)

let close_rect () =
  let px, py, pw, _ = panel_rect () in
  (px + pw - 44, py + 12, 30, 30)

let summary_types_rect () =
  let px, py, pw, _ = panel_rect () in
  let left_page_w = (pw / 2) - 70 in
  (px + 20, py + 68, left_page_w, 44)

let summary_locked_1_rect () =
  let px, py, pw, _ = panel_rect () in
  let left_page_w = (pw / 2) - 70 in
  (px + 20, py + 114, left_page_w, 44)

let summary_locked_2_rect () =
  let px, py, pw, _ = panel_rect () in
  let left_page_w = (pw / 2) - 70 in
  (px + 20, py + 160, left_page_w, 44)

let back_rect () =
  let px, py, _, ph = panel_rect () in
  (px + 38, py + ph - 54, 190, 34)

let train_rect () =
  let px, py, pw, ph = panel_rect () in
  (px + pw - 238, py + ph - 54, 200, 34)

let inside x y (rx, ry, rw, rh) =
  x >= rx && x <= rx + rw && y >= ry && y <= ry + rh

let click state x y =
  if not state.active then
    No_action
  else if inside x y (close_rect ()) then (
    close_panel state;
    Close_panel
  ) else
    match state.page with
    | Summary ->
        if inside x y (summary_types_rect ()) then (
          state.page <- Types;
          Open_types
        ) else
          No_action
    | Types ->
        if inside x y (back_rect ()) then (
          state.page <- Summary;
          Go_summary
        ) else if inside x y (train_rect ()) then
          Start_training
        else
          No_action

let next_training_challenge state =
  let idx = state.training_index mod 3 in
  state.training_index <- state.training_index + 1;
  state.current_training <- Some (idx + 1);
  match idx with
  | 0 -> Code_challenge.TypeExercise 1
  | 1 -> Code_challenge.TypeExercise 2
  | _ -> Code_challenge.TypeExercise 3

let mark_training_success state =
  match state.current_training with
  | Some n when n >= 1 && n <= 3 ->
      state.training_done.(n - 1) <- true;
      state.current_training <- None
  | _ ->
      state.current_training <- None

let training_progress state =
  Array.fold_left (fun acc done_ -> if done_ then acc + 1 else acc) 0 state.training_done

let training_progress_text state =
  Printf.sprintf "%d/3" (training_progress state)

let training_complete state =
  training_progress state = 3

let reset_training_progress state =
  for i = 0 to Array.length state.training_done - 1 do
    state.training_done.(i) <- false
  done;
  state.training_index <- 0;
  state.current_training <- None

let training_button_label state =
  if training_complete state then "Recommencer" else "S'entrainer"
