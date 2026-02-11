let key_table = Hashtbl.create 16
let has_key s = Hashtbl.mem key_table s
let set_key s = Hashtbl.replace key_table s ()
let unset_key s = Hashtbl.remove key_table s

let key_pressed_table = Hashtbl.create 16
let was_key_just_pressed s = 
  if Hashtbl.mem key_pressed_table s then begin
    Hashtbl.remove key_pressed_table s;
    true
  end else
    false

let action_table : (string, unit -> unit) Hashtbl.t = Hashtbl.create 16
let register key action = Hashtbl.replace action_table key action

let global_cache : Global.t option ref = ref None
let player_cache : Component_defs.player option ref = ref None

let get_global_cached () =
  match !global_cache with
  | Some g -> g
  | None ->
      let g = Global.get () in
      global_cache := Some g;
      g

let get_player_cached () =
  match !player_cache with
  | Some p -> p
  | None ->
      let p = Player.player () in
      player_cache := Some p;
      p

let invalidate_caches () =
  global_cache := None;
  player_cache := None;
  Hashtbl.clear key_table;
  Hashtbl.clear key_pressed_table

let distance_sq v1 v2 =
  let dx = Vector.sub v1 v2 in
  Vector.dot dx dx

let mouse_clicked = ref false

let convert_mouse_coords x y =
  let global = get_global_cached () in
  let window_width, window_height = Gfx.get_window_size global.window in
  let logical_width, logical_height = Gfx.get_context_logical_size global.ctx in
  let scale_x = float_of_int logical_width /. float_of_int window_width in
  let scale_y = float_of_int logical_height /. float_of_int window_height in
  (int_of_float (float_of_int x *. scale_x), int_of_float (float_of_int y *. scale_y))

let handle_interaction () =
  let global = get_global_cached () in
  let dialogue_state = global.dialogue_state in
  let tutorial_state = global.tutorial_state in
  let player = get_player_cached () in
  let player_pos = player#position#get in
  let player_box = player#box#get in

  if dialogue_state.active then begin
    let was_finished = Dialogue.is_finished dialogue_state in
    Dialogue.next_line dialogue_state;
    if was_finished then begin
      let current_scene = Scene.current () in
      if current_scene = Scene.House then
        Tutorial.show_message tutorial_state "move"
    end
  end
  else match Interaction.find_npc_at player_pos player_box with
  | Some npc ->
      let data = npc#npc_data#get in
      Dialogue.start_dialogue dialogue_state data.Component_defs.dialogue;
      Tutorial.complete_message tutorial_state "interact"
  | None ->
      match Interaction.find_sign_at player_pos player_box with
      | Some sign ->
          let data = sign#sign_data#get in
          let sign_dialogue = Dialogue.create_dialogue [
            { speaker = data.Component_defs.title; text = data.Component_defs.text }
          ] in
          Dialogue.start_dialogue dialogue_state sign_dialogue;
          Tutorial.complete_message tutorial_state "interact"
      | None ->
          ()

let rec handle_input () =
  let () =
    match Gfx.poll_event () with
      KeyDown s -> 
        if not (has_key s) then
          Hashtbl.replace key_pressed_table s ();
        set_key s; 
        handle_input ()
    | KeyUp s -> unset_key s; handle_input ()
    | MouseMove (x, y) ->
        let global = get_global_cached () in
        (match global.menu_state with
         | Some menu ->
             let x', y' = convert_mouse_coords x y in
             Menu.set_mouse_position menu x' y';
             Menu.update_hover menu
         | None -> ());
        handle_input ()
    | MouseButton (_, pressed, x, y) -> 
        if pressed then begin
          let global = get_global_cached () in
          match global.menu_state with
          | Some menu ->
              let x', y' = convert_mouse_coords x y in
              Menu.set_mouse_position menu x' y';
              Menu.update_hover menu;
              Menu.click_button menu
          | None -> ()
        end;
        handle_input ()
    | Quit -> exit 0
    | _ -> ()
  in
  Hashtbl.iter (fun key action ->
    if has_key key then action ()) action_table

let () =
  register "z" (fun () -> 
    let player = get_player_cached () in
    let global = get_global_cached () in
    if not global.dialogue_state.active && global.menu_state = None then begin
      Player.move_player player Cst.player_v_up;
      Tutorial.complete_message global.tutorial_state "move"
    end
  );
  register "s" (fun () -> 
    let player = get_player_cached () in
    let global = get_global_cached () in
    if not global.dialogue_state.active && global.menu_state = None then begin
      Player.move_player player Cst.player_v_down;
      Tutorial.complete_message global.tutorial_state "move"
    end
  );
  register "d" (fun () -> 
    let player = get_player_cached () in
    let global = get_global_cached () in
    if not global.dialogue_state.active && global.menu_state = None then begin
      Player.move_player player Cst.player_v_right;
      Tutorial.complete_message global.tutorial_state "move"
    end
  );
  register "q" (fun () -> 
    let player = get_player_cached () in
    let global = get_global_cached () in
    if not global.dialogue_state.active && global.menu_state = None then begin
      Player.move_player player Cst.player_v_left;
      Tutorial.complete_message global.tutorial_state "move"
    end
  );
  
  register "escape" (fun () ->
    let global = get_global_cached () in
    Tutorial.complete_message global.tutorial_state "menu"
  );
  
  register "space" (fun () ->
    if was_key_just_pressed "space" then
      handle_interaction ()
  )