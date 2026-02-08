let key_table = Hashtbl.create 16
let has_key s = Hashtbl.mem key_table s
let set_key s = Hashtbl.replace key_table s ()
let unset_key s = Hashtbl.remove key_table s

let action_table = Hashtbl.create 16
let register key action = Hashtbl.replace action_table key action

let mouse_clicked = ref false

let cached_global = ref None

let get_global () =
  match !cached_global with
  | Some g -> g
  | None ->
      let g = Global.get () in
      cached_global := Some g;
      g

let invalidate_global_cache () =
  cached_global := None

let handle_interaction () =
  let global = get_global () in
  let dialogue_state = global.dialogue_state in
  let tutorial_state = global.tutorial_state in
  let player_pos = Player.(player())#position#get in

  if dialogue_state.active then begin
    if Dialogue.is_finished dialogue_state then begin
      Dialogue.close_dialogue dialogue_state;
      let current_scene = Scene.current () in
      if current_scene = Scene.House then
        Tutorial.show_message tutorial_state "move"
    end else
      Dialogue.next_line dialogue_state
  end
  else match Interaction.find_npc_at player_pos 50 with
  | Some npc ->
      let data = npc#npc_data#get in
      Dialogue.start_dialogue dialogue_state data.Component_defs.dialogue;
      Tutorial.complete_message tutorial_state "interact_npc"
  | None ->
      match Interaction.find_sign_at player_pos 50 with
      | Some sign ->
          let data = sign#sign_data#get in
          let sign_dialogue = Dialogue.create_dialogue [
            { speaker = data.Component_defs.title; text = data.Component_defs.text }
          ] in
          Dialogue.start_dialogue dialogue_state sign_dialogue;
          Tutorial.complete_message tutorial_state "interact_sign"
      | None ->
          ()

let rec handle_input () =
  let () =
    match Gfx.poll_event () with
      KeyDown s -> set_key s; handle_input ()
    | KeyUp s -> unset_key s; handle_input ()
    | MouseButton (button, pressed, _x, _y) ->
        if button = 1 && pressed then
          mouse_clicked := true;
        handle_input ()
    | Quit -> exit 0
    | _ -> ()
  in
  Hashtbl.iter (fun key action ->
    if has_key key then action ()) action_table;
  if !mouse_clicked then begin
    mouse_clicked := false;
    handle_interaction ()
  end
let cached_player = ref None

let get_player () =
  match !cached_player with
  | Some p -> p
  | None ->
      let p = Player.player () in
      cached_player := Some p;
      p

let () =
  register "z" (fun () -> 
    let player = get_player () in
    let global = get_global () in
    Player.move_player player Cst.player_v_up;
    Tutorial.complete_message global.tutorial_state "move"
  );
  register "s" (fun () -> 
    let player = get_player () in
    let global = get_global () in
    Player.move_player player Cst.player_v_down;
    Tutorial.complete_message global.tutorial_state "move"
  );
  register "d" (fun () -> 
    let player = get_player () in
    let global = get_global () in
    Player.move_player player Cst.player_v_right;
    Tutorial.complete_message global.tutorial_state "move"
  );
  register "q" (fun () -> 
    let player = get_player () in
    let global = get_global () in
    Player.move_player player Cst.player_v_left;
    Tutorial.complete_message global.tutorial_state "move"
  );
  
  register "escape" (fun () ->
    let global = get_global () in
    Tutorial.complete_message global.tutorial_state "menu"
  );
  
  register "space" (fun () ->
    handle_interaction ()
  )