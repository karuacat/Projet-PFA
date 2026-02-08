let last_scene = ref Scene.House

let town_npc_tutorial_shown = ref false

let check_scene_tutorials () =
  let current_scene = Scene.current () in
  
  if !last_scene <> current_scene then begin
    last_scene := current_scene;
    
    match current_scene with
    | Scene.House ->
        ()
    | Scene.Town ->
        if not !town_npc_tutorial_shown then begin
          let global = Global.get () in
          let tutorial_state = global.tutorial_state in
          Tutorial.show_message tutorial_state "interact_npc";
          town_npc_tutorial_shown := true
        end
  end

let show_next_tutorial_town () =
  let global = Global.get () in
  let tutorial_state = global.tutorial_state in
  
  try
    let npc_msg = Hashtbl.find tutorial_state.Tutorial.messages "interact_npc" in
    if npc_msg.shown && not (Tutorial.is_active tutorial_state) then begin
      let sign_msg = Hashtbl.find tutorial_state.Tutorial.messages "interact_sign" in
      if not sign_msg.shown && not sign_msg.active then
        Tutorial.show_message tutorial_state "interact_sign"
    end
  with Not_found -> ()
