let last_scene = ref Scene.House

let town_interact_tutorial_shown = ref false

let check_scene_tutorials () =
  let current_scene = Scene.current () in
  
  if !last_scene <> current_scene then begin
    last_scene := current_scene;
    
    match current_scene with
    | Scene.Menu -> ()
    | Scene.CharacterCreation -> ()
    | Scene.House -> ()
    | Scene.Town ->
        if not !town_interact_tutorial_shown then begin
          let global = Global.get () in
          let tutorial_state = global.tutorial_state in
          Tutorial.show_message tutorial_state "interact";
          town_interact_tutorial_shown := true
        end
    | Scene.Academy -> ()
  end
