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
    | Scene.School -> ()
    | Scene.Library ->
        let global = Global.get () in
        Tutorial.complete_message global.tutorial_state "find_library";
        if not global.library_intro_seen then begin
          global.library_intro_seen <- true;
          if Library_guide.should_show_intro global.library_guide_state then begin
            Library_guide.mark_intro_shown global.library_guide_state;
            if not global.dialogue_state.active then
              Dialogue.start_dialogue global.dialogue_state
                (Dialogue.create_dialogue [
                  {
                    Component_defs.speaker = "Moi";
                    text = "Ce livre a l'air etrange et si j'allais le voir ?";
                  };
                ])
          end
        end
    | Scene.Classroom -> ()
  end
