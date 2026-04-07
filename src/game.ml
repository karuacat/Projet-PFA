open System_defs
open Component_defs
open Ecs

let global_cache = ref None

let update dt =
  let global = match !global_cache with
    | Some g -> g
    | None ->
        let g = Global.get () in
        global_cache := Some g;
        g
  in
  
  match global.menu_state with
  | Some menu ->
      let () = Input.handle_input () in
      Menu_system.update () |> ignore;
      Menu_system.draw () |> ignore;
      Gfx.commit global.ctx;
      None
  | None ->
      match global.character_creation_state with
      | Some char_state ->
          let () = Input.handle_input () in
          Character_creation_system.update () |> ignore;
          Character_creation_system.draw () |> ignore;
          Gfx.commit global.ctx;
          None
      | None ->
          let () = Player.stop_player () in
          let () = Input.handle_input () in
          let () = Story_events_system.update () in
          
          let () = Tutorial_manager.check_scene_tutorials () in
          
          Move_system.update dt;
          Collision_system.update dt;
          Door_transition_system.update dt;
          Draw_system.update dt;
          Library_cinematic_system.update dt;
          Tutorial_system.update dt;
          Dialogue_system.update dt;
          Code_challenge_system.update dt Seq.empty;
          Library_guide_system.update dt Seq.empty;
          
          Gfx.commit global.ctx;
          None

let run () =
  let window_spec =
    Format.sprintf "OCaml Quest:%dx%d:"
      Cst.window_width Cst.window_height
  in
  let window = Gfx.create window_spec in
  let ctx = Gfx.get_context window in

  let font = Gfx.load_font "ressources/fonts/PressStart2P.ttf" "" 16 in
  World_bootstrap.init_world ctx;
  
  let player = Player.players () in
  let dialogue_state = Dialogue.create_state () in
  let tutorial_state = Tutorial.create_state () in
  let code_challenge_state = Code_challenge.create_state () in
  let library_guide_state = Library_guide.create_state () in
  
  Tutorial.register_message tutorial_state "move" "Utiliser \"ZQSD\"\npour se déplacer";
  Tutorial.register_message tutorial_state "menu" "Utiliser \"Echap\"\npour ouvrir le menu";
  Tutorial.register_message tutorial_state "interact" "Utiliser \"ESPACE\"\npour intéragir";
  Tutorial.register_message tutorial_state "town_explore" "Explore les rues d'OCamlon\net parle aux habitants";
  Tutorial.register_message tutorial_state "town_signs" "Lis les panneaux pour\ntrouver l'Académie";
  Tutorial.register_message tutorial_state "find_library" "Trouver la bibliothèque";
  
  let menu = Menu.create () in
  let global = Global.{ window; ctx; player; waiting = 0; dialogue_state; tutorial_state; font; menu_state = Some menu; character_creation_state = None; on_character_complete = None; on_escape_pressed = None; player_name = "Apprenti"; code_challenge_state = Some code_challenge_state; house_exit_attempted = false; has_secret_book = false; chest_challenge_completed = false; knight_challenge_completed = false; school_students_event_completed = false; classroom_intro_completed = false; lambda_duel_started = false; lambda_duel_stage = 0; lambda_golem_hp = 20; lambda_golem_hp_visible = false; lambda_duel_completed = false; dynamic_magic_cinematic_done = false; dynamic_magic_cinematic_active = false; dynamic_magic_phase = 0; dynamic_magic_timer = 0.0; dynamic_magic_pending_target_scene = None; dynamic_magic_spawn_x = 0; dynamic_magic_spawn_y = 0; library_guide_state; library_intro_seen = false } in
  Global.set global;
  
  let rec start_new_game () =
    let char_creation = Character_creation.create () in
    ignore (Savegame.delete ());
    global.house_exit_attempted <- false;
    global.has_secret_book <- false;
    global.chest_challenge_completed <- false;
    global.knight_challenge_completed <- false;
    global.school_students_event_completed <- false;
    global.classroom_intro_completed <- false;
    global.lambda_duel_started <- false;
    global.lambda_duel_stage <- 0;
    global.lambda_golem_hp <- 20;
    global.lambda_golem_hp_visible <- false;
    global.lambda_duel_completed <- false;
    global.dynamic_magic_cinematic_done <- false;
    global.dynamic_magic_cinematic_active <- false;
    global.dynamic_magic_phase <- 0;
    global.dynamic_magic_timer <- 0.0;
    global.dynamic_magic_pending_target_scene <- None;
    global.dynamic_magic_spawn_x <- 0;
    global.dynamic_magic_spawn_y <- 0;
    Library_guide.close_panel global.library_guide_state;
    global.library_intro_seen <- false;
    Story_events_system.reset_for_new_game ();
    global.player_name <- "Apprenti";
    global.player#position#set Vector.{ x = float Cst.player_start_x; y = float Cst.player_start_y };
    Player.set_skin_tag "male";
    Player.refresh_player_sprite global.player;
    global.menu_state <- None;
    global.character_creation_state <- Some char_creation;
    global.on_character_complete <- Some finish_character_creation;
    Scene.set_scene Scene.CharacterCreation;
    Input.invalidate_caches ();
  
  and finish_character_creation () =
    (match global.character_creation_state with
     | Some char_state ->
       global.player_name <- Character_creation.get_name char_state;
         (match Character_creation.get_gender char_state with
          | Some gender -> Player.set_skin_from_gender gender
          | None -> ())
     | None -> ());
    Player.refresh_player_sprite global.player;
    Scene.set_scene Scene.House;
    global.character_creation_state <- None;
    global.on_character_complete <- None;
    Input.invalidate_caches ();
    Dialogue.start_dialogue dialogue_state Dialogue.intro_wake_up;
    Tutorial.show_message tutorial_state "move";
  
  and start_continue () =
    (match Savegame.load () with
     | Some payload ->
         Savegame.apply_to_global global payload;
         global.menu_state <- None
     | None ->
       Scene.set_scene Scene.Menu;
       setup_main_menu ();
       Menu.set_info_message menu "Aucune sauvegarde disponible";
       global.menu_state <- Some menu);
    Input.invalidate_caches ();

  and save_game () =
    let success = Savegame.save global in
    if success then
      Menu.set_info_message menu "Partie sauvegardee"
    else
      Menu.set_info_message menu "Echec de sauvegarde"

  and close_pause_menu () =
    global.menu_state <- None;
    Input.invalidate_caches ()

  and open_pause_menu () =
    Menu.setup_paused_menu menu close_pause_menu save_game open_pause_options return_to_main_menu (fun () -> exit 0);
    global.menu_state <- Some menu;
    Input.invalidate_caches ()

  and open_pause_options () =
    Menu.setup_options_menu menu (fun () -> open_pause_menu ());
    global.menu_state <- Some menu;
    Input.invalidate_caches ()

  and return_to_main_menu () =
    Scene.set_scene Scene.Menu;
    global.character_creation_state <- None;
    global.on_character_complete <- None;
    setup_main_menu ();
    global.menu_state <- Some menu;
    Input.invalidate_caches ()
  
  and open_options () =
    Menu.setup_options_menu menu (fun () ->
      setup_main_menu ()
    )
  
  and setup_main_menu () =
    Menu.setup_main_menu menu start_new_game start_continue open_options (fun () -> exit 0);
    Menu.set_button_enabled menu "continue" (Savegame.has_save ())
  in

  global.on_escape_pressed <- Some (fun () ->
    if Scene.current () = Scene.Menu then
      ()
    else
      match global.menu_state with
      | Some current_menu ->
          (match current_menu.Menu.state with
           | Menu.Paused -> close_pause_menu ()
           | Menu.Options -> open_pause_menu ()
           | Menu.MainMenu -> ())
      | None ->
          if global.character_creation_state = None then
            open_pause_menu ()
  );
  
  setup_main_menu ();
  
  Gfx.main_loop update (fun () -> ())