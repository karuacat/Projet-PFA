open System_defs
open Component_defs
open Ecs

let global_cache = ref None
let knight_guardian_ref : Component_defs.npc_entity option ref = ref None
let merchant_ref : Component_defs.npc_entity option ref = ref None
let scholar_ref : Component_defs.npc_entity option ref = ref None
let knight_patrol_left_x = ref 0.0
let knight_patrol_right_x = ref 0.0
let knight_patrol_dir = ref 1.0
let knight_walk_tick = ref 0
let knight_moved_aside = ref false
let knight_side_x = ref 0.0
let knight_side_y = ref 0.0
let school_student_a_ref : Component_defs.npc_entity option ref = ref None
let school_student_b_ref : Component_defs.npc_entity option ref = ref None
let school_messenger_ref : Component_defs.npc_entity option ref = ref None
let school_intro_started = ref false
let school_intro_done = ref false
let school_students_running = ref false
let school_messenger_spoke = ref false
let school_student_a_stage = ref 0
let school_student_b_stage = ref 0
let school_messenger_stage = ref 0
let school_walk_tick = ref 0

let sprite_row_down = 0
let sprite_row_left = 1
let sprite_row_right = 2
let sprite_row_up = 3

let set_npc_sprite_row npc row =
  match npc#texture#get with
  | Texture.Sprite (img, sx, _, frame_w, frame_h) ->
      let col = if frame_w > 0 then sx / frame_w else 0 in
      let block_start = (col / 3) * 3 in
      npc#texture#set (Texture.Sprite (img, (block_start + 1) * frame_w, row * frame_h, frame_w, frame_h))
  | _ -> ()

let set_npc_walk_sprite npc row phase =
  match npc#texture#get with
  | Texture.Sprite (img, sx, _, frame_w, frame_h) ->
      let col = if frame_w > 0 then sx / frame_w else 0 in
      let block_start = (col / 3) * 3 in
      let walk_col = block_start + (phase mod 3) in
      npc#texture#set (Texture.Sprite (img, walk_col * frame_w, row * frame_h, frame_w, frame_h))
  | _ -> ()

let facing_row_toward_player npc_pos player_pos =
  let dx = player_pos.Vector.x -. npc_pos.Vector.x in
  let dy = player_pos.Vector.y -. npc_pos.Vector.y in
  if abs_float dx > abs_float dy then
    if dx >= 0.0 then sprite_row_right else sprite_row_left
  else if dy >= 0.0 then
    sprite_row_down
  else
    sprite_row_up

let distance_sq v1 v2 =
  let dx = v1.Vector.x -. v2.Vector.x in
  let dy = v1.Vector.y -. v2.Vector.y in
  (dx *. dx) +. (dy *. dy)

let move_npc_toward npc target_x target_y speed =
  let pos = npc#position#get in
  let dx = target_x -. pos.Vector.x in
  let dy = target_y -. pos.Vector.y in
  let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
  if dist <= speed || dist = 0.0 then (
    npc#position#set Vector.{x = target_x; y = target_y};
    true
  ) else (
    npc#position#set Vector.{
      x = pos.Vector.x +. (dx /. dist *. speed);
      y = pos.Vector.y +. (dy /. dist *. speed);
    };
    false
  )

let move_npc_toward_animated npc target_x target_y speed =
  let pos = npc#position#get in
  let dx = target_x -. pos.Vector.x in
  let dy = target_y -. pos.Vector.y in
  let row =
    if abs_float dx > abs_float dy then
      if dx >= 0.0 then sprite_row_right else sprite_row_left
    else if dy >= 0.0 then sprite_row_down else sprite_row_up
  in
  incr school_walk_tick;
  let phase =
    if !school_walk_tick mod 18 < 6 then 0
    else if !school_walk_tick mod 18 < 12 then 1
    else 2
  in
  let reached = move_npc_toward npc target_x target_y speed in
  if reached then set_npc_sprite_row npc row
  else set_npc_walk_sprite npc row phase;
  reached

let update_school_intro_event () =
  let global = Global.get () in
  let player_pos = global.player#position#get in
  if Scene.current () <> Scene.School then
    ()
  else begin
    if not !school_intro_started then begin
      school_intro_started := true;
      school_intro_done := false;
      school_students_running := false;
      school_messenger_spoke := false;
      school_student_a_stage := 0;
      school_student_b_stage := 0;
      school_messenger_stage := 0;
      Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
        { speaker = "Moi"; text = "Incroyable… c'est encore plus impressionnant que ce que j'imaginais." };
        { speaker = "Moi"; text = "Chaque pierre semble vibrer…\ncomme si le bâtiment lui-même exécutait un sort." };
      ])
    end;

    if !school_intro_done then
      ()
    else if not !school_students_running then begin
      if not global.dialogue_state.active then
        school_students_running := true
    end else begin
      let class_x, class_y, class_w, class_h = School_map.class_door_rect () in
      let class_target_x = float_of_int (class_x + (class_w / 2) - 20) in
        let class_target_y = float_of_int (class_y + class_h + 4) in
        let class_entry_y = float_of_int (class_y - 72) in
      let lane_y = class_target_y +. 105.0 in
      let lane_a_x = class_target_x +. 30.0 in
      let lane_b_x = class_target_x +. 72.0 in
      let stop_x = class_target_x -. 40.0 in
      let stop_y = lane_y +. 8.0 in

      (match !school_student_a_ref with
         | Some npc ->
           if not !school_messenger_spoke then
             set_npc_sprite_row npc sprite_row_right
           else if not global.dialogue_state.active then
             (match !school_student_a_stage with
              | 0 ->
                  if move_npc_toward_animated npc lane_a_x lane_y 2.2 then school_student_a_stage := 1
              | 1 ->
                  if move_npc_toward_animated npc class_target_x lane_y 2.2 then school_student_a_stage := 2
              | 2 ->
                  if move_npc_toward_animated npc class_target_x class_entry_y 2.4 then (
                    school_student_a_stage := 3;
                    npc#tag#set (InScene Scene.Menu)
                  )
              | _ -> ())
       | None -> ());
      (match !school_student_b_ref with
         | Some npc ->
           if not !school_messenger_spoke then
             set_npc_sprite_row npc sprite_row_left
           else if not global.dialogue_state.active then
             (match !school_student_b_stage with
              | 0 ->
                  if move_npc_toward_animated npc lane_b_x lane_y 2.0 then school_student_b_stage := 1
              | 1 ->
                  if move_npc_toward_animated npc (class_target_x +. 26.0) lane_y 2.0 then school_student_b_stage := 2
              | 2 ->
                  if move_npc_toward_animated npc (class_target_x +. 26.0) class_entry_y 2.2 then (
                    school_student_b_stage := 3;
                    npc#tag#set (InScene Scene.Menu)
                  )
              | _ -> ())
       | None -> ());

      (match !school_messenger_ref with
       | Some npc ->
           if not !school_messenger_spoke then begin
             let arrived = move_npc_toward_animated npc stop_x stop_y 2.0 in
             if arrived && not global.dialogue_state.active then begin
               school_messenger_spoke := true;
               let look_row = facing_row_toward_player (npc#position#get) player_pos in
               set_npc_sprite_row npc look_row;
               Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
                 { speaker = "Élève"; text = "Vite ! Le Professeur Lambda va commencer son cours !" };
               ])
             end
           end else if not global.dialogue_state.active then begin
             (match !school_messenger_stage with
              | 0 ->
                  if move_npc_toward_animated npc (class_target_x +. 12.0) lane_y 2.2 then school_messenger_stage := 1
              | 1 ->
                  if move_npc_toward_animated npc (class_target_x +. 12.0) class_entry_y 2.5 then (
                    school_messenger_stage := 2;
                    npc#tag#set (InScene Scene.Menu)
                  )
              | _ -> ())
           end
       | None -> ());

      if !school_student_a_stage >= 3 && !school_student_b_stage >= 3 && !school_messenger_stage >= 2 then
        school_intro_done := true
    end
  end

let update_town_npc_facing () =
  let global = Global.get () in
  if Scene.current () <> Scene.Town then
    ()
  else
    let player_pos = global.player#position#get in
    let active_speaker =
      if global.dialogue_state.active then
        match Dialogue.current_line global.dialogue_state with
        | Some line -> Some line.Component_defs.speaker
        | None -> None
      else
        None
    in
    let dialogue_target_name =
      match active_speaker with
      | Some "Marchand" -> Some "Marchand"
      | Some "Vieille érudite" -> Some "Vieille érudite"
      | _ ->
          if global.dialogue_state.active then
            let candidate name npc_ref =
              match !npc_ref with
              | Some npc ->
                  let d = distance_sq (npc#position#get) player_pos in
                  Some (name, d)
              | None -> None
            in
            let candidates = List.filter_map (fun x -> x) [
              candidate "Marchand" merchant_ref;
              candidate "Vieille érudite" scholar_ref;
            ] in
            (match List.sort (fun (_, d1) (_, d2) -> Float.compare d1 d2) candidates with
             | (name, d) :: _ when d <= (90.0 *. 90.0) -> Some name
             | _ -> None)
          else
            None
    in
    let update_one npc_ref npc_name default_row =
      match !npc_ref with
      | Some npc ->
          let row =
            match dialogue_target_name with
            | Some speaker when String.equal speaker npc_name ->
                facing_row_toward_player (npc#position#get) player_pos
            | _ -> default_row
          in
          set_npc_sprite_row npc row
      | None -> ()
    in
    update_one merchant_ref "Marchand" sprite_row_down;
    update_one scholar_ref "Vieille érudite" sprite_row_right

let set_knight_sprite knight frame_index moving_right =
  let frame_w = 48 in
  let frame_h = 48 in
  let row = if moving_right then 2 else 1 in
  let col =
    match frame_index with
    | 0 -> 0
    | 1 -> 1
    | _ -> 2
  in
  match knight#texture#get with
  | Texture.Sprite (img, _, _, _, _) ->
      knight#texture#set (Texture.Sprite (img, col * frame_w, row * frame_h, frame_w, frame_h))
  | _ -> ()

let update_knight_patrol () =
  let global = Global.get () in
  match !knight_guardian_ref with
  | Some knight when Scene.current () = Scene.Town ->
      if global.knight_challenge_completed then (
        if not !knight_moved_aside then (
          knight#position#set Vector.{ x = !knight_side_x; y = !knight_side_y };
          set_knight_sprite knight 1 false;
          knight_moved_aside := true
        )
      ) else if
        match global.code_challenge_state with
        | Some challenge_state -> challenge_state.Code_challenge.active
        | None -> false
      then
        set_knight_sprite knight 1 (!knight_patrol_dir > 0.0)
      else if global.dialogue_state.active then
        set_knight_sprite knight 1 (!knight_patrol_dir > 0.0)
      else
        let pos = knight#position#get in
        let speed = 1.2 in
        let next_x = pos.x +. (!knight_patrol_dir *. speed) in
        let clamped_x = max !knight_patrol_left_x (min !knight_patrol_right_x next_x) in
        knight#position#set Vector.{ pos with x = clamped_x };
        if clamped_x <= !knight_patrol_left_x +. 0.1 then knight_patrol_dir := 1.0;
        if clamped_x >= !knight_patrol_right_x -. 0.1 then knight_patrol_dir := -1.0;
        incr knight_walk_tick;
        let phase =
          if !knight_walk_tick mod 20 < 7 then 0
          else if !knight_walk_tick mod 20 < 14 then 1
          else 2
        in
        set_knight_sprite knight phase (!knight_patrol_dir > 0.0)
  | _ -> ()

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
          let () = update_knight_patrol () in
          let () = update_town_npc_facing () in
          let () = update_school_intro_event () in
          
          let () = Tutorial_manager.check_scene_tutorials () in
          
          Move_system.update dt;
          Collision_system.update dt;
          Door_transition_system.update dt;
          Draw_system.update dt;
          Tutorial_system.update dt;
          Dialogue_system.update dt;
          Code_challenge_system.update dt Seq.empty;
          
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

  let _house_walls = Wall.house_walls () in
  let _town_walls = Wall.town_walls () in
  let _school_walls = Wall.school_walls () in
  let _house_doors = Door.house_doors () in
  let _town_doors = Door.town_doors () in
  let _school_doors = Door.school_doors () in

  let knight_cx, knight_cy =
    match Town_map.find_marker 'G' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (390, 50)
  in
  let sign_cx, sign_cy =
    match Town_map.find_marker 'P' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (200, 150)
  in
  
  let _sign1 = Sign.create_sign (sign_cx - 12) (sign_cy - 16)
    { title = "Panneau"; text = "Académie d'OCamlon\n-> Nord\n\nStabilité, Savoir, Structure"; scene = Scene.Town }
    Scene.Town in
  _sign1#texture#set Texture.transparent;

  let knight_sheet = Gfx.load_image ctx "ressources/personnages/Chevalier.png" in
  let marchand_sheet = Gfx.load_image ctx "ressources/personnages/Marchand.png" in
  let vieux_sheet = Gfx.load_image ctx "ressources/personnages/Vieux.png" in
  let knight_texture =
    match Gfx.get_resource_opt knight_sheet with
    | Some img -> Some (Texture.Sprite (img, 0, 2 * 48, 48, 48))
    | None -> None
  in
  let merchant_texture =
    match Gfx.get_resource_opt marchand_sheet with
    | Some img -> Some (Texture.Sprite (img, 48, 0, 48, 48))
    | None -> None
  in
  let scholar_texture =
    match Gfx.get_resource_opt vieux_sheet with
    | Some img -> Some (Texture.Sprite (img, 4 * 48, 2 * 48, 48, 48))
    | None -> None
  in

  let knight_start_x = knight_cx - 20 in
  let knight_start_y = knight_cy - 24 in

  let knight_guardian = Npc.create_npc ?texture:knight_texture "Chevalier Gardien" knight_start_x knight_start_y
    { name = "Chevalier Gardien"; dialogue = Dialogue.knight_guardian_intro; scene = Scene.Town }
    Scene.Town in

  let merchant_cx, merchant_cy =
    match Town_map.find_marker 'M' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (550, 280)
  in
  let scholar_cx, scholar_cy =
    match Town_map.find_marker 'V' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (180, 200)
  in

  let merchant = Npc.create_npc ?texture:merchant_texture "Marchand" (merchant_cx - 20) (merchant_cy - 36)
    { name = "Marchand"; dialogue = Dialogue.npc_villager_2; scene = Scene.Town }
    Scene.Town in

  let scholar = Npc.create_npc ?texture:scholar_texture "Vieille érudite" (scholar_cx - 20) (scholar_cy - 24)
    { name = "Vieille érudite"; dialogue = Dialogue.npc_villager_1; scene = Scene.Town }
    Scene.Town in

  knight_guardian_ref := Some (knight_guardian :> Component_defs.npc_entity);
  merchant_ref := Some (merchant :> Component_defs.npc_entity);
  scholar_ref := Some (scholar :> Component_defs.npc_entity);
  let gate_left_col = Town_map.academy_gate_left_col in
  let gate_right_col = Town_map.academy_gate_right_col in
  knight_patrol_left_x := float_of_int (gate_left_col * Cst.town_cell_w + 6);
  knight_patrol_right_x := float_of_int (gate_right_col * Cst.town_cell_w - 6);
  knight_patrol_dir := 1.0;
  knight_side_x := !knight_patrol_right_x +. 24.0;
  knight_side_y := float_of_int (knight_start_y + 6);
  knight_moved_aside := false;
  
  let eleves_sheet = Gfx.load_image ctx "ressources/personnages/Eleves.png" in
  let student_boy_texture =
    match Gfx.get_resource_opt eleves_sheet with
    | Some img ->
        let w, h = Gfx.surface_size img in
        let frame_w = if w >= 6 then w / 6 else 48 in
        let frame_h = if h >= 4 then h / 4 else 48 in
        Some (Texture.Sprite (img, frame_w, 0, frame_w, frame_h))
    | None -> None
  in
  let student_girl_texture =
    match Gfx.get_resource_opt eleves_sheet with
    | Some img ->
        let w, h = Gfx.surface_size img in
        let frame_w = if w >= 6 then w / 6 else 48 in
        let frame_h = if h >= 4 then h / 4 else 48 in
        Some (Texture.Sprite (img, 4 * frame_w, 0, frame_w, frame_h))
    | None -> None
  in
  let student_boy = Npc.create_npc ?texture:student_boy_texture "Élève A" 280 280
    { name = "Élève A"; dialogue = Dialogue.create_dialogue [{ speaker = "Élève"; text = "Le cours va commencer !" }]; scene = Scene.School }
    Scene.School in
  let student_girl = Npc.create_npc ?texture:student_girl_texture "Élève B" 325 290
    { name = "Élève B"; dialogue = Dialogue.create_dialogue [{ speaker = "Élève"; text = "Vite, on y va !" }]; scene = Scene.School }
    Scene.School in
  let student_messenger = Npc.create_npc ?texture:student_girl_texture "Élève" 360 285
    { name = "Élève"; dialogue = Dialogue.create_dialogue [{ speaker = "Élève"; text = "Vite ! Le Professeur Lambda va commencer son cours !" }]; scene = Scene.School }
    Scene.School in
  school_student_a_ref := Some (student_boy :> Component_defs.npc_entity);
  school_student_b_ref := Some (student_girl :> Component_defs.npc_entity);
  school_messenger_ref := Some (student_messenger :> Component_defs.npc_entity);
  set_npc_sprite_row student_boy sprite_row_right;
  set_npc_sprite_row student_girl sprite_row_left;
  set_npc_sprite_row student_messenger sprite_row_left;
  school_intro_started := false;
  school_intro_done := false;
  school_students_running := false;
  school_messenger_spoke := false;
  school_student_a_stage := 0;
  school_student_b_stage := 0;
  school_messenger_stage := 0;

  let house_cols = 12 in
  let house_rows = 9 in
  let cell_x c = Cst.house_offset_x + (c * Cst.house_width) / house_cols in
  let cell_y r = Cst.house_offset_y + (r * Cst.house_height) / house_rows in
  let cell_w c = (Cst.house_offset_x + ((c + 1) * Cst.house_width) / house_cols) - cell_x c in
  let cell_h r = (Cst.house_offset_y + ((r + 1) * Cst.house_height) / house_rows) - cell_y r in

  let chest_col = 2 in
  let chest_row = 3 in
  let chest_x = cell_x chest_col + cell_w chest_col - 12 in
  let chest_y = cell_y chest_row + (cell_h chest_row / 2) - 12 in

  let _apprentice_chest = Sign.create_sign chest_x chest_y
    { title = "Coffre de l'Apprenti"; text = "Un coffre verrouillé par un sceau magique."; scene = Scene.House }
    Scene.House in
  
  let object_cols = 15 in
  let object_rows = 9 in
  let obj_cell_x c = Cst.house_offset_x + (c * Cst.house_width) / object_cols in
  let obj_cell_y r = Cst.house_offset_y + (r * Cst.house_height) / object_rows in
  let obj_cell_w c = (Cst.house_offset_x + ((c + 1) * Cst.house_width) / object_cols) - obj_cell_x c in
  let obj_cell_h r = (Cst.house_offset_y + ((r + 1) * Cst.house_height) / object_rows) - obj_cell_y r in
  let book_col = 8 - 1 in
  let book_row = 7 - 1 in
  let book_x = obj_cell_x book_col + (obj_cell_w book_col - 14) / 2 in
  let book_y = obj_cell_y book_row + (obj_cell_h book_row - 12) / 2 in

  let _secret_book = Book.create_book book_x book_y Scene.House in
  _apprentice_chest#texture#set Texture.transparent;
  
  let player = Player.players () in
  let dialogue_state = Dialogue.create_state () in
  let tutorial_state = Tutorial.create_state () in
  let code_challenge_state = Code_challenge.create_state () in
  
  Tutorial.register_message tutorial_state "move" "Utiliser \"ZQSD\"\npour se déplacer";
  Tutorial.register_message tutorial_state "menu" "Utiliser \"Echap\"\npour ouvrir le menu";
  Tutorial.register_message tutorial_state "interact" "Utiliser \"ESPACE\"\npour intéragir";
  Tutorial.register_message tutorial_state "town_explore" "Explore les rues d'OCamlon\net parle aux habitants";
  Tutorial.register_message tutorial_state "town_signs" "Lis les panneaux pour\ntrouver l'Académie";
  
  let menu = Menu.create () in
  let global = Global.{ window; ctx; player; waiting = 0; dialogue_state; tutorial_state; font; menu_state = Some menu; character_creation_state = None; on_character_complete = None; on_escape_pressed = None; player_name = "Apprenti"; code_challenge_state = Some code_challenge_state; house_exit_attempted = false; has_secret_book = false; chest_challenge_completed = false; knight_challenge_completed = false } in
  Global.set global;
  
  let rec start_new_game () =
    let char_creation = Character_creation.create () in
    ignore (Savegame.delete ());
    global.house_exit_attempted <- false;
    global.has_secret_book <- false;
    global.chest_challenge_completed <- false;
    global.knight_challenge_completed <- false;
    knight_moved_aside := false;
    school_intro_started := false;
    school_intro_done := false;
    school_students_running := false;
    school_messenger_spoke := false;
    school_student_a_stage := 0;
    school_student_b_stage := 0;
    school_messenger_stage := 0;
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