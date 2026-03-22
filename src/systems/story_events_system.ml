open Component_defs

let knight_guardian_ref : npc_entity option ref = ref None
let merchant_ref : npc_entity option ref = ref None
let scholar_ref : npc_entity option ref = ref None
let school_student_a_ref : npc_entity option ref = ref None
let school_student_b_ref : npc_entity option ref = ref None
let school_messenger_ref : npc_entity option ref = ref None
let professor_lambda_ref : npc_entity option ref = ref None

let knight_patrol_left_x = ref 0.0
let knight_patrol_right_x = ref 0.0
let knight_patrol_dir = ref 1.0
let knight_walk_tick = ref 0
let knight_moved_aside = ref false
let knight_side_x = ref 0.0
let knight_side_y = ref 0.0

let school_students_intro_started = ref false
let school_students_running = ref false
let school_messenger_spoke = ref false
let school_student_a_stage = ref 0
let school_student_b_stage = ref 0
let school_messenger_stage = ref 0
let school_intro_started = ref false
let school_intro_done = ref false
let school_challenge_started = ref false
let school_challenge_completed = ref false
let school_walk_tick = ref 0
let classroom_seating_started = ref false
let classroom_seating_done = ref false
let classroom_seat_target : (float * float) option ref = ref None
let classroom_seating_stuck_ticks = ref 0
let classroom_last_player_pos : Vector.t option ref = ref None
let classroom_seating_total_ticks = ref 0

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

let school_npc_target col row =
  let cx, cy = School_map.cell_center col row in
  (float_of_int (cx - 20), float_of_int (cy - 30))

let choose_classroom_seat_target player_pos =
  let _ = player_pos in
  let seat_cx, seat_cy = Classroom_map.cell_center 13 12 in
  (float_of_int (seat_cx - (Cst.player_width / 2)), float_of_int (seat_cy - Cst.player_height + 2))

let move_player_toward_auto player target_x target_y speed =
  let pos = player#position#get in
  let dx = target_x -. pos.Vector.x in
  let dy = target_y -. pos.Vector.y in
  let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
  if dist <= speed || dist = 0.0 then (
    player#position#set Vector.{x = target_x; y = target_y};
    Player.stop_player ();
    true
  ) else (
    let vx = dx /. dist *. speed in
    let vy = dy /. dist *. speed in
    Player.move_player player Vector.{x = vx; y = vy};
    player#position#set Vector.{x = pos.Vector.x +. vx; y = pos.Vector.y +. vy};
    player#velocity#set Vector.zero;
    false
  )

let setup_town_npcs ~(knight:npc_entity) ~(merchant:npc_entity) ~(scholar:npc_entity) =
  knight_guardian_ref := Some knight;
  merchant_ref := Some merchant;
  scholar_ref := Some scholar;
  let gate_left_col = Town_map.academy_gate_left_col in
  let gate_right_col = Town_map.academy_gate_right_col in
  knight_patrol_left_x := float_of_int (gate_left_col * Cst.town_cell_w + 6);
  knight_patrol_right_x := float_of_int (gate_right_col * Cst.town_cell_w - 6);
  knight_patrol_dir := 1.0;
  let pos = knight#position#get in
  knight_side_x := !knight_patrol_right_x +. 24.0;
  knight_side_y := pos.Vector.y +. 6.0;
  knight_moved_aside := false

let setup_school_npcs ~(student_a:npc_entity) ~(student_b:npc_entity) ~(messenger:npc_entity) =
  school_student_a_ref := Some student_a;
  school_student_b_ref := Some student_b;
  school_messenger_ref := Some messenger;
  let students = [student_a; student_b; messenger] in
  let sorted_by_y_then_x =
    List.sort
      (fun a b ->
        let pa = a#position#get in
        let pb = b#position#get in
        let y_cmp = Float.compare pa.Vector.y pb.Vector.y in
        if y_cmp <> 0 then y_cmp else Float.compare pa.Vector.x pb.Vector.x)
      students
  in
  (match sorted_by_y_then_x with
   | top :: rest ->
       let rest_by_x =
         List.sort
           (fun a b -> Float.compare (a#position#get).Vector.x (b#position#get).Vector.x)
           rest
       in
       set_npc_sprite_row top sprite_row_down;
       (match rest_by_x with
        | [left; right] ->
            set_npc_sprite_row left sprite_row_right;
            set_npc_sprite_row right sprite_row_left
        | _ ->
            set_npc_sprite_row student_a sprite_row_right;
            set_npc_sprite_row student_b sprite_row_left;
            set_npc_sprite_row messenger sprite_row_left)
   | _ ->
       set_npc_sprite_row student_a sprite_row_right;
       set_npc_sprite_row student_b sprite_row_left;
       set_npc_sprite_row messenger sprite_row_left);
  school_students_intro_started := false;
  school_students_running := false;
  school_messenger_spoke := false;
  school_student_a_stage := 0;
  school_student_b_stage := 0;
  school_messenger_stage := 0

let setup_classroom_npcs ~(professor:npc_entity) ~(student_a:npc_entity) ~(student_b:npc_entity) ~(student_c:npc_entity) =
  professor_lambda_ref := Some professor;
  set_npc_sprite_row professor sprite_row_down;
  set_npc_sprite_row student_a sprite_row_up;
  set_npc_sprite_row student_b sprite_row_up;
  set_npc_sprite_row student_c sprite_row_up;
  classroom_seating_started := false;
  classroom_seating_done := false;
  classroom_seat_target := None;
  classroom_seating_stuck_ticks := 0;
  classroom_last_player_pos := None;
  classroom_seating_total_ticks := 0;
  school_intro_started := false;
  school_intro_done := false;
  school_challenge_started := false;
  school_challenge_completed := false

let reset_for_new_game () =
  knight_moved_aside := false;
  school_students_intro_started := false;
  school_students_running := false;
  school_messenger_spoke := false;
  school_student_a_stage := 0;
  school_student_b_stage := 0;
  school_messenger_stage := 0;
  classroom_seating_started := false;
  classroom_seating_done := false;
  classroom_seat_target := None;
  classroom_seating_stuck_ticks := 0;
  classroom_last_player_pos := None;
  classroom_seating_total_ticks := 0;
  school_intro_started := false;
  school_intro_done := false;
  school_challenge_started := false;
  school_challenge_completed := false

let update_school_students_event () =
  let global = Global.get () in
  let player_pos = global.player#position#get in
  if Scene.current () <> Scene.School then
    ()
  else if global.school_students_event_completed then begin
    (match !school_student_a_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ());
    (match !school_student_b_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ());
    (match !school_messenger_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ())
  end
  else begin
    if not !school_students_intro_started then begin
      school_students_intro_started := true;
      school_students_running := false;
      school_messenger_spoke := false;
      school_student_a_stage := 0;
      school_student_b_stage := 0;
      school_messenger_stage := 0;
      Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
        { speaker = "Moi"; text = "L'academie est immense..." };
        { speaker = "Moi"; text = "Il faut que je trouve la salle de classe." };
      ])
    end;

    if not !school_students_running then begin
      if not global.dialogue_state.active then school_students_running := true
    end else begin
      let queue_a_x, queue_a_y = school_npc_target 8 4 in
      let queue_b_x, queue_b_y = school_npc_target 10 4 in
      let queue_c_x, queue_c_y = school_npc_target 7 4 in
      let class_door_x, class_door_y, class_door_w, _ = School_map.class_door_rect () in
      let door_center_x = float_of_int (class_door_x + (class_door_w / 2) - 20) in
      let door_queue_y = float_of_int (class_door_y + Cst.school_cell_h + 6 - 30) in
      let class_entry_y = float_of_int (class_door_y - 12) in

      (match !school_student_a_ref with
       | Some npc ->
           if not !school_messenger_spoke then set_npc_sprite_row npc sprite_row_right
           else if not global.dialogue_state.active then
             (match !school_student_a_stage with
              | 0 -> if move_npc_toward_animated npc queue_a_x queue_a_y 2.1 then school_student_a_stage := 1
              | 1 -> if move_npc_toward_animated npc door_center_x queue_a_y 2.1 then school_student_a_stage := 2
              | 2 ->
                  if move_npc_toward_animated npc door_center_x door_queue_y 2.2 then school_student_a_stage := 3
              | 3 ->
                  if move_npc_toward_animated npc door_center_x class_entry_y 2.4 then (
                    school_student_a_stage := 3;
                    npc#tag#set (InScene Scene.Menu)
                  )
              | _ -> ())
       | None -> ());

      (match !school_student_b_ref with
       | Some npc ->
           if not !school_messenger_spoke then set_npc_sprite_row npc sprite_row_left
           else if (not global.dialogue_state.active) && !school_student_a_stage >= 2 then
             (match !school_student_b_stage with
              | 0 -> if move_npc_toward_animated npc queue_b_x queue_b_y 2.0 then school_student_b_stage := 1
              | 1 -> if move_npc_toward_animated npc door_center_x queue_b_y 2.0 then school_student_b_stage := 2
              | 2 ->
                  if move_npc_toward_animated npc door_center_x door_queue_y 2.2 then school_student_b_stage := 3
              | 3 ->
                  if move_npc_toward_animated npc door_center_x class_entry_y 2.4 then (
                    school_student_b_stage := 3;
                    npc#tag#set (InScene Scene.Menu)
                  )
              | _ -> ())
       | None -> ());

      (match !school_messenger_ref with
       | Some npc ->
           if not !school_messenger_spoke then begin
             let arrived = move_npc_toward_animated npc queue_c_x queue_c_y 2.0 in
             if arrived && not global.dialogue_state.active then begin
               school_messenger_spoke := true;
               let look_row = facing_row_toward_player (npc#position#get) player_pos in
               set_npc_sprite_row npc look_row;
               Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
                 { speaker = "Eleve"; text = "Vite ! Le Professeur Lambda va commencer son cours !" };
               ])
             end
           end else if (not global.dialogue_state.active) && !school_student_b_stage >= 2 then begin
             (match !school_messenger_stage with
              | 0 -> if move_npc_toward_animated npc door_center_x queue_c_y 2.1 then school_messenger_stage := 1
              | 1 ->
                  if move_npc_toward_animated npc door_center_x class_entry_y 2.4 then (
                    school_messenger_stage := 2;
                    npc#tag#set (InScene Scene.Menu)
                  )
              | _ -> ())
           end
       | None -> ());

      let is_hidden npc_ref =
        match npc_ref with
        | Some npc ->
            (match npc#tag#get with
             | InScene Scene.Menu -> true
             | _ -> false)
        | None -> true
      in
      if
        (not global.school_students_event_completed)
        && is_hidden !school_student_a_ref
        && is_hidden !school_student_b_ref
        && is_hidden !school_messenger_ref
      then
        global.school_students_event_completed <- true
    end
  end

let update_school_intro_event () =
  let global = Global.get () in
  (match !professor_lambda_ref with
   | Some lambda ->
       if Scene.current () = Scene.Classroom then begin
         lambda#tag#set (InScene Scene.Classroom);
         if global.dialogue_state.active then
           match Dialogue.current_line global.dialogue_state with
           | Some line when String.equal line.Component_defs.speaker "Professeur Lambda" ->
               let row = facing_row_toward_player (lambda#position#get) (global.player#position#get) in
               set_npc_sprite_row lambda row
           | _ -> ()
       end
   | None -> ());
  if Scene.current () <> Scene.Classroom then
    ()
  else if global.classroom_intro_completed then begin
    classroom_seating_started := true;
    classroom_seating_done := true;
    school_intro_started := true;
    school_intro_done := true;
    school_challenge_started := true;
    school_challenge_completed := true
  end
  else begin
    if not !classroom_seating_started then begin
      classroom_seating_started := true;
      classroom_seating_done := false;
      classroom_seat_target := Some (choose_classroom_seat_target (global.player#position#get));
      classroom_seating_stuck_ticks := 0;
      classroom_last_player_pos := Some (global.player#position#get);
      classroom_seating_total_ticks := 0;
      Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
        { speaker = "Professeur Lambda"; text = "Merci de vite vous asseoir a votre place !" };
      ])
    end
    else if not !classroom_seating_done then begin
      if not global.dialogue_state.active then begin
        let before = global.player#position#get in
        let target_x, target_y =
          match !classroom_seat_target with
          | Some t -> t
          | None -> choose_classroom_seat_target (global.player#position#get)
        in
        incr classroom_seating_total_ticks;
        if move_player_toward_auto global.player target_x target_y 2.1 then begin
          classroom_seating_done := true;
          Player.move_player global.player Vector.{x = 0.0; y = -.0.1};
          global.player#velocity#set Vector.zero
        end else begin
          let after = global.player#position#get in
          let moved =
            abs_float (after.Vector.x -. before.Vector.x) +. abs_float (after.Vector.y -. before.Vector.y)
          in
          if moved < 0.15 then incr classroom_seating_stuck_ticks
          else classroom_seating_stuck_ticks := 0;
          if !classroom_seating_stuck_ticks >= 20 || !classroom_seating_total_ticks >= 180 then begin
            global.player#position#set Vector.{x = target_x; y = target_y};
            Player.move_player global.player Vector.{x = 0.0; y = -.0.1};
            global.player#velocity#set Vector.zero;
            classroom_seating_done := true;
            classroom_seating_stuck_ticks := 0;
            classroom_seating_total_ticks := 0
          end;
          classroom_last_player_pos := Some after
        end
      end
    end
    else begin
      if not !school_intro_started then begin
        school_intro_started := true;
        school_intro_done := false;
        school_challenge_started := false;
        school_challenge_completed := false;
        Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
          { speaker = "Professeur Lambda"; text = "Bienvenue a l'Academie d'OCamlon." };
          { speaker = "Professeur Lambda"; text = "Ici, nous ne manipulons pas la magie..." };
          { speaker = "Professeur Lambda"; text = "Nous la decrivons avec precision." };
          { speaker = "Professeur Lambda"; text = "Et le monde obeit." };
          { speaker = "Professeur Lambda"; text = "Dis-moi... sais-tu manipuler les nombres ?" };
        ])
      end;

      if (not global.dialogue_state.active) && (not !school_challenge_started) && (not !school_challenge_completed) then begin
        school_challenge_started := true;
        match global.code_challenge_state with
        | Some challenge_state when not challenge_state.Code_challenge.active ->
            let on_success () =
              school_challenge_completed := true;
              school_intro_done := true;
              global.classroom_intro_completed <- true;
              Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
                { speaker = "Narration"; text = "Une lumiere apparait dans la salle." };
                { speaker = "Narration"; text = "Une sphere d'energie se forme." };
                { speaker = "Professeur Lambda"; text = "Bien." };
                { speaker = "Professeur Lambda"; text = "Les nombres sont l'energie brute de la magie." };
                { speaker = "Professeur Lambda"; text = "Continue, et n'oublie jamais la rigueur." };
              ])
            in
            let on_failure () =
              let failure_dialogue =
                match Code_challenge.get_last_failure_reason challenge_state with
                | Some Code_challenge.TypeError ->
                    Dialogue.create_dialogue [
                      { speaker = "Professeur Lambda"; text = "Tu melanges des essences incompatibles." };
                      { speaker = "Professeur Lambda"; text = "Les types sont les lois fondamentales de ce monde." };
                      { speaker = "Narration"; text = "Une etincelle magique frappe le joueur." };
                    ]
                | Some Code_challenge.SyntaxError ->
                    Dialogue.create_dialogue [
                      { speaker = "Narration"; text = "Ton sort s'effondre avant meme d'exister." };
                      { speaker = "Professeur Lambda"; text = "La magie exige de la structure." };
                      { speaker = "Narration"; text = "Le decor tremble legerement." };
                    ]
                | _ ->
                    Dialogue.create_dialogue [
                      { speaker = "Professeur Lambda"; text = "Recommence. Structure ton incantation." };
                    ]
              in
              Dialogue.start_dialogue global.dialogue_state failure_dialogue
            in
            Code_challenge.start_challenge challenge_state Code_challenge.PowerCalculation on_success on_failure
        | _ -> ()
      end
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

let update () =
  update_knight_patrol ();
  update_town_npc_facing ();
  update_school_students_event ();
  update_school_intro_event ()
