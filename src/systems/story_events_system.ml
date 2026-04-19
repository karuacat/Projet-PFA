open Component_defs

let knight_guardian_ref : npc_entity option ref = ref None
let merchant_ref : npc_entity option ref = ref None
let scholar_ref : npc_entity option ref = ref None
let school_student_a_ref : npc_entity option ref = ref None
let school_student_b_ref : npc_entity option ref = ref None
let school_messenger_ref : npc_entity option ref = ref None
let classroom_student_a_ref : npc_entity option ref = ref None
let classroom_student_b_ref : npc_entity option ref = ref None
let classroom_student_c_ref : npc_entity option ref = ref None
let professor_lambda_ref : npc_entity option ref = ref None
let aerin_ref : npc_entity option ref = ref None

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
let classroom_seat_waypoints : (float * float) list ref = ref []
let classroom_seating_stuck_ticks = ref 0
let classroom_last_player_pos : Vector.t option ref = ref None
let classroom_seating_total_ticks = ref 0
let classroom_post_success_started = ref false
let classroom_post_phase = ref 0
let classroom_post_move_ticks = ref 0
let aerin_post_target : (float * float) option ref = ref None
let lambda_post_target : (float * float) option ref = ref None
let student_a_post_target : (float * float) option ref = ref None
let student_b_post_target : (float * float) option ref = ref None
let student_c_post_target : (float * float) option ref = ref None
let aerin_route_waypoints : (float * float) list ref = ref []
let lambda_route_waypoints : (float * float) list ref = ref []
let student_a_route_waypoints : (float * float) list ref = ref []
let student_b_route_waypoints : (float * float) list ref = ref []
let student_c_route_waypoints : (float * float) list ref = ref []

let aerin_exit_started = ref false
let aerin_exit_phase = ref 0
let aerin_exit_waypoint : (float * float) option ref = ref None
let aerin_exit_route_waypoints : (float * float) list ref = ref []
let aerin_exit_target : (float * float) option ref = ref None
let aerin_exit_dialogue_started = ref false
let lambda_post_aerin_dialogue_started = ref false
let students_exit_started = ref false
let students_exit_phase = ref 0
let lambda_return_started = ref false
let school_students_after_course_spawned = ref false
let classroom_students_hidden_after_exit = ref false
let previous_scene : Scene.scene option ref = ref None

let is_post_duel_sequence_active () =
  !aerin_exit_started || !students_exit_started || !lambda_return_started

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

type path_cache_entry = {
  scene : Scene.scene;
  goal : int * int;
  ignore_player : bool;
  mutable cells : (int * int) list;
}

let npc_path_cache : (npc_entity, path_cache_entry) Hashtbl.t = Hashtbl.create 64
let npc_blocked_ticks : (npc_entity, int) Hashtbl.t = Hashtbl.create 64

let school_intro_npc_refs () =
  [ !school_student_a_ref; !school_student_b_ref; !school_messenger_ref ]

let collides_other_school_intro_npc (npc : npc_entity) (candidate : Rect.rect_f) =
  let global = Global.get () in
  if Scene.current () <> Scene.School || global.school_students_event_completed then
    false
  else
    List.exists
      (function
        | Some other when not (other == npc) ->
            (match other#tag#get with
             | InScene Scene.School ->
                 let other_pos = other#position#get in
                 let other_box = other#box#get in
                 let other_rect : Rect.rect_f =
                   {
                     x = other_pos.Vector.x;
                     y = other_pos.Vector.y;
                     width = float_of_int other_box.Rect.width;
                     height = float_of_int other_box.Rect.height;
                   }
                 in
                 Rect.collides candidate other_rect
             | _ -> false)
        | _ -> false)
      (school_intro_npc_refs ())

let route_mode_for_current_state () =
  let global = Global.get () in
  if
    (Scene.current () = Scene.School && not global.school_students_event_completed)
    ||
    (Scene.current () = Scene.Classroom
     &&
    (not global.classroom_intro_completed
     || !classroom_post_success_started
     || !aerin_exit_started
     || !students_exit_started
     || !lambda_return_started))
  then
    Navigation_debug.Scripted
  else
    Navigation_debug.Dynamic

let rect_from_pos_box (pos : Vector.t) (box : Rect.t) : Rect.rect_f =
  {
    x = pos.Vector.x;
    y = pos.Vector.y;
    width = float_of_int box.width;
    height = float_of_int box.height;
  }

let rect_for_int x y w h : Rect.rect_f =
  {
    x = float_of_int x;
    y = float_of_int y;
    width = float_of_int w;
    height = float_of_int h;
  }

let rect_collides_player (candidate : Rect.rect_f) =
  let global = Global.get () in
  let player_pos = global.player#position#get in
  let player_box = global.player#box#get in
  let player_rect = rect_from_pos_box player_pos player_box in
  Rect.collides candidate player_rect

let collides_town_obstacles (candidate : Rect.rect_f) =
  let min_col = max 0 (int_of_float (candidate.x /. float_of_int Cst.town_cell_w)) in
  let max_col =
    min (Cst.town_cols - 1)
      (int_of_float ((candidate.x +. candidate.width -. 0.001) /. float_of_int Cst.town_cell_w))
  in
  let min_row = max 0 (int_of_float (candidate.y /. float_of_int Cst.town_cell_h)) in
  let max_row =
    min (Cst.town_rows - 1)
      (int_of_float ((candidate.y +. candidate.height -. 0.001) /. float_of_int Cst.town_cell_h))
  in
  let rec rows r =
    if r > max_row then false
    else
      let rec cols c =
        if c > max_col then false
        else if Town_map.is_blocked c r then true
        else cols (c + 1)
      in
      if cols min_col then true else rows (r + 1)
  in
  rows min_row

let collides_school_obstacles (candidate : Rect.rect_f) =
  let min_col = max 0 (int_of_float (candidate.x /. float_of_int Cst.school_cell_w)) in
  let max_col =
    min (Cst.school_cols - 1)
      (int_of_float ((candidate.x +. candidate.width -. 0.001) /. float_of_int Cst.school_cell_w))
  in
  let min_row = max 0 (int_of_float (candidate.y /. float_of_int Cst.school_cell_h)) in
  let max_row =
    min (Cst.school_rows - 1)
      (int_of_float ((candidate.y +. candidate.height -. 0.001) /. float_of_int Cst.school_cell_h))
  in
  let rec rows r =
    if r > max_row then false
    else
      let rec cols c =
        if c > max_col then false
        else if School_map.is_blocked c r then true
        else cols (c + 1)
      in
      if cols min_col then true else rows (r + 1)
  in
  rows min_row

let collides_classroom_obstacles (candidate : Rect.rect_f) =
  let min_col = max 0 (int_of_float (candidate.x /. float_of_int Cst.classroom_cell_w)) in
  let max_col =
    min (Cst.classroom_cols - 1)
      (int_of_float ((candidate.x +. candidate.width -. 0.001) /. float_of_int Cst.classroom_cell_w))
  in
  let min_row = max 0 (int_of_float (candidate.y /. float_of_int Cst.classroom_cell_h)) in
  let max_row =
    min (Cst.classroom_rows - 1)
      (int_of_float ((candidate.y +. candidate.height -. 0.001) /. float_of_int Cst.classroom_cell_h))
  in
  let rec rows r =
    if r > max_row then false
    else
      let rec cols c =
        if c > max_col then false
        else if Classroom_map.is_blocked c r then true
        else cols (c + 1)
      in
      if cols min_col then true else rows (r + 1)
  in
  let blocked_cells = rows min_row in
  if blocked_cells then true
  else
    List.exists
      (fun (x, y, w, h) -> Rect.collides candidate (rect_for_int x y w h))
      (Classroom_map.npc_collision_rects ())

let npc_can_stand_at ?(ignore_player=false) (npc : npc_entity) (x : float) (y : float) =
  let box = npc#box#get in
  let candidate = rect_from_pos_box Vector.{x; y} box in
  let out_of_bounds =
    candidate.x < 0.0
    || candidate.y < 0.0
    || candidate.x +. candidate.width > float_of_int Cst.window_width
    || candidate.y +. candidate.height > float_of_int Cst.window_height
  in
  if out_of_bounds || ((not ignore_player) && rect_collides_player candidate) then false
  else if collides_other_school_intro_npc npc candidate then false
  else
    match Scene.current () with
    | Scene.Town -> not (collides_town_obstacles candidate)
    | Scene.School -> not (collides_school_obstacles candidate)
    | Scene.Classroom -> not (collides_classroom_obstacles candidate)
    | _ -> true

let classify_block_reason ?(ignore_player=false) (npc : npc_entity) (x : float) (y : float) =
  let box = npc#box#get in
  let candidate = rect_from_pos_box Vector.{x; y} box in
  let out_of_bounds =
    candidate.x < 0.0
    || candidate.y < 0.0
    || candidate.x +. candidate.width > float_of_int Cst.window_width
    || candidate.y +. candidate.height > float_of_int Cst.window_height
  in
  if out_of_bounds then
    Some "bounds"
  else if (not ignore_player) && rect_collides_player candidate then
    Some "player"
  else if collides_other_school_intro_npc npc candidate then
    Some "npc"
  else
    match Scene.current () with
    | Scene.Town -> if collides_town_obstacles candidate then Some "obstacle" else Some "unknown"
    | Scene.School -> if collides_school_obstacles candidate then Some "obstacle" else Some "unknown"
    | Scene.Classroom -> if collides_classroom_obstacles candidate then Some "obstacle" else Some "unknown"
    | _ -> Some "unknown"

let scene_grid_info () =
  match Scene.current () with
  | Scene.School ->
      Some
        ( Cst.school_cols,
          Cst.school_rows,
          (fun col row ->
            let cx, cy = School_map.cell_center col row in
            (float_of_int (cx - 20), float_of_int (cy - 30))) )
  | Scene.Classroom ->
      Some
        ( Cst.classroom_cols,
          Cst.classroom_rows,
          (fun col row ->
            let cx, cy = Classroom_map.cell_center col row in
            (float_of_int (cx - 20), float_of_int (cy - 30))) )
  | _ -> None

let find_nearest_walkable_cell ?(ignore_player=false) (npc : npc_entity) (px : float) (py : float) =
  match scene_grid_info () with
  | None -> None
  | Some (cols, rows, to_anchor) ->
      let best = ref None in
      for row = 0 to rows - 1 do
        for col = 0 to cols - 1 do
          let ax, ay = to_anchor col row in
          if npc_can_stand_at ~ignore_player npc ax ay then begin
            let dx = ax -. px in
            let dy = ay -. py in
            let d2 = (dx *. dx) +. (dy *. dy) in
            match !best with
            | None -> best := Some (col, row, d2)
            | Some (_, _, best_d2) when d2 < best_d2 -> best := Some (col, row, d2)
            | _ -> ()
          end
        done
      done;
      match !best with
      | Some (col, row, _) -> Some (col, row)
      | None -> None

let reconstruct_path came_from start_cell goal_cell =
  let rec build acc current =
    if current = start_cell then current :: acc
    else
      match Hashtbl.find_opt came_from current with
      | Some prev -> build (current :: acc) prev
      | None -> []
  in
  build [] goal_cell

let astar_path_for_npc ?(ignore_player=false) (npc : npc_entity) (start_cell : int * int) (goal_cell : int * int) =
  match scene_grid_info () with
  | None -> []
  | Some (cols, rows, to_anchor) ->
      let neighbors (c, r) =
        [ (c + 1, r); (c - 1, r); (c, r + 1); (c, r - 1) ]
        |> List.filter (fun (nc, nr) -> nc >= 0 && nr >= 0 && nc < cols && nr < rows)
      in
      let heuristic (c, r) =
        let gc, gr = goal_cell in
        abs (gc - c) + abs (gr - r)
      in
      let open_set = ref [ start_cell ] in
      let came_from : ((int * int), (int * int)) Hashtbl.t = Hashtbl.create 256 in
      let g_score : ((int * int), int) Hashtbl.t = Hashtbl.create 256 in
      Hashtbl.add g_score start_cell 0;
      let f_score : ((int * int), int) Hashtbl.t = Hashtbl.create 256 in
      Hashtbl.add f_score start_cell (heuristic start_cell);
      let rec loop () =
        match !open_set with
        | [] -> []
        | _ ->
            let current =
              List.fold_left
                (fun best node ->
                  let best_f = Option.value (Hashtbl.find_opt f_score best) ~default:max_int in
                  let node_f = Option.value (Hashtbl.find_opt f_score node) ~default:max_int in
                  if node_f < best_f then node else best)
                (List.hd !open_set)
                !open_set
            in
            if current = goal_cell then
              reconstruct_path came_from start_cell goal_cell
            else begin
              open_set := List.filter (fun n -> n <> current) !open_set;
              let current_g = Option.value (Hashtbl.find_opt g_score current) ~default:max_int in
              List.iter
                (fun nb ->
                  let nc, nr = nb in
                  let ax, ay = to_anchor nc nr in
                  if npc_can_stand_at ~ignore_player npc ax ay then begin
                    let tentative = current_g + 1 in
                    let old_g = Option.value (Hashtbl.find_opt g_score nb) ~default:max_int in
                    if tentative < old_g then begin
                      Hashtbl.replace came_from nb current;
                      Hashtbl.replace g_score nb tentative;
                      Hashtbl.replace f_score nb (tentative + heuristic nb);
                      if not (List.mem nb !open_set) then
                        open_set := nb :: !open_set
                    end
                  end)
                (neighbors current);
              loop ()
            end
      in
      loop ()

let move_npc_step ?(ignore_player=false) npc target_x target_y speed =
  let pos = npc#position#get in
  let dx = target_x -. pos.Vector.x in
  let dy = target_y -. pos.Vector.y in
  let adx = abs_float dx in
  let ady = abs_float dy in
  if adx < 0.01 && ady < 0.01 then
    true
  else (
    let step_x = if adx > 0.0 then (if dx > 0.0 then min speed adx else -. (min speed adx)) else 0.0 in
    let step_y = if ady > 0.0 then (if dy > 0.0 then min speed ady else -. (min speed ady)) else 0.0 in
    let try_move_x () =
      adx > 0.01
      && npc_can_stand_at ~ignore_player npc (pos.Vector.x +. step_x) pos.Vector.y
      && (npc#position#set Vector.{x = pos.Vector.x +. step_x; y = pos.Vector.y}; true)
    in
    let try_move_y () =
      ady > 0.01
      && npc_can_stand_at ~ignore_player npc pos.Vector.x (pos.Vector.y +. step_y)
      && (npc#position#set Vector.{x = pos.Vector.x; y = pos.Vector.y +. step_y}; true)
    in
    let prefer_x = adx >= ady in
    let moved = if prefer_x then try_move_x () || try_move_y () else try_move_y () || try_move_x () in
    if not moved then false
    else
      let after = npc#position#get in
      abs_float (after.Vector.x -. target_x) < 0.01
      && abs_float (after.Vector.y -. target_y) < 0.01
  )

let move_npc_toward npc target_x target_y speed =
  let mode = route_mode_for_current_state () in
  let ignore_player_mode = mode = Navigation_debug.Scripted in
  match scene_grid_info () with
  | None ->
      Navigation_debug.set npc mode target_x target_y;
      move_npc_step ~ignore_player:ignore_player_mode npc target_x target_y speed
  | Some (_, _, to_anchor) ->
      let pos = npc#position#get in
      let ensure_path ignore_player_mode =
        let start_cell = find_nearest_walkable_cell ~ignore_player:ignore_player_mode npc pos.Vector.x pos.Vector.y in
        let goal_cell = find_nearest_walkable_cell ~ignore_player:ignore_player_mode npc target_x target_y in
        match start_cell, goal_cell with
        | Some start_cell, Some goal_cell ->
            let need_recompute =
              match Hashtbl.find_opt npc_path_cache npc with
              | Some cache ->
                  cache.scene <> Scene.current ()
                  || cache.goal <> goal_cell
                  || cache.ignore_player <> ignore_player_mode
                  || cache.cells = []
              | None -> true
            in
            if need_recompute then begin
              let cells = astar_path_for_npc ~ignore_player:ignore_player_mode npc start_cell goal_cell in
              Hashtbl.replace npc_path_cache npc { scene = Scene.current (); goal = goal_cell; ignore_player = ignore_player_mode; cells }
            end;
            Hashtbl.find_opt npc_path_cache npc
        | _ -> None
      in
      let cache_opt =
        if ignore_player_mode then
          ensure_path true
        else
          match ensure_path false with
          | Some cache when cache.cells <> [] -> Some cache
          | _ -> ensure_path true
      in
      (match cache_opt with
       | Some cache ->
           let rec drop_reached = function
             | [] -> []
             | (c, r) :: rest ->
                 let ax, ay = to_anchor c r in
                 if distance_sq pos Vector.{x = ax; y = ay} <= (speed +. 1.0) *. (speed +. 1.0) then
                   drop_reached rest
                 else
                   (c, r) :: rest
           in
           cache.cells <- drop_reached cache.cells;
           let waypoint =
             match cache.cells with
             | (c, r) :: _ ->
                 let ax, ay = to_anchor c r in
                 Some (ax, ay)
             | [] -> None
           in
           let path_points =
             List.map
               (fun (c, r) ->
                 let ax, ay = to_anchor c r in
                 (ax, ay))
               cache.cells
           in
           Navigation_debug.set ?waypoint ~path_points npc mode target_x target_y;
           (match cache.cells with
            | [] -> move_npc_step ~ignore_player:cache.ignore_player npc target_x target_y speed
            | (c, r) :: rest ->
                let ax, ay = to_anchor c r in
                let before = npc#position#get in
                let reached = move_npc_step ~ignore_player:cache.ignore_player npc ax ay speed in
                let after = npc#position#get in
                if reached then cache.cells <- rest;
                if before = after && not reached then cache.cells <- [];
                if reached && rest = [] then
                  move_npc_step ~ignore_player:cache.ignore_player npc target_x target_y speed
                else
                  false)
       | None ->
           let reached = move_npc_step ~ignore_player:ignore_player_mode npc target_x target_y speed in
           let blocked_reason = if reached then None else Some "no_path" in
           Navigation_debug.set ?blocked_reason npc mode target_x target_y;
           reached)

let snap_npc_near_target npc target_x target_y =
  match scene_grid_info () with
  | Some (_, _, to_anchor) ->
      (match find_nearest_walkable_cell ~ignore_player:true npc target_x target_y with
       | Some (c, r) ->
           let ax, ay = to_anchor c r in
           npc#position#set Vector.{x = ax; y = ay};
           true
       | None ->
           if npc_can_stand_at ~ignore_player:true npc target_x target_y then (
             npc#position#set Vector.{x = target_x; y = target_y};
             true
           ) else
             false)
  | None ->
      if npc_can_stand_at ~ignore_player:true npc target_x target_y then (
        npc#position#set Vector.{x = target_x; y = target_y};
        true
      ) else
        false

let move_npc_toward_animated npc target_x target_y speed =
  let mode = route_mode_for_current_state () in
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
  let before = npc#position#get in
  let reached = move_npc_toward npc target_x target_y speed in
  let after = npc#position#get in
  let moved =
    abs_float (after.Vector.x -. before.Vector.x) > 0.05
    || abs_float (after.Vector.y -. before.Vector.y) > 0.05
  in
  if reached then begin
    Hashtbl.remove npc_blocked_ticks npc;
    Navigation_debug.update_block_reason npc None;
    set_npc_sprite_row npc row;
    true
  end else if moved then begin
    Hashtbl.remove npc_blocked_ticks npc;
    Navigation_debug.update_block_reason npc None;
    set_npc_walk_sprite npc row phase;
    false
  end else begin
    let stuck = 1 + Option.value (Hashtbl.find_opt npc_blocked_ticks npc) ~default:0 in
    Hashtbl.replace npc_blocked_ticks npc stuck;
    let blocked_reason = classify_block_reason ~ignore_player:(mode = Navigation_debug.Scripted) npc target_x target_y in
    Navigation_debug.update_block_reason npc blocked_reason;
    if mode = Navigation_debug.Scripted && stuck >= 40 then begin
      let snapped = snap_npc_near_target npc target_x target_y in
      Hashtbl.remove npc_blocked_ticks npc;
      if snapped then set_npc_sprite_row npc row;
      if snapped then Navigation_debug.update_block_reason npc None;
      snapped
    end else if mode = Navigation_debug.Dynamic && stuck >= 20 then begin
      Hashtbl.remove npc_path_cache npc;
      false
    end else begin
      false
    end
  end

let move_npc_step_axis ?(ignore_player=false) ?(check_collisions=true) npc target_x target_y speed =
  let pos = npc#position#get in
  let box = npc#box#get in
  let can_stand nx ny =
    if check_collisions then
      npc_can_stand_at ~ignore_player npc nx ny
    else
      let candidate = rect_from_pos_box Vector.{x = nx; y = ny} box in
      candidate.x >= 0.0
      && candidate.y >= 0.0
      && candidate.x +. candidate.width <= float_of_int Cst.window_width
      && candidate.y +. candidate.height <= float_of_int Cst.window_height
      && (ignore_player || not (rect_collides_player candidate))
  in
  let dx = target_x -. pos.Vector.x in
  let dy = target_y -. pos.Vector.y in
  let adx = abs_float dx in
  let ady = abs_float dy in
  if adx < 0.01 && ady < 0.01 then
    true
  else (
    let step_x = if adx > 0.0 then (if dx > 0.0 then min speed adx else -. (min speed adx)) else 0.0 in
    let step_y = if ady > 0.0 then (if dy > 0.0 then min speed ady else -. (min speed ady)) else 0.0 in
    let try_move_x () =
      adx > 0.01
      && can_stand (pos.Vector.x +. step_x) pos.Vector.y
      && (npc#position#set Vector.{x = pos.Vector.x +. step_x; y = pos.Vector.y}; true)
    in
    let try_move_y () =
      ady > 0.01
      && can_stand pos.Vector.x (pos.Vector.y +. step_y)
      && (npc#position#set Vector.{x = pos.Vector.x; y = pos.Vector.y +. step_y}; true)
    in
    let prefer_x = adx >= ady in
    let moved = if prefer_x then try_move_x () || try_move_y () else try_move_y () || try_move_x () in
    if not moved then false
    else
      let after = npc#position#get in
      abs_float (after.Vector.x -. target_x) < 0.01
      && abs_float (after.Vector.y -. target_y) < 0.01
  )

let move_npc_toward_axis_animated ?(ignore_player=false) ?(check_collisions=true) npc target_x target_y speed =
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
  let before = npc#position#get in
  let reached = move_npc_step_axis ~ignore_player ~check_collisions npc target_x target_y speed in
  let after = npc#position#get in
  let moved =
    abs_float (after.Vector.x -. before.Vector.x) > 0.05
    || abs_float (after.Vector.y -. before.Vector.y) > 0.05
  in
  if reached then set_npc_sprite_row npc row
  else if moved then set_npc_walk_sprite npc row phase
  else ();
  reached

let school_npc_target col row =
  let cx, cy = School_map.cell_center col row in
  (float_of_int (cx - 20), float_of_int (cy - 30))

let classroom_npc_target col row =
  let cx, cy = Classroom_map.cell_center col row in
  (float_of_int (cx - 20), float_of_int (cy - 30))

let classroom_marker_target marker fallback_col fallback_row =
  match Classroom_map.marker_cells marker with
  | (col, row) :: _ -> classroom_npc_target col row
  | [] -> classroom_npc_target fallback_col fallback_row

let sorted_cells_by_row_col cells =
  List.sort (fun (c1, r1) (c2, r2) ->
    let by_row = Int.compare r1 r2 in
    if by_row <> 0 then by_row else Int.compare c1 c2
  ) cells

let spread_three_cells cells fallback =
  let sorted = sorted_cells_by_row_col cells in
  let len = List.length sorted in
  if len >= 3 then [
    List.nth sorted 0;
    List.nth sorted (len / 2);
    List.nth sorted (len - 1)
  ] else fallback

let assign_post_targets_from_markers () =
  let target_1 = classroom_marker_target '1' 1 6 in
  let target_2 = classroom_marker_target '2' 1 8 in
  let target_3 = classroom_marker_target '3' 1 12 in
  let target_a = classroom_marker_target 'A' 3 6 in
  let target_l = classroom_marker_target 'L' 5 9 in

  aerin_post_target := Some target_a;
  lambda_post_target := Some target_l;
  student_a_post_target := Some target_1;
  student_b_post_target := Some target_2;
  student_c_post_target := Some target_3;

  let student_a_turn_1 = classroom_npc_target 8 7 in
  let student_a_turn_2 = classroom_npc_target 8 6 in
  let student_b_turn_1 = classroom_npc_target 14 8 in
  let student_c_turn_1 = classroom_npc_target 15 11 in
  let student_c_turn_2 = classroom_npc_target 15 12 in
  let aerin_turn_1 = classroom_npc_target 15 7 in
  let aerin_turn_2 = classroom_npc_target 15 6 in
  let lambda_turn_1 = classroom_npc_target 7 3 in
  let lambda_turn_2 = classroom_npc_target 7 5 in
  let lambda_turn_3 = classroom_npc_target 5 5 in

  student_a_route_waypoints := [ student_a_turn_1; student_a_turn_2 ];
  student_b_route_waypoints := [ student_b_turn_1 ];
  student_c_route_waypoints := [ student_c_turn_1; student_c_turn_2 ];
  aerin_route_waypoints := [ aerin_turn_1; aerin_turn_2 ];
  lambda_route_waypoints := [ lambda_turn_1; lambda_turn_2; lambda_turn_3 ]

let move_npc_ref_to_target ?arrival_row npc_ref target_ref speed =
  match !npc_ref, !target_ref with
  | Some npc, Some (tx, ty) ->
      let pos = npc#position#get in
      let close_enough =
        abs_float (pos.Vector.x -. tx) <= 3.0
        && abs_float (pos.Vector.y -. ty) <= 6.0
      in
      if close_enough then begin
        npc#position#set Vector.{x = tx; y = ty};
        (match arrival_row with Some row -> set_npc_sprite_row npc row | None -> ());
        true
      end else if move_npc_toward_axis_animated ~ignore_player:true ~check_collisions:false npc tx ty speed then begin
        npc#position#set Vector.{x = tx; y = ty};
        (match arrival_row with Some row -> set_npc_sprite_row npc row | None -> ());
        true
      end else
        false
  | _ -> true

let move_npc_ref_via_waypoints ?arrival_row npc_ref waypoints_ref target_ref speed =
  match !npc_ref, !target_ref with
  | Some npc, Some (tx, ty) ->
      (match !waypoints_ref with
       | next_waypoint :: rest_waypoints ->
           let wx, wy = next_waypoint in
           let pos = npc#position#get in
           let close_enough =
             abs_float (pos.Vector.x -. wx) <= 3.0
             && abs_float (pos.Vector.y -. wy) <= 6.0
           in
           let reached_waypoint =
             if close_enough then begin
               npc#position#set Vector.{x = wx; y = wy};
               true
             end else if move_npc_toward_axis_animated ~ignore_player:true ~check_collisions:false npc wx wy speed then begin
               npc#position#set Vector.{x = wx; y = wy};
               true
             end else
               false
           in
           if reached_waypoint then waypoints_ref := rest_waypoints;
           false
       | [] -> move_npc_ref_to_target ?arrival_row npc_ref target_ref speed)
  | _ -> true

let orient_classroom_bench_students_right () =
  (match !classroom_student_a_ref with Some npc -> set_npc_sprite_row npc sprite_row_right | None -> ());
  (match !classroom_student_b_ref with Some npc -> set_npc_sprite_row npc sprite_row_right | None -> ());
  (match !classroom_student_c_ref with Some npc -> set_npc_sprite_row npc sprite_row_right | None -> ())

let start_classroom_post_success_sequence (global : Global.t) =
  if not !classroom_post_success_started then begin
    classroom_post_success_started := true;
    classroom_post_phase := 1;
    classroom_post_move_ticks := 0;
    assign_post_targets_from_markers ();
    global.classroom_intro_completed <- false
  end

let choose_classroom_seat_target player_pos =
  let _ = player_pos in
  let seat_cx, seat_cy = Classroom_map.cell_center 13 12 in
  (float_of_int (seat_cx - (Cst.player_width / 2)), float_of_int (seat_cy - Cst.player_height + 2))

let player_can_stand_at_auto player x y =
  let box = player#box#get in
  let candidate = rect_from_pos_box Vector.{x; y} box in
  let out_of_bounds =
    candidate.x < 0.0
    || candidate.y < 0.0
    || candidate.x +. candidate.width > float_of_int Cst.window_width
    || candidate.y +. candidate.height > float_of_int Cst.window_height
  in
  if out_of_bounds then false
  else
    match Scene.current () with
    | Scene.Town -> not (collides_town_obstacles candidate)
    | Scene.School -> not (collides_school_obstacles candidate)
    | Scene.Classroom ->
        not
          (List.exists
             (fun (rx, ry, rw, rh) -> Rect.collides candidate (rect_for_int rx ry rw rh))
             (Classroom_map.collision_rects ()))
    | _ -> true

let move_player_toward_auto player target_x target_y speed =
  let pos = player#position#get in
  let dx = target_x -. pos.Vector.x in
  let dy = target_y -. pos.Vector.y in
  let adx = abs_float dx in
  let ady = abs_float dy in
  if adx <= speed && ady <= speed then (
    player#position#set Vector.{x = target_x; y = target_y};
    Player.stop_player ();
    true
  ) else (
    let try_move nx ny vx vy =
      if player_can_stand_at_auto player nx ny then begin
        Player.move_player player Vector.{x = vx; y = vy};
        player#position#set Vector.{x = nx; y = ny};
        player#velocity#set Vector.zero;
        true
      end else
        false
    in
    (* Axis-only movement: vertical first, then horizontal. *)
    if ady > 0.01 then begin
      let step_y = if dy > 0.0 then min speed ady else -. (min speed ady) in
      let ny = pos.Vector.y +. step_y in
      ignore (try_move pos.Vector.x ny 0.0 step_y);
      false
    end else if adx > 0.01 then begin
      let step_x = if dx > 0.0 then min speed adx else -. (min speed adx) in
      let nx = pos.Vector.x +. step_x in
      ignore (try_move nx pos.Vector.y step_x 0.0);
      false
    end else (
      Player.stop_player ();
      true
    )
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

let setup_classroom_npcs ~(professor:npc_entity) ~(aerin:npc_entity) ~(student_a:npc_entity) ~(student_b:npc_entity) ~(student_c:npc_entity) =
  professor_lambda_ref := Some professor;
  aerin_ref := Some aerin;
  classroom_student_a_ref := Some student_a;
  classroom_student_b_ref := Some student_b;
  classroom_student_c_ref := Some student_c;
  set_npc_sprite_row professor sprite_row_down;
  set_npc_sprite_row aerin sprite_row_up;
  aerin#tag#set (InScene Scene.Classroom);
  set_npc_sprite_row student_a sprite_row_up;
  set_npc_sprite_row student_b sprite_row_up;
  set_npc_sprite_row student_c sprite_row_up;
  classroom_seating_started := false;
  classroom_seating_done := false;
  classroom_seat_target := None;
  classroom_seat_waypoints := [];
  classroom_seating_stuck_ticks := 0;
  classroom_last_player_pos := None;
  classroom_seating_total_ticks := 0;
  classroom_post_success_started := false;
  classroom_post_phase := 0;
  classroom_post_move_ticks := 0;
  aerin_post_target := None;
  lambda_post_target := None;
  student_a_post_target := None;
  student_b_post_target := None;
  student_c_post_target := None;
  school_intro_started := false;
  school_intro_done := false;
  school_challenge_started := false;
  school_challenge_completed := false

let remove_aerin_from_classroom () =
  match !aerin_ref with
  | Some aerin -> aerin#tag#set (InScene Scene.Menu)
  | None -> ()

let clear_classroom_students_for_return () =
  (match !classroom_student_a_ref with
   | Some npc -> npc#tag#set (InScene Scene.Menu)
   | None -> ());
  (match !classroom_student_b_ref with
   | Some npc -> npc#tag#set (InScene Scene.Menu)
   | None -> ());
  (match !classroom_student_c_ref with
   | Some npc -> npc#tag#set (InScene Scene.Menu)
   | None -> ())

let start_aerin_exit_sequence () =
  let sx, sy, sw, _ = Classroom_map.school_door_rect () in
  let door_center_x = sx + (sw / 2) in
  let door_target =
    (float_of_int (door_center_x - 20), float_of_int (sy - 30))
  in
  aerin_exit_waypoint := None;
  aerin_exit_route_waypoints := [];
  aerin_exit_target := Some door_target;
  aerin_exit_started := true;
  aerin_exit_phase := 1;
  aerin_exit_dialogue_started := false;
  lambda_post_aerin_dialogue_started := false

let setup_students_exit_targets () =
  let sx, sy, sw, _ = Classroom_map.school_door_rect () in
  let door_center_x = sx + (sw / 2) in
  let door_target = (float_of_int (door_center_x - 20), float_of_int (sy - 30)) in
  student_a_post_target := Some door_target;
  student_b_post_target := Some door_target;
  student_c_post_target := Some door_target

let setup_lambda_return_to_desk () =
  let desk_col, desk_row = 14, 3 in
  lambda_post_target := Some (classroom_npc_target desk_col desk_row)

let send_classroom_students_to_school_map () =
  let global = Global.get () in
  let player_pos = global.player#position#get in
  let available = School_map.post_course_spawn_cells () in
  let min_distance_sq = (2.0 *. float_of_int Cst.school_cell_w) ** 2.0 in
  let far_enough cells =
    List.filter (fun (col, row) ->
      let cx, cy = School_map.cell_center col row in
      let dx = float_of_int cx -. player_pos.Vector.x in
      let dy = float_of_int cy -. player_pos.Vector.y in
      (dx *. dx) +. (dy *. dy) >= min_distance_sq
    ) cells
  in
  let filtered = far_enough available in
  let fallback_cells = [ (5, 5); (6, 4); (7, 5) ] in
  let pick_three cells =
    if List.length cells >= 3 then begin
      let arr = Array.of_list cells in
      let n = Array.length arr in
      let i1 = Random.int n in
      let rec pick_other avoid =
        let i = Random.int n in
        if List.mem i avoid then pick_other avoid else i
      in
      let i2 = pick_other [i1] in
      let i3 = pick_other [i1; i2] in
      [ arr.(i1); arr.(i2); arr.(i3) ]
    end else
      fallback_cells
  in
  let picks = pick_three filtered in
  let e1 = List.nth picks 0 in
  let e2 = List.nth picks 1 in
  let e3 = List.nth picks 2 in
  let place_student npc_ref (col, row) dialogue_text_opt =
    match !npc_ref with
    | Some npc ->
        let x, y = school_npc_target col row in
        npc#position#set Vector.{x; y};
        npc#tag#set (InScene Scene.School);
        set_npc_sprite_row npc sprite_row_down;
        let data = npc#npc_data#get in
        let updated_dialogue =
          match dialogue_text_opt with
          | Some text -> Dialogue.create_dialogue [ { speaker = "Eleve"; text } ]
          | None -> data.dialogue
        in
        npc#npc_data#set { data with scene = Scene.School; dialogue = updated_dialogue }
    | None -> ()
  in
  place_student school_student_a_ref e1 (Some "La bibliotheque est en haut de l'echelle.");
  place_student school_student_b_ref e2 (Some "Les types... c'est la base de tout en magie.");
  place_student school_messenger_ref e3 (Some "Un type decrit exactement ce qu'on peut faire.");
  school_students_after_course_spawned := true

let update_aerin_exit_sequence () =
  if !aerin_exit_started then begin
    let global = Global.get () in
    match !aerin_exit_phase with
    | 1 ->
        if (not !aerin_exit_dialogue_started) && not global.dialogue_state.active then begin
          aerin_exit_dialogue_started := true;
          Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
            { speaker = "Aerin"; text = "Hmph. Debutant chanceux." };
            { speaker = "Aerin"; text = "On se reverra." };
          ])
        end;
        if !aerin_exit_dialogue_started && not global.dialogue_state.active then begin
          let sx, _, sw, _ = Classroom_map.school_door_rect () in
          let door_center_x = sx + (sw / 2) in
          let stop_before_player =
            let player_pos = global.player#position#get in
            let aerin_x =
              match !aerin_ref with
              | Some aerin -> (aerin#position#get).Vector.x
              | None -> player_pos.Vector.x
            in
            (aerin_x, player_pos.Vector.y -. float_of_int Cst.classroom_cell_h)
          in
          let align_with_exit_column =
            (float_of_int (door_center_x - 20), snd stop_before_player)
          in
          aerin_exit_waypoint := Some stop_before_player;
          aerin_exit_route_waypoints := [ align_with_exit_column ];
          aerin_exit_phase := 2
        end
    | 2 ->
        let reached_stop =
          match !aerin_ref, !aerin_exit_waypoint with
          | Some npc, Some (ux, uy) ->
              move_npc_toward_axis_animated ~ignore_player:true ~check_collisions:false npc ux uy 2.0
          | _ -> true
        in
        if reached_stop then begin
          aerin_exit_waypoint := None;
          aerin_exit_phase := 3
        end
    | 3 ->
        let done_exit = move_npc_ref_via_waypoints ~arrival_row:sprite_row_down aerin_ref aerin_exit_route_waypoints aerin_exit_target 2.2 in
        if done_exit then begin
          (match !aerin_ref with
           | Some aerin -> aerin#tag#set (InScene Scene.Menu)
           | None -> ());
          aerin_exit_phase := 4;
          aerin_exit_waypoint := None;
          aerin_exit_route_waypoints := []
        end
    | 4 ->
        if (not !lambda_post_aerin_dialogue_started) && not global.dialogue_state.active then begin
          let player_name = if String.length global.player_name > 0 then global.player_name else "Apprenti" in
          lambda_post_aerin_dialogue_started := true;
          Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
            {
              speaker = "Professeur Lambda";
              text =
                "Bien le cours est fini pour aujourd'hui, "
                ^ player_name
                ^ " vient me voir avant de partir s'il te plait !";
            };
          ])
        end;
        if !lambda_post_aerin_dialogue_started && not global.dialogue_state.active then begin
          (match !aerin_ref with
           | Some aerin -> aerin#tag#set (InScene Scene.Menu)
           | None -> ());
          aerin_exit_started := false;
          aerin_exit_phase := 0;
          aerin_exit_waypoint := None;
          aerin_exit_route_waypoints := [];
          aerin_exit_target := None;
          aerin_exit_dialogue_started := false;
          lambda_post_aerin_dialogue_started := false;
          setup_students_exit_targets ();
          setup_lambda_return_to_desk ();
          students_exit_started := true;
          students_exit_phase := 1;
          lambda_return_started := false
        end
    | _ -> ()
  end;
  if !students_exit_started then begin
    (match !aerin_ref with
     | Some aerin -> aerin#tag#set (InScene Scene.Menu)
     | None -> ());
    match !students_exit_phase with
    | 1 ->
        let s1_done = move_npc_ref_to_target classroom_student_a_ref student_a_post_target 2.1 in
        let s2_done = move_npc_ref_to_target classroom_student_b_ref student_b_post_target 2.0 in
        let s3_done = move_npc_ref_to_target classroom_student_c_ref student_c_post_target 2.0 in
        if s1_done then
          (match !classroom_student_a_ref with
           | Some npc -> npc#tag#set (InScene Scene.Menu)
           | None -> ());
        if s2_done then
          (match !classroom_student_b_ref with
           | Some npc -> npc#tag#set (InScene Scene.Menu)
           | None -> ());
        if s3_done then
          (match !classroom_student_c_ref with
           | Some npc -> npc#tag#set (InScene Scene.Menu)
           | None -> ());
        if s1_done && s2_done && s3_done then begin
          send_classroom_students_to_school_map ();
          students_exit_phase := 2;
          lambda_return_started := true
        end
    | 2 ->
        if !lambda_return_started then begin
          let lambda_done = move_npc_ref_to_target professor_lambda_ref lambda_post_target 1.5 in
          if lambda_done then begin
            (match !professor_lambda_ref with
             | Some lambda -> set_npc_sprite_row lambda sprite_row_down
             | None -> ());
            students_exit_started := false;
            students_exit_phase := 0;
            lambda_return_started := false;
            student_a_post_target := None;
            student_b_post_target := None;
            student_c_post_target := None;
            lambda_post_target := None
          end
        end
    | _ -> ()
  end

let reset_for_new_game () =
  Hashtbl.clear npc_path_cache;
  Hashtbl.clear npc_blocked_ticks;
  Navigation_debug.clear ();
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
  classroom_seat_waypoints := [];
  classroom_seating_stuck_ticks := 0;
  classroom_last_player_pos := None;
  classroom_seating_total_ticks := 0;
  classroom_post_success_started := false;
  classroom_post_phase := 0;
  classroom_post_move_ticks := 0;
  aerin_post_target := None;
  lambda_post_target := None;
  student_a_post_target := None;
  student_b_post_target := None;
  student_c_post_target := None;
  school_intro_started := false;
  school_intro_done := false;
  school_challenge_started := false;
  school_challenge_completed := false;
  aerin_exit_started := false;
  aerin_exit_phase := 0;
  aerin_exit_waypoint := None;
  aerin_exit_route_waypoints := [];
  aerin_exit_target := None;
  aerin_exit_dialogue_started := false;
  lambda_post_aerin_dialogue_started := false;
  students_exit_started := false;
  students_exit_phase := 0;
  lambda_return_started := false;
  school_students_after_course_spawned := false;
  classroom_students_hidden_after_exit := false;
  previous_scene := None;
  (match !school_student_a_ref with Some npc -> npc#tag#set (InScene Scene.School) | None -> ());
  (match !school_student_b_ref with Some npc -> npc#tag#set (InScene Scene.School) | None -> ());
  (match !school_messenger_ref with Some npc -> npc#tag#set (InScene Scene.School) | None -> ())

let update_school_students_event () =
  let global = Global.get () in
  let player_pos = global.player#position#get in
  if Scene.current () <> Scene.School then
    ()
  else if global.school_students_event_completed then begin
    if global.lambda_duel_completed then begin
      if not !school_students_after_course_spawned then
        send_classroom_students_to_school_map ()
    end else begin
      (match !school_student_a_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ());
      (match !school_student_b_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ());
      (match !school_messenger_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ())
    end
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
      if not global.dialogue_state.active then begin
        school_students_running := true;
        if not !school_messenger_spoke then begin
          school_messenger_spoke := true;
          (match !school_messenger_ref with
           | Some npc ->
               let look_row = facing_row_toward_player (npc#position#get) player_pos in
               set_npc_sprite_row npc look_row
           | None -> ());
          Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
            { speaker = "Eleve"; text = "Vite ! Le Professeur Lambda va commencer son cours !" };
          ])
        end
      end
    end else begin
      let c1_cell, c2_cell =
        match School_map.marker_cells 'C' with
        | c1 :: c2 :: _ -> (c1, c2)
        | [ c1 ] -> (c1, c1)
        | _ -> ((9, 2), (10, 2))
      in
      let c1_x, c1_y = school_npc_target (fst c1_cell) (snd c1_cell) in
      let c2_x, c2_y = school_npc_target (fst c2_cell) (snd c2_cell) in
      let can_move_students = !school_messenger_spoke in
      let move_student_right_then_to_c npc stage_ref c_x c_y speed =
        let finish_on_c () =
          npc#position#set Vector.{x = c_x; y = c_y};
          stage_ref := 2;
          npc#tag#set (InScene Scene.Menu)
        in
        match !stage_ref with
        | 0 ->
            let pos = npc#position#get in
            if move_npc_toward_axis_animated ~ignore_player:true ~check_collisions:false npc c_x pos.Vector.y speed then
              stage_ref := 1
        | 1 ->
            let pos = npc#position#get in
            let close_enough =
              abs_float (pos.Vector.x -. c_x) <= 3.0
              && abs_float (pos.Vector.y -. c_y) <= 6.0
            in
            if close_enough then
              finish_on_c ()
            else if move_npc_toward_axis_animated ~ignore_player:true ~check_collisions:false npc c_x c_y speed then
              finish_on_c ()
        | _ -> ()
      in

      (match !school_student_a_ref with
       | Some npc ->
           if not can_move_students then set_npc_sprite_row npc sprite_row_right
           else move_student_right_then_to_c npc school_student_a_stage c1_x c1_y 2.2
       | None -> ());

      (match !school_student_b_ref with
       | Some npc ->
           if not can_move_students then set_npc_sprite_row npc sprite_row_left
           else move_student_right_then_to_c npc school_student_b_stage c2_x c2_y 2.1
       | None -> ());

      (match !school_messenger_ref with
       | Some npc ->
           if not can_move_students then begin
             let look_row = facing_row_toward_player (npc#position#get) player_pos in
             set_npc_sprite_row npc look_row
           end else move_student_right_then_to_c npc school_messenger_stage c1_x c1_y 2.0
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
  global.classroom_scripted_movement_active <-
    Scene.current () = Scene.Classroom
    && (
         not global.classroom_intro_completed
         || !classroom_post_success_started
         || !aerin_exit_started
         || !students_exit_started
         || !lambda_return_started
       );
  update_aerin_exit_sequence ();
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
  (match !aerin_ref with
   | Some aerin ->
       if Scene.current () = Scene.Classroom then begin
         if (not global.lambda_duel_completed) || !aerin_exit_started then
           aerin#tag#set (InScene Scene.Classroom);
         if global.lambda_duel_completed && not !aerin_exit_started then
           aerin#tag#set (InScene Scene.Menu);
         if global.dialogue_state.active then
           match Dialogue.current_line global.dialogue_state with
           | Some line when String.equal line.Component_defs.speaker "Aerin" ->
               set_npc_sprite_row aerin sprite_row_down
           | _ -> ()
       end
   | None -> ());
  (* New update for students exit and lambda return *)
  if !students_exit_started then begin
    match !students_exit_phase with
    | 1 ->
        let s1_done = move_npc_ref_to_target classroom_student_a_ref student_a_post_target 2.1 in
        let s2_done = move_npc_ref_to_target classroom_student_b_ref student_b_post_target 2.0 in
        let s3_done = move_npc_ref_to_target classroom_student_c_ref student_c_post_target 2.0 in
        if s1_done then
          (match !classroom_student_a_ref with
           | Some npc -> npc#tag#set (InScene Scene.Menu)
           | None -> ());
        if s2_done then
          (match !classroom_student_b_ref with
           | Some npc -> npc#tag#set (InScene Scene.Menu)
           | None -> ());
        if s3_done then
          (match !classroom_student_c_ref with
           | Some npc -> npc#tag#set (InScene Scene.Menu)
           | None -> ());
        if s1_done && s2_done && s3_done then begin
          students_exit_phase := 2;
          lambda_return_started := true
        end
    | 2 ->
        if !lambda_return_started then begin
          let lambda_done = move_npc_ref_to_target professor_lambda_ref lambda_post_target 1.5 in
          if lambda_done then begin
            (match !professor_lambda_ref with
             | Some lambda -> set_npc_sprite_row lambda sprite_row_down
             | None -> ());
            students_exit_started := false;
            students_exit_phase := 0;
            lambda_return_started := false;
            student_a_post_target := None;
            student_b_post_target := None;
            student_c_post_target := None;
            lambda_post_target := None
          end
        end
    | _ -> ()
  end;
  if not global.classroom_scripted_movement_active then
    global.classroom_scripted_movement_active <- false;
  if Scene.current () <> Scene.Classroom then
    ()
  else if global.classroom_intro_completed && not !classroom_post_success_started then begin
    classroom_seating_started := true;
    classroom_seating_done := true;
    school_intro_started := true;
    school_intro_done := true;
    school_challenge_started := true;
    school_challenge_completed := true
  end
  else begin
    if !classroom_post_success_started then begin
      (match !classroom_post_phase with
       | 1 ->
           if not global.dialogue_state.active then begin
             let player_name =
               if String.length global.player_name > 0 then global.player_name else "Apprenti"
             in
             Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
               { speaker = "Professeur Lambda"; text = player_name ^ ", viens me voir quand tu seras pret !" };
             ]);
             classroom_post_phase := 2;
             classroom_post_move_ticks := 0
           end
       | 2 ->
           incr classroom_post_move_ticks;
           let aerin_done = move_npc_ref_via_waypoints ~arrival_row:sprite_row_down aerin_ref aerin_route_waypoints aerin_post_target 2.0 in
           if (not global.dialogue_state.active) then begin
             (match !aerin_ref, !aerin_post_target with
              | Some npc, Some (x, y) when aerin_done ->
                  npc#position#set Vector.{x; y};
                  set_npc_sprite_row npc sprite_row_down
              | _ -> ());
             classroom_post_phase := 3;
             classroom_post_move_ticks := 0
           end else if aerin_done || !classroom_post_move_ticks >= 220 then begin
             (match !aerin_ref, !aerin_post_target with
              | Some npc, Some (x, y) ->
                  npc#position#set Vector.{x; y};
                  set_npc_sprite_row npc sprite_row_down
              | _ -> ());
              aerin_route_waypoints := [];
           end
       | 3 ->
           global.player#velocity#set Vector.zero;
           incr classroom_post_move_ticks;
            let aerin_done = move_npc_ref_via_waypoints ~arrival_row:sprite_row_down aerin_ref aerin_route_waypoints aerin_post_target 2.0 in
            let lambda_done = move_npc_ref_via_waypoints ~arrival_row:sprite_row_down professor_lambda_ref lambda_route_waypoints lambda_post_target 1.8 in
           let start_students = !classroom_post_move_ticks >= 6 in
            let s1_done = if start_students then move_npc_ref_via_waypoints ~arrival_row:sprite_row_left classroom_student_a_ref student_a_route_waypoints student_a_post_target 1.8 else false in
            let s2_done = if start_students then move_npc_ref_via_waypoints ~arrival_row:sprite_row_left classroom_student_b_ref student_b_route_waypoints student_b_post_target 1.8 else false in
            let s3_done = if start_students then move_npc_ref_via_waypoints ~arrival_row:sprite_row_left classroom_student_c_ref student_c_route_waypoints student_c_post_target 1.8 else false in
            if aerin_done && lambda_done && s1_done && s2_done && s3_done then begin
             orient_classroom_bench_students_right ();
             classroom_post_success_started := false;
             classroom_post_phase := 0;
             classroom_post_move_ticks := 0;
             global.classroom_intro_completed <- true;
             school_intro_done := true
           end else if !classroom_post_move_ticks >= 420 then begin
              (match !aerin_ref, !aerin_post_target with
               | Some npc, Some (x, y) ->
                 npc#position#set Vector.{x; y};
                 set_npc_sprite_row npc sprite_row_down
               | _ -> ());
             (match !professor_lambda_ref, !lambda_post_target with
              | Some npc, Some (x, y) ->
                npc#position#set Vector.{x; y};
                set_npc_sprite_row npc sprite_row_down
              | _ -> ());
             (match !classroom_student_a_ref, !student_a_post_target with
              | Some npc, Some (x, y) ->
                npc#position#set Vector.{x; y};
                set_npc_sprite_row npc sprite_row_right
              | _ -> ());
             (match !classroom_student_b_ref, !student_b_post_target with
              | Some npc, Some (x, y) ->
                npc#position#set Vector.{x; y};
                set_npc_sprite_row npc sprite_row_right
              | _ -> ());
             (match !classroom_student_c_ref, !student_c_post_target with
              | Some npc, Some (x, y) ->
                npc#position#set Vector.{x; y};
                set_npc_sprite_row npc sprite_row_right
              | _ -> ());
             orient_classroom_bench_students_right ();
             classroom_post_success_started := false;
             classroom_post_phase := 0;
             classroom_post_move_ticks := 0;
             global.classroom_intro_completed <- true;
             school_intro_done := true
           end
       | _ -> ())
    end
    else if not !classroom_seating_started then begin
      classroom_seating_started := true;
      classroom_seating_done := false;
      let seat_target = choose_classroom_seat_target (global.player#position#get) in
      classroom_seat_target := Some seat_target;
      let player_pos = global.player#position#get in
      classroom_seat_waypoints := [ (player_pos.Vector.x, snd seat_target) ];
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
        let reached_target =
          match !classroom_seat_waypoints with
          | (wx, wy) :: rest ->
              let reached_waypoint = move_player_toward_auto global.player wx wy 2.1 in
              if reached_waypoint then classroom_seat_waypoints := rest;
              false
          | [] -> move_player_toward_auto global.player target_x target_y 2.1
        in
        if reached_target then begin
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
          if !classroom_seating_stuck_ticks >= 45 || !classroom_seating_total_ticks >= 360 then begin
            let player_pos = global.player#position#get in
            classroom_seat_waypoints := [ (player_pos.Vector.x, target_y) ];
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
              global.classroom_intro_completed <- false;
              (match !aerin_ref with
               | Some aerin ->
                   aerin#tag#set (InScene Scene.Classroom);
                   set_npc_sprite_row aerin sprite_row_down
               | None -> ());
              Dialogue.start_dialogue global.dialogue_state (Dialogue.create_dialogue [
                { speaker = "???"; text = "Serieusement ? C'etait ca ton sort ?" };
                { speaker = "Narration"; text = "Un etudiant aux vetements luxueux se tourne vers vous." };
                { speaker = "Aerin"; text = "Moi j'ai appris ca quand j'avais huit ans." };
                { speaker = "Aerin"; text = "L'Academie devient vraiment trop facile..." };
                { speaker = "Professeur Lambda"; text = "Aerin." };
                { speaker = "Professeur Lambda"; text = "Peut-etre aimerais-tu montrer l'exemple ?" };
                { speaker = "Narration"; text = "Aerin sourit." };
              ]);
              start_classroom_post_success_sequence global
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
  let current_scene = Scene.current () in
  (match !previous_scene with
   | Some Scene.Classroom when current_scene <> Scene.Classroom ->
       classroom_students_hidden_after_exit := true
   | _ -> ());
  previous_scene := Some current_scene;
  if current_scene = Scene.Classroom && !classroom_students_hidden_after_exit then begin
    (match !classroom_student_a_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ());
    (match !classroom_student_b_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ());
    (match !classroom_student_c_ref with Some npc -> npc#tag#set (InScene Scene.Menu) | None -> ())
  end;
  update_knight_patrol ();
  update_town_npc_facing ();
  update_school_students_event ();
  update_school_intro_event ()
