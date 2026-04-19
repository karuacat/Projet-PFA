type legacy_data = {
  scene : Scene.scene;
  player_x : float;
  player_y : float;
  player_name : string;
  house_exit_attempted : bool;
  has_secret_book : bool;
  chest_challenge_completed : bool;
  knight_challenge_completed : bool;
  skin : string;
  school_students_event_completed : bool;
  classroom_intro_completed : bool;
  lambda_duel_started : bool;
  lambda_duel_stage : int;
  lambda_golem_hp : int;
  lambda_golem_hp_visible : bool;
  lambda_duel_completed : bool;
  lambda_library_instruction_seen : bool;
  dynamic_magic_cinematic_done : bool;
  library_intro_seen : bool;
}

type data = {
  scene : Scene.scene;
  player_x : float;
  player_y : float;
  player_name : string;
  house_exit_attempted : bool;
  has_secret_book : bool;
  chest_challenge_completed : bool;
  knight_challenge_completed : bool;
  skin : string;
  school_students_event_completed : bool;
  classroom_intro_completed : bool;
  lambda_duel_started : bool;
  lambda_duel_stage : int;
  lambda_golem_hp : int;
  lambda_golem_hp_visible : bool;
  lambda_duel_completed : bool;
  lambda_library_instruction_seen : bool;
  classroom_scripted_movement_active : bool;
  dynamic_magic_cinematic_done : bool;
  library_intro_seen : bool;
}

type file_payload =
  | Save_v2 of data

let save_path = "savegame.dat"

let has_save () = Sys.file_exists save_path

let delete () =
  if has_save () then
    try
      Sys.remove save_path;
      true
    with _ ->
      false
  else
    true

let save (global : Global.t) =
  let player_pos = global.player#position#get in
  let payload = Save_v2 {
    scene = Scene.current ();
    player_x = player_pos.x;
    player_y = player_pos.y;
    player_name = global.player_name;
    house_exit_attempted = global.house_exit_attempted;
    has_secret_book = global.has_secret_book;
    chest_challenge_completed = global.chest_challenge_completed;
    knight_challenge_completed = global.knight_challenge_completed;
    skin = Player.get_skin_tag ();
    school_students_event_completed = global.school_students_event_completed;
    classroom_intro_completed = global.classroom_intro_completed;
    lambda_duel_started = global.lambda_duel_started;
    lambda_duel_stage = global.lambda_duel_stage;
    lambda_golem_hp = global.lambda_golem_hp;
    lambda_golem_hp_visible = global.lambda_golem_hp_visible;
    lambda_duel_completed = global.lambda_duel_completed;
    lambda_library_instruction_seen = global.lambda_library_instruction_seen;
    classroom_scripted_movement_active = global.classroom_scripted_movement_active;
    dynamic_magic_cinematic_done = global.dynamic_magic_cinematic_done;
    library_intro_seen = global.library_intro_seen;
  }
  in
  try
    let out_channel = open_out_bin save_path in
    Marshal.to_channel out_channel payload [];
    close_out out_channel;
    true
  with _ ->
    false

let load () =
  if not (has_save ()) then
    None
  else
    try
      let in_channel = open_in_bin save_path in
      let loaded : file_payload = Marshal.from_channel in_channel in
      close_in in_channel;
      match loaded with
      | Save_v2 payload -> Some payload
    with _ ->
      (* Backward compatibility with old unversioned saves. *)
      try
        let in_channel = open_in_bin save_path in
        let payload : legacy_data = Marshal.from_channel in_channel in
        close_in in_channel;
        Some {
          scene = payload.scene;
          player_x = payload.player_x;
          player_y = payload.player_y;
          player_name = payload.player_name;
          house_exit_attempted = payload.house_exit_attempted;
          has_secret_book = payload.has_secret_book;
          chest_challenge_completed = payload.chest_challenge_completed;
          knight_challenge_completed = payload.knight_challenge_completed;
          skin = payload.skin;
          school_students_event_completed = payload.school_students_event_completed;
          classroom_intro_completed = payload.classroom_intro_completed;
          lambda_duel_started = payload.lambda_duel_started;
          lambda_duel_stage = payload.lambda_duel_stage;
          lambda_golem_hp = payload.lambda_golem_hp;
          lambda_golem_hp_visible = payload.lambda_golem_hp_visible;
          lambda_duel_completed = payload.lambda_duel_completed;
          lambda_library_instruction_seen = payload.lambda_library_instruction_seen;
          classroom_scripted_movement_active = false;
          dynamic_magic_cinematic_done = payload.dynamic_magic_cinematic_done;
          library_intro_seen = payload.library_intro_seen;
        }
      with _ ->
        None

let apply_to_global (global : Global.t) payload =
  Scene.set_scene payload.scene;
  global.player#position#set Vector.{ x = payload.player_x; y = payload.player_y };
  global.player_name <- payload.player_name;
  global.house_exit_attempted <- payload.house_exit_attempted;
  global.has_secret_book <- payload.has_secret_book;
  global.chest_challenge_completed <- payload.chest_challenge_completed;
  global.knight_challenge_completed <- payload.knight_challenge_completed;
  global.school_students_event_completed <- payload.school_students_event_completed;
  global.classroom_intro_completed <- payload.classroom_intro_completed;
  global.lambda_duel_started <- payload.lambda_duel_started;
  global.lambda_duel_stage <- payload.lambda_duel_stage;
  global.lambda_golem_hp <- payload.lambda_golem_hp;
  global.lambda_golem_hp_visible <- payload.lambda_golem_hp_visible;
  global.lambda_duel_completed <- payload.lambda_duel_completed;
  global.lambda_library_instruction_seen <- payload.lambda_library_instruction_seen;
  global.classroom_scripted_movement_active <- payload.classroom_scripted_movement_active;
  global.dynamic_magic_cinematic_done <- payload.dynamic_magic_cinematic_done;
  global.dynamic_magic_cinematic_active <- false;
  global.dynamic_magic_phase <- 0;
  global.dynamic_magic_timer <- 0.0;
  global.dynamic_magic_pending_target_scene <- None;
  global.library_intro_seen <- payload.library_intro_seen;
  Library_guide.close_panel global.library_guide_state;
  Player.set_skin_tag payload.skin;
  Player.refresh_player_sprite global.player
