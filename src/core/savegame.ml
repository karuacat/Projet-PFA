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
}

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
  let payload = {
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
      let payload : data = Marshal.from_channel in_channel in
      close_in in_channel;
      Some payload
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
  Player.set_skin_tag payload.skin;
  Player.refresh_player_sprite global.player
