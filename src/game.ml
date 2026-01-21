open System_defs
open Component_defs
open Ecs

let update dt =
  let () = Player.stop_player () in
  let () = Input.handle_input () in
  Move_system.update dt;
  Collision_system.update dt;
  Draw_system.update dt;
  None

let run () =
  let window_spec =
    Format.sprintf "game_canvas:%dx%d:" Cst.window_width Cst.window_height
  in
  let window = Gfx.create window_spec in
  let ctx = Gfx.get_context window in
  let _walls = Wall.walls () in
  let player = Player.players () in
  let global = Global.{ window; ctx; player; waiting = 0; } in
  Global.set global;
  Gfx.main_loop update (fun () -> ())