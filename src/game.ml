open System_defs
open Component_defs
open Ecs

let update dt =
  let () = Player.stop_player () in
  let () = Input.handle_input () in
  Move_system.update dt;
  Collision_system.update dt;
  Door_transition_system.update dt;
  Draw_system.update dt;
  None

let run () =
  let window_spec =
    Format.sprintf "game_canvas:%dx%d:"
      Cst.window_width Cst.window_height
  in
  let window = Gfx.create window_spec in
  let ctx = Gfx.get_context window in

  let _house_walls = Wall.house_walls () in
  let _town_walls = Wall.town_walls () in
  let _town_house = Wall.town_house_building () in
  let _house_doors = Door.house_doors () in
  let _town_doors = Door.town_doors () in
  
  let player = Player.players () in
  let global = Global.{ window; ctx; player; waiting = 0; } in
  Global.set global;
  Gfx.main_loop update (fun () -> ())