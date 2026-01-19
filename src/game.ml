open System_defs

open Ecs

let run () =
  let window_spec =
    Format.sprintf "game_canvas:%dx%d" Cst.window_width Cst.window_height
  in
  let window = Gfx.create window_spec in
  let ctx = Gfx.get_context window in
  let global = Global.{ window; ctx; } in
  Global.set global;