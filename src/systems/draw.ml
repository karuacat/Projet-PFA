open Ecs
open Component_defs
open Cst

type t = drawable

let init _ = ()

let white = Gfx.color 255 255 255 255
let black = Gfx.color 0 0 0 255

let should_draw entity =
  let current_scene = Scene.current () in
  try
    match entity#tag#get with
    | Component_defs.InScene scene -> scene = current_scene
    | Door door_config -> door_config.current_scene = current_scene
    | _ -> true
  with _ -> true

let update _dt el =
  let Global.{window; ctx; _} = Global.get () in
  let surface = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  let current_scene = Scene.current () in
  let bg = match current_scene with
    | Scene.House -> black
    | Scene.Town -> white
  in
  Gfx.set_color ctx bg;
  Gfx.fill_rect ctx surface 0 0 ww wh;
  (match current_scene with
   | Scene.House ->
       let house_width = Cst.window_width / 2 in
       let house_height = Cst.window_height / 2 in
       let offset_x = Cst.window_width / 4 in
       let offset_y = Cst.window_height / 4 in
       Gfx.set_color ctx white;
       Gfx.fill_rect ctx surface offset_x offset_y house_width house_height
   | Scene.Town -> ());
  if Cst.debug_draw_grid then begin
    let cell = Cst.aabb_cell_size in
    let grid_color = Gfx.color 180 180 180 120 in
    Gfx.set_color ctx grid_color;
    let x = ref 0 in
    while !x <= ww do
      Gfx.fill_rect ctx surface !x 0 1 wh;
      x := !x + cell
    done;
    let y = ref 0 in
    while !y <= wh do
      Gfx.fill_rect ctx surface 0 !y ww 1;
      y := !y + cell
    done
  end;
  Seq.iter (fun (e:t) ->
    if should_draw e then (
      let pos = e#position#get in
      let box = e#box#get in
      let txt = e#texture#get in
      Texture.draw ctx surface pos box txt
    )
  ) el