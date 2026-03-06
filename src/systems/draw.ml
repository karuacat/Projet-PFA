open Ecs
open Component_defs
open Cst

type t = drawable

let init _ = ()

let white = Gfx.color 255 255 255 255
let black = Gfx.color 0 0 0 255
let house_background : Gfx.surface Gfx.resource option ref = ref None
let town_background : Gfx.surface Gfx.resource option ref = ref None
let school_background : Gfx.surface Gfx.resource option ref = ref None

let get_house_background ctx =
  match !house_background with
  | Some res -> res
  | None ->
      let res = Gfx.load_image ctx "ressources/scenes/House.png" in
      house_background := Some res;
      res

let get_town_background ctx =
  match !town_background with
  | Some res -> res
  | None ->
      let res = Gfx.load_image ctx "ressources/scenes/Town.png" in
      town_background := Some res;
      res

let get_school_background ctx =
  match !school_background with
  | Some res -> res
  | None ->
      let res = Gfx.load_image ctx "ressources/scenes/School.png" in
      school_background := Some res;
      res

let should_draw entity =
  let current_scene = Scene.current () in
  try
    match entity#tag#get with
    | Component_defs.InScene scene -> scene = current_scene
    | Door door_config -> door_config.current_scene = current_scene
    | _ -> true
  with _ -> true

let draw_player ctx surface (pos : Vector.t) txt =
  let draw_pos = Vector.{
    x = pos.x +. float_of_int Cst.player_render_offset_x;
    y = pos.y +. float_of_int Cst.player_render_offset_y;
  } in
  let draw_box = Rect.{
    width = Cst.player_render_width;
    height = Cst.player_render_height;
  } in
  Texture.draw ctx surface draw_pos draw_box txt

let update _dt el =
  let Global.{window; ctx; _} = Global.get () in
  let surface = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  let current_scene = Scene.current () in
  let bg = match current_scene with
    | Scene.Menu -> black
    | Scene.CharacterCreation -> black
    | Scene.House -> black
    | Scene.Town -> white
    | Scene.School -> white
  in
  Gfx.set_color ctx bg;
  Gfx.fill_rect ctx surface 0 0 ww wh;
  (match current_scene with
   | Scene.Menu -> ()
   | Scene.CharacterCreation -> ()
   | Scene.House ->
       let house_bg = get_house_background ctx in
       (match Gfx.get_resource_opt house_bg with
       | Some img -> Gfx.blit_scale ctx surface img Cst.house_offset_x Cst.house_offset_y Cst.house_width Cst.house_height
      | None -> ())
     | Scene.Town ->
       let town_bg = get_town_background ctx in
       (match Gfx.get_resource_opt town_bg with
       | Some img -> Gfx.blit_scale ctx surface img 0 0 Cst.window_width Cst.window_height
       | None -> ())
       | Scene.School ->
         let school_bg = get_school_background ctx in
         (match Gfx.get_resource_opt school_bg with
         | Some img -> Gfx.blit_scale ctx surface img 0 0 Cst.window_width Cst.window_height
         | None -> ()));
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
  let drawables =
    el
    |> Seq.filter should_draw
    |> List.of_seq
  in
  let z_of (e:t) =
    let pos = e#position#get in
    let box = e#box#get in
    pos.Vector.y +. float_of_int box.Rect.height
  in
  let drawables = List.sort (fun a b -> Float.compare (z_of a) (z_of b)) drawables in
  List.iter (fun (e:t) ->
    let pos = e#position#get in
    let box = e#box#get in
    let txt = e#texture#get in
    match e#tag#get with
    | Player -> draw_player ctx surface pos txt
    | _ -> Texture.draw ctx surface pos box txt
  ) drawables