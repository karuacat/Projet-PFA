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
let library_background : Gfx.surface Gfx.resource option ref = ref None
let classroom_background : Gfx.surface Gfx.resource option ref = ref None

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

let get_library_background ctx =
  match !library_background with
  | Some res -> res
  | None ->
      let res = Gfx.load_image ctx "ressources/scenes/Library.png" in
      library_background := Some res;
      res

let get_classroom_background ctx =
  match !classroom_background with
  | Some res -> res
  | None ->
  let res = Gfx.load_image ctx "ressources/scenes/Classroom.png" in
  classroom_background := Some res;
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
    | Scene.Library -> white
    | Scene.Classroom -> white
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
         | None -> ())
      | Scene.Library ->
        let library_bg = get_library_background ctx in
        (match Gfx.get_resource_opt library_bg with
        | Some img -> Gfx.blit_scale ctx surface img 0 0 Cst.window_width Cst.window_height
        | None -> ())
      | Scene.Classroom ->
         let classroom_bg = get_classroom_background ctx in
         (match Gfx.get_resource_opt classroom_bg with
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
  let collision_rects_for_scene =
    match current_scene with
    | Scene.House -> House_map.collision_rects ()
    | Scene.Town -> Town_map.collision_rects ()
    | Scene.School -> School_map.collision_rects ()
    | Scene.Library -> Library_map.collision_rects ()
    | Scene.Classroom -> Classroom_map.collision_rects ()
    | _ -> []
  in
  if Cst.debug_draw_classroom_collisions && collision_rects_for_scene <> [] then begin
    List.iter
      (fun (x, y, w, h) ->
        Gfx.set_color ctx (Gfx.color 255 80 80 80);
        Gfx.fill_rect ctx surface x y w h;
        Gfx.set_color ctx (Gfx.color 255 120 120 220);
        Gfx.fill_rect ctx surface x y w 1;
        Gfx.fill_rect ctx surface x (y + h - 1) w 1;
        Gfx.fill_rect ctx surface x y 1 h;
        Gfx.fill_rect ctx surface (x + w - 1) y 1 h)
      collision_rects_for_scene
  end;
  if Cst.debug_draw_npc_navigation && (current_scene = Scene.School || current_scene = Scene.Classroom) then begin
    List.iter
      (fun (dbg : Navigation_debug.snapshot) ->
        if dbg.scene = current_scene then begin
          List.iter
            (fun (px, py) ->
              Gfx.set_color ctx (Gfx.color 80 170 255 180);
              Gfx.fill_rect ctx surface (int_of_float px + 10) (int_of_float py + 14) 4 4)
            dbg.path_points;
          (match dbg.waypoint with
           | Some (wx, wy) ->
               Gfx.set_color ctx (Gfx.color 255 170 70 210);
               Gfx.fill_rect ctx surface (int_of_float wx + 8) (int_of_float wy + 12) 8 8
           | None -> ());
          let tx, ty = dbg.target in
          Gfx.set_color ctx (Gfx.color 70 220 220 220);
          Gfx.fill_rect ctx surface (int_of_float tx + 8) (int_of_float ty + 12) 8 8;
          let rx, ry, rw, rh = dbg.npc_rect in
          let box_color =
            match dbg.blocked_reason with
            | None -> Gfx.color 90 220 120 220
            | Some "player" -> Gfx.color 245 210 80 220
            | Some "bounds" -> Gfx.color 255 90 180 220
            | Some "obstacle" -> Gfx.color 255 100 100 220
            | _ -> Gfx.color 190 190 190 220
          in
          Gfx.set_color ctx box_color;
          Gfx.fill_rect ctx surface rx ry rw 1;
          Gfx.fill_rect ctx surface rx (ry + rh - 1) rw 1;
          Gfx.fill_rect ctx surface rx ry 1 rh;
          Gfx.fill_rect ctx surface (rx + rw - 1) ry 1 rh
        end)
      (Navigation_debug.all ())
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