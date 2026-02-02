open Component_defs
open System_defs

let wall (x, y, width, height, horiz, scene) =
  let e = new wall () in
  e#texture#set Texture.black;
  e#position#set Vector.{x = float x; y = float y};
  e#tag#set (InScene scene);
  e#box#set Rect.{width; height};
  e#resolve#set (fun v t ->
    match t#tag#get with
      | Player ->
          ()
      | _ -> ()
    );
  Draw_system.(register (e :> t));
  Collision_system.(register (e :> t));
  e

let house_walls () =
  let house_width = Cst.window_width / 2 in
  let house_height = Cst.window_height / 2 in
  let offset_x = Cst.window_width / 4 in
  let offset_y = Cst.window_height / 4 in
  [
    wall (offset_x, offset_y, house_width, 10, true, Scene.House);
    wall (offset_x, offset_y - 10, house_width, 10, true, Scene.House);
    wall (offset_x, offset_y + house_height - 10, house_width, 10, true, Scene.House);
    wall (offset_x, offset_y, 10, house_height, false, Scene.House);
    wall (offset_x + house_width - 10, offset_y, 10, house_height / 2 - 30, false, Scene.House);
    wall (offset_x + house_width - 10, offset_y + house_height / 2 + 30, 10, house_height / 2 - 30, false, Scene.House);
  ]

let town_walls () =
  [
    wall (0, 0, Cst.window_width, 10, true, Scene.Town);
    wall (0, Cst.window_height - 10, Cst.window_width, 10, true, Scene.Town);
    wall (0, 0, 10, Cst.window_height, false, Scene.Town);
    wall (Cst.window_width - 10, 0, 10, Cst.window_height, false, Scene.Town);
  ]

let town_house_building () =
  let small_size = 80 in
  let offset_x = (Cst.window_width / 2) - (small_size / 2) in
  let offset_y = (Cst.window_height / 2) - (small_size / 2) in
  [wall (offset_x, offset_y, small_size, small_size, true, Scene.Town)]