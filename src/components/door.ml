open Ecs
open Component_defs
open System_defs

let door (config, x, y, width, height, txt) =
  let e = new door () in
  e#texture#set txt;
  e#tag#set (Door config);
  e#position#set Vector.{x = float x; y = float y};
  e#box#set Rect.{width; height};
  e#resolve#set (fun v t ->
    match t#tag#get with
      | Player ->
          ()
      | _ -> ()
    );
  Draw_system.(register (e :> t));
  Collision_system.(register (e :> t));
  Door_transition_system.(register (e :> t));
  e

let house_doors () =
  let cols = 12 in
  let rows = 9 in
  let cell_x c = Cst.house_offset_x + (c * Cst.house_width) / cols in
  let cell_y r = Cst.house_offset_y + (r * Cst.house_height) / rows in
  let cell_h r = (Cst.house_offset_y + ((r + 1) * Cst.house_height) / rows) - cell_y r in
  let door_x = cell_x 5 in
  let door_y = cell_y 8 in
  let door_w = (cell_x 7) - door_x in
  let door_h = cell_h 8 in
  let town_spawn_x, town_spawn_y = Town_map.spawn_from_house () in
  [
    door ({
      id = "house_exit";
      current_scene = Scene.House;
      target_scene = Scene.Town;
      player_spawn_x = town_spawn_x;
      player_spawn_y = town_spawn_y;
    }, door_x, door_y, door_w, door_h, Texture.transparent)
  ]

let town_doors () =
  let house_door_x, house_door_y, house_door_w, house_door_h = Town_map.house_door_rect () in
  let academy_door_x, academy_door_y, academy_door_w, academy_door_h = Town_map.academy_door_rect () in
  let school_spawn_x, school_spawn_y = School_map.spawn_from_town () in
  [
    door ({
      id = "town_house_door";
      current_scene = Scene.Town;
      target_scene = Scene.House;
      player_spawn_x = 365;
      player_spawn_y = 380;
    }, house_door_x, house_door_y, house_door_w, house_door_h, Texture.transparent);
    door ({
      id = "town_school_door";
      current_scene = Scene.Town;
      target_scene = Scene.School;
      player_spawn_x = school_spawn_x;
      player_spawn_y = school_spawn_y;
    }, academy_door_x, academy_door_y, academy_door_w, academy_door_h, Texture.transparent)
  ]

let school_doors () =
  let town_door_x, town_door_y, town_door_w, town_door_h = School_map.town_door_rect () in
  [
    door ({
      id = "school_town_door";
      current_scene = Scene.School;
      target_scene = Scene.Town;
      player_spawn_x = 400;
      player_spawn_y = 100;
    }, town_door_x, town_door_y, town_door_w, town_door_h, Texture.transparent)
  ]
