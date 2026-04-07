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
  let door_x, door_y, door_w, door_h = House_map.house_exit_rect () in
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
  let house_spawn_x, house_spawn_y = House_map.spawn_from_town () in
  [
    door ({
      id = "town_house_door";
      current_scene = Scene.Town;
      target_scene = Scene.House;
      player_spawn_x = house_spawn_x;
      player_spawn_y = house_spawn_y;
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
  let class_door_x, class_door_y, class_door_w, class_door_h = School_map.class_door_rect () in
  let library_door_x, library_door_y, library_door_w, library_door_h = School_map.library_door_rect () in
  let school_door_x, school_door_y, school_door_w, school_door_h = Classroom_map.school_door_rect () in
  let classroom_spawn_x, classroom_spawn_y = Classroom_map.spawn_from_school () in
  let school_spawn_x, school_spawn_y = School_map.spawn_from_classroom () in
  let library_spawn_x, library_spawn_y = Library_map.spawn_from_school () in
  [
    door ({
      id = "school_town_door";
      current_scene = Scene.School;
      target_scene = Scene.Town;
      player_spawn_x = 400;
      player_spawn_y = 100;
    }, town_door_x, town_door_y, town_door_w, town_door_h, Texture.transparent);
    door ({
      id = "school_classroom_door";
      current_scene = Scene.School;
      target_scene = Scene.Classroom;
      player_spawn_x = classroom_spawn_x;
      player_spawn_y = classroom_spawn_y;
    }, class_door_x, class_door_y, class_door_w, class_door_h, Texture.transparent);
    door ({
      id = "school_library_door";
      current_scene = Scene.School;
      target_scene = Scene.Library;
      player_spawn_x = library_spawn_x;
      player_spawn_y = library_spawn_y;
    }, library_door_x, library_door_y, library_door_w, library_door_h, Texture.transparent);
    door ({
      id = "classroom_school_door";
      current_scene = Scene.Classroom;
      target_scene = Scene.School;
      player_spawn_x = school_spawn_x;
      player_spawn_y = school_spawn_y;
    }, school_door_x, school_door_y, school_door_w, school_door_h, Texture.transparent)
  ]

let library_doors () =
  let school_door_x, school_door_y, school_door_w, school_door_h = Library_map.school_door_rect () in
  let school_spawn_x, school_spawn_y = School_map.spawn_from_library () in
  [
    door ({
      id = "library_school_door";
      current_scene = Scene.Library;
      target_scene = Scene.School;
      player_spawn_x = school_spawn_x;
      player_spawn_y = school_spawn_y;
    }, school_door_x, school_door_y, school_door_w, school_door_h, Texture.transparent)
  ]
