open Ecs
open Component_defs
open System_defs

type door_config = {
  id: string; 
  current_scene: Scene.scene;
  target_scene: Scene.scene;
  player_spawn_x: int;
  player_spawn_y: int;
}

let door (config, x, y, width, height, txt) =
  let e = new door () in
  e#texture#set txt;
  e#tag#set (Door_transition.Door config);
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
  let house_width = Cst.window_width / 2 in
  let house_height = Cst.window_height / 2 in
  let offset_x = Cst.window_width / 4 in
  let offset_y = Cst.window_height / 4 in
  [
    door ({
      id = "house_exit";
      current_scene = Scene.House;
      target_scene = Scene.Town;
      player_spawn_x = 460;
      player_spawn_y = 290;
    }, offset_x + house_width - 10, offset_y + house_height / 2 - 30, 10, 60, Texture.brown)
  ]

let town_doors () =
  let small_size = 80 in
  let offset_x = (Cst.window_width / 2) - (small_size / 2) in
  let offset_y = (Cst.window_height / 2) - (small_size / 2) in
  let door_x = offset_x + small_size in
  let door_y = offset_y + (small_size / 2) - 30 in
  [
    door ({
      id = "town_house_door";
      current_scene = Scene.Town;
      target_scene = Scene.House;
      player_spawn_x = 540;
      player_spawn_y = 280;
    }, door_x, door_y, 10, 60, Texture.brown)
  ]
