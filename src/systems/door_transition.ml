open Ecs
open Component_defs

type door_config = {
  id: string;
  current_scene: Scene.scene;
  target_scene: Scene.scene;
  player_spawn_x: int;
  player_spawn_y: int;
}

type tag += Door of door_config

type t = Component_defs.door

let init _ = ()

let check_door_collision doors =
  let Global.{player; _} = Global.get () in
  let p_pos : Vector.t = player#position#get in
  let p_box : Rect.t = player#box#get in
  
  let player_rect : Rect.rect_f = {
    x = p_pos.x;
    y = p_pos.y;
    width = float p_box.width;
    height = float p_box.height;
  } in
  
  Seq.iter (fun (e : t) ->
    try
      match e#tag#get with
      | Door door_config ->
          if door_config.current_scene = Scene.current () then (
            let d_pos : Vector.t = e#position#get in
            let d_box : Rect.t = e#box#get in
            let door_rect : Rect.rect_f = {
              x = d_pos.x;
              y = d_pos.y;
              width = float d_box.width;
              height = float d_box.height;
            } in
            
            if Rect.collides player_rect door_rect then (
              Scene.set_scene door_config.target_scene;
              player#position#set Vector.{
                x = float door_config.player_spawn_x;
                y = float door_config.player_spawn_y;
              }
            )
          )
      | _ -> ()
    with _ -> ()
  ) doors

let update _dt doors =
  check_door_collision doors
