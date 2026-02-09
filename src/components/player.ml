open Ecs
open Component_defs
open System_defs

let player (name, x, y, txt, width, height) =
  let e = new player name in
  e#texture#set txt;
  e#tag#set Player;
  e#position#set Vector.{x = float x; y = float y};
  e#box#set Rect.{width; height};
  e#velocity#set Vector.zero;
  e#resolve#set (fun v t ->
    match t#tag#get with
      | InScene scene when scene = Scene.current () ->
          e#velocity#set Vector.zero;
          e#position#set (Vector.add e#position#get v)
        | Door _ ->
          ()     
      | _ -> ()
    );
  Move_system.(register (e :> t));
  Collision_system.(register (e :> t));
  Draw_system.(register (e :> t));
  e

let players () =
  player Cst.("player", player_start_x, player_start_y, player_color, player_width, player_height)

let player () =
  let Global.{player; _} = Global.get () in
  player

let stop_player () =
  let Global.{player; _} = Global.get () in
  player#velocity#set Vector.zero

let move_player player v =
  player#velocity#set v