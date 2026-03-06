open Ecs
open Component_defs
open System_defs

type direction = Up | Down | Left | Right
type skin = MaleSkin | FemaleSkin

let current_direction = ref Down
let walk_tick = ref 0
let current_skin = ref MaleSkin
let player_sheet_male : Gfx.surface Gfx.resource option ref = ref None
let player_sheet_female : Gfx.surface Gfx.resource option ref = ref None

let get_player_sheet ctx =
  match !current_skin with
  | MaleSkin ->
      (match !player_sheet_male with
       | Some res -> res
       | None ->
           let res = Gfx.load_image ctx "ressources/personnages/PlayerH.png" in
           player_sheet_male := Some res;
           res)
  | FemaleSkin ->
      (match !player_sheet_female with
       | Some res -> res
       | None ->
           let res = Gfx.load_image ctx "ressources/personnages/PlayerF.png" in
           player_sheet_female := Some res;
           res)

let set_skin skin =
  current_skin := skin

let get_skin_tag () =
  match !current_skin with
  | MaleSkin -> "male"
  | FemaleSkin -> "female"

let set_skin_tag tag =
  match String.lowercase_ascii tag with
  | "female" | "f" -> set_skin FemaleSkin
  | _ -> set_skin MaleSkin

let set_skin_from_gender (gender : Character_creation.gender) =
  match gender with
  | Character_creation.Male -> set_skin MaleSkin
  | Character_creation.Female -> set_skin FemaleSkin

let sprite_frame_for direction frame_index =
  let frame_w = 48 in
  let frame_h = 48 in
  let block_x = 0 in
  let block_y = 0 in
  let row =
    match direction with
    | Down -> 0
    | Left -> 1
    | Right -> 2
    | Up -> 3
  in
  let col =
    match frame_index with
    | 0 -> 0
    | 1 -> 1
    | _ -> 2
  in
  (block_x + (col * frame_w), block_y + (row * frame_h), frame_w, frame_h)

let set_player_sprite player frame_index =
  let global = Global.get () in
  let sheet = get_player_sheet global.ctx in
  match Gfx.get_resource_opt sheet with
  | Some img ->
      let sx, sy, sw, sh = sprite_frame_for !current_direction frame_index in
      player#texture#set (Texture.Sprite (img, sx, sy, sw, sh))
  | None ->
      ()

let refresh_player_sprite player =
  set_player_sprite player 1

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
  player#velocity#set Vector.zero;
  set_player_sprite player 1

let move_player player v =
  player#velocity#set v;
  if v.Vector.x > 0.0 then current_direction := Right
  else if v.Vector.x < 0.0 then current_direction := Left
  else if v.Vector.y > 0.0 then current_direction := Down
  else if v.Vector.y < 0.0 then current_direction := Up;
  incr walk_tick;
  let phase =
    if !walk_tick mod 20 < 7 then 0
    else if !walk_tick mod 20 < 14 then 1
    else 2
  in
  set_player_sprite player phase