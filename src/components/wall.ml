open Component_defs
open System_defs

type tag += HWall of wall| VWall of wall

let wall (x, y, txt, width, height, horiz) =
  let e = new wall () in
  e#texture#set txt;
  e#position#set Vector.{x = float x; y = float y};
  e#tag#set (if horiz then HWall e else VWall e);
  e#box#set Rect.{width; height};
  Draw_system.(register (e :> t));
  Collision_system.(register (e :> t));
  e

let walls () =
  List.map wall
  Cst.[
    (hwall1_x, hwall1_y, Texture.transparent, hwall_width, hwall_height, true);
    (hwall2_x, hwall2_y, Texture.transparent, hwall_width, hwall_height, true);
    (vwall1_x, vwall1_y, Texture.transparent, vwall_width, vwall_height, false);
    (vwall2_x, vwall2_y, Texture.transparent, vwall_width, vwall_height, false);
  ]