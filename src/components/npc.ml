open Component_defs
open Ecs
open System_defs

let create_npc ?texture name x y (npc_data : npc_data) scene =
  let npc = object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit tagged ()
    inherit resolver ()
    inherit npc_component ()
  end in
  
  npc#position#set Vector.{x = float_of_int x; y = float_of_int y};
  let (width, height) = match name with
    | "Chevalier Gardien" -> (40, 48)
    | "Marchand" -> (40, 48)
    | "Vieille érudite" -> (40, 48)
    | "Élève A" -> (46, 56)
    | "Élève B" -> (46, 56)
    | "Élève" -> (46, 56)
    | _ -> (32, 32)
  in
  npc#box#set Rect.{ width; height };
  (match texture with
   | Some txt -> npc#texture#set txt
   | None ->
       let color = match name with
         | _ -> Gfx.color 200 150 100 255
       in
       npc#texture#set (Texture.Color color));
  npc#tag#set (InScene scene);
  npc#npc_data#set npc_data;
  npc#resolve#set (fun _ _ -> ());
  
  Draw_system.(register (npc :> t));
  Collision_system.register (npc :> Collision.t);
  Interaction.register_npc npc;
  
  npc
