open Component_defs
open Ecs
open System_defs

let create_npc name x y (npc_data : npc_data) scene =
  let npc = object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit tagged ()
    inherit npc_component ()
  end in
  
  npc#position#set Vector.{x = float_of_int x; y = float_of_int y};
  npc#box#set Rect.{ width = 32; height = 32 };
  npc#texture#set (Texture.Color (Gfx.color 200 150 100 255));
  npc#tag#set (InScene scene);
  npc#npc_data#set npc_data;
  
  Draw_system.(register (npc :> t));
  Interaction.register_npc npc;
  
  npc
