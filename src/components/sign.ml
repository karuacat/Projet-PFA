open Component_defs
open Ecs
open System_defs

let create_sign x y (sign_data : sign_data) scene =
  let sign = object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit tagged ()
    inherit sign_component ()
  end in
  
  sign#position#set Vector.{x = float_of_int x; y = float_of_int y};
  sign#box#set Rect.{ width = 24; height = 32 };
  sign#texture#set (Texture.Color (Gfx.color 139 90 43 255));
  sign#tag#set (InScene scene);
  sign#sign_data#set sign_data;
  
  Draw_system.(register (sign :> t));
  
  Interaction.register_sign sign;
  
  sign
