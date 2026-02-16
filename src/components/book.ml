open Component_defs
open Ecs
open System_defs

let create_book x y scene =
  let book = object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit tagged ()
    inherit resolver ()
    inherit sign_component ()
  end in
  
  book#position#set Vector.{x = float_of_int x; y = float_of_int y};
  book#box#set Rect.{ width = 24; height = 20 };
  book#texture#set (Texture.Color (Gfx.color 200 100 50 255));
  book#tag#set (InScene scene);
  book#sign_data#set { 
    title = "Livre Ancien"; 
    text = "Les Fondations de l'Art des Symboles"; 
    scene;
  };
  book#resolve#set (fun _ _ -> ());
  
  Draw_system.(register (book :> t));
  Collision_system.register (book :> Collision.t);
  Interaction.register_sign book;
  
  book
