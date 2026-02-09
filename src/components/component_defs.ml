open Ecs

type dialogue_line = {
  speaker : string;
  text : string;
}

type dialogue = {
  lines : dialogue_line list;
  mutable current_line : int;
}

type dialogue_state = {
  mutable active : bool;
  mutable current_dialogue : dialogue option;
}

class position () =
  let r = Component.init Vector.zero in
  object
    method position = r
  end

class box () =
  let r = Component.init Rect.{width = 0; height = 0} in
  object
    method box = r
  end

class texture () =
  let r = Component.init (Texture.Color (Gfx.color 0 0 0 255)) in
  object
    method texture = r
  end

class velocity () =
  let r = Component.init Vector.zero in
  object
    method velocity = r
  end

type npc_data = {
  name : string;
  dialogue : dialogue;
  scene : Scene.scene;
}

type sign_data = {
  title : string;
  text : string;
  scene : Scene.scene;
}

type door_config = {
  id: string;
  current_scene: Scene.scene;
  target_scene: Scene.scene;
  player_spawn_x: int;
  player_spawn_y: int;
}

type tag = ..
type tag += 
  | No_tag 
  | Player 
  | InScene of Scene.scene
  | Door of door_config

class tagged () =
  let r = Component.init No_tag in
  object
    method tag = r
  end

class resolver () =
  let r = Component.init (fun (_ : Vector.t) (_ : tagged) -> ()) in
  object
    method resolve = r
  end

class npc_component () =
  let default_dialogue = { lines = []; current_line = 0 } in
  let r = Component.init { name = ""; dialogue = default_dialogue; scene = Scene.Town } in
  object
    method npc_data = r
  end

class sign_component () =
  let r = Component.init { title = ""; text = ""; scene = Scene.Town } in
  object
    method sign_data = r
  end

class type collidable =
  object
    inherit Entity.t 
    inherit position
    inherit box
    inherit tagged
    inherit resolver
  end

class type drawable =
  object
    inherit Entity.t 
    inherit position
    inherit box
    inherit texture
    inherit tagged
  end

class type movable =
  object
    inherit Entity.t 
    inherit position
    inherit velocity
  end

class type npc_entity =
  object
    inherit Entity.t
    inherit position
    inherit box
    inherit texture
    inherit tagged
    inherit resolver
    inherit npc_component
  end

class type sign_entity =
  object
    inherit Entity.t
    inherit position
    inherit box
    inherit texture
    inherit tagged
    inherit resolver
    inherit sign_component
  end

class player name =
  object
    inherit Entity.t ~name ()
    inherit position ()
    inherit box ()
    inherit tagged ()
    inherit texture ()
    inherit resolver ()
    inherit velocity ()
  end

class wall () =
  object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit tagged ()
    inherit texture ()
    inherit resolver ()
  end

class door () =
  object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit tagged ()
    inherit texture ()
    inherit resolver ()
  end
