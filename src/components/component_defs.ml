open Ecs

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

type tag = ..
type tag += No_tag | Player

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

class velocity () =
  let r = Component.init Vector.zero in
  object
    method velocity = r
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
  end

class type movable =
  object
    inherit Entity.t 
    inherit position
    inherit velocity
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