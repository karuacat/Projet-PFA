open Component_defs
open System_defs

let wall (x, y, width, height, horiz, texture, scene) =
  let e = new wall () in
  e#texture#set texture;
  e#position#set Vector.{x = float x; y = float y};
  e#tag#set (InScene scene);
  e#box#set Rect.{width; height};
  e#resolve#set (fun v t ->
    match t#tag#get with
      | Player ->
          ()
      | _ -> ()
    );
  Draw_system.(register (e :> t));
  Collision_system.(register (e :> t));
  e

let house_walls () =
  let blockers = ref [] in
  List.iter
    (fun (x, y, w, h) ->
      blockers :=
        wall (x, y, w, h, true, Texture.transparent, Scene.House)
        :: !blockers)
    (House_map.collision_rects ());
  List.rev !blockers

let town_walls () =
  let blockers = ref [] in
  List.iter
    (fun (x, y, w, h) ->
      blockers :=
        wall (x, y, w, h, true, Texture.transparent, Scene.Town)
        :: !blockers)
    (Town_map.collision_rects ());
  List.rev !blockers

let school_walls () =
  let blockers = ref [] in
  List.iter
    (fun (x, y, w, h) ->
      blockers :=
        wall (x, y, w, h, true, Texture.transparent, Scene.School)
        :: !blockers)
    (School_map.collision_rects ());
  List.rev !blockers

let library_walls () =
  let blockers = ref [] in
  List.iter
    (fun (x, y, w, h) ->
      blockers :=
        wall
          ( x,
            y,
            w,
            h,
            true,
            Texture.transparent,
            Scene.Library )
        :: !blockers)
    (Library_map.collision_rects ());
  List.rev !blockers

let classroom_walls () =
  let blockers = ref [] in
  List.iter
    (fun (x, y, w, h) ->
      blockers :=
        wall
          ( x,
            y,
            w,
            h,
            true,
            Texture.transparent,
            Scene.Classroom )
        :: !blockers)
    (Classroom_map.collision_rects ());
  List.rev !blockers