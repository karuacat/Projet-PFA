type door_config = {
  id: string;
  current_scene: Scene.scene;
  target_scene: Scene.scene;
  player_spawn_x: int;
  player_spawn_y: int;
}

type Component_defs.tag += Door of door_config

type t = Component_defs.door

val init : float -> unit
val update : float -> t Seq.t -> unit
