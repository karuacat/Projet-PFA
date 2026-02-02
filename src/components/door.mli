type door_config = {
  id: string;
  current_scene: Scene.scene;
  target_scene: Scene.scene;
  player_spawn_x: int;
  player_spawn_y: int;
}

val house_doors : unit -> Component_defs.door list
val town_doors : unit -> Component_defs.door list
