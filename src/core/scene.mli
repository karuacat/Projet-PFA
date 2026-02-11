type scene =
  | Menu
  | CharacterCreation
  | House
  | Town

val current : unit -> scene
val set_scene : scene -> unit
val get_scene_dimensions : unit -> int * int
