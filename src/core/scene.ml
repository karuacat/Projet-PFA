type scene =
  | Menu
  | CharacterCreation
  | House
  | Town
  | Academy

let scene_ref : scene ref = ref Menu

let current () = !scene_ref

let set_scene s = scene_ref := s

let get_scene_dimensions () =
  match !scene_ref with
  | Menu -> (Cst.window_width, Cst.window_height)
  | CharacterCreation -> (Cst.window_width, Cst.window_height)
  | House -> (Cst.window_width / 2, Cst.window_height / 2)
  | Town -> (Cst.window_width, Cst.window_height)
  | Academy -> (Cst.window_width, Cst.window_height)
