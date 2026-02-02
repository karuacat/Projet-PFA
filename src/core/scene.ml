type scene =
  | House
  | Town

let scene_ref : scene ref = ref House

let current () = !scene_ref

let set_scene s = scene_ref := s

let get_scene_dimensions () =
  match !scene_ref with
  | House -> (Cst.window_width / 2, Cst.window_height / 2)
  | Town -> (Cst.window_width, Cst.window_height)
