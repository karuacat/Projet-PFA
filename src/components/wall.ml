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
  let cols = 12 in
  let rows = 9 in
  let layout = [|
    "NNNNNNNNNNNN";
    "NNNNNNNNNNNN";
    "NNNNNNNNNNNN";
    "NNNOOOOOONNN";
    "NNOOOONOOOON";
    "NOOOONNNOOON";
    "NOOOONNNOOON";
    "NNOOOOOOOONN";
    "NNNNNOONNNNN";
  |] in
  let cell_x c = Cst.house_offset_x + (c * Cst.house_width) / cols in
  let cell_y r = Cst.house_offset_y + (r * Cst.house_height) / rows in
  let cell_w c = (Cst.house_offset_x + ((c + 1) * Cst.house_width) / cols) - cell_x c in
  let cell_h r = (Cst.house_offset_y + ((r + 1) * Cst.house_height) / rows) - cell_y r in
  let blockers = ref [] in

  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if layout.(r).[c] = 'N' then
        blockers := wall (cell_x c, cell_y r, cell_w c, cell_h r, true, Texture.transparent, Scene.House) :: !blockers
    done
  done;

  let house_right = Cst.house_offset_x + Cst.house_width in
  let house_bottom = Cst.house_offset_y + Cst.house_height in

  blockers := wall (0, 0, Cst.window_width, Cst.house_offset_y, true, Texture.transparent, Scene.House) :: !blockers;
  blockers := wall (0, house_bottom, Cst.window_width, Cst.window_height - house_bottom, true, Texture.transparent, Scene.House) :: !blockers;
  blockers := wall (0, Cst.house_offset_y, Cst.house_offset_x, Cst.house_height, true, Texture.transparent, Scene.House) :: !blockers;
  blockers := wall (house_right, Cst.house_offset_y, Cst.window_width - house_right, Cst.house_height, true, Texture.transparent, Scene.House) :: !blockers;

  List.rev !blockers

let town_walls () =
  let layout = Town_map.layout in
  let blockers = ref [] in
  for r = 0 to Cst.town_rows - 1 do
    for c = 0 to Cst.town_cols - 1 do
      if layout.(r).[c] = 'N' then
        blockers :=
          wall (Town_map.cell_x c, Town_map.cell_y r, Cst.town_cell_w, Cst.town_cell_h, true, Texture.transparent, Scene.Town)
          :: !blockers
    done
  done;
  let border = 4 in
  blockers := wall (0, Cst.window_height - border, Cst.window_width, border, true, Texture.transparent, Scene.Town) :: !blockers;
  blockers := wall (0, 0, border, Cst.window_height, false, Texture.transparent, Scene.Town) :: !blockers;
  blockers := wall (Cst.window_width - border, 0, border, Cst.window_height, false, Texture.transparent, Scene.Town) :: !blockers;
  blockers := wall (0, 0, Cst.window_width, border, true, Texture.transparent, Scene.Town) :: !blockers;
  List.rev !blockers

let school_walls () =
  let layout = School_map.layout in
  let blockers = ref [] in
  for r = 0 to Cst.school_rows - 1 do
    for c = 0 to Cst.school_cols - 1 do
      if layout.(r).[c] = 'N' then
        blockers :=
          wall (School_map.cell_x c, School_map.cell_y r, Cst.school_cell_w, Cst.school_cell_h, true, Texture.transparent, Scene.School)
          :: !blockers
    done
  done;
  List.rev !blockers