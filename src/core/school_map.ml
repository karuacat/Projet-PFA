let layout = [|
  "NNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNN";
  "NNNNNNNNNCCNNNNN";
  "NOOOOOOONOOOOOON";
  "NONNNOEOOOOOOONN";
  "NONNNEOEOOOOOONN";
  "NONNNOOOOOOOOONN";
  "NONNNOOOOOOOOONN";
  "NOOOOOOOOOOOOOON";
  "NNNNNNOOONNNNNNN";
  "NNNNNNOOONNNNNNN";
  "NNNNNNTTTNNNNNNN";
|]

let cell_x col = col * Cst.school_cell_w
let cell_y row = row * Cst.school_cell_h

let cell_center col row =
  (cell_x col + (Cst.school_cell_w / 2),
   cell_y row + (Cst.school_cell_h / 2))

let is_blocked col row =
  layout.(row).[col] = 'N'

let marker_cells marker =
  let cells = ref [] in
  for row = 0 to Array.length layout - 1 do
    for col = 0 to String.length layout.(row) - 1 do
      if layout.(row).[col] = marker then
        cells := (col, row) :: !cells
    done
  done;
  List.rev !cells

let rect_for_marker marker default_rect =
  match marker_cells marker with
  | [] -> default_rect
  | (c0, r0) :: rest ->
      let min_c, max_c, min_r, max_r =
        List.fold_left (fun (min_c, max_c, min_r, max_r) (c, r) ->
          (min min_c c, max max_c c, min min_r r, max max_r r)
        ) (c0, c0, r0, r0) rest
      in
      let x = cell_x min_c in
      let y = cell_y min_r in
      let w = (max_c - min_c + 1) * Cst.school_cell_w in
      let h = (max_r - min_r + 1) * Cst.school_cell_h in
      (x, y, w, h)

let town_door_rect () =
  rect_for_marker 'T' (Cst.school_cell_w * 6, Cst.school_cell_h * 11, Cst.school_cell_w * 3, Cst.school_cell_h)

let class_door_rect () =
  rect_for_marker 'C' (Cst.school_cell_w * 9, Cst.school_cell_h * 2, Cst.school_cell_w * 2, Cst.school_cell_h)

let spawn_from_town () =
  let tx, ty, tw, _ = town_door_rect () in
  let center_x = tx + (tw / 2) in
  let spawn_y = ty - Cst.player_height - 6 in
  (center_x - (Cst.player_width / 2), spawn_y)

let spawn_from_classroom () =
  let cx, cy, cw, ch = class_door_rect () in
  let center_x = cx + (cw / 2) in
  let spawn_y = cy + ch + 6 in
  (center_x - (Cst.player_width / 2), spawn_y)
