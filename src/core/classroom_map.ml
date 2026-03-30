let layout = [|
  "NNNNNNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNNNNNN";
  "NOOOOCNCOOOOOOOOCNCN";
  "NOOOOCNCOOOOOOOOCCCN";
  "NBTTTCCCOOOOOOOOOOON";
  "NBTATOOOOCCOOCCOOCCN";
  "NBTTTOONOCCOOCCOOCCN";
  "NBTTTOOOOCCOOCCOOCCN";
  "NBTTTLONOCCOOCCOOCCN";
  "NBTTTOOOOCCOOCCOOCCN";
  "NBTJTOONOCCOOCCOOCCN";
  "NBTTTOOOOOOOOOOOOOON";
  "NNNNNOONNNNNNNNNNNNN";
  "NNNNNSSNNNNNNNNNNNNN";
|]

let cell_x col = col * Cst.classroom_cell_w
let cell_y row = row * Cst.classroom_cell_h

let cell_center col row =
  (cell_x col + (Cst.classroom_cell_w / 2),
   cell_y row + (Cst.classroom_cell_h / 2))

let in_bounds col row =
  row >= 0
  && row < Array.length layout
  && col >= 0
  && col < String.length layout.(row)

let has_marker col row marker =
  in_bounds col row && layout.(row).[col] = marker

let marker_cells marker =
  let cells = ref [] in
  for row = 0 to Array.length layout - 1 do
    for col = 0 to String.length layout.(row) - 1 do
      if layout.(row).[col] = marker then cells := (col, row) :: !cells
    done
  done;
  List.rev !cells

let rect_for_marker marker default_rect =
  match marker_cells marker with
  | [] -> default_rect
  | (c0, r0) :: rest ->
      let min_c, max_c, min_r, max_r =
        List.fold_left
          (fun (min_c, max_c, min_r, max_r) (c, r) ->
            (min min_c c, max max_c c, min min_r r, max max_r r))
          (c0, c0, r0, r0)
          rest
      in
      let x = cell_x min_c in
      let y = cell_y min_r in
      let w = (max_c - min_c + 1) * Cst.classroom_cell_w in
      let h = (max_r - min_r + 1) * Cst.classroom_cell_h in
      (x, y, w, h)

let is_blocked col row =
  match layout.(row).[col] with
  | 'N' -> true
  | _ -> false

let collision_rects () =
  let rects = ref [] in
  let add rect = rects := rect :: !rects in
  for row = 0 to Array.length layout - 1 do
    for col = 0 to String.length layout.(row) - 1 do
      let x = cell_x col in
      let y = cell_y row in
      let w = Cst.classroom_cell_w in
      let h = Cst.classroom_cell_h in
      match layout.(row).[col] with
      | 'N' -> add (x, y, w, h)
      | 'B' -> add (x + 6, y + 2, w - 12, h - 4)
      | 'C' ->
          let left = has_marker (col - 1) row 'C' in
          let right = has_marker (col + 1) row 'C' in
          let up = has_marker col (row - 1) 'C' in
          let down = has_marker col (row + 1) 'C' in
          if left || right then
            add (x + 3, y + 10, w - 6, h - 18)
          else if up || down then
            add (x + 8, y + 4, w - 16, h - 8)
          else
            ()
      | _ -> ()
    done
  done;
  List.rev !rects

let school_door_rect () =
  rect_for_marker
    'S'
    (Cst.classroom_cell_w * 5, Cst.classroom_cell_h * 14, Cst.classroom_cell_w * 2, Cst.classroom_cell_h)

let spawn_from_school () =
  let sx, sy, sw, _ = school_door_rect () in
  let center_x = sx + (sw / 2) in
  let spawn_y = sy - Cst.player_height - 6 in
  (center_x - (Cst.player_width / 2), spawn_y)