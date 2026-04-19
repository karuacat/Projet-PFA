let layout = [|
  "NNNNNNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNNNNNN";
  "NOOOOCNCOOOOOOOOCNCN";
  "NOOOOCNCOOOOOOOPCCCN";
  "NBTTTCCCOOOOOOOOOOON";
  "N1TATOOOOCCOOCCOOCCN";
  "NBTTTOONOCCOOCCOOCCN";
  "N2TTTOOOOCCOOCCOOCCN";
  "NBTTTLONOCCOOCCOOCCN";
  "NBTTTOOOOCCOOCCOOCCN";
  "N3TJTOONOCCOOCCOOCCN";
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
        | 'B' | '1' | '2' | '3' ->
          add (x + 2, y + 2, w - 4, h - 4)
      | 'A' ->
          ()
      | 'C' ->
          let in_student_desks = col >= 9 in
          let is_tabletop_row = row = 7 || row = 9 || row = 11 in
          if in_student_desks && is_tabletop_row then
            begin
              add (x + 3, y + 3, w - 6, 13);
              add (x + 3, y - 10, w - 6, 13)
            end
          else
            ()
      | 'T' ->
          ()
      | _ -> ()
    done
  done;
  List.rev !rects

let npc_collision_rects () =
  List.filter
    (fun (x, y, w, h) ->
      let cell_col = x / Cst.classroom_cell_w in
      let cell_row = y / Cst.classroom_cell_h in
      not (in_bounds cell_col cell_row && layout.(cell_row).[cell_col] = 'B'))
    (collision_rects ())

let school_door_rect () =
  rect_for_marker
    'S'
    (Cst.classroom_cell_w * 5, Cst.classroom_cell_h * 14, Cst.classroom_cell_w * 2, Cst.classroom_cell_h)

let spawn_from_school () =
  let sx, sy, sw, _ = school_door_rect () in
  let center_x = sx + (sw / 2) in
  let spawn_y = sy - Cst.player_height - 6 in
  (center_x - (Cst.player_width / 2), spawn_y)