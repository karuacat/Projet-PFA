let layout = [|
  "NNNNNNNNNNNN";
  "NNNNNNNNNNNN";
  "NNNNNNNNNNNN";
  "NNCOOOOOONNN";
  "NNOOOONOOOON";
  "NOOOONNNOOON";
  "NOOOONNLOOON";
  "NNOOOOOOOONN";
  "NNNNNSSNNNNN";
|]

let cell_x col = Cst.house_offset_x + (col * Cst.house_width) / Cst.house_cols
let cell_y row = Cst.house_offset_y + (row * Cst.house_height) / Cst.house_rows

let cell_w col =
  (Cst.house_offset_x + ((col + 1) * Cst.house_width) / Cst.house_cols) - cell_x col

let cell_h row =
  (Cst.house_offset_y + ((row + 1) * Cst.house_height) / Cst.house_rows) - cell_y row

let cell_center col row =
  (cell_x col + (cell_w col / 2), cell_y row + (cell_h row / 2))

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
        List.fold_left
          (fun (min_c, max_c, min_r, max_r) (c, r) ->
            (min min_c c, max max_c c, min min_r r, max max_r r))
          (c0, c0, r0, r0)
          rest
      in
      let x = cell_x min_c in
      let y = cell_y min_r in
      let w = (cell_x (max_c + 1)) - x in
      let h = (cell_y (max_r + 1)) - y in
      (x, y, w, h)

let is_blocked col row =
  match layout.(row).[col] with
  | 'N' | 'L' | 'C' -> true
  | _ -> false

let house_exit_rect () =
  rect_for_marker
    'S'
    (cell_x 5, cell_y 8, (cell_x 7) - (cell_x 5), cell_h 8)

let spawn_from_town () =
  let sx, sy, sw, _ = house_exit_rect () in
  let center_x = sx + (sw / 2) in
  let spawn_y = sy - Cst.player_height - 6 in
  (center_x - (Cst.player_width / 2), spawn_y)

let chest_pos () =
  match marker_cells 'C' with
  | (col, row) :: _ ->
      let x = cell_x col + cell_w col - 12 in
      let y = cell_y row + (cell_h row / 2) - 12 in
      (x, y)
  | [] -> (cell_x 2 + cell_w 2 - 12, cell_y 3 + (cell_h 3 / 2) - 12)

let book_pos () =
  match marker_cells 'L' with
  | (col, row) :: _ ->
      let x = cell_x col + (cell_w col - 14) / 2 in
      let y = cell_y row + (cell_h row - 12) / 2 in
      (x, y)
  | [] ->
      let x = cell_x 6 + (cell_w 6 - 14) / 2 in
      let y = cell_y 6 + (cell_h 6 - 12) / 2 in
      (x, y)