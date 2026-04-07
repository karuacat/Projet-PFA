let layout = [|
  "NNNNNNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNNNNNN";
  "NOOOOOOONLNOOOOOOOON";
  "NNNNNNNOOOOONNNNNNNN";
  "NNNNNNNOOOOONNNNNNNN";
  "NOOOOOOOOOOOOOOOPPON";
  "NNNNNNNOOSOONNNNNNNN";
  "NNNNNNNOOOOONNNNNNNN";
  "NOOOOOOOOOOOOOOOOOON";
  "NNNNNNNNNOOONNOONNON";
  "NNNNNNNNNOOONNOONNON";
  "NAOOOOOOOOOONNOONNON";
  "NNNNNNNNNNNNNNNNNNNN";
  "NNNNNNNNNNNNNNNNNNNN";
|]

let cols = 20
let rows = 15
let cell_w = Cst.window_width / cols
let cell_h = Cst.window_height / rows

let cell_x col = col * cell_w
let cell_y row = row * cell_h

let cell_center col row =
  (cell_x col + (cell_w / 2),
   cell_y row + (cell_h / 2))

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
      let w = (max_c - min_c + 1) * cell_w in
      let h = (max_r - min_r + 1) * cell_h in
      (x, y, w, h)

let school_door_rect () =
  rect_for_marker 'S' (cell_w * 9, cell_h * 7, cell_w, cell_h)

let training_book_rect () =
  rect_for_marker 'L' (cell_w * 9, cell_h * 3, cell_w, cell_h)

let professors_rect () =
  rect_for_marker 'P' (cell_w * 16, cell_h * 6, cell_w * 2, cell_h)

let aerin_marker_rect () =
  rect_for_marker 'A' (cell_w, cell_h * 12, cell_w, cell_h)

let spawn_from_school () =
  let sx, sy, sw, _ = school_door_rect () in
  let center_x = sx + (sw / 2) in
  let spawn_y = sy - Cst.player_height - 6 in
  (center_x - (Cst.player_width / 2), spawn_y)

let is_blocked col row =
  layout.(row).[col] = 'N'