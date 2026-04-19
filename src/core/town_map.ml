let layout = [|
  "NNNNNNNOOOOONNNNNNNN";
  "NNNNNNNOOOOONNNNNNNN";
  "OOOOOOOOOOOOPOOOONNN";
  "OOONNOOOOOOOOOOOONNN";
  "OOONNOOOOOOOOOOONNNN";
  "OOOOOOOONNNOOOOOONNN";
  "OOOOOOONNNNNOOMOOONN";
  "OOOOOOONNNNNONNNOOON";
  "OOOOOOONNNNNONNNOOOO";
  "OOOOOOOONNNOOOOOOOOO";
  "OONNNOOOOOOOOOONNONN";
  "OONNNNOOOOOOOONNNNNN";
  "OONHNNOOOOOVNNNNNNNN";
  "OOOOOOOOOOOONNNNNNNN";
  "OOOOOOOOOOOONNNNNNNN";
|]

let cell_x col = col * Cst.town_cell_w
let cell_y row = row * Cst.town_cell_h

let cell_center col row =
  (cell_x col + (Cst.town_cell_w / 2),
   cell_y row + (Cst.town_cell_h / 2))

let is_blocked col row =
  layout.(row).[col] = 'N'

let find_marker marker =
  let found = ref None in
  for row = 0 to Array.length layout - 1 do
    for col = 0 to String.length layout.(row) - 1 do
      if layout.(row).[col] = marker then found := Some (col, row)
    done
  done;
  !found

let academy_gate_left_col = 7
let academy_gate_right_col = 11

let house_door_rect () =
  match find_marker 'H' with
  | Some (col, row) ->
      let x = cell_x col + (Cst.town_cell_w / 4) in
      let y = cell_y row + (Cst.town_cell_h / 4) in
      let w = Cst.town_cell_w / 2 in
      let h = Cst.town_cell_h / 2 in
      (x, y, w, h)
  | None ->
      (Cst.town_cell_w, Cst.town_cell_h, Cst.town_cell_w / 2, Cst.town_cell_h / 2)

let spawn_from_house () =
  match find_marker 'H' with
  | Some (col, row) ->
      let rec find_walkable_row r =
        if r >= Cst.town_rows then row
        else if not (is_blocked col r) then r
        else find_walkable_row (r + 1)
      in
      let spawn_row = find_walkable_row (row + 1) in
      let cx, cy = cell_center col spawn_row in
      (cx - (Cst.player_width / 2), cy - (Cst.player_height / 2))
  | None ->
      (Cst.window_width / 2, Cst.window_height - 80)

let academy_door_rect () =
  let x = cell_x academy_gate_left_col in
  let y = 0 in
  let w = (academy_gate_right_col - academy_gate_left_col + 1) * Cst.town_cell_w in
  let h = Cst.town_cell_h / 3 in
  (x, y, w, h)

let collision_rects () =
  let rects = ref [] in
  let add rect = rects := rect :: !rects in
  for r = 0 to Cst.town_rows - 1 do
    for c = 0 to Cst.town_cols - 1 do
      if layout.(r).[c] = 'N' then
        add (cell_x c, cell_y r, Cst.town_cell_w, Cst.town_cell_h)
    done
  done;
  let border = 4 in
  add (0, Cst.window_height - border, Cst.window_width, border);
  add (0, 0, border, Cst.window_height);
  add (Cst.window_width - border, 0, border, Cst.window_height);
  add (0, 0, Cst.window_width, border);
  List.rev !rects