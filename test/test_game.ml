module CC = Game__Code_challenge
module Classroom_map = Game__Classroom_map
module House_map = Game__House_map
module Town_map = Game__Town_map
module Cst = Game__Cst

let fail msg =
  prerr_endline ("[FAIL] " ^ msg);
  exit 1

let check cond msg =
  if not cond then fail msg

let run_test name f =
  f ();
  print_endline ("[OK] " ^ name)

let test_classroom_markers_present () =
  check (Classroom_map.marker_cells '1' <> []) "Marker '1' should exist in classroom map";
  check (Classroom_map.marker_cells '2' <> []) "Marker '2' should exist in classroom map";
  check (Classroom_map.marker_cells '3' <> []) "Marker '3' should exist in classroom map";
  check (Classroom_map.marker_cells 'S' <> []) "Marker 'S' should exist in classroom map"

let test_classroom_npc_collision_layer_excludes_benches () =
  let b_cells = Classroom_map.marker_cells 'B' in
  let npc_rects = Classroom_map.npc_collision_rects () in
  check (npc_rects <> []) "NPC collision layer should not be empty";
  let benches_filtered =
    List.for_all
      (fun (x, y, _, _) ->
        let col = x / Cst.classroom_cell_w in
        let row = y / Cst.classroom_cell_h in
        not (List.mem (col, row) b_cells))
      npc_rects
  in
  check benches_filtered "NPC collision layer should exclude bench cells"

let test_house_collision_layer_has_world_borders () =
  let rects = House_map.collision_rects () in
  let expected_top = (0, 0, Cst.window_width, Cst.house_offset_y) in
  let expected_left = (0, Cst.house_offset_y, Cst.house_offset_x, Cst.house_height) in
  check (List.mem expected_top rects) "House collision layer should include top world border";
  check (List.mem expected_left rects) "House collision layer should include left world border"

let test_town_collision_layer_has_borders () =
  let rects = Town_map.collision_rects () in
  let border = 4 in
  let expected_bottom = (0, Cst.window_height - border, Cst.window_width, border) in
  let expected_right = (Cst.window_width - border, 0, border, Cst.window_height) in
  check (List.mem expected_bottom rects) "Town collision layer should include bottom border";
  check (List.mem expected_right rects) "Town collision layer should include right border"

let mk_state challenge code =
  let st = CC.create_state () in
  CC.start_challenge st challenge (fun () -> ()) (fun () -> ());
  st.code <- code;
  st

let test_code_challenge_bool_validation () =
  let st = mk_state (CC.BoolVariable ("porte_ouverte", true)) "let porte_ouverte = true;;" in
  check (CC.validate_code st) "Bool challenge should validate correct code"

let test_code_challenge_power_validation () =
  let st_ok = mk_state CC.PowerCalculation "let puissance = 10 + 5;;" in
  let st_bad = mk_state CC.PowerCalculation "let puissance = 10 + 2;;" in
  check (CC.validate_code st_ok) "Power challenge should accept 10 + 5";
  check (not (CC.validate_code st_bad)) "Power challenge should reject wrong sum"

let test_code_challenge_extract_damage () =
  check (CC.extract_golem_damage "let degats = 7;;" = Some 7) "Damage extraction should parse degats";
  check (CC.extract_golem_damage "let pv = true;;" = None) "Damage extraction should reject unrelated code"

let test_code_challenge_type_exercise_case_insensitive () =
  let st = mk_state (CC.TypeExercise 3) "let salle = \"BIBLIOTHEQUE\";;" in
  check (CC.validate_code st) "Type exercise should accept bibliotheque case-insensitively"

let () =
  run_test "classroom markers" test_classroom_markers_present;
  run_test "classroom npc collision layer" test_classroom_npc_collision_layer_excludes_benches;
  run_test "house collision borders" test_house_collision_layer_has_world_borders;
  run_test "town collision borders" test_town_collision_layer_has_borders;
  run_test "code challenge bool" test_code_challenge_bool_validation;
  run_test "code challenge power" test_code_challenge_power_validation;
  run_test "code challenge damage extraction" test_code_challenge_extract_damage;
  run_test "type exercise case-insensitive" test_code_challenge_type_exercise_case_insensitive;
  print_endline "All tests passed.";
  exit 0
