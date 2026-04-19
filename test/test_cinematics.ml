module SES = Game__Story_events_system
module Classroom_map = Game__Classroom_map
module Cst = Game__Cst

let fail msg =
  prerr_endline ("[FAIL] " ^ msg);
  exit 1

let check cond msg =
  if not cond then fail msg

let check_some = function
  | Some v -> v
  | None -> fail "Expected Some value"

let float_eq a b =
  abs_float (a -. b) <= 0.001

let point_eq (x1, y1) (x2, y2) =
  float_eq x1 x2 && float_eq y1 y2

let axis_aligned (x1, y1) (x2, y2) =
  float_eq x1 x2 || float_eq y1 y2

let run_test name f =
  f ();
  print_endline ("[OK] " ^ name)

let test_classroom_marker_target_fallback () =
  let tx, ty = SES.classroom_marker_target 'Z' 4 5 in
  let cx, cy = Classroom_map.cell_center 4 5 in
  let ex = float_of_int (cx - 20) in
  let ey = float_of_int (cy - 30) in
  check (point_eq (tx, ty) (ex, ey)) "Missing marker should use fallback cell coordinates"

let test_spread_three_cells_sorting () =
  let input = [ (5, 4); (2, 1); (9, 7); (1, 3); (4, 2) ] in
  let picked = SES.spread_three_cells input [ (0, 0); (1, 0); (2, 0) ] in
  let expected = [ (2, 1); (1, 3); (9, 7) ] in
  check (picked = expected) "spread_three_cells should pick first/middle/last after row/col sort"

let test_assign_post_targets_from_markers_populates_targets () =
  SES.assign_post_targets_from_markers ();

  let student_a_target = check_some !SES.student_a_post_target in
  let student_b_target = check_some !SES.student_b_post_target in
  let student_c_target = check_some !SES.student_c_post_target in
  let aerin_target = check_some !SES.aerin_post_target in
  let lambda_target = check_some !SES.lambda_post_target in

  let expected_1 = SES.classroom_marker_target '1' 1 6 in
  let expected_2 = SES.classroom_marker_target '2' 1 8 in
  let expected_3 = SES.classroom_marker_target '3' 1 12 in
  let expected_a = SES.classroom_marker_target 'A' 3 6 in
  let expected_l = SES.classroom_marker_target 'L' 5 9 in

  check (point_eq student_a_target expected_1) "Student A target should come from marker '1'";
  check (point_eq student_b_target expected_2) "Student B target should come from marker '2'";
  check (point_eq student_c_target expected_3) "Student C target should come from marker '3'";
  check (point_eq aerin_target expected_a) "Aerin target should come from marker 'A'";
  check (point_eq lambda_target expected_l) "Lambda target should come from marker 'L'"

let test_waypoints_are_axis_aligned () =
  SES.assign_post_targets_from_markers ();
  let all_routes =
    [ !SES.student_a_route_waypoints;
      !SES.student_b_route_waypoints;
      !SES.student_c_route_waypoints;
      !SES.aerin_route_waypoints;
      !SES.lambda_route_waypoints ]
  in
  let each_route_axis_aligned route =
    let rec loop = function
      | a :: (b :: _ as rest) -> axis_aligned a b && loop rest
      | _ -> true
    in
    loop route
  in
  check (List.for_all each_route_axis_aligned all_routes)
    "All cinematic waypoint segments should be axis-aligned"

let test_waypoint_counts_are_stable () =
  SES.assign_post_targets_from_markers ();
  check (List.length !SES.student_a_route_waypoints = 2) "Student A waypoint count regression";
  check (List.length !SES.student_b_route_waypoints = 1) "Student B waypoint count regression";
  check (List.length !SES.student_c_route_waypoints = 2) "Student C waypoint count regression";
  check (List.length !SES.aerin_route_waypoints = 2) "Aerin waypoint count regression";
  check (List.length !SES.lambda_route_waypoints = 3) "Lambda waypoint count regression"

let () =
  run_test "marker fallback" test_classroom_marker_target_fallback;
  run_test "spread-three sorting" test_spread_three_cells_sorting;
  run_test "assign targets from markers" test_assign_post_targets_from_markers_populates_targets;
  run_test "axis-aligned waypoints" test_waypoints_are_axis_aligned;
  run_test "stable waypoint counts" test_waypoint_counts_are_stable;
  print_endline "All cinematic regression tests passed.";
  exit 0
