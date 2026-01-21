let key_table = Hashtbl.create 16
let has_key s = Hashtbl.mem key_table s
let set_key s = Hashtbl.replace key_table s ()
let unset_key s = Hashtbl.remove key_table s

let action_table = Hashtbl.create 16
let register key action = Hashtbl.replace action_table key action

let rec handle_input () =
  let () =
    match Gfx.poll_event () with
      KeyDown s -> set_key s; handle_input ()
    | KeyUp s -> unset_key s; handle_input ()
    | Quit -> exit 0
    | _ -> ()
  in
  Hashtbl.iter (fun key action ->
    if has_key key then action ()) action_table

let () =
  register "z" (fun () -> Player.(move_player (player()) Cst.player_v_up));
  register "s" (fun () -> Player.(move_player (player()) Cst.player_v_down));
  register "d" (fun () -> Player.(move_player (player()) Cst.player_v_right));
  register "q" (fun () -> Player.(move_player (player()) Cst.player_v_left));
  register "m" (fun () ->
    let global = Global.get () in
    global.waiting <- 1;
    )