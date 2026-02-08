type tutorial_message = {
  id : string;
  text : string;
  mutable shown : bool;
  mutable active : bool;
}

type tutorial_state = {
  messages : (string, tutorial_message) Hashtbl.t;
  mutable current : tutorial_message option;
}

let create_state () = {
  messages = Hashtbl.create 16;
  current = None;
}

let register_message state id text =
  let msg = { id; text; shown = false; active = false } in
  Hashtbl.replace state.messages id msg

let show_message state id =
  try
    let msg = Hashtbl.find state.messages id in
    if not msg.shown then begin
      msg.active <- true;
      state.current <- Some msg
    end
  with Not_found -> ()

let hide_current state =
  match state.current with
  | None -> ()
  | Some msg ->
      msg.active <- false;
      state.current <- None

let complete_message state id =
  try
    let msg = Hashtbl.find state.messages id in
    msg.shown <- true;
    msg.active <- false;
    if state.current = Some msg then
      state.current <- None
  with Not_found -> ()

let is_active state =
  match state.current with
  | None -> false
  | Some msg -> msg.active

let current_message state =
  match state.current with
  | None -> None
  | Some msg -> if msg.active then Some msg.text else None

let reset_all state =
  Hashtbl.iter (fun _ msg ->
    msg.shown <- false;
    msg.active <- false
  ) state.messages;
  state.current <- None
