open Component_defs

type t = {
  window : Gfx.window;
  ctx : Gfx.context;
  player : player;
  mutable waiting : int;
}

let state = ref None

let get () : t =
  match !state with
    None -> failwith "Uninitialized global state"
  | Some s -> s

let set s = state := Some s