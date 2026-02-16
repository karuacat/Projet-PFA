open Component_defs

type t = {
  window : Gfx.window;
  ctx : Gfx.context;
  player : player;
  mutable waiting : int;
  dialogue_state : dialogue_state;
  tutorial_state : Tutorial.tutorial_state;
  font : Gfx.font;
  mutable menu_state : Menu.menu option;
  mutable character_creation_state : Character_creation.state option;
  mutable on_character_complete : (unit -> unit) option;
  mutable code_challenge_state : Code_challenge.state option;
  mutable knight_challenge_completed : bool;
}

let state = ref None

let get () : t =
  match !state with
    None -> failwith "Uninitialized global state"
  | Some s -> s

let set s = state := Some s