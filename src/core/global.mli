open Component_defs

type t = {
    window : Gfx.window;
    ctx : Gfx.context;
    player : player;
    mutable waiting : int;
    dialogue_state : dialogue_state;
    tutorial_state : Tutorial.tutorial_state;
    font : Gfx.font;
}

val get : unit -> t
val set : t -> unit