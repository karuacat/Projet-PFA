open Component_defs

type t = {
    window : Gfx.window;
    ctx : Gfx.context;
    player : player;
    mutable waiting : int;
}

val get : unit -> t
val set : t -> unit