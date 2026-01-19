type t = {
    window : Gfx.window;
    ctx : Gfx.context;
}

val get : unit -> t
val set : t -> unit