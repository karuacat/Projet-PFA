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
}

val get : unit -> t
val set : t -> unit