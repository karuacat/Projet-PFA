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
    mutable on_escape_pressed : (unit -> unit) option;
    mutable player_name : string;
    mutable code_challenge_state : Code_challenge.state option;
    mutable house_exit_attempted : bool;
    mutable has_secret_book : bool;
    mutable chest_challenge_completed : bool;
    mutable knight_challenge_completed : bool;
    mutable school_students_event_completed : bool;
    mutable classroom_intro_completed : bool;
}

val get : unit -> t
val set : t -> unit