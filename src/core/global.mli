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
    mutable lambda_duel_started : bool;
    mutable lambda_duel_stage : int;
    mutable lambda_golem_hp : int;
    mutable lambda_golem_hp_visible : bool;
    mutable lambda_duel_completed : bool;
    mutable lambda_library_instruction_seen : bool;
    mutable classroom_scripted_movement_active : bool;
    mutable dynamic_magic_cinematic_done : bool;
    mutable dynamic_magic_cinematic_active : bool;
    mutable dynamic_magic_phase : int;
    mutable dynamic_magic_timer : float;
    mutable dynamic_magic_pending_target_scene : Scene.scene option;
    mutable dynamic_magic_spawn_x : int;
    mutable dynamic_magic_spawn_y : int;
    library_guide_state : Library_guide.state;
    mutable library_intro_seen : bool;
}

val get : unit -> t
val set : t -> unit