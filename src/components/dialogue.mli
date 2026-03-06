open Component_defs

val create_state : unit -> dialogue_state

val create_dialogue : dialogue_line list -> dialogue

val start_dialogue : dialogue_state -> dialogue -> unit

val next_line : dialogue_state -> unit

val current_line : dialogue_state -> dialogue_line option

val is_finished : dialogue_state -> bool

val close_dialogue : dialogue_state -> unit

val create_choice_dialogue : dialogue_line -> dialogue_choice list -> choice_dialogue
val get_selected_choice : choice_dialogue -> dialogue_choice option
val next_choice : choice_dialogue -> unit
val prev_choice : choice_dialogue -> unit

val intro_wake_up : dialogue
val house_exit_blocked : dialogue

val chest_intro : dialogue
val chest_success : dialogue
val chest_failure : dialogue
val chest_post_victory : dialogue

val npc_villager_1 : dialogue
val npc_villager_2 : dialogue

val knight_guardian_intro : dialogue
val knight_guardian_failure : dialogue
val knight_guardian_admission_success : dialogue
val knight_guardian_post_victory : dialogue

val secret_book_for_name : string -> dialogue