open Component_defs

val create_state : unit -> dialogue_state

val create_dialogue : dialogue_line list -> dialogue

val start_dialogue : dialogue_state -> dialogue -> unit

val next_line : dialogue_state -> unit

val current_line : dialogue_state -> dialogue_line option

val is_finished : dialogue_state -> bool

val close_dialogue : dialogue_state -> unit

val intro_wake_up : dialogue

val npc_villager_1 : dialogue
val npc_villager_2 : dialogue
val npc_guard : dialogue

val knight_guardian_intro : dialogue
val knight_guardian_waiting : dialogue
val knight_guardian_success : dialogue
val knight_guardian_failure : dialogue
val knight_guardian_admission_success : dialogue
val knight_guardian_post_victory : dialogue

val secret_book : dialogue
