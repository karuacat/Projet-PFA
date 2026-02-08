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