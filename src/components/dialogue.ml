open Component_defs

let create_state () = {
  active = false;
  current_dialogue = None;
}

let create_dialogue lines = {
  lines;
  current_line = 0;
}

let start_dialogue state dialogue =
  dialogue.current_line <- 0;
  state.current_dialogue <- Some dialogue;
  state.active <- true

let next_line state =
  match state.current_dialogue with
  | None -> ()
  | Some dialogue ->
      if dialogue.current_line < List.length dialogue.lines - 1 then
        dialogue.current_line <- dialogue.current_line + 1
      else
        state.active <- false

let current_line state =
  match state.current_dialogue with
  | None -> None
  | Some dialogue ->
      if dialogue.current_line < List.length dialogue.lines then
        Some (List.nth dialogue.lines dialogue.current_line)
      else
        None

let is_finished state =
  match state.current_dialogue with
  | None -> true
  | Some dialogue ->
      dialogue.current_line >= List.length dialogue.lines - 1

let close_dialogue state =
  state.active <- false;
  state.current_dialogue <- None

let intro_wake_up = create_dialogue [
  { speaker = "Moi"; text = "Aujourd'hui c'est mon premier jour\na l'ecole de magie !" };
  { speaker = "Moi"; text = "Je vais enfin pouvoir realiser\nmon reve de devenir chevalier d'OCamlon !" };
]

let npc_villager_1 = create_dialogue [
  { speaker = "Villageois"; text = "Bonjour jeune homme !" };
  { speaker = "Villageois"; text = "Tu cherches l'ecole de magie ?" };
  { speaker = "Villageois"; text = "Elle se trouve au nord de la ville." };
]

let npc_villager_2 = create_dialogue [
  { speaker = "Marchande"; text = "Bienvenue dans notre belle ville !" };
  { speaker = "Marchande"; text = "Si tu as besoin de quoi que ce soit,\nn'hesite pas." };
]

let npc_guard = create_dialogue [
  { speaker = "Garde"; text = "Bonjour jeune apprenti !" };
  { speaker = "Garde"; text = "L'ecole de magie est juste la-bas." };
  { speaker = "Garde"; text = "Bonne chance pour ta formation !" };
]
