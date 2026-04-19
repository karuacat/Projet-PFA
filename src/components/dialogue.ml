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
      if dialogue.current_line < List.length dialogue.lines - 1 then begin
        dialogue.current_line <- dialogue.current_line + 1
      end else begin
        state.active <- false;
        state.current_dialogue <- None
      end

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

let create_choice_dialogue question choices = {
  question;
  choices;
  selected_choice = 0;
}

let get_selected_choice choice_dialogue =
  if choice_dialogue.selected_choice < List.length choice_dialogue.choices then
    Some (List.nth choice_dialogue.choices choice_dialogue.selected_choice)
  else
    None

let next_choice choice_dialogue =
  if choice_dialogue.selected_choice < List.length choice_dialogue.choices - 1 then
    choice_dialogue.selected_choice <- choice_dialogue.selected_choice + 1

let prev_choice choice_dialogue =
  if choice_dialogue.selected_choice > 0 then
    choice_dialogue.selected_choice <- choice_dialogue.selected_choice - 1

let intro_wake_up = create_dialogue [
  { speaker = "Moi"; text = "…Encore ce rêve étrange…" };
  { speaker = "Moi"; text = "Des symboles qui brillent…" };
  { speaker = "Moi"; text = "Aujourd'hui, c'est mon premier jour\nà l'école de magie." };
  { speaker = "Moi"; text = "Père disait toujours que les mots\nfaçonnent le monde…" };
]

let house_exit_blocked = create_dialogue [
  { speaker = "Moi"; text = "Je ne peux pas partir sans mes clefs\nni mes affaires !" };
  { speaker = "Moi"; text = "Normalement tout devrait se trouver\ndans mon coffre…" };
]

let chest_intro = create_dialogue [
  { speaker = "Moi"; text = "Bien sûr il est fermé à clé !" };
  { speaker = "Moi"; text = "Il faut que je les fasse apparaître." };
]

let chest_success = create_dialogue [
  { speaker = "Coffre de l'Apprenti"; text = "Une lumière stable émane du coffre." };
  { speaker = "Moi"; text = "Maintenant que j'ai mes affaires,\nil faut que je me dépêche d'aller\nà l'académie." };
  { speaker = "Moi"; text = "Je ne voudrais pas être en retard\ndès le premier jour !" };
]

let chest_failure = create_dialogue [
  { speaker = "Coffre de l'Apprenti"; text = "Le sceau résiste." };
  { speaker = "Coffre de l'Apprenti"; text = "Essaie avec : let cles = true;;" };
]

let chest_post_victory = create_dialogue [
  { speaker = "Coffre de l'Apprenti"; text = "Le coffre est déjà ouvert." };
]

let npc_villager_1 = create_dialogue [
  { speaker = "Vieille érudite"; text = "Les jeunes apprennent à lancer des sorts…" };
  { speaker = "Vieille érudite"; text = "Mais peu comprennent leur sens." };
]

let npc_villager_2 = create_dialogue [
  { speaker = "Marchand"; text = "Tu vas à l'Académie ?" };
  { speaker = "Marchand"; text = "Alors écoute bien :" };
  { speaker = "Marchand"; text = "ici, mal nommer une chose,\nc'est déjà l'affaiblir." };
]

let knight_guardian_intro = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Halte !" };
  { speaker = "Chevalier Gardien"; text = "Aucun acier ne protège mieux\nqu'un symbole bien formé." };
  { speaker = "Chevalier Gardien"; text = "Montre-moi que tu sais nommer\nsans ambiguïté." };
  { speaker = "Chevalier Gardien"; text = "Déclare ton nom avec une variable string." };
  { speaker = "Chevalier Gardien"; text = "L'épreuve commence maintenant.\n" };
]

let knight_guardian_failure = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Ces mots tremblent !" };
  { speaker = "Chevalier Gardien"; text = "N'as-tu donc pas un livre de sorts ?" };
]
let knight_guardian_admission_success = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Les mots ont du pouvoir."; };
  { speaker = "Chevalier Gardien"; text = "Tu sembles connaître la base\nde l'Art des Symboles."; };
  { speaker = "Chevalier Gardien"; text = "Entre, apprenti."; };
]

let knight_guardian_post_victory = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Bien, très bien." };
  { speaker = "Chevalier Gardien"; text = "Tu as montré de la clarté dans tes paroles." };
  { speaker = "Chevalier Gardien"; text = "L'Académie t'attend." };
  { speaker = "Chevalier Gardien"; text = "Embrasse les mystères qui t'y attendent." };
  { speaker = "Chevalier Gardien"; text = "Et souviens-toi : \nc'est par l'ordre des symboles\nque se manifeste la vraie puissance." };
]

let secret_book_for_name player_name =
  let shown_name =
    let trimmed = String.trim player_name in
    if String.length trimmed = 0 then "Apprenti" else trimmed
  in
  create_dialogue [
    { speaker = "Livre Ancien"; text = "Les Fondations de l'Art des Symboles"; };
    { speaker = "Livre Ancien"; text = "Une variable stocke une valeur avec un nom."; };
    { speaker = "Livre Ancien"; text = "Une fonction transforme une entrée en sortie."; };
    { speaker = "Livre Ancien"; text = "Un string est une chaîne entre guillemets."; };
    { speaker = "Livre Ancien"; text = "Exemple: let nom = \"" ^ shown_name ^ "\";;"; };
  ]

let dynamic_magic_professors = create_dialogue [
  { speaker = "Professeur 1"; text = "As-tu entendu ?" };
  { speaker = "Professeur 1"; text = "Quelqu'un tente de modifier le Compilateur Originel..." };
  { speaker = "Professeur 2"; text = "Impossible. Sans typage... le monde s'effondrerait." };
]

let dynamic_magic_book = create_dialogue [
  { speaker = "Livre"; text = "Magie dynamique." };
]
