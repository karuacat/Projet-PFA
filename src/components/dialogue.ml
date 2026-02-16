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

let intro_wake_up = create_dialogue [
  { speaker = "Moi"; text = "Aujourd'hui c'est mon premier jour\nà l'école de magie !" };
  { speaker = "Moi"; text = "Je vais enfin pouvoir réaliser\nmon rêve de devenir chevalier d'OCamlon !" };
]

let npc_villager_1 = create_dialogue [
  { speaker = "Vieille érudite"; text = "Ah, un nouvel apprenti !" };
  { speaker = "Vieille érudite"; text = "Les jeunes apprennent à lancer des sorts..." };
  { speaker = "Vieille érudite"; text = "Mais peu comprennent leur véritable sens." };
  { speaker = "Vieille érudite"; text = "Bon courage, jeune mage." };
]

let npc_villager_2 = create_dialogue [
  { speaker = "Marchand"; text = "Bienvenue!" };
  { speaker = "Marchand"; text = "Tu vas à l'Académie ?" };
  { speaker = "Marchand"; text = "Alors écoute bien : ici, mal nommer une \nchose, c'est déjà l'affaiblir." };
  { speaker = "Marchand"; text = "Les mots ont du poids." };
]

let npc_guard = create_dialogue [
  { speaker = "Garde des Portes"; text = "Bienvenue dans nos rues !" };
  { speaker = "Garde des Portes"; text = "L'Académie d'OCamlon est au nord." };
  { speaker = "Garde des Portes"; text = "Sois prudent et respectueux." };
]

let town_academy_sign = create_dialogue [
  { speaker = "Panneau"; text = "Académie d'OCamlon\n→ Nord\n\nStabilité, Savoir, Structure" };
]

let town_market_sign = create_dialogue [
  { speaker = "Panneau"; text = "Place du Marché\n→ Est\n\nCommerces et services" };
]
let knight_guardian_intro = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Halte !" };
  { speaker = "Chevalier Gardien"; text = "Aucun acier ne protège mieux\nqu'un symbole bien formé." };
  { speaker = "Chevalier Gardien"; text = "Montre-moi que tu sais nommer\nsans ambiguïté." };
  { speaker = "Chevalier Gardien"; text = "Déclare une variable de type string\navec le code d'entrée de l'académie." };
  { speaker = "Chevalier Gardien"; text = "[Appuie sur C pour tenter l'épreuve]\n[Retour à la ligne avec ENTREE]\n[Ctrl+ENTREE pour valider]" };
]

let knight_guardian_waiting = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Je t'écoute, apprenti." };
  { speaker = "Chevalier Gardien"; text = "Déclare une variable string\navec ton nom." };
]

let knight_guardian_success = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Hm... Stable. Cohérent." };
  { speaker = "Chevalier Gardien"; text = "Tu peux entrer." };
]

let knight_guardian_failure = create_dialogue [
  { speaker = "Chevalier Gardien"; text = "Ces mots tremblent ! \n Reprends-toi." };
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

let secret_book = create_dialogue [
  { speaker = "Livre Ancien"; text = "Les Fondations de l'Art des Symboles"; };
  { speaker = "Livre Ancien"; text = "Chapitre 1: Les Variables String"; };
  { speaker = "Livre Ancien"; text = "Un String est une chaîne de caractères\nentourée de guillemets."; };
  { speaker = "Livre Ancien"; text = "Pour déclarer une variable string,\nutilise: let admission = \"...\""; };
  { speaker = "Livre Ancien"; text = "Le code de l'admission est:"; };
  { speaker = "Livre Ancien"; text = "let admission = \"Soyez fier de devenir le \nfutur protecteur du pays !\""; };
]