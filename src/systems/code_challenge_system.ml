open Ecs

type t = Entity.t

let init _ = ()

let golem_img_res : Gfx.surface Gfx.resource option ref = ref None
let aerin_fight_img_res : Gfx.surface Gfx.resource option ref = ref None

let get_or_load_image cache ctx path =
  match !cache with
  | Some res -> res
  | None ->
      let res = Gfx.load_image ctx path in
      cache := Some res;
      res

let wrap_text_lines ctx font max_width text =
  let words = String.split_on_char ' ' text in
  let rec loop acc current_line remaining_words =
    match remaining_words with
    | [] ->
        if String.length current_line = 0 then List.rev acc
        else List.rev (current_line :: acc)
    | word :: tail ->
        let candidate =
          if String.length current_line = 0 then word
          else current_line ^ " " ^ word
        in
        let width, _ = Gfx.measure_text candidate font in
        if width <= max_width || String.length current_line = 0 then
          loop acc candidate tail
        else
          loop (current_line :: acc) word tail
  in
  loop [] "" words

let is_lambda_duel_challenge state =
  match state.Code_challenge.challenge with
  | Some Code_challenge.GolemActivateHp
  | Some Code_challenge.GolemDealDamage -> true
  | _ -> false

let draw_code_lines ctx surface font code x y line_height =
  if String.length code > 0 then begin
    let lines = String.split_on_char '\n' code in
    let line_num = ref 0 in
    List.iter
      (fun line ->
        if String.length line > 0 then begin
          let code_surf = Gfx.render_text ctx line font in
          Gfx.blit ctx surface code_surf x (y + (!line_num * line_height));
          incr line_num
        end else
          incr line_num)
      lines
  end

let draw_code_cursor ctx surface font code cursor_pos x y line_height =
  let safe_pos = max 0 (min cursor_pos (String.length code)) in
  let before = String.sub code 0 safe_pos in
  let lines_before = String.split_on_char '\n' before in
  let line_index = max 0 (List.length lines_before - 1) in
  let current_line_prefix =
    match List.rev lines_before with
    | hd :: _ -> hd
    | [] -> ""
  in
  let cursor_x_offset, _ = Gfx.measure_text current_line_prefix font in
  let cursor_x = x + cursor_x_offset in
  let cursor_y = y + (line_index * line_height) in
  Gfx.set_color ctx (Gfx.color 220 240 255 255);
  Gfx.fill_rect ctx surface cursor_x cursor_y 2 (line_height - 2)

let draw_hint_box ctx surface font state code_x code_y code_w =
  match Code_challenge.get_last_failure_reason state with
  | None -> ()
    | Some _ ->
      (match Code_challenge.get_type_exercise_hint state with
       | None -> ()
       | Some hint ->
         let hint_box_y = code_y + 140 in
         let hint_box_h = 42 in
         Gfx.set_color ctx (Gfx.color 38 46 66 255);
         Gfx.fill_rect ctx surface (code_x + 14) hint_box_y (code_w - 28) hint_box_h;
         Gfx.set_color ctx (Gfx.color 128 190 255 255);
         Gfx.fill_rect ctx surface (code_x + 14) hint_box_y (code_w - 28) 2;
         let hint_surf = Gfx.render_text ctx hint font in
         Gfx.blit ctx surface hint_surf (code_x + 24) (hint_box_y + 12))

let draw_single_aerin_frame ctx surface aerin_img dx dy dw dh =
  let sw, sh = Gfx.surface_size aerin_img in
  if sw >= 3 && sh >= 4 then begin
    let frame_w = sw / 3 in
    let frame_h = sh / 4 in
    let sx = frame_w in
    let sy = 0 in
    Gfx.blit_full ctx surface aerin_img sx sy frame_w frame_h dx dy dw dh
  end else
    Gfx.blit_scale ctx surface aerin_img dx dy dw dh

let lambda_duel_comment state =
  match state.Code_challenge.challenge with
  | Some Code_challenge.GolemActivateHp ->
      "Professeur Lambda: Il faut que tu active les points de vie du Golem avant de pouvoir l'attaquer."
  | Some Code_challenge.GolemDealDamage ->
      "Professeur Lambda: Maintenant inflige lui des degats equivalent a ses pv."
  | _ ->
      "Professeur Lambda: Concentre-toi sur ton incantation."

let draw_lambda_duel_battle_screen ctx surface font state =
  let global = Global.get () in
  let ww = Cst.window_width in
  let wh = Cst.window_height in
  let side_margin = 20 in
  let top_margin = 16 in
  let gap = 10 in
  let comment_h = 74 in
  let code_h = 182 in
  let arena_x = side_margin in
  let arena_y = top_margin in
  let arena_w = ww - (2 * side_margin) in
  let arena_h = max 220 (wh - top_margin - comment_h - code_h - (2 * gap) - 10) in
  let comment_x = side_margin in
  let comment_y = arena_y + arena_h + gap in
  let comment_w = arena_w in
  let code_x = side_margin in
  let code_y = comment_y + comment_h + gap in
  let code_w = arena_w in

  (* Combat backdrop *)
  Gfx.set_color ctx (Gfx.color 30 48 86 255);
  Gfx.fill_rect ctx surface 0 0 ww wh;
  Gfx.set_color ctx (Gfx.color 56 88 142 255);
  Gfx.fill_rect ctx surface 0 0 ww (wh / 2);

  Gfx.set_color ctx (Gfx.color 22 32 56 245);
  Gfx.fill_rect ctx surface arena_x arena_y arena_w arena_h;
  Gfx.set_color ctx (Gfx.color 122 166 236 255);
  Gfx.fill_rect ctx surface arena_x arena_y arena_w 3;
  Gfx.fill_rect ctx surface arena_x (arena_y + arena_h - 3) arena_w 3;
  Gfx.fill_rect ctx surface arena_x arena_y 3 arena_h;
  Gfx.fill_rect ctx surface (arena_x + arena_w - 3) arena_y 3 arena_h;

  let golem_res = get_or_load_image golem_img_res ctx "ressources/personnages/Golem.png" in
  let aerin_res = get_or_load_image aerin_fight_img_res ctx "ressources/personnages/Aerin.png" in

  let center_x = arena_x + (arena_w / 2) in
  let golem_w = 250 in
  let golem_h = 250 in
  let golem_x = center_x - (golem_w / 2) in
  let golem_y = arena_y + arena_h - golem_h - 10 in

  let aerin_w = 260 in
  let aerin_h = 320 in
  let aerin_x = golem_x - 140 in
  let aerin_y = golem_y - 40 in
  (match Gfx.get_resource_opt aerin_res with
   | Some img -> draw_single_aerin_frame ctx surface img aerin_x aerin_y aerin_w aerin_h
   | None -> ());

  (match Gfx.get_resource_opt golem_res with
   | Some img -> Gfx.blit_scale ctx surface img golem_x golem_y golem_w golem_h
   | None -> ());

  let duel_label = Gfx.render_text ctx "Duel magique - Salle d'entrainement" font in
  Gfx.blit ctx surface duel_label (arena_x + 16) (arena_y + 12);

  if global.lambda_golem_hp_visible || state.Code_challenge.challenge = Some Code_challenge.GolemDealDamage then begin
    let hp = max 0 global.lambda_golem_hp in
    let hp_box_x_preferred = golem_x + golem_w + 18 in
    let hp_box_x = min (arena_x + arena_w - 262 - 10) hp_box_x_preferred in
    let hp_box_y = arena_y + 26 in
    let hp_box_w = 262 in
    let hp_box_h = 64 in
    let hp_bar_w = 232 in
    let hp_fill_w = hp * hp_bar_w / 20 in
    Gfx.set_color ctx (Gfx.color 18 24 38 255);
    Gfx.fill_rect ctx surface hp_box_x hp_box_y hp_box_w hp_box_h;
    Gfx.set_color ctx (Gfx.color 250 250 250 255);
    Gfx.fill_rect ctx surface hp_box_x hp_box_y hp_box_w 2;
    Gfx.fill_rect ctx surface hp_box_x (hp_box_y + hp_box_h - 2) hp_box_w 2;
    Gfx.fill_rect ctx surface hp_box_x hp_box_y 2 hp_box_h;
    Gfx.fill_rect ctx surface (hp_box_x + hp_box_w - 2) hp_box_y 2 hp_box_h;
    Gfx.set_color ctx (Gfx.color 35 40 52 255);
    Gfx.fill_rect ctx surface (hp_box_x + 14) (hp_box_y + 28) hp_bar_w 16;
    Gfx.set_color ctx (Gfx.color 228 66 66 255);
    Gfx.fill_rect ctx surface (hp_box_x + 14) (hp_box_y + 28) hp_fill_w 16;
    Gfx.set_color ctx (Gfx.color 245 245 245 255);
    let hp_surf = Gfx.render_text ctx (Printf.sprintf "PV = %d" hp) font in
    Gfx.blit ctx surface hp_surf (hp_box_x + 14) (hp_box_y + 6)
  end;

  (* Lambda comment strip above code area *)
  Gfx.set_color ctx (Gfx.color 28 34 52 250);
  Gfx.fill_rect ctx surface comment_x comment_y comment_w comment_h;
  Gfx.set_color ctx (Gfx.color 170 205 255 255);
  Gfx.fill_rect ctx surface comment_x comment_y comment_w 2;
  Gfx.fill_rect ctx surface comment_x (comment_y + comment_h - 2) comment_w 2;
  let comment_lines = wrap_text_lines ctx font (comment_w - 26) (lambda_duel_comment state) in
  ignore
    (List.fold_left
       (fun offset line ->
         let line_s = Gfx.render_text ctx line font in
         Gfx.blit ctx surface line_s (comment_x + 12) (comment_y + 10 + offset);
         offset + 20)
       0 comment_lines);

  (* Bottom coding panel *)
  Gfx.set_color ctx (Gfx.color 16 20 34 255);
  Gfx.fill_rect ctx surface code_x code_y code_w code_h;
  Gfx.set_color ctx (Gfx.color 230 230 235 255);
  Gfx.fill_rect ctx surface code_x code_y code_w 3;
  let code_title = Gfx.render_text ctx "Commande de sort" font in
  Gfx.blit ctx surface code_title (code_x + 12) (code_y + 10);

  let input_x = code_x + 14 in
  let input_y = code_y + 40 in
  let input_w = code_w - 28 in
  let input_h = code_h - 72 in
  Gfx.set_color ctx (Gfx.color 8 10 18 255);
  Gfx.fill_rect ctx surface input_x input_y input_w input_h;
  Gfx.set_color ctx (Gfx.color 88 126 188 255);
  Gfx.fill_rect ctx surface input_x input_y input_w 2;
  Gfx.fill_rect ctx surface input_x (input_y + input_h - 2) input_w 2;
  Gfx.fill_rect ctx surface input_x input_y 2 input_h;
  Gfx.fill_rect ctx surface (input_x + input_w - 2) input_y 2 input_h;

  Gfx.set_color ctx (Gfx.color 120 255 130 255);
  draw_code_lines ctx surface font state.Code_challenge.code (input_x + 10) (input_y + 10) 20;
  draw_code_cursor ctx surface font state.Code_challenge.code state.Code_challenge.cursor_pos (input_x + 10) (input_y + 10) 20;

  Gfx.set_color ctx (Gfx.color 190 190 198 255);
  let instr_surf =
    Gfx.render_text ctx "ENTREE: Valider | Ctrl+ENTREE: Nouvelle ligne" font
  in
  Gfx.blit ctx surface instr_surf (code_x + 12) (code_y + code_h - 24);
  draw_hint_box ctx surface font state code_x code_y code_w

let draw_code_interface state =
  let global = Global.get () in
  let ctx = global.ctx in
  let surface = Gfx.get_surface global.window in
  let font = global.font in

  if is_lambda_duel_challenge state then begin
    draw_lambda_duel_battle_screen ctx surface font state
  end else begin

    let box_x = 50 in
    let box_y = 150 in
    let box_width = Cst.window_width - 100 in
    let box_height = 300 in
  
  Gfx.set_color ctx (Gfx.color 20 20 30 255);
  Gfx.fill_rect ctx surface box_x box_y box_width box_height;
  
  let border_thickness = 3 in
  Gfx.set_color ctx (Gfx.color 100 150 200 255);
  Gfx.fill_rect ctx surface box_x box_y box_width border_thickness;
  Gfx.fill_rect ctx surface box_x (box_y + box_height - border_thickness) box_width border_thickness;
  Gfx.fill_rect ctx surface box_x box_y border_thickness box_height;
  Gfx.fill_rect ctx surface (box_x + box_width - border_thickness) box_y border_thickness box_height;
  
  Gfx.set_color ctx (Gfx.color 255 200 100 255);
  let title =
    match state.Code_challenge.challenge with
    | Some Code_challenge.PowerCalculation -> "Defi magique : Calcul elementaire"
    | Some Code_challenge.GolemActivateHp
    | Some Code_challenge.GolemDealDamage -> "Duel magique : Golem d'entrainement"
    | Some (Code_challenge.TypeExercise _) -> "Tutoriel : Les types"
    | _ -> "Epreuve de Magie"
  in
  let title_surf = Gfx.render_text ctx title font in
  Gfx.blit ctx surface title_surf (box_x + 10) (box_y + 10);
  
  Gfx.set_color ctx (Gfx.color 200 200 255 255);
  let prompt = Code_challenge.get_prompt state in
  let prompt_lines = wrap_text_lines ctx font (box_width - 30) prompt in
  let _ = List.fold_left (fun offset line ->
    let prompt_surf = Gfx.render_text ctx line font in
    Gfx.blit ctx surface prompt_surf (box_x + 10) (box_y + 40 + offset);
    offset + 20
  ) 0 prompt_lines in
  
  let input_x = box_x + 20 in
  let input_y = box_y + 105 in
  let input_width = box_width - 40 in
  let input_height = 100 in
  
  Gfx.set_color ctx (Gfx.color 10 10 20 255);
  Gfx.fill_rect ctx surface input_x input_y input_width input_height;
  
  Gfx.set_color ctx (Gfx.color 80 120 160 255);
  Gfx.fill_rect ctx surface input_x input_y input_width 2;
  Gfx.fill_rect ctx surface input_x (input_y + input_height - 2) input_width 2;
  Gfx.fill_rect ctx surface input_x input_y 2 input_height;
  Gfx.fill_rect ctx surface (input_x + input_width - 2) input_y 2 input_height;
  
    Gfx.set_color ctx (Gfx.color 100 255 100 255);
    draw_code_lines ctx surface font state.Code_challenge.code (input_x + 10) (input_y + 10) 20;
    draw_code_cursor ctx surface font state.Code_challenge.code state.Code_challenge.cursor_pos (input_x + 10) (input_y + 10) 20;
  
  Gfx.set_color ctx (Gfx.color 150 150 150 255);
    let instr_surf = Gfx.render_text ctx "ENTREE: Valider | ECHAP: Annuler | Ctrl+ENTREE: Nouvelle ligne" font in
    Gfx.blit ctx surface instr_surf (box_x + 10) (box_y + 250)
  end

let update (_ : float) (_ : t Seq.t) =
  let global = Global.get () in
  match global.code_challenge_state with
  | Some state when state.Code_challenge.active ->
      draw_code_interface state
  | _ -> ()
