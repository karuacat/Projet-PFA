open Ecs

type t = Entity.t

let init _ = ()

let book_bg_res : Gfx.surface Gfx.resource option ref = ref None

let get_book_bg ctx =
  match !book_bg_res with
  | Some res -> res
  | None ->
      let res = Gfx.load_image ctx "ressources/scenes/Livre.jpg" in
      book_bg_res := Some res;
      res

let draw_text ctx surface font x y text =
  let surf = Gfx.render_text ctx text font in
  Gfx.blit ctx surface surf x y

let draw_shadow_text ctx surface font x y text =
  draw_text ctx surface font (x + 2) (y + 2) text;
  draw_text ctx surface font x y text

let wrap_text_lines ctx font max_width text =
  let words = String.split_on_char ' ' text in
  let rec loop acc current_line remaining =
    match remaining with
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

let draw_paragraph ctx surface font x y max_width text =
  let lines = wrap_text_lines ctx font max_width text in
  List.fold_left
    (fun yy line ->
      draw_shadow_text ctx surface font x yy line;
      yy + 28)
    y lines

let draw_summary ctx surface font =
  let tx, ty, pw, _ = Library_guide.panel_rect () in
  let left_x = tx + 16 in
  let left_w = (pw / 2) - 66 in
  let y1 = draw_paragraph ctx surface font left_x (ty + 26) left_w "Sommaire" in
  let y2 = draw_paragraph ctx surface font left_x (y1 + 18) left_w "Chapitre 1 - Les types" in
  let y3 = draw_paragraph ctx surface font left_x (y2 + 18) left_w "Chapitre 2 - ??? (verrouille)" in
  let y4 = draw_paragraph ctx surface font left_x (y3 + 18) left_w "Chapitre 3 - ??? (verrouille)" in
  ignore (draw_paragraph ctx surface font left_x (y4 + 20) left_w "Clique sur le chapitre 1 pour commencer.");
  ()

let draw_types_page (global : Global.t) ctx surface font =
  let tx, ty, pw, _ = Library_guide.panel_rect () in
  let left_x = tx + 22 in
  let right_x = tx + (pw / 2) + 40 in
  let page_w = (pw / 2) - 72 in
  let y0 = ty + 24 in
  let y1 = draw_paragraph ctx surface font left_x y0 page_w "Chapitre 1 - Les types" in
  let y2 = draw_paragraph ctx surface font left_x (y1 + 8) page_w "int : nombre entier. Exemple : 3, 12, 0." in
  ignore (draw_paragraph ctx surface font left_x (y2 + 8) page_w "bool : true (vrai) ou false (faux).");
  let r1 = draw_paragraph ctx surface font right_x (ty + 24) page_w "string : texte entre guillemets. Exemple : \"Aerin\"." in
  let r2 = draw_paragraph ctx surface font right_x (r1 + 8) page_w "Exemple : let age = 16;;" in
  let r3 = draw_paragraph ctx surface font right_x (r2 + 4) page_w "Exemple : let porte_ouverte = false;;" in
  ignore (draw_paragraph ctx surface font right_x (r3 + 4) page_w "Exemple : let ville = \"OCamlon\";;");
  let bx, by, _, _ = Library_guide.back_rect () in
  draw_shadow_text ctx surface font bx by "Sommaire";
  let ex, ey, ew, _ = Library_guide.train_rect () in
  draw_shadow_text ctx surface font ex ey (Library_guide.training_button_label global.library_guide_state);

  let progress = Library_guide.training_progress global.library_guide_state in
  let progress_text = Library_guide.training_progress_text global.library_guide_state in
  let bar_x = ex in
  let bar_y = ey + 30 in
  let bar_w = ew in
  let bar_h = 12 in
  let fill_w = (progress * bar_w) / 3 in
  Gfx.set_color ctx (Gfx.color 206 190 164 255);
  Gfx.fill_rect ctx surface bar_x bar_y bar_w bar_h;
  Gfx.set_color ctx (Gfx.color 157 92 73 255);
  Gfx.fill_rect ctx surface bar_x bar_y fill_w bar_h;
  draw_shadow_text ctx surface font (bar_x + (bar_w / 2) - 16) (bar_y + 16) progress_text;
  ()

let update (_ : float) (_ : t Seq.t) =
  let global = Global.get () in
  if global.library_guide_state.active then begin
    let ctx = global.ctx in
    let surface = Gfx.get_surface global.window in
    let font = global.font in
    (match Gfx.get_resource_opt (get_book_bg ctx) with
     | Some img -> Gfx.blit_scale ctx surface img 0 0 Cst.window_width Cst.window_height
     | None ->
         Gfx.set_color ctx (Gfx.color 72 53 27 255);
         Gfx.fill_rect ctx surface 0 0 Cst.window_width Cst.window_height);
    Gfx.set_color ctx (Gfx.color 151 78 61 255);
    let cx, cy, cw, ch = Library_guide.close_rect () in
    Gfx.set_color ctx (Gfx.color 165 82 59 255);
    Gfx.fill_rect ctx surface cx cy cw ch;
    Gfx.set_color ctx (Gfx.color 245 236 220 255);
    draw_shadow_text ctx surface font (cx + 8) (cy + 4) "X";
    Gfx.set_color ctx (Gfx.color 151 78 61 255);
    (match global.library_guide_state.page with
     | Library_guide.Summary -> draw_summary ctx surface font
      | Library_guide.Types -> draw_types_page global ctx surface font)
  end
