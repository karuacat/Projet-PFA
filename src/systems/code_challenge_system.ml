open Ecs

type t = Entity.t

let init _ = ()

let draw_code_interface state =
  let global = Global.get () in
  let ctx = global.ctx in
  let surface = Gfx.get_surface global.window in
  let font = global.font in

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
  let title_surf = Gfx.render_text ctx "Epreuve de Magie" font in
  Gfx.blit ctx surface title_surf (box_x + 10) (box_y + 10);
  
  Gfx.set_color ctx (Gfx.color 200 200 255 255);
  let prompt = Code_challenge.get_prompt state in
  let prompt_surf = Gfx.render_text ctx prompt font in
  Gfx.blit ctx surface prompt_surf (box_x + 10) (box_y + 40);
  
  let input_x = box_x + 20 in
  let input_y = box_y + 80 in
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
  let code = state.Code_challenge.code in
  if String.length code > 0 then begin
    let lines = String.split_on_char '\n' code in
    let line_height = 20 in
    let line_num = ref 0 in
    List.iter (fun line ->
      if String.length line > 0 then begin
        let code_surf = Gfx.render_text ctx line font in
        Gfx.blit ctx surface code_surf (input_x + 10) (input_y + 10 + (!line_num * line_height));
        incr line_num
      end else
        incr line_num
    ) lines
  end;
  
  Gfx.set_color ctx (Gfx.color 150 150 150 255);
  let instr_surf = Gfx.render_text ctx "ENTREE: Valider | ECHAP: Annuler" font in
  Gfx.blit ctx surface instr_surf (box_x + 10) (box_y + 250)

let update (_ : float) (_ : t Seq.t) =
  let global = Global.get () in
  match global.code_challenge_state with
  | Some state when state.Code_challenge.active ->
      draw_code_interface state
  | _ -> ()
