open Ecs
open Component_defs

type t = Entity.t

let init _ = ()

let cached_text = ref ""
let cached_text_lines = ref []

let split_text text =
  String.split_on_char '\n' text

let update (_ : float) (_ : t Seq.t) =
  let global = Global.get () in
  let tutorial_state = global.tutorial_state in
  
  if Tutorial.is_active tutorial_state then
    let ctx = global.ctx in
    let surface = Gfx.get_surface global.window in
    
    match Tutorial.current_message tutorial_state with
    | None -> ()
    | Some text ->
        let font = global.font in
        
        let text_lines = 
          if !cached_text = text && !cached_text_lines <> [] then
            !cached_text_lines
          else begin
            Gfx.set_color ctx (Gfx.color 255 255 255 255);
            let lines = split_text text in
            let surfs = List.map (fun line_text -> Gfx.render_text ctx line_text font) lines in
            cached_text := text;
            cached_text_lines := surfs;
            surfs
          end
        in
        
        let box_x = 10 in
        let box_y = 10 in
        let box_width = 400 in
        let line_height = 20 in
        let num_lines = List.length text_lines in
        let box_height = 30 + (num_lines * line_height) in
        
        Gfx.set_color ctx (Gfx.color 40 40 60 220);
        Gfx.fill_rect ctx surface box_x box_y box_width box_height;
        
        let border_thickness = 2 in
        Gfx.set_color ctx (Gfx.color 150 200 255 255);
        Gfx.fill_rect ctx surface box_x box_y box_width border_thickness;
        Gfx.fill_rect ctx surface box_x (box_y + box_height - border_thickness) box_width border_thickness;
        Gfx.fill_rect ctx surface box_x box_y border_thickness box_height;
        Gfx.fill_rect ctx surface (box_x + box_width - border_thickness) box_y border_thickness box_height;
    
        let _ = List.fold_left (fun y_offset text_surf ->
          Gfx.blit ctx surface text_surf (box_x + 10) (box_y + 15 + y_offset);
          y_offset + line_height
        ) 0 text_lines in
        
        let dot_size = 6 in
        let dot_x = box_x + box_width - 15 in
        let dot_y = box_y + box_height / 2 - dot_size / 2 in
        Gfx.set_color ctx (Gfx.color 100 255 150 255);
        Gfx.fill_rect ctx surface dot_x dot_y dot_size dot_size
  else begin
    cached_text := "";
    cached_text_lines := []
  end
