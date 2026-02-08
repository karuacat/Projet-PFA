open Ecs
open Component_defs

type t = Entity.t

let init _ = ()

let cached_text = ref ""
let cached_text_surf = ref None

let update (_ : float) (_ : t Seq.t) =
  let global = Global.get () in
  let tutorial_state = global.tutorial_state in
  
  if Tutorial.is_active tutorial_state then
    let ctx = global.ctx in
    let surface = Gfx.get_surface global.window in
    
    match Tutorial.current_message tutorial_state with
    | None -> ()
    | Some text ->
        let box_x = 10 in
        let box_y = 10 in
        let box_width = 400 in
        let box_height = 50 in
        
        Gfx.set_color ctx (Gfx.color 40 40 60 220);
        Gfx.fill_rect ctx surface box_x box_y box_width box_height;
        
        let border_thickness = 2 in
        Gfx.set_color ctx (Gfx.color 150 200 255 255);
        Gfx.fill_rect ctx surface box_x box_y box_width border_thickness;
        Gfx.fill_rect ctx surface box_x (box_y + box_height - border_thickness) box_width border_thickness;
        Gfx.fill_rect ctx surface box_x box_y border_thickness box_height;
        Gfx.fill_rect ctx surface (box_x + box_width - border_thickness) box_y border_thickness box_height;
    
        let font = global.font in
        
        let text_surf = 
          if !cached_text = text && !cached_text_surf <> None then
            match !cached_text_surf with Some s -> s | None -> assert false
          else begin
            Gfx.set_color ctx (Gfx.color 255 255 255 255);
            let surf = Gfx.render_text ctx text font in
            cached_text := text;
            cached_text_surf := Some surf;
            surf
          end
        in
        
        Gfx.blit ctx surface text_surf (box_x + 10) (box_y + 15);
        
        let dot_size = 6 in
        let dot_x = box_x + box_width - 15 in
        let dot_y = box_y + box_height / 2 - dot_size / 2 in
        Gfx.set_color ctx (Gfx.color 100 255 150 255);
        Gfx.fill_rect ctx surface dot_x dot_y dot_size dot_size
  else begin
    cached_text := "";
    cached_text_surf := None
  end
