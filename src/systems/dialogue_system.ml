open Ecs
open Component_defs

type t = Entity.t

let init _ = ()

let cached_speaker = ref ""
let cached_text = ref ""
let cached_speaker_surf = ref None
let cached_text_surf = ref None

let update (_ : float) (_ : t Seq.t) =
  let global = Global.get () in
  let dialogue_state = global.dialogue_state in
  
  if dialogue_state.active then
    let ctx = global.ctx in
    let surface = Gfx.get_surface global.window in
    
    let box_x = 50 in
    let box_y = Cst.window_height - 150 in
    let box_width = Cst.window_width - 100 in
    let box_height = 100 in
    
    Gfx.set_color ctx (Gfx.color 20 20 40 255);
    Gfx.fill_rect ctx surface box_x box_y box_width box_height;
    
    let border_thickness = 2 in
    Gfx.set_color ctx (Gfx.color 200 200 255 255);
    Gfx.fill_rect ctx surface box_x box_y box_width border_thickness;
    Gfx.fill_rect ctx surface box_x (box_y + box_height - border_thickness) box_width border_thickness;
    Gfx.fill_rect ctx surface box_x box_y border_thickness box_height;
    Gfx.fill_rect ctx surface (box_x + box_width - border_thickness) box_y border_thickness box_height;
    
    match Dialogue.current_line dialogue_state with
    | None -> ()
    | Some line ->
        let font = global.font in
        
        let speaker_surf = 
          if !cached_speaker = line.speaker && !cached_speaker_surf <> None then
            match !cached_speaker_surf with Some s -> s | None -> assert false
          else begin
            Gfx.set_color ctx (Gfx.color 255 200 100 255);
            let surf = Gfx.render_text ctx line.speaker font in
            cached_speaker := line.speaker;
            cached_speaker_surf := Some surf;
            surf
          end
        in
        
        let text_surf = 
          if !cached_text = line.text && !cached_text_surf <> None then
            match !cached_text_surf with Some s -> s | None -> assert false
          else begin
            Gfx.set_color ctx (Gfx.color 255 255 255 255);
            let surf = Gfx.render_text ctx line.text font in
            cached_text := line.text;
            cached_text_surf := Some surf;
            surf
          end
        in
        
        Gfx.blit ctx surface speaker_surf (box_x + 10) (box_y + 10);
        Gfx.blit ctx surface text_surf (box_x + 10) (box_y + 40);
        
        let indicator_size = 10 in
        let indicator_x = box_x + box_width - 20 in
        let indicator_y = box_y + box_height - 20 in
        if not (Dialogue.is_finished dialogue_state) then
          Gfx.set_color ctx (Gfx.color 100 255 100 255)
        else
          Gfx.set_color ctx (Gfx.color 255 150 50 255);
        Gfx.fill_rect ctx surface indicator_x indicator_y indicator_size indicator_size
  else begin
    cached_speaker := "";
    cached_text := "";
    cached_speaker_surf := None;
    cached_text_surf := None
  end
