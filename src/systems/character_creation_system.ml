type button = {
  label : string;
  mutable x : int;
  mutable y : int;
  mutable width : int;
  mutable height : int;
  mutable hovered : bool;
  mutable enabled : bool;
  action : unit -> unit;
}

let draw_button ctx surface font button =
  let color = 
    if not button.enabled then 
      Gfx.color 50 50 50 255
    else if button.hovered then 
      Gfx.color 100 150 255 255
    else 
      Gfx.color 70 70 70 255
  in
  let border_color = 
    if not button.enabled then 
      Gfx.color 30 30 30 255
    else if button.hovered then 
      Gfx.color 50 100 200 255
    else 
      Gfx.color 50 50 50 255
  in
  
  Gfx.set_color ctx color;
  Gfx.fill_rect ctx surface button.x button.y button.width button.height;
  
  Gfx.set_color ctx border_color;
  for i = 0 to 2 do
    Gfx.fill_rect ctx surface (button.x + i) (button.y + i) (button.width - 2*i) 1;
    Gfx.fill_rect ctx surface (button.x + i) (button.y + i) 1 (button.height - 2*i);
    Gfx.fill_rect ctx surface (button.x + button.width - i - 1) (button.y + i) 1 (button.height - 2*i);
    Gfx.fill_rect ctx surface (button.x + i) (button.y + button.height - i - 1) (button.width - 2*i) 1
  done;
  
  let text_color = 
    if not button.enabled then 
      Gfx.color 150 150 150 255
    else 
      Gfx.color 255 255 255 255
  in
  Gfx.set_color ctx text_color;
  let text_surface = Gfx.render_text ctx button.label font in
  let text_width, text_height = Gfx.surface_size text_surface in
  let text_x = button.x + (button.width - text_width) / 2 in
  let text_y = button.y + (button.height - text_height) / 2 in
  Gfx.blit ctx surface text_surface text_x text_y

let draw_gender_button ctx surface font label x y width height selected =
  let color = 
    if selected then 
      Gfx.color 100 150 255 255
    else 
      Gfx.color 70 70 70 255
  in
  let border_color = 
    if selected then 
      Gfx.color 50 100 200 255
    else 
      Gfx.color 50 50 50 255
  in
  
  Gfx.set_color ctx color;
  Gfx.fill_rect ctx surface x y width height;

  Gfx.set_color ctx border_color;
  for i = 0 to 2 do
    Gfx.fill_rect ctx surface (x + i) (y + i) (width - 2*i) 1;
    Gfx.fill_rect ctx surface (x + i) (y + i) 1 (height - 2*i);
    Gfx.fill_rect ctx surface (x + width - i - 1) (y + i) 1 (height - 2*i);
    Gfx.fill_rect ctx surface (x + i) (y + height - i - 1) (width - 2*i) 1
  done;
  
  let text_color = Gfx.color 255 255 255 255 in
  Gfx.set_color ctx text_color;
  let text_surface = Gfx.render_text ctx label font in
  let text_width, text_height = Gfx.surface_size text_surface in
  let text_x = x + (width - text_width) / 2 in
  let text_y = y + (height - text_height) / 2 in
  Gfx.blit ctx surface text_surface text_x text_y

let update () =
  None

let draw () =
  let global = Global.get () in
  match global.character_creation_state with
  | None -> None
  | Some char_state ->
      let surface = Gfx.get_surface global.window in
      let ww, wh = Gfx.get_context_logical_size global.ctx in
      
      Gfx.set_color global.ctx (Gfx.color 20 20 20 255);
      Gfx.fill_rect global.ctx surface 0 0 ww wh;
      
      let title_color = Gfx.color 255 200 100 255 in
      Gfx.set_color global.ctx title_color;
      let title_surface = Gfx.render_text global.ctx "Creation du Personnage" global.font in
      let title_width, _ = Gfx.surface_size title_surface in
      Gfx.blit global.ctx surface title_surface 
        ((ww - title_width) / 2) 30;
      
      let gender_y = 120 in
      let gender_label_surface = Gfx.render_text global.ctx "Choisir le genre :" global.font in
      Gfx.blit global.ctx surface gender_label_surface 100 gender_y;
      
      let button_width = 150 in
      let button_height = 60 in
      let button_y = gender_y + 50 in
      
      let male_selected = 
        match Character_creation.get_gender char_state with
        | Some Character_creation.Male -> true
        | _ -> false
      in
      let female_selected = 
        match Character_creation.get_gender char_state with
        | Some Character_creation.Female -> true
        | _ -> false
      in
      
      draw_gender_button global.ctx surface global.font "Masculin" 150 button_y button_width button_height male_selected;
      draw_gender_button global.ctx surface global.font "Feminin" 450 button_y button_width button_height female_selected;
    
      let name_y = button_y + 120 in
      let name_label_surface = Gfx.render_text global.ctx "Nom du personnage (max 10):" global.font in
      Gfx.blit global.ctx surface name_label_surface 100 name_y;

      let input_y = name_y + 50 in
      let input_width = 300 in
      let input_height = 50 in
      let input_x = (ww - input_width) / 2 in
      
      Gfx.set_color global.ctx (Gfx.color 40 40 60 255);
      Gfx.fill_rect global.ctx surface input_x input_y input_width input_height;
      
      let border_color = if char_state.input_focused then Gfx.color 100 150 255 255 else Gfx.color 100 100 100 255 in
      Gfx.set_color global.ctx border_color;
      for i = 0 to 2 do
        Gfx.fill_rect global.ctx surface (input_x + i) (input_y + i) (input_width - 2*i) 1;
        Gfx.fill_rect global.ctx surface (input_x + i) (input_y + i) 1 (input_height - 2*i);
        Gfx.fill_rect global.ctx surface (input_x + input_width - i - 1) (input_y + i) 1 (input_height - 2*i);
        Gfx.fill_rect global.ctx surface (input_x + i) (input_y + input_height - i - 1) (input_width - 2*i) 1
      done;

      let name = Character_creation.get_name char_state in
      let text_color = Gfx.color 255 255 255 255 in
      Gfx.set_color global.ctx text_color;
      if String.length name > 0 then begin
        let text_surface = Gfx.render_text global.ctx name global.font in
        Gfx.blit global.ctx surface text_surface (input_x + 10) (input_y + 12)
      end;

      if String.length name = 0 then begin
        let hint_color = Gfx.color 150 150 150 255 in
        Gfx.set_color global.ctx hint_color;
        let hint_surface = Gfx.render_text global.ctx "Tappez votre nom ici..." global.font in
        Gfx.blit global.ctx surface hint_surface (input_x + 10) (input_y + 12)
      end;
      
      if Character_creation.is_complete char_state then begin
        let button_width = 200 in
        let button_height = 60 in
        let button_x = (ww - button_width) / 2 in
        let button_y = input_y + 100 in
        
        let color = Gfx.color 100 150 255 255 in
        let border_color = Gfx.color 50 100 200 255 in
        
        Gfx.set_color global.ctx color;
        Gfx.fill_rect global.ctx surface button_x button_y button_width button_height;
        
        Gfx.set_color global.ctx border_color;
        for i = 0 to 2 do
          Gfx.fill_rect global.ctx surface (button_x + i) (button_y + i) (button_width - 2*i) 1;
          Gfx.fill_rect global.ctx surface (button_x + i) (button_y + i) 1 (button_height - 2*i);
          Gfx.fill_rect global.ctx surface (button_x + button_width - i - 1) (button_y + i) 1 (button_height - 2*i);
          Gfx.fill_rect global.ctx surface (button_x + i) (button_y + button_height - i - 1) (button_width - 2*i) 1
        done;
        
        let text_color = Gfx.color 255 255 255 255 in
        Gfx.set_color global.ctx text_color;
        let text_surface = Gfx.render_text global.ctx "Continuer" global.font in
        let text_width, text_height = Gfx.surface_size text_surface in
        let text_x = button_x + (button_width - text_width) / 2 in
        let text_y = button_y + (button_height - text_height) / 2 in
        Gfx.blit global.ctx surface text_surface text_x text_y
      end;
      
      None

