let draw_button ctx surface font button =
  let color = 
    if not button.Menu.enabled then 
      Gfx.color 50 50 50 255
    else if button.Menu.hovered then 
      Gfx.color 100 100 255 255
    else 
      Gfx.color 70 70 70 255
  in
  let border_color = 
    if not button.Menu.enabled then 
      Gfx.color 30 30 30 255
    else if button.Menu.hovered then 
      Gfx.color 50 50 200 255
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
    if not button.Menu.enabled then 
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

let update () =
  let global = Global.get () in
  match global.menu_state with
  | Some menu ->
      Menu.update_hover menu;
      None
  | None -> None

let draw () =
  let global = Global.get () in
  match global.menu_state with
  | Some menu ->
      let surface = Gfx.get_surface global.window in
      let ww, wh = Gfx.get_context_logical_size global.ctx in
      
      Gfx.set_color global.ctx (Gfx.color 20 20 20 255);
      Gfx.fill_rect global.ctx surface 0 0 ww wh;

      let title_color = Gfx.color 255 200 100 255 in
      Gfx.set_color global.ctx title_color;
      let title_surface = Gfx.render_text global.ctx "OCaml Quest" global.font in
      let title_width, _ = Gfx.surface_size title_surface in
      Gfx.blit global.ctx surface title_surface 
        ((ww - title_width) / 2) 50;

      List.iter (fun button ->
        draw_button global.ctx surface global.font button
      ) menu.Menu.buttons;
      
      None
  | None -> None


