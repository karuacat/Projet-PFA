type menu_state =
  | MainMenu
  | Options
  | Paused

type button = {
  id : string;
  label : string;
  mutable x : int;
  mutable y : int;
  mutable width : int;
  mutable height : int;
  mutable hovered : bool;
  mutable enabled : bool;
  action : unit -> unit;
}

type menu = {
  mutable state : menu_state;
  mutable buttons : button list;
  mutable mouse_x : int;
  mutable mouse_y : int;
}

let create () = {
  state = MainMenu;
  buttons = [];
  mouse_x = 0;
  mouse_y = 0;
}

let add_button menu button =
  menu.buttons <- menu.buttons @ [button]

let clear_buttons menu =
  menu.buttons <- []

let create_button id label x y width height action = {
  id;
  label;
  x;
  y;
  width;
  height;
  hovered = false;
  enabled = true;
  action;
}

let set_mouse_position menu x y =
  menu.mouse_x <- x;
  menu.mouse_y <- y

let update_hover menu =
  List.iter (fun button ->
    if button.enabled then begin
      let hovered = 
        menu.mouse_x >= button.x && 
        menu.mouse_x <= button.x + button.width &&
        menu.mouse_y >= button.y &&
        menu.mouse_y <= button.y + button.height
      in
      button.hovered <- hovered
    end else
      button.hovered <- false
  ) menu.buttons

let click_button menu =
  List.iter (fun button ->
    if button.hovered && button.enabled then button.action ()
  ) menu.buttons

let set_button_enabled menu button_id enabled =
  List.iter (fun button ->
    if button.id = button_id then button.enabled <- enabled
  ) menu.buttons

let setup_main_menu menu callback_new_game callback_continue callback_options callback_quit =
  clear_buttons menu;
  menu.state <- MainMenu;
  
  let button_width = 250 in
  let button_height = 60 in
  let start_x = (Cst.window_width - button_width) / 2 in
  let start_y = 150 in
  let spacing = 90 in
  
  add_button menu (create_button "new_game" "Nouvelle partie" start_x start_y button_width button_height callback_new_game);
  add_button menu (create_button "continue" "Continuer" start_x (start_y + spacing) button_width button_height callback_continue);
  add_button menu (create_button "options" "Options" start_x (start_y + spacing * 2) button_width button_height callback_options);
  add_button menu (create_button "quit" "Quitter" start_x (start_y + spacing * 3) button_width button_height callback_quit);
  
  set_button_enabled menu "continue" false

let setup_options_menu menu callback_back =
  clear_buttons menu;
  menu.state <- Options;
  
  let button_width = 200 in
  let button_height = 60 in
  let start_x = (Cst.window_width - button_width) / 2 in
  let start_y = 400 in
  
  add_button menu (create_button "back" "Retour" start_x start_y button_width button_height callback_back)

let setup_paused_menu menu callback_resume callback_menu callback_quit =
  clear_buttons menu;
  menu.state <- Paused;
  
  let button_width = 250 in
  let button_height = 60 in
  let start_x = (Cst.window_width - button_width) / 2 in
  let start_y = 150 in
  let spacing = 90 in
  
  add_button menu (create_button "resume" "Reprendre" start_x start_y button_width button_height callback_resume);
  add_button menu (create_button "menu" "Menu principal" start_x (start_y + spacing) button_width button_height callback_menu);
  add_button menu (create_button "quit" "Quitter" start_x (start_y + spacing * 2) button_width button_height callback_quit)
