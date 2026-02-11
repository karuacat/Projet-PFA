open System_defs
open Component_defs
open Ecs

let global_cache = ref None

let update dt =
  let global = match !global_cache with
    | Some g -> g
    | None ->
        let g = Global.get () in
        global_cache := Some g;
        g
  in
  
  match global.menu_state with
  | Some menu ->
      let () = Input.handle_input () in
      Menu_system.update () |> ignore;
      Menu_system.draw () |> ignore;
      Gfx.commit global.ctx;
      None
  | None ->
      let () = Player.stop_player () in
      let () = Input.handle_input () in
      
      let () = Tutorial_manager.check_scene_tutorials () in
      
      Move_system.update dt;
      Collision_system.update dt;
      Door_transition_system.update dt;
      Draw_system.update dt;
      Tutorial_system.update dt;
      Dialogue_system.update dt;
      
      Gfx.commit global.ctx;
      None

let run () =
  let window_spec =
    Format.sprintf "OCaml Quest:%dx%d:"
      Cst.window_width Cst.window_height
  in
  let window = Gfx.create window_spec in
  let ctx = Gfx.get_context window in

  let font = Gfx.load_font "ressources/fonts/PressStart2P.ttf" "" 16 in

  let _house_walls = Wall.house_walls () in
  let _town_walls = Wall.town_walls () in
  let _town_house = Wall.town_house_building () in
  let _house_doors = Door.house_doors () in
  let _town_doors = Door.town_doors () in
  
  let _npc1 = Npc.create_npc "Villageois" 300 200 
    { name = "Villageois"; dialogue = Dialogue.npc_villager_1; scene = Scene.Town }
    Scene.Town in
  
  let _npc2 = Npc.create_npc "Marchande" 500 300
    { name = "Marchande"; dialogue = Dialogue.npc_villager_2; scene = Scene.Town }
    Scene.Town in
  
  let _npc3 = Npc.create_npc "Garde" 400 150
    { name = "Garde"; dialogue = Dialogue.npc_guard; scene = Scene.Town }
    Scene.Town in
  
  let _sign1 = Sign.create_sign 250 250
    { title = "Ecole de Magie"; text = "Direction : Nord"; scene = Scene.Town }
    Scene.Town in
  
  let _sign2 = Sign.create_sign 450 400
    { title = "Place du Marche"; text = "Boutiques et commerces"; scene = Scene.Town }
    Scene.Town in
  
  let player = Player.players () in
  let dialogue_state = Dialogue.create_state () in
  let tutorial_state = Tutorial.create_state () in
  
  Tutorial.register_message tutorial_state "move" "Utiliser \"ZQSD\"\npour se deplacer";
  Tutorial.register_message tutorial_state "menu" "Utiliser \"Echap\"\npour ouvrir le menu";
  Tutorial.register_message tutorial_state "interact" "Utilisez \"ESPACE\"\npour interagir";
  
  let menu = Menu.create () in
  let global = Global.{ window; ctx; player; waiting = 0; dialogue_state; tutorial_state; font; menu_state = Some menu } in
  Global.set global;
  
  let rec start_new_game () =
    Scene.set_scene Scene.House;
    global.menu_state <- None;
    Input.invalidate_caches ();
    Dialogue.start_dialogue dialogue_state Dialogue.intro_wake_up;
  
  and start_continue () =
    Scene.set_scene Scene.House;
    global.menu_state <- None;
    Input.invalidate_caches ();
  
  and open_options () =
    Menu.setup_options_menu menu (fun () ->
      setup_main_menu ()
    )
  
  and setup_main_menu () =
    Menu.setup_main_menu menu start_new_game start_continue open_options (fun () -> exit 0)
  in
  
  setup_main_menu ();
  
  Gfx.main_loop update (fun () -> ())