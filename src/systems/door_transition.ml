open Ecs
open Component_defs

type t = Component_defs.door

let init _ = ()

let last_player_pos = ref Vector.zero

let check_door_collision doors =
  let global = Global.get () in
  let Global.{player; tutorial_state; dialogue_state; _} = global in
  let p_pos : Vector.t = player#position#get in
  
  if Vector.equal !last_player_pos p_pos then
    ()
  else begin
    last_player_pos := p_pos;
    
    let p_box : Rect.t = player#box#get in
    
    let player_rect : Rect.rect_f = {
      x = p_pos.x;
      y = p_pos.y;
      width = float p_box.width;
      height = float p_box.height;
    } in
    
    Seq.iter (fun (e : t) ->
      try
        match e#tag#get with
        | Door door_config ->
            if door_config.current_scene = Scene.current () then (
              let d_pos : Vector.t = e#position#get in
              let d_box : Rect.t = e#box#get in
              let door_rect : Rect.rect_f = {
                x = d_pos.x;
                y = d_pos.y;
                width = float d_box.width;
                height = float d_box.height;
              } in
              
              let can_pass =
                if door_config.id = "house_exit" then
                  global.chest_challenge_completed
                else if door_config.id = "town_school_door" then
                  global.knight_challenge_completed
                else
                  true
              in
              
              if Rect.collides player_rect door_rect then (
                if can_pass then (
                  Scene.set_scene door_config.target_scene;
                  player#position#set Vector.{
                    x = float door_config.player_spawn_x;
                    y = float door_config.player_spawn_y;
                  };
                  last_player_pos := player#position#get;
                  (match door_config.target_scene with
                   | Scene.Town -> 
                       Tutorial.show_message tutorial_state "town_explore";
                       Tutorial.show_message tutorial_state "town_signs"
                   | _ -> ())
                ) else if door_config.id = "house_exit" then (
                  if not global.house_exit_attempted then (
                    global.house_exit_attempted <- true;
                    if not dialogue_state.active then
                      Dialogue.start_dialogue dialogue_state Dialogue.house_exit_blocked
                  )
                )
              )
            )
        | _ -> ()
      with _ -> ()
    ) doors
  end

let update _dt doors =
  check_door_collision doors
