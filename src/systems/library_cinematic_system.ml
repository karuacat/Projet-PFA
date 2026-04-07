open Ecs

type t = Entity.t

let init _ = ()

let actor1_sheet : Gfx.surface Gfx.resource option ref = ref None
let aerin_sheet : Gfx.surface Gfx.resource option ref = ref None
let zoom_aerin_sheet : Gfx.surface Gfx.resource option ref = ref None
let row_down = 0
let row_left = 1
let row_right = 2

let get_or_load cache ctx path =
  match !cache with
  | Some res -> res
  | None ->
      let res = Gfx.load_image ctx path in
      cache := Some res;
      res

let save_status : (string * float) option ref = ref None

let save_button_rect () =
  let w = 190 in
  let h = 44 in
  let x = (Cst.window_width / 2) - w - 14 in
  let y = (Cst.window_height / 2) + 38 in
  (x, y, w, h)

let continue_button_rect () =
  let w = 190 in
  let h = 44 in
  let x = (Cst.window_width / 2) + 14 in
  let y = (Cst.window_height / 2) + 38 in
  (x, y, w, h)

let inside x y (rx, ry, rw, rh) =
  x >= rx && x <= rx + rw && y >= ry && y <= ry + rh

let draw_actor1_professor ctx surface x y w h block_col row =
  let res = get_or_load actor1_sheet ctx "ressources/personnages/Actor1.png" in
  match Gfx.get_resource_opt res with
  | None -> ()
  | Some img ->
      let sw, sh = Gfx.surface_size img in
      if sw >= 12 && sh >= 8 then begin
        let char_block_w = sw / 4 in
        let char_block_h = sh / 2 in
        let frame_w = char_block_w / 3 in
        let frame_h = char_block_h / 4 in
        let sx = (block_col * char_block_w) + frame_w in
        let sy = row * frame_h in
        Gfx.blit_full ctx surface img sx sy frame_w frame_h x y w h
      end

let draw_centered_zoom_aerin global =
  let ctx = global.Global.ctx in
  let surface = Gfx.get_surface global.Global.window in
  let ww = Cst.window_width in
  let wh = Cst.window_height in
  let res = get_or_load zoom_aerin_sheet ctx "ressources/scenes/Zoom-Aerin.png" in
  match Gfx.get_resource_opt res with
  | None -> ()
  | Some img ->
      let sw, sh = Gfx.surface_size img in
      let max_w = int_of_float (float_of_int ww *. 0.82) in
      let max_h = int_of_float (float_of_int wh *. 0.72) in
      let scale = min (float_of_int max_w /. float_of_int sw) (float_of_int max_h /. float_of_int sh) in
      let draw_w = max 1 (int_of_float (float_of_int sw *. scale)) in
      let draw_h = max 1 (int_of_float (float_of_int sh *. scale)) in
      let x = (ww - draw_w) / 2 in
      let y = (wh - draw_h) / 2 in
      Gfx.blit_scale ctx surface img x y draw_w draw_h

let draw_library_cast global =
  let ctx = global.Global.ctx in
  let surface = Gfx.get_surface global.Global.window in
  let px, py, pw, ph = Library_map.professors_rect () in

  let p_w = 44 in
  let p_h = 58 in
  draw_actor1_professor ctx surface (px + (pw / 2) - p_w - 10) (py - 24) p_w p_h 2 row_right;
  draw_actor1_professor ctx surface (px + (pw / 2) + 10) (py - 24) p_w p_h 3 row_left

let draw_chapter_end global =
  let ctx = global.Global.ctx in
  let surface = Gfx.get_surface global.Global.window in
  let ww = Cst.window_width in
  let wh = Cst.window_height in
  let font = global.Global.font in
  Gfx.set_color ctx (Gfx.color 0 0 0 255);
  Gfx.fill_rect ctx surface 0 0 ww wh;
  Gfx.set_color ctx (Gfx.color 230 230 230 255);
  let title = Gfx.render_text ctx "Fin du Chapitre 1" font in
  Gfx.blit ctx surface title (ww / 2 - 138) (wh / 2 - 52);

  let sx, sy, sw, sh = save_button_rect () in
  let cx, cy, cw, ch = continue_button_rect () in
  Gfx.set_color ctx (Gfx.color 52 84 146 255);
  Gfx.fill_rect ctx surface sx sy sw sh;
  Gfx.set_color ctx (Gfx.color 58 128 82 255);
  Gfx.fill_rect ctx surface cx cy cw ch;
  Gfx.set_color ctx (Gfx.color 245 245 245 255);
  let save_t = Gfx.render_text ctx "Sauvegarder" font in
  let cont_t = Gfx.render_text ctx "Continuer" font in
  Gfx.blit ctx surface save_t (sx + 24) (sy + 12);
  Gfx.blit ctx surface cont_t (cx + 30) (cy + 12);
  (match !save_status with
   | Some (msg, _) ->
       let msg_surf = Gfx.render_text ctx msg font in
       Gfx.blit ctx surface msg_surf (ww / 2 - 110) (cy + ch + 12)
   | None -> ())

let finish_cinematic global =
  global.Global.dynamic_magic_cinematic_active <- false;
  global.Global.dynamic_magic_cinematic_done <- true;
  global.Global.dynamic_magic_phase <- 0;
  global.Global.dynamic_magic_timer <- 0.0;
  (match global.Global.dynamic_magic_pending_target_scene with
   | Some target ->
       Scene.set_scene target;
       global.Global.player#position#set Vector.{
         x = float_of_int global.Global.dynamic_magic_spawn_x;
         y = float_of_int global.Global.dynamic_magic_spawn_y;
       }
   | None -> ());
  global.Global.dynamic_magic_pending_target_scene <- None

let can_start_on_library_exit global =
  Scene.current () = Scene.Library
  && Library_guide.training_progress global.Global.library_guide_state > 0
  && not global.Global.dynamic_magic_cinematic_done
  && not global.Global.dynamic_magic_cinematic_active

let start_on_library_exit ~target_scene ~spawn_x ~spawn_y =
  let global = Global.get () in
  if can_start_on_library_exit global then begin
    global.Global.dynamic_magic_cinematic_active <- true;
    global.Global.dynamic_magic_phase <- 0;
    global.Global.dynamic_magic_timer <- 0.0;
    global.Global.dynamic_magic_pending_target_scene <- Some target_scene;
    global.Global.dynamic_magic_spawn_x <- spawn_x;
    global.Global.dynamic_magic_spawn_y <- spawn_y;
    Dialogue.start_dialogue global.Global.dialogue_state Dialogue.dynamic_magic_professors
  end

type click_action =
  | Click_none
  | Click_save
  | Click_continue

let handle_click x y =
  let global = Global.get () in
  if global.Global.dynamic_magic_cinematic_active
     && global.Global.dynamic_magic_phase = 2
  then begin
    if inside x y (save_button_rect ()) then begin
      Click_save
    end else if inside x y (continue_button_rect ()) then begin
      finish_cinematic global;
      Click_continue
    end else
      Click_none
  end else
    Click_none

let set_save_feedback ok =
  save_status := Some ((if ok then "Sauvegarde reussie" else "Echec sauvegarde"), 2.0)

let update dt (_ : t Seq.t) =
  let global = Global.get () in
  if Scene.current () = Scene.Library then
    draw_library_cast global;

  if global.Global.dynamic_magic_cinematic_active then begin
    (match global.Global.dynamic_magic_phase with
     | 0 ->
         if not global.Global.dialogue_state.active then begin
           global.Global.dynamic_magic_phase <- 1;
           global.Global.dynamic_magic_timer <- 0.0;
           Dialogue.start_dialogue global.Global.dialogue_state (Dialogue.create_dialogue [
             { speaker = "Livre"; text = "Magie Dynamique" };
           ])
         end
     | 1 ->
         draw_centered_zoom_aerin global;
         if not global.Global.dialogue_state.active then begin
           global.Global.dynamic_magic_phase <- 2;
           global.Global.dynamic_magic_timer <- 0.0
         end
     | 2 ->
       (match !save_status with
        | Some (msg, tleft) ->
          let nt = tleft -. dt in
          if nt <= 0.0 then save_status := None
          else save_status := Some (msg, nt)
        | None -> ());
       draw_chapter_end global
     | _ -> ());

    if Scene.current () = Scene.Library then begin
      (match global.Global.dynamic_magic_phase with
       | 0 ->
           ()
       | 1 ->
           draw_centered_zoom_aerin global
       | 2 ->
           draw_chapter_end global
       | _ -> ())
      end else if global.Global.dynamic_magic_phase = 2 then
        draw_chapter_end global
  end
