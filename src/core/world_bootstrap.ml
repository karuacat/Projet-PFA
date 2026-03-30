open Component_defs

let init_world ctx =
  let _house_walls = Wall.house_walls () in
  let _town_walls = Wall.town_walls () in
  let _school_walls = Wall.school_walls () in
  let _classroom_walls = Wall.classroom_walls () in
  let _house_doors = Door.house_doors () in
  let _town_doors = Door.town_doors () in
  let _school_doors = Door.school_doors () in

  let knight_cx, knight_cy =
    match Town_map.find_marker 'G' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (390, 50)
  in
  let sign_cx, sign_cy =
    match Town_map.find_marker 'P' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (200, 150)
  in

  let _sign1 = Sign.create_sign (sign_cx - 12) (sign_cy - 16)
    {
      title = "Panneau";
      text = "Académie d'OCamlon\n-> Nord\n\nStabilité, Savoir, Structure";
      scene = Scene.Town;
    }
    Scene.Town
  in
  _sign1#texture#set Texture.transparent;

  let knight_sheet = Gfx.load_image ctx "ressources/personnages/Chevalier.png" in
  let marchand_sheet = Gfx.load_image ctx "ressources/personnages/Marchand.png" in
  let vieux_sheet = Gfx.load_image ctx "ressources/personnages/Vieux.png" in
  let knight_texture =
    match Gfx.get_resource_opt knight_sheet with
    | Some img -> Some (Texture.Sprite (img, 0, 2 * 48, 48, 48))
    | None -> None
  in
  let merchant_texture =
    match Gfx.get_resource_opt marchand_sheet with
    | Some img -> Some (Texture.Sprite (img, 48, 0, 48, 48))
    | None -> None
  in
  let scholar_texture =
    match Gfx.get_resource_opt vieux_sheet with
    | Some img -> Some (Texture.Sprite (img, 4 * 48, 2 * 48, 48, 48))
    | None -> None
  in

  let knight_start_x = knight_cx - 20 in
  let knight_start_y = knight_cy - 24 in

  let knight_guardian =
    Npc.create_npc ?texture:knight_texture "Chevalier Gardien" knight_start_x knight_start_y
      { name = "Chevalier Gardien"; dialogue = Dialogue.knight_guardian_intro; scene = Scene.Town }
      Scene.Town
  in

  let merchant_cx, merchant_cy =
    match Town_map.find_marker 'M' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (550, 280)
  in
  let scholar_cx, scholar_cy =
    match Town_map.find_marker 'V' with
    | Some (col, row) -> Town_map.cell_center col row
    | None -> (180, 200)
  in

  let merchant =
    Npc.create_npc ?texture:merchant_texture "Marchand" (merchant_cx - 20) (merchant_cy - 36)
      { name = "Marchand"; dialogue = Dialogue.npc_villager_2; scene = Scene.Town }
      Scene.Town
  in

  let scholar =
    Npc.create_npc ?texture:scholar_texture "Vieille érudite" (scholar_cx - 20) (scholar_cy - 24)
      { name = "Vieille érudite"; dialogue = Dialogue.npc_villager_1; scene = Scene.Town }
      Scene.Town
  in

  Story_events_system.setup_town_npcs
    ~knight:(knight_guardian :> Component_defs.npc_entity)
    ~merchant:(merchant :> Component_defs.npc_entity)
    ~scholar:(scholar :> Component_defs.npc_entity);

  let eleves_sheet = Gfx.load_image ctx "ressources/personnages/Eleves.png" in
  let student_boy_texture =
    match Gfx.get_resource_opt eleves_sheet with
    | Some img ->
        let w, h = Gfx.surface_size img in
        let frame_w = if w >= 6 then w / 6 else 48 in
        let frame_h = if h >= 4 then h / 4 else 48 in
        Some (Texture.Sprite (img, frame_w, 0, frame_w, frame_h))
    | None -> None
  in
  let student_girl_texture =
    match Gfx.get_resource_opt eleves_sheet with
    | Some img ->
        let w, h = Gfx.surface_size img in
        let frame_w = if w >= 6 then w / 6 else 48 in
        let frame_h = if h >= 4 then h / 4 else 48 in
        Some (Texture.Sprite (img, 4 * frame_w, 0, frame_w, frame_h))
    | None -> None
  in

  let default_school_starts = [ (5, 8); (6, 8); (7, 8) ] in
  let school_start_cells =
    match School_map.marker_cells 'E' with
    | [] -> default_school_starts
    | cells ->
        let take =
          if List.length cells >= 3 then
            List.fold_left
              (fun (acc, n) cell -> if n < 3 then (cell :: acc, n + 1) else (acc, n))
              ([], 0)
              cells
            |> fst
            |> List.rev
          else
            cells @ List.filteri (fun idx _ -> idx >= List.length cells) default_school_starts
        in
        take
  in
  let school_start_pos (col, row) = Story_events_system.school_npc_target col row in
  let student_a_start_x, student_a_start_y = school_start_pos (List.nth school_start_cells 0) in
  let student_b_start_x, student_b_start_y = school_start_pos (List.nth school_start_cells 1) in
  let student_c_start_x, student_c_start_y = school_start_pos (List.nth school_start_cells 2) in
  let student_boy =
    Npc.create_npc ?texture:student_boy_texture "Élève A" (int_of_float student_a_start_x)
      (int_of_float student_a_start_y)
      {
        name = "Élève A";
        dialogue = Dialogue.create_dialogue [ { speaker = "Eleve"; text = "Le cours va commencer !" } ];
        scene = Scene.School;
      }
      Scene.School
  in
  let student_girl =
    Npc.create_npc ?texture:student_girl_texture "Élève B" (int_of_float student_b_start_x)
      (int_of_float student_b_start_y)
      {
        name = "Élève B";
        dialogue = Dialogue.create_dialogue [ { speaker = "Eleve"; text = "Vite, on y va !" } ];
        scene = Scene.School;
      }
      Scene.School
  in
  let student_messenger =
    Npc.create_npc ?texture:student_girl_texture "Élève" (int_of_float student_c_start_x)
      (int_of_float student_c_start_y)
      {
        name = "Élève";
        dialogue =
          Dialogue.create_dialogue
            [ { speaker = "Eleve"; text = "Vite ! Le Professeur Lambda va commencer son cours !" } ];
        scene = Scene.School;
      }
      Scene.School
  in
  Story_events_system.setup_school_npcs
    ~student_a:(student_boy :> Component_defs.npc_entity)
    ~student_b:(student_girl :> Component_defs.npc_entity)
    ~messenger:(student_messenger :> Component_defs.npc_entity);

  let lambda_sprite_path =
    if Sys.file_exists "ressources/personnages/ProfesseurLambda.png" then
      "ressources/personnages/ProfesseurLambda.png"
    else
      "ressources/personnages/ProfLambda.png"
  in
  let lambda_sheet = Gfx.load_image ctx lambda_sprite_path in
  let aerin_sheet = Gfx.load_image ctx "ressources/personnages/Aerin.png" in
  let lambda_texture =
    match Gfx.get_resource_opt lambda_sheet with
    | Some img ->
        let frame_w = 48 in
        let frame_h = 48 in
        Some (Texture.Sprite (img, frame_w, 0, frame_w, frame_h))
    | None -> None
  in
  let aerin_texture =
    match Gfx.get_resource_opt aerin_sheet with
    | Some img ->
        let frame_w = 48 in
        let frame_h = 48 in
        Some (Texture.Sprite (img, frame_w, 0, frame_w, frame_h))
    | None -> None
  in

  let stool_a_x, stool_a_y = Classroom_map.cell_center 9 7 in
  let stool_b_x, stool_b_y = Classroom_map.cell_center 14 7 in
  let stool_c_x, stool_c_y = Classroom_map.cell_center 17 11 in
  let classroom_student_a =
    Npc.create_npc ?texture:student_boy_texture "Élève A" (stool_a_x - 20) (stool_a_y - 30)
      {
        name = "Élève A";
        dialogue = Dialogue.create_dialogue [ { speaker = "Eleve"; text = "Chut... le cours commence." } ];
        scene = Scene.Classroom;
      }
      Scene.Classroom
  in
  let classroom_student_b =
    Npc.create_npc ?texture:student_girl_texture "Élève B" (stool_b_x - 20) (stool_b_y - 30)
      {
        name = "Élève B";
        dialogue = Dialogue.create_dialogue [ { speaker = "Eleve"; text = "Je prends des notes." } ];
        scene = Scene.Classroom;
      }
      Scene.Classroom
  in
  let classroom_student_c =
    Npc.create_npc ?texture:student_boy_texture "Élève" (stool_c_x - 20) (stool_c_y - 30)
      {
        name = "Élève";
        dialogue =
          Dialogue.create_dialogue
            [ { speaker = "Eleve"; text = "Le professeur manipule les symboles..." } ];
        scene = Scene.Classroom;
      }
      Scene.Classroom
  in

  let aerin_stool_x, aerin_stool_y = Classroom_map.cell_center 17 7 in
  let aerin =
    Npc.create_npc ?texture:aerin_texture "Aerin" (aerin_stool_x - 20) (aerin_stool_y - 30)
      {
        name = "Aerin";
        dialogue =
          Dialogue.create_dialogue
            [ { speaker = "Aerin"; text = "Moi j'ai appris ca quand j'avais huit ans." } ];
        scene = Scene.Classroom;
      }
      Scene.Classroom
  in

  let lambda_cx, lambda_cy = Classroom_map.cell_center 14 3 in
  let professor_lambda =
    Npc.create_npc ?texture:lambda_texture "Professeur Lambda" (lambda_cx - 22) (lambda_cy - 30)
      {
        name = "Professeur Lambda";
        dialogue =
          Dialogue.create_dialogue
            [ { speaker = "Professeur Lambda"; text = "Concentre-toi sur le defi: let puissance = 10 + 5;;" } ];
        scene = Scene.Classroom;
      }
      Scene.Classroom
  in
  Story_events_system.setup_classroom_npcs
    ~professor:(professor_lambda :> Component_defs.npc_entity)
    ~aerin:(aerin :> Component_defs.npc_entity)
    ~student_a:(classroom_student_a :> Component_defs.npc_entity)
    ~student_b:(classroom_student_b :> Component_defs.npc_entity)
    ~student_c:(classroom_student_c :> Component_defs.npc_entity);

  let chest_x, chest_y = House_map.chest_pos () in
  let _apprentice_chest =
    Sign.create_sign chest_x chest_y
      {
        title = "Coffre de l'Apprenti";
        text = "Un coffre verrouillé par un sceau magique.";
        scene = Scene.House;
      }
      Scene.House
  in

  let book_x, book_y = House_map.book_pos () in
  let _secret_book = Book.create_book book_x book_y Scene.House in
  _apprentice_chest#texture#set Texture.transparent
