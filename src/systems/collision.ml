open Ecs
open Component_defs

type t = collidable

let init _ = ()

let cell_size = float_of_int Cst.aabb_cell_size

let cell_coord v = int_of_float (floor (v /. cell_size))

let iter_cells (pos : Vector.t) (box : Rect.t) f =
  let min_x = cell_coord pos.Vector.x in
  let min_y = cell_coord pos.Vector.y in
  let max_x = cell_coord (pos.Vector.x +. float_of_int box.width) in
  let max_y = cell_coord (pos.Vector.y +. float_of_int box.height) in
  for x = min_x to max_x do
    for y = min_y to max_y do
      f (x, y)
    done
  done

let cells_of pos box =
  let cells = ref [] in
  iter_cells pos box (fun cell -> cells := cell :: !cells);
  !cells

type entry = {
  entity : t;
  pos : Vector.t;
  box : Rect.t;
  cells : (int * int) list;
}

let grid : ((int * int), t list) Hashtbl.t = Hashtbl.create 64
let entries : (int, entry) Hashtbl.t = Hashtbl.create 64

let remove_from_grid entity cells =
  let id = Oo.id entity in
  List.iter (fun cell ->
    try
      let lst = Hashtbl.find grid cell in
      let lst' = List.filter (fun e -> Oo.id e <> id) lst in
      if lst' = [] then
        Hashtbl.remove grid cell
      else
        Hashtbl.replace grid cell lst'
    with Not_found -> ()
  ) cells

let add_to_grid entity cells =
  List.iter (fun cell ->
    let lst =
      try Hashtbl.find grid cell with
      | Not_found -> []
    in
    Hashtbl.replace grid cell (entity :: lst)
  ) cells

let update_entry (e : t) pos box =
  let id = Oo.id e in
  match (try Some (Hashtbl.find entries id) with Not_found -> None) with
  | Some entry when Vector.equal entry.pos pos && entry.box = box -> ()
  | Some entry ->
      remove_from_grid entry.entity entry.cells;
      let cells = cells_of pos box in
      add_to_grid e cells;
      Hashtbl.replace entries id { entity = e; pos; box; cells }
  | None ->
      let cells = cells_of pos box in
      add_to_grid e cells;
      Hashtbl.add entries id { entity = e; pos; box; cells }

let cleanup_entries seen =
  let to_remove = ref [] in
  Hashtbl.iter (fun id entry ->
    if not (Hashtbl.mem seen id) then
      to_remove := entry :: !to_remove
  ) entries;
  List.iter (fun entry ->
    remove_from_grid entry.entity entry.cells;
    Hashtbl.remove entries (Oo.id entry.entity)
  ) !to_remove

let last_player_pos = ref Vector.zero

let update _ el =
  let Global.{player; _} = Global.get () in
  let player_pos = player#position#get in
  let player_moved = not (Vector.equal !last_player_pos player_pos) in
  last_player_pos := player_pos;
  
  if player_moved then begin
    let player_entity = ref None in
    let seen : (int, unit) Hashtbl.t = Hashtbl.create 64 in

    el |> Seq.iter (fun (e:t) ->
      match e#tag#get with
      | Player -> player_entity := Some e
      | _ ->
          let id = Oo.id e in
          Hashtbl.replace seen id ();
          let pos = e#position#get in
          let box = e#box#get in
          update_entry e pos box
    );

    cleanup_entries seen;

    match !player_entity with
    | None -> ()
    | Some p ->
        let pos1 = p#position#get in
        let box1 = p#box#get in
        let seen_candidates : (int, unit) Hashtbl.t = Hashtbl.create 32 in
        let candidates = ref [] in
        iter_cells pos1 box1 (fun cell ->
          try
            let lst = Hashtbl.find grid cell in
            List.iter (fun e2 ->
              let id = Oo.id e2 in
              if not (Hashtbl.mem seen_candidates id) then begin
                Hashtbl.add seen_candidates id ();
                candidates := e2 :: !candidates
              end
            ) lst
          with Not_found -> ()
        );

        List.iter (fun (e2:t) ->
          let pos2 : Vector.t = e2#position#get in
          let box2 : Rect.t = e2#box#get in
          match Rect.rebound pos1 box1 pos2 box2 with
          | None -> ()
          | Some v -> p#resolve#get v (e2 :> tagged)
        ) !candidates
  end
