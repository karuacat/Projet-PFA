open Component_defs

type route_mode =
  | Scripted
  | Dynamic

type snapshot = {
  scene : Scene.scene;
  npc_rect : int * int * int * int;
  target : float * float;
  waypoint : (float * float) option;
  path_points : (float * float) list;
  blocked_reason : string option;
  mode : route_mode;
}

let snapshots : (npc_entity, snapshot) Hashtbl.t = Hashtbl.create 64

let set ?waypoint ?(path_points=[]) ?blocked_reason npc mode target_x target_y =
  let pos = npc#position#get in
  let box = npc#box#get in
  Hashtbl.replace snapshots npc {
    scene = Scene.current ();
    npc_rect =
      ( int_of_float pos.Vector.x,
        int_of_float pos.Vector.y,
        box.Rect.width,
        box.Rect.height );
    target = (target_x, target_y);
    waypoint;
    path_points;
    blocked_reason;
    mode;
  }

let update_block_reason npc blocked_reason =
  match Hashtbl.find_opt snapshots npc with
  | Some dbg -> Hashtbl.replace snapshots npc { dbg with blocked_reason }
  | None -> ()

let clear () =
  Hashtbl.clear snapshots

let all () =
  Hashtbl.fold (fun _ dbg acc -> dbg :: acc) snapshots []
