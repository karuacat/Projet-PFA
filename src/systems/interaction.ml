open Ecs
open Component_defs

let npc_table : (npc_entity, unit) Entity.Table.t = Entity.Table.create 16
let sign_table : (sign_entity, unit) Entity.Table.t = Entity.Table.create 16

let register_npc (npc : npc_entity) =
  if not (Entity.Table.mem npc_table npc) then begin
    Entity.Table.add npc_table npc ();
    Entity.register (npc :> Entity.t) (fun () -> Entity.Table.remove npc_table npc)
  end

let register_sign (sign : sign_entity) =
  if not (Entity.Table.mem sign_table sign) then begin
    Entity.Table.add sign_table sign ();
    Entity.register (sign :> Entity.t) (fun () -> Entity.Table.remove sign_table sign)
  end

let boxes_adjacent (v1 : Vector.t) (r1 : Rect.t) (v2 : Vector.t) (r2 : Rect.t) =
  (* Retourne true si les boîtes se touchent ou se chevauchent *)
  let right1 = v1.x +. float r1.width in
  let bottom1 = v1.y +. float r1.height in
  let right2 = v2.x +. float r2.width in
  let bottom2 = v2.y +. float r2.height in
  v1.x <= right2 && right1 >= v2.x && v1.y <= bottom2 && bottom1 >= v2.y

let find_npc_at player_pos player_box =
  let current_scene = Scene.current () in
  let closest = ref None in
  let closest_dist_sq = ref max_float in
  Entity.Table.to_seq_keys npc_table
  |> Seq.iter (fun npc ->
      match npc#tag#get with
      | InScene scene when scene = current_scene ->
          let npc_pos = npc#position#get in
          let npc_box = npc#box#get in
          if boxes_adjacent player_pos player_box npc_pos npc_box then begin
            let dist_vec = Vector.sub npc_pos player_pos in
            let distance_sq = Vector.dot dist_vec dist_vec in
            if distance_sq < !closest_dist_sq then begin
              closest := Some npc;
              closest_dist_sq := distance_sq
            end
          end
      | _ -> ()
    );
  !closest

let find_sign_at player_pos player_box =
  let current_scene = Scene.current () in
  let closest = ref None in
  let closest_dist_sq = ref max_float in
  Entity.Table.to_seq_keys sign_table
  |> Seq.iter (fun sign ->
      match sign#tag#get with
      | InScene scene when scene = current_scene ->
          let sign_pos = sign#position#get in
          let sign_box = sign#box#get in
          if boxes_adjacent player_pos player_box sign_pos sign_box then begin
            let dist_vec = Vector.sub sign_pos player_pos in
            let distance_sq = Vector.dot dist_vec dist_vec in
            if distance_sq < !closest_dist_sq then begin
              closest := Some sign;
              closest_dist_sq := distance_sq
            end
          end
      | _ -> ()
    );
  !closest

let reset () =
  Entity.Table.clear npc_table;
  Entity.Table.clear sign_table