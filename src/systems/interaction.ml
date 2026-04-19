open Ecs
open Component_defs

let npc_table : (npc_entity, unit) Entity.Table.t = Entity.Table.create 16
let sign_table : (sign_entity, unit) Entity.Table.t = Entity.Table.create 16
let npc_entity_table : (Entity.t, unit) Entity.Table.t = Entity.Table.create 16

let register_npc (npc : npc_entity) =
  if not (Entity.Table.mem npc_table npc) then begin
    Entity.Table.add npc_table npc ();
    let as_entity = (npc :> Entity.t) in
    Entity.Table.add npc_entity_table as_entity ();
    Entity.register as_entity (fun () ->
      Entity.Table.remove npc_table npc;
      Entity.Table.remove npc_entity_table as_entity)
  end

let is_registered_npc_entity (entity : Entity.t) =
  Entity.Table.mem npc_entity_table entity

let register_sign (sign : sign_entity) =
  if not (Entity.Table.mem sign_table sign) then begin
    Entity.Table.add sign_table sign ();
    Entity.register (sign :> Entity.t) (fun () -> Entity.Table.remove sign_table sign)
  end

let boxes_adjacent (v1 : Vector.t) (r1 : Rect.t) (v2 : Vector.t) (r2 : Rect.t) =
  let right1 = v1.x +. float r1.width in
  let bottom1 = v1.y +. float r1.height in
  let right2 = v2.x +. float r2.width in
  let bottom2 = v2.y +. float r2.height in
  v1.x <= right2 && right1 >= v2.x && v1.y <= bottom2 && bottom1 >= v2.y

let center_distance_sq (v1 : Vector.t) (r1 : Rect.t) (v2 : Vector.t) (r2 : Rect.t) =
  let c1x = v1.x +. (float r1.width /. 2.0) in
  let c1y = v1.y +. (float r1.height /. 2.0) in
  let c2x = v2.x +. (float r2.width /. 2.0) in
  let c2y = v2.y +. (float r2.height /. 2.0) in
  let dx = c1x -. c2x in
  let dy = c1y -. c2y in
  (dx *. dx) +. (dy *. dy)

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
  let interact_radius_sq = 44.0 *. 44.0 in
  Entity.Table.to_seq_keys sign_table
  |> Seq.iter (fun sign ->
      match sign#tag#get with
      | InScene scene when scene = current_scene ->
          let sign_pos = sign#position#get in
          let sign_box = sign#box#get in
          let distance_sq = center_distance_sq player_pos player_box sign_pos sign_box in
          if boxes_adjacent player_pos player_box sign_pos sign_box || distance_sq <= interact_radius_sq then begin
            if distance_sq < !closest_dist_sq then begin
              closest := Some sign;
              closest_dist_sq := distance_sq
            end
          end
      | _ -> ()
    );
  !closest
