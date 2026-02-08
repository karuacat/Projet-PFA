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

let find_npc_at pos range =
  let current_scene = Scene.current () in
  let range_f = float_of_int range in
  let closest = ref None in
  let closest_dist = ref max_float in
  Entity.Table.to_seq_keys npc_table
  |> Seq.iter (fun npc ->
      match npc#tag#get with
      | InScene scene when scene = current_scene ->
          let npc_pos = npc#position#get in
          let distance = Vector.norm (Vector.sub pos npc_pos) in
          if distance <= range_f && distance < !closest_dist then begin
            closest := Some npc;
            closest_dist := distance
          end
      | _ -> ()
    );
  !closest

let find_sign_at pos range =
  let current_scene = Scene.current () in
  let range_f = float_of_int range in
  let closest = ref None in
  let closest_dist = ref max_float in
  Entity.Table.to_seq_keys sign_table
  |> Seq.iter (fun sign ->
      match sign#tag#get with
      | InScene scene when scene = current_scene ->
          let sign_pos = sign#position#get in
          let distance = Vector.norm (Vector.sub pos sign_pos) in
          if distance <= range_f && distance < !closest_dist then begin
            closest := Some sign;
            closest_dist := distance
          end
      | _ -> ()
    );
  !closest

let reset () =
  Entity.Table.clear npc_table;
  Entity.Table.clear sign_table