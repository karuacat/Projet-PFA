open Ecs
open Component_defs

type t = collidable

let init _ = ()

let rec iter_pairs f s =
  match s () with
    Seq.Nil -> ()
  | Seq.Cons(e, s') ->
    Seq.iter (fun e' -> f e e') s';
    iter_pairs f s'


let update _ el =
  el
  |> iter_pairs (fun (e1:t) (e2:t) ->

      let pos1 : Vector.t = e1#position#get in
      let pos2 : Vector.t = e2#position#get in
      let box1 : Rect.t = e1#box#get in
      let box2 : Rect.t = e2#box#get in
      match e1#tag#get, e2#tag#get with
        | Player, _ ->
            (match Rect.rebound pos1 box1 pos2 box2 with
              | None -> ()
              | Some v -> e1#resolve#get v (e2 :> tagged))
        | _, Player ->
            (match Rect.rebound pos2 box2 pos1 box1 with
              | None -> ()
              | Some v -> e2#resolve#get v (e1 :> tagged))
        | _ -> ()
    )
