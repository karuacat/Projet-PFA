open Ecs
open Component_defs

type t = movable

let init _ = ()

let update _ el =
  Seq.iter ( fun (e:t) ->
    e#position#set Vector.(add e#velocity#get e#position#get)) el