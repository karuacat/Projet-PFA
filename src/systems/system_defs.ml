open Ecs

module Collision_system = System.Make(Collision)
module Draw_system = System.Make(Draw)
module Move_system = System.Make(Move)
module Door_transition_system = System.Make(Door_transition)