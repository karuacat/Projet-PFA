type t = { x : float; y : float }

val add : t -> t -> t
val sub : t -> t -> t

val mult : float -> t -> t
val dot : t -> t -> float
val norm : t -> float
val normalize : t -> t
val pp : Format.formatter -> t -> unit

val zero : t
val is_zero : t -> bool