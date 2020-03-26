
type t

val traverse : f:(parents:string list -> string -> Json.t -> unit) -> t
  -> unit

val make : name:string -> Json.t -> t list -> t