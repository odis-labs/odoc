
type t

val traverse : f:(parents:string list -> string -> string -> unit) -> t
  -> unit

val make : name:string -> string -> t list -> t