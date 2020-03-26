
(** {1 JSON values representation} *)

type t
(** The type for JSON values. *)

(** {2 JSON value constructors} *)

val int : int -> t

val float : float -> t

val bool : bool -> t

val string : string -> t

val array : t list -> t

val obj : (string * t) list -> t

val null : t


(** {2 Pretty printing} *)

val pp : Format.formatter -> t -> unit
