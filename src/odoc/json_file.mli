
open Or_error

(** Produces .html files from a .odoc file. *)

val from_odoc : env:Env.builder -> output:Fs.Directory.t -> Fs.File.t
    -> (unit, [> msg]) result