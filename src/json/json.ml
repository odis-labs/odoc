type t =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Array of t list
  | Obj of (string * t) list
  | Null


let int x = Int x

let float x = Float x

let bool x = Bool x

let string x = String x

let array xs =
  Array xs

let obj xs = Obj xs

let null = Null


(* Pretty printing *)

let rec pp ppf x =
  let pf = Format.fprintf in
  match x with
  | Null -> pf ppf "null"
  | Bool true -> pf ppf "true"
  | Bool false -> pf ppf "false"
  | Int x -> pf ppf "%d" x
  | Float x -> pf ppf "%f" x
  | String x -> pf ppf "%S" x
  | Array xs ->
    let pp_sep ppf () = pf ppf ",@;" in
    pf ppf "@[<1>[%a@]]" (Format.pp_print_list ~pp_sep pp) xs
  | Obj xs ->
    let pp_sep ppf () = pf ppf ",@;" in
    let pp_kv ppf (k, v) = pf ppf "%S: %a" k pp v in
    pf ppf "@[<1>{@,%a@]}" (Format.pp_print_list ~pp_sep pp_kv) xs
