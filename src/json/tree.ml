type t = {
  name : string;
  content : Json.t;
  children : t list
}

let make ~name content children =
  {
    name;
    content;
    children
  }


let traverse ~f t =
  let rec loop parents node =
    f ~parents node.name node.content;
    List.iter (loop (node.name :: parents)) node.children
  in
  loop [] t