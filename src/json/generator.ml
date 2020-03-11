
module Lang = Odoc_model.Lang

module Local = struct
  let render_comment comment =
    let html = Odoc_html.Comment.to_html comment in
    let ppf = Format.str_formatter in
    let pp_sep ppf () = Format.fprintf ppf "\n" in
    Format.fprintf ppf "%a"
      (Format.pp_print_list ~pp_sep (Tyxml.Html.pp_elt ()))
      html;
    Format.flush_str_formatter ()
end


module Module : sig
  val signature : Lang.Signature.t -> (Json.t * Tree.t list)
end = struct
  let signature_item (item : Lang.Signature.item) =
    match item with
    | Type _ -> Json.string "TODO: Type"
    | TypeSubstitution _ ->  Json.string "TODO: TypeSubstitution"
    | TypExt _ ->  Json.string "TODO: TypExt"
    | Exception _ ->  Json.string "TODO: Exception"
    | Value _ ->  Json.string "TODO: Value"
    | External _ ->  Json.string "TODO: External"
    | ModuleSubstitution _ ->  Json.string "TODO: ModuleSubstitution"
    | Module _ -> Json.string "TODO: Module"
    | ModuleType _ -> Json.string "TODO: ModuleType"
    | Include _ -> Json.string "TODO: Include"
    | Class _ -> Json.string "TODO: Class"
    | ClassType _ -> Json.string "TODO: ClassType"
    | Comment (`Docs comment) -> Json.string (Local.render_comment comment)
    | Comment `Stop -> Json.string "TODO: Comment Stop"

  let signature (items : Lang.Signature.t) =
    let content = Json.array signature_item items in
    let children = [] in
    (content, children)
end


let compilation_unit (unit : Lang.Compilation_unit.t) : Tree.t =
  let package =
    match unit.id with
    | `Root (a, _) -> a.package
    | _ -> assert false
  in
  let name = Odoc_model.Paths.Identifier.name unit.id in
  (* The HTML tree is currently used for cross-references. *)
  Odoc_html.Tree.enter package;
  Odoc_html.Tree.enter name;
  let doc = Local.render_comment unit.doc in
  let content, children =
    match unit.content with
    | Module x -> Module.signature x
    | Pack _packed -> failwith "TODO: Pack"
  in
  let content = Json.obj [
    ("name", Json.string name);
    ("doc", Json.string doc);
    ("content", content);
  ] in
  Tree.make ~name content children
    (* let header_docs, html, subtree =
      match unit.content with
      | Module sign ->
        let html, toc, subpages = signature ?theme_uri sign in
        let header_docs =
          match toc with
          | [] -> header_docs
          | _ -> header_docs @ (Top_level_markup.render_toc toc)
        in
        header_docs, html, subpages
      | Pack packed ->
        header_docs, pack packed, []
    in
    Tree.make ~header_docs ?theme_uri html subtree *)
