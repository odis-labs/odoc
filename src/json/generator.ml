
module Lang = Odoc_model.Lang
module Paths = Odoc_model.Paths

module Local = struct
  let render_html html =
    let ppf = Format.str_formatter in
    let pp_sep ppf () = Format.fprintf ppf "\n" in
    Format.fprintf ppf "%a"
      (Format.pp_print_list ~pp_sep (Tyxml.Html.pp_elt ()))
      html;
    Format.flush_str_formatter ()

  let render_comment comment =
    let html = Odoc_html.Comment.to_html comment in
    render_html html
end

module Type_expression : sig
  val type_expr : Lang.TypeExpr.t -> Json.t
end = struct
  let rec type_expr (texpr : Lang.TypeExpr.t) =
    match texpr with
    | Var s ->
      Json.array [Json.string "var"; Json.string s]
    | Any  -> Json.string "TODO: TypeExpression.Any"
    | Alias _ ->
      Json.string "TODO: TypeExpression.Alias"
    | Arrow (None, _src, _dst)
    | Arrow (Some _, _src, _dst) -> Json.string "TODO: TypeExpression.Arrow"
    | Tuple _xs -> Json.string "TODO: Tuple"
    | Constr (path, params) ->
      let link = Odoc_html.Tree.Relative_link.of_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path params link
    | Polymorphic_variant _pvar -> Json.string "TODO: Polyvariant"
    | Object _ -> Json.string "TODO: Object"
    | Class _ -> Json.string "TODO: Class"
    | Poly _ -> Json.string "TODO: Poly"
    | Package _ -> Json.string "TODO: Package"

  and format_type_path (params : Odoc_model.Lang.TypeExpr.t list) path =
    match params with
    | [] -> Json.string (Local.render_html path)
    | [param] ->
      let param = type_expr param in
      Json.array [Json.string (Local.render_html path); param]
    | params  ->
      let params = List.map type_expr params in
      Json.array params
end

let _foo x = Type_expression.type_expr x

module Module : sig
  val signature : Lang.Signature.t -> (Json.t * Tree.t list)
end = struct
  let signature_item (item : Lang.Signature.item) =
    match item with
    | Type _ -> Json.string "TODO: Type"
    | TypeSubstitution _ ->  Json.string "TODO: TypeSubstitution"
    | TypExt _ ->  Json.string "TODO: TypExt"
    | Exception _ ->  Json.string "TODO: Exception"
    | Value x ->
      Json.obj [
        ("id", Json.string (Paths.Identifier.name x.id));
        ("kind", Json.string "value");
        ("doc", Json.string (Local.render_comment x.doc));
        ("type", Json.string (Md.Type_expression.type_expr x.type_));
      ]
    | External x ->
      Json.obj [
        ("id", Json.string (Paths.Identifier.name x.id));
        ("kind", Json.string "external");
        ("doc", Json.string (Local.render_comment x.doc));
        ("type", Json.string (Md.Type_expression.type_expr x.type_));
      ]
    | ModuleSubstitution _ ->  Json.string "TODO: ModuleSubstitution"
    | Module (_rec, x) ->
      Json.obj [
        ("id", Json.string (Paths.Identifier.name x.id));
        ("kind", Json.string "module");
        ("doc", Json.string (Local.render_comment x.doc));
      ]
    | ModuleType _ -> Json.string "TODO: ModuleType"
    | Include _ -> Json.string "TODO: Include"
    | Class _ -> Json.string "TODO: Class"
    | ClassType _ -> Json.string "TODO: ClassType"
    | Comment (`Docs comment) -> Json.string (Local.render_comment comment)
    | Comment `Stop -> Json.string "TODO: Comment Stop"

  let signature (items : Lang.Signature.t) =
    let content = Json.array (List.map signature_item items) in
    let children = [] in
    (content, children)
end


let compilation_unit (unit : Lang.Compilation_unit.t) : Tree.t =
  let package =
    match unit.id with
    | `Root (a, _) -> a.package
    | _ -> assert false
  in
  let name = Paths.Identifier.name unit.id in
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
