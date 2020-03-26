
module Lang = Odoc_model.Lang
module Paths = Odoc_model.Paths

let str = Format.asprintf

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


type output = string

module Type_expression : sig
  val type_expr : Lang.TypeExpr.t -> output
end = struct
  let rec type_expr (texpr : Lang.TypeExpr.t) =
    match texpr with
    | Var s -> "'" ^ s
    | Any  -> "<TypeExpression.Any>"
    | Alias _ -> "<TypeExpression.Alias>"
    | Arrow (lbl_opt, src, dst) ->
      let rec flatten acc (expr : Lang.TypeExpr.t) =
        match expr with
        | Arrow (lbl_opt, src, dst) -> flatten ((lbl_opt, type_expr src) :: acc) dst
        | _ -> (None, type_expr expr) :: acc
      in
      let rev_ts = flatten [(lbl_opt, type_expr src)] dst in
      begin
        match rev_ts with
        | (None, return_t) :: rev_params ->
          let params =
            List.rev_map
              (fun ((lbl_opt : Lang.TypeExpr.label option), t) ->
                match lbl_opt with
                | Some (Label lbl) -> str "~%s: %s" lbl t
                | Some (Optional lbl) -> str "~%s: %s=?" lbl t
                | None -> t)
              rev_params in
          let params = String.concat ", " params in
          str "(%s) => %s" params return_t
        | _ -> assert false
      end
    | Tuple _xs -> "<Tuple>"
    | Constr (path, params) ->
      let link = Odoc_html.Tree.Relative_link.of_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path params link
    | Polymorphic_variant _pvar -> "<Polyvariant>"
    | Object _ -> "<Object>"
    | Class _ -> "<Class>"
    | Poly _ -> "<Poly>"
    | Package _ -> "<Package>"

  and format_type_path (params : Odoc_model.Lang.TypeExpr.t list) path =
    match params with
    | [] -> Local.render_html path
    | params ->
      let params = List.map type_expr params in
      let params = String.concat ", " params in
      Local.render_html path ^ "(" ^ params ^ ")"
end


module Module : sig
  val signature : Lang.Signature.t -> (output * Tree.t list)
end = struct
  let signature_item (item : Lang.Signature.item) =
    match item with
    | Type _ -> "<Type>\n"
    | TypeSubstitution _ ->  "<TypeSubstitution>\n"
    | TypExt _ ->  "<TypExt>\n"
    | Exception _ ->  "<Exception>\n"
    | Value x ->
      str "## %s\n\n```re sig\nlet %s: %s;\n```\n\n%s\n\n"
        (Paths.Identifier.name x.id)
        (Paths.Identifier.name x.id)
        (Type_expression.type_expr x.type_)
        (Local.render_comment x.doc)
    | External x ->
      str "## %s\n\n```re sig\nexternal %s: %s;\n```\n\n%s\n\n"
        (Paths.Identifier.name x.id)
        (Paths.Identifier.name x.id)
        (Type_expression.type_expr x.type_)
        (Local.render_comment x.doc)
    | ModuleSubstitution _ ->  "<ModuleSubstitution>\n"
    | Module _ -> "<Module>\n"
    | ModuleType _ -> "<ModuleType>\n"
    | Include _ -> "<Include>\n"
    | Class _ -> "<Class>\n"
    | ClassType _ -> "<ClassType>\n"
    | Comment (`Docs comment) -> (Local.render_comment comment)
    | Comment `Stop -> "<CommentStop>\n"

  let signature (items : Lang.Signature.t) =
    let content = List.map signature_item items in
    let content = String.concat "\n" content in
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
  let content =  str "# %s\n\n%s\n\n%s" name doc content in
  Tree.make ~name content children
  