open StdLabels
open Or_error

let from_odoc ~env ~output:root_dir input =
Root.read input >>= fun root ->
  match root.file with
  | Page _ ->
    Error (`Msg "JSON output for documentation pages is not supported.")
  | Compilation_unit {hidden = _; _} ->
    Compilation_unit.load input >>= fun unit ->
    let unit = Odoc_xref.Lookup.lookup unit in
    begin
      (* See comment in compile for explanation regarding the env duplication. *)
      let resolve_env = Env.build env (`Unit unit) in
      Odoc_xref.resolve (Env.resolver resolve_env) unit >>= fun resolved ->
      let expand_env = Env.build env (`Unit resolved) in
      Odoc_xref.expand (Env.expander expand_env) resolved >>= fun expanded ->
      Odoc_xref.Lookup.lookup expanded
      |> Odoc_xref.resolve (Env.resolver expand_env) (* Yes, again. *)
    end >>= fun odoctree ->
    let pkg_dir =
      Fs.Directory.reach_from ~dir:root_dir root.package
    in
    let files = Odoc_json.Generator.compilation_unit odoctree in
    Odoc_json.Tree.traverse files ~f:(fun ~parents name content ->
      let directory =
        let dir =
          List.fold_right ~f:(fun name dir -> Fs.Directory.reach_from ~dir name)
            parents ~init:pkg_dir
        in
        Fs.Directory.reach_from ~dir name
      in
      let oc =
        Fs.Directory.mkdir_p directory;
        let file = Fs.File.create ~directory ~name:"index.json" in
        open_out (Fs.File.to_string file)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@.@?" Odoc_json.Json.pp content;
      close_out oc
    );
    Ok ()
