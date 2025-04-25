(* To make a toplevel frontend.js *)

let mk libs dir name =
  let txt =
    Format.sprintf {|let _ = Frontend.main []|} 
  in
  let file = Fpath.(dir / (name ^ "_frontend.ml")) in
  Util.write_file file [ txt ];
  let cmd =
    Bos.Cmd.(v "ocamlfind" % "ocamlc" % "-package" % "odoc_notebook.frontend")
  in
  let add_pkgs cmd =
    Util.StringSet.fold
      (fun lib cmd -> Bos.Cmd.(cmd % "-package" % lib))
      libs cmd
  in
  let cmd = add_pkgs cmd in
  let cmd = Bos.Cmd.(cmd % "-linkpkg" % "-linkall" % Fpath.to_string file) in
  let cmd = Bos.Cmd.(cmd % "-o" % Fpath.(dir / (name ^ ".bc") |> to_string)) in
  let _ = Util.lines_of_process cmd in
  let cmd =
    Bos.Cmd.(
      v "ocamlfind" % "query" % "-format" % "%+(jsoo_runtime)" % "-r"
      % "odoc_notebook.frontend")
  in
  let cmd = Util.StringSet.fold (fun lib cmd -> Bos.Cmd.(cmd % lib)) libs cmd in
  let js_files =
    Util.lines_of_process cmd
    |> List.filter (fun x -> String.length x > 0)
    |> List.map (fun x -> Astring.String.cuts ~sep:" " x)
    |> List.flatten
  in
  let cmd =
    Bos.Cmd.(
      v "js_of_ocaml" % "--linkall" % "--no-cmis" % "--pretty" % "--effects=cps")
  in
  let cmd =
    List.fold_right
      (fun a cmd -> Bos.Cmd.(cmd % a))
      (js_files @ [ "+dynlink.js" ])
      cmd
  in
  let cmd =
    Bos.Cmd.(
      cmd
      % Fpath.(dir / (name ^ ".bc") |> to_string)
      % "-o"
      % Fpath.(dir / (name ^ ".js") |> to_string))
  in
  let _ = Util.lines_of_process cmd in
  ()
