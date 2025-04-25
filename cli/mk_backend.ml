(* To make a toplevel backend.js *)

let mk libs dir =
  let txt = {|let _ = Js_top_worker_web.Worker.run ()|} in
  let file = Fpath.(dir / "worker.ml") in
  Util.write_file file [ txt ];
  let cmd =
    Bos.Cmd.(
      v "ocamlfind" % "ocamlc" % "-package" % "js_of_ocaml-ppx.as-lib"
      % "-package" % "js_top_worker-web")
  in
  let cmd = Bos.Cmd.(cmd % "-linkpkg" % "-linkall" % Fpath.to_string file) in
  let cmd = Bos.Cmd.(cmd % "-o" % Fpath.(dir / "worker.bc" |> to_string)) in
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
      v "js_of_ocaml" % "--toplevel" % "--no-cmis" % "--linkall" % "--pretty"
      % "--effects=cps")
  in
  let cmd =
    List.fold_right
      (fun a cmd -> Bos.Cmd.(cmd % a))
      (js_files
      @ [
          "+dynlink.js";
          "+toplevel.js";
          "+bigstringaf/runtime.js";
          "+merlin-js.worker/stubs.js";
        ])
      cmd
  in
  let cmd =
    Bos.Cmd.(
      cmd
      % Fpath.(dir / "worker.bc" |> to_string)
      % "-o"
      % Fpath.(dir / "worker.js" |> to_string))
  in
  let _ = Util.lines_of_process cmd in
  ()
