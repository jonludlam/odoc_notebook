
let test files =
  match Lwt_main.run (Odoc_notebook_lib.Test.run files) with
  | Ok () -> `Ok ()
  | Error (`Msg m) ->
      Format.eprintf "Error: %s\n%!" m;
      `Error (false, m) 

open Cmdliner

let test_cmd =
  let files = Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILE") in
  let info = Cmd.info "test" ~doc:"Test an mld file" in
  Cmd.v info Term.(ret (const test $ files))

let _ =
  exit (Cmd.eval test_cmd)
