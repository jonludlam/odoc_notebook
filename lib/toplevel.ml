open Js_top_worker_rpc
module M = Idl.IdM (* Server is synchronous *)
module IdlM = Idl.Make (M)

module UnixWorker = struct
  let capture f () =
    let stdout_backup = Unix.dup ~cloexec:true Unix.stdout in
    let stderr_backup = Unix.dup ~cloexec:true Unix.stderr in
    let filename_out = Filename.temp_file "ocaml-mdx-" ".stdout" in
    let filename_err = Filename.temp_file "ocaml-mdx-" ".stderr" in
    let fd_out =
      Unix.openfile filename_out
        Unix.[ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ]
        0o600
    in
    let fd_err =
      Unix.openfile filename_err
        Unix.[ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ]
        0o600
    in
    Unix.dup2 ~cloexec:false fd_out Unix.stdout;
    Unix.dup2 ~cloexec:false fd_err Unix.stderr;
    let ic_out = open_in filename_out in
    let ic_err = open_in filename_err in
    let capture oc ic fd buf =
      flush oc;
      let len = Unix.lseek fd 0 Unix.SEEK_CUR in
      Buffer.add_channel buf ic len
    in
    Fun.protect
      (fun () ->
        let x = f () in
        let buf_out = Buffer.create 1024 in
        let buf_err = Buffer.create 1024 in
        capture stdout ic_out fd_out buf_out;
        capture stderr ic_err fd_err buf_err;
        ( {
            Js_top_worker.Impl.stdout = Buffer.contents buf_out;
            stderr = Buffer.contents buf_err;
          },
          x ))
      ~finally:(fun () ->
        close_in_noerr ic_out;
        close_in_noerr ic_out;
        Unix.close fd_out;
        Unix.close fd_err;
        Unix.dup2 ~cloexec:false stdout_backup Unix.stdout;
        Unix.dup2 ~cloexec:false stderr_backup Unix.stderr;
        Unix.close stdout_backup;
        Unix.close stderr_backup;
        Sys.remove filename_out;
        Sys.remove filename_err)

  let sync_get _ = None
  let create_file ~name:_ ~content:_ = failwith "Not implemented: create_file"

  let import_scripts urls =
    if urls = [] then ()
    else (
      List.iter (fun url -> Format.eprintf "Not importing: %s\n%!" url) urls;
      failwith "Not implemented: import_scripts")

  let init_function _ = failwith "Not implemented: init_function"
end

module U = Js_top_worker.Impl.Make (UnixWorker)

(* open Bos *)

type directive = Directory of string | Load of string

let init_findlib () = Findlib.init ()

let deps pkgs =
  try
    let packages =
      Fl_package_base.requires_deeply ~preds:[ "ppx_driver" ] pkgs
    in
    Ok packages
  with e -> Error (`Msg (Printexc.to_string e))

let get_dir lib =
  try
    Fl_package_base.query lib |> fun x ->
    Format.eprintf "Package %s is in directory %s@." lib x.package_dir;
    Ok Fpath.(v x.package_dir |> to_dir_path)
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    Error (`Msg "Error getting directory")

type t = unit

let init ~verbose:_ ~silent:_ ~verbose_findlib:_ ~directives:_ ~packages
    ~predicates:_ () =
  let open IdlM.ErrM in
  let opam_path = Sys.getenv "OPAM_SWITCH_PREFIX" in
  init_findlib ();
  let deps =
    match deps packages with
    | Ok x -> x
    | Error (`Msg m) ->
        Format.eprintf "Error getting dependencies: %s\n%!" m;
        failwith "error"
  in
  let dirs = List.map get_dir deps in
  let urls =
    List.filter_map
      (function
        | Ok x -> Some ("file://" ^ Fpath.to_string x) | Error _ -> None)
      dirs
  in
  let dynamic_cmis =
    List.map
      (fun url ->
        Toplevel_api_gen.
          { dcs_url = url; dcs_file_prefixes = []; dcs_toplevel_modules = [] })
      urls
  in
  let result =
    U.init
      {
        Toplevel_api_gen.path = Printf.sprintf "%s/lib/ocaml" opam_path;
        cmas = [];
        cmis = { static_cmis = []; dynamic_cmis };
      }
    >>= fun () ->
    U.setup () >>= fun _ -> return ()
  in
  match result |> IdlM.T.get |> M.run with
  | Ok x -> x
  | Error (InternalError e) ->
      Format.eprintf "Bad stuff here! '%s'\n%!" e;
      failwith "error"

let eval () list =
  match U.execute (String.concat "\n" list) |> IdlM.T.get |> M.run with
  | Ok r ->
      Ok
        ( r.mime_vals,
          Option.to_list r.stdout @ Option.to_list r.stderr
          @ Option.to_list r.caml_ppf )
  | Error _e -> Error [ "error" ]

let eval_toplevel () str = U.exec_toplevel str |> IdlM.T.get |> M.run

let compile_js () id str =
  match U.compile_js id str |> IdlM.T.get |> M.run with
  | Ok r -> Ok r
  | Error (InternalError m) ->
      Format.eprintf "Bad stuff here this time! '%s'\n%!" m;
      Error [ "Error in compile_js" ]

let in_env _ f = f ()
