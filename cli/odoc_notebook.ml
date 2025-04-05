(* MMMmmmmm *)

(*

   odoc-notebook generate <foo.mld bar.mld baz.mld> -o html

   outputs:

   html/assets/cmis/*.cmi
   html/assets/odoc.css
   html/assets/toplevel.js
   html/assets/worker.js
   html/foo.html
   html/bar.html
   html/baz.html
*)

open Cmdliner

type breadcrumb = { name : string; href : string; kind : string }
[@@deriving yojson]

type toc = { title : string; href : string; children : toc list }
[@@deriving yojson]

type node = { url : string option; kind : string option; content : string }
and 'a tree = { node : 'a; children : 'a tree list }
and sidebar = node tree list [@@deriving yojson]

type as_json = {
  header : string;
  type_ : string; [@key "type"]
  uses_katex : bool;
  breadcrumbs : breadcrumb list;
  toc : toc list;
  preamble : string;
  source_anchor : string option;
  content : string;
}
[@@deriving yojson]

let cmi_files dir =
  Bos.OS.Dir.fold_contents ~traverse:`None ~elements:`Files
    (fun path acc ->
      if Fpath.has_ext ".cmi" path then Fpath.filename path :: acc else acc)
    [] dir

let gen_cmis cmis =
  let all_cmis =
    List.map snd cmis |> List.flatten
    |> List.map (fun s -> String.sub s 0 (String.length s - 4))
  in
  let hidden, non_hidden =
    List.partition (fun x -> Astring.String.is_infix ~affix:"__" x) all_cmis
  in
  let prefixes =
    List.filter_map
      (fun x ->
        match Astring.String.cuts ~sep:"__" x with
        | x :: _ -> Some (x ^ "__")
        | _ -> None)
      hidden
  in
  let prefixes = Util.StringSet.of_list prefixes in
  Format.asprintf
    {|{
  static_cmis=[];
  dynamic_cmis=
    [{dcs_url="cmis/";
      dcs_toplevel_modules = [%s];
      dcs_file_prefixes = [%s];}]} |}
    (String.concat ";"
       (List.map (fun m -> "\"" ^ String.capitalize_ascii m ^ "\"") non_hidden))
    (String.concat ";"
       (List.map (fun m -> "\"" ^ m ^ "\"") (Util.StringSet.to_list prefixes)))

let generate_page parent_id mld =
  Odoc.compile ~output_dir:(Fpath.v "_odoc") ~includes:Fpath.Set.empty
    ~input_file:(Fpath.v mld)
    ~parent_id
    ~warnings_tag:(Some "odoc_notebook") ~ignore_output:true

let generate output_dir_str odoc_dir files =
  let verbose = true in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let () = Worker_pool.start_workers env sw 16 in
  let output_dir = Fpath.v output_dir_str in
  let metas =
    List.filter_map (fun f -> Odoc_notebook_lib.Mld.meta_of_mld f) files
  in
  let mld_libs =
    List.fold_left
      (fun acc meta ->
        Util.StringSet.(union (of_list meta.Odoc_notebook_lib.Mld.libs) acc))
      (Util.StringSet.singleton "stdlib")
      metas
  in
  let lib_map =
      let ic = open_in Fpath.(odoc_dir / "lib_map.json" |> Fpath.to_string) in
      let yojson = Yojson.Safe.from_channel ic in
      let lib_map = Yojson.Safe.Util.to_assoc yojson in
      let get_str x =
        Fpath.(odoc_dir // (Yojson.Safe.Util.to_string x |> Fpath.v))
      in
      List.fold_left
        (fun acc (k, v) -> Util.StringMap.add k (get_str v) acc)
        Util.StringMap.empty lib_map
  in
  let cmi_dirs =
    match Ocamlfind.deps (Util.StringSet.to_list mld_libs) with
    | Ok libs ->
        let dirs =
          List.filter_map
            (fun lib ->
              match Ocamlfind.get_dir lib with Ok x -> Some x | _ -> None)
            libs
        in
        dirs
    | Error (`Msg m) ->
        Format.eprintf "Failed to find libs: %s\n%!" m;
        []
  in
  let cmis =
    List.fold_left
      (fun acc dir ->
        match cmi_files dir with
        | Ok files -> (dir, files) :: acc
        | Error _ -> acc)
      [] cmi_dirs
  in
  let ( let* ) = Result.bind in
  let _ =
    let* _ = Bos.OS.Dir.create output_dir in
    let* _ = Bos.OS.Dir.create Fpath.(output_dir / "assets") in
    let* _ = Bos.OS.Dir.create Fpath.(output_dir / "assets" / "cmis") in
    List.iter
      (fun (dir, files) ->
        List.iter
          (fun f ->
            let dest = Fpath.(output_dir / "assets" / "cmis" / f) in
            match Bos.OS.File.exists dest with
            | Ok true -> ()
            | Ok false -> Util.cp Fpath.(dir / f) dest
            | Error _ -> failwith "file exists failed")
          files)
      cmis;
    (* Format.eprintf "@[<hov 2>dir: %a [%a]@]\n%!" Fpath.pp dir (Fmt.list ~sep:Fmt.sp Fmt.string) files) cmis; *)
    Ok ()
  in
  let worker_oc =
    open_out Fpath.(output_dir / "assets" / "worker.js" |> to_string)
  in
  Printf.fprintf worker_oc "%s"
    (Odoc_notebook_crunch.read "worker.js" |> Option.get);
  close_out worker_oc;
  let notebook_css =
    open_out Fpath.(output_dir / "assets" / "odoc-notebook.css" |> to_string)
  in
  Printf.fprintf notebook_css "%s" Notebook_css.notebook_css;
  close_out notebook_css;
  ignore (Odoc.support_files Fpath.(output_dir / "assets"));
  let init_cmis = gen_cmis cmis in
  List.iter (fun mld ->
    let (dir, file) = Fpath.split_base (Fpath.v mld) in
    let parent_id =
      Odoc.Id.of_fpath Fpath.(normalize (append (v "notebooks") dir))
    in
    generate_page parent_id mld) files;
  List.iter
    (fun mld ->
      let meta = Odoc_notebook_lib.Mld.meta_of_mld mld in
      let dir, file = Fpath.split_base (Fpath.v mld) in
      let libs_list =
        match meta with None -> [ "stdlib" ] | Some l -> "stdlib" :: l.libs
      in
      let libs =
        List.map
          (fun lib_name -> (lib_name, Util.StringMap.find lib_name lib_map))
          libs_list
      in
      let x = Fpath.set_ext ".odoc" file in
      let odoc_file = "page-" ^ (Fpath.to_string x) in
      let odoc_path = Fpath.(append (odoc_dir / "notebooks") dir) in
      Odoc.link
        ~input_file:Fpath.(odoc_path / odoc_file)
        ~libs ~docs:[] ~includes:[] ~ignore_output:true ~custom_layout:true
        ~warnings_tags:[ "odoc_notebook" ] ())
    files;

  Odoc.compile_index ~json:false
    ~roots:[ odoc_dir ]
    ~simplified:false ~wrap:false
    ~output_file:(Fpath.v "index.odoc-index")
    ();

  Odoc.sidebar_generate ~output_file:(Fpath.v "sidebar.json") ~json:true
    (Fpath.v "index.odoc-index")
    ();

  let sidebar =
    let ic = open_in "sidebar.json" in
    let yojson = Yojson.Safe.from_channel ic in
    let result = sidebar_of_yojson yojson in
    close_in ic;
    result
  in
  let () =
    match sidebar with
    | Ok _ -> ()
    | Error e ->
        Format.eprintf "Failed to parse sidebar: %s\n%!" e;
        ()
  in
  let sidebar = Result.value ~default:[] sidebar in

  let globaltoc =
    match sidebar with
    | [] ->
        Format.eprintf "No global sidebar found\n%!";
        ""
    | _ ->
        let rec aux tree =
          let children = List.map aux tree.children in
          let children =
            if List.length children > 0 then
              Printf.sprintf "<ul>%s</ul>" (String.concat "" children)
            else ""
          in
          let content =
            match tree.node.url with
            | Some url ->
                Printf.sprintf "<a href=\"/%s\">%s</a>" url tree.node.content
            | None -> tree.node.content
          in
          Printf.sprintf "<li>%s%s</li>" content children
        in
        Printf.sprintf "<ul>%s</ul>" (String.concat "" (List.map aux sidebar))
  in

  List.iter
    (fun mld ->
      let dir, file = Fpath.split_base (Fpath.v mld) in
      let meta = Odoc_notebook_lib.Mld.meta_of_mld mld in
      let libs_list =
        match meta with None -> [ "stdlib" ] | Some l -> "stdlib" :: l.libs
      in
      let libs_set = Util.StringSet.of_list libs_list in
      let odoc_file = Fpath.(set_ext "odoc" file |> to_string |> (fun x -> "page-" ^ x) |> v) in
      let odocl_file = Fpath.(set_ext "odocl" odoc_file) in
      let odoc_dir = Fpath.(append (odoc_dir / "notebooks") dir) in
      let _ =
        Odoc.html_generate ~output_dir:output_dir_str
          ~input_file:(Fpath.append odoc_dir odocl_file)
          ~as_json:true ()
      in
      let json_file =
        Fpath.(append (v "html" / "notebooks") dir / (Fpath.set_ext ".html.json" file |> to_string))
      in
      let x = Fpath.rem_ext file |> Fpath.to_string in
      Mk_frontend.mk init_cmis libs_set
        Fpath.(output_dir / "assets")
        ("frontend_" ^ x);

      let ic = open_in (Fpath.to_string json_file) in
      let yojson = Yojson.Safe.from_channel ic in
      let _ =
        let json = as_json_of_yojson yojson in
        let json = match json with Ok x -> x | Error e -> failwith e in
        Format.eprintf "We've got the result\n%!";
        let breadcrumbs = {|<a href="../index.html">Up</a> – notebooks|} in
        let localtoc =
          let rec aux (toc : toc) =
            let children = List.map aux toc.children in
            let children =
              if List.length children > 0 then
                Printf.sprintf "<ul>%s</ul>" (String.concat "" children)
              else ""
            in
            Printf.sprintf "<li><a href=\"%s\">%s</a>%s</li>" toc.href toc.title
              children
          in
          let toc = List.map aux json.toc in
          Printf.sprintf "<ul>%s</ul>" (String.concat "" toc)
        in
        let post_content = "" in
        Format.eprintf "Creating html page\n%!";
        let* html =
          Html_page.(
            create
              {
                title = mld;
                header = json.header;
                breadcrumbs;
                localtoc;
                globaltoc;
                odoc_assets_path = "/assets";
                preamble = json.preamble;
                frontend = "frontend_" ^ x ^ ".js";
                content = json.content;
                post_content;
              })
        in
        Format.eprintf "Created\n%!";
        let oc =
          open_out Fpath.((append (output_dir / "notebooks") dir) / (x ^ ".html") |> to_string)
        in
        Printf.fprintf oc "%s" html;
        close_out oc;
        Ok ()
      in
      ())
    files;
  `Ok ()

let test files =
  match Odoc_notebook_lib.Test.run files with
  | Ok () -> `Ok ()
  | Error (`Msg m) ->
      Format.eprintf "Error: %s\n%!" m;
      `Error (false, m)

let fpath_arg =
  let print ppf v = Fpath.pp ppf v in
  Arg.conv (Fpath.of_string, print)

let odoc_dir =
  let doc = "Odoc directory from odoc_driver" in
  Arg.(required & opt (some fpath_arg) None & info [ "odoc-dir" ] ~doc)

let generate_cmd =
  let output_dir =
    let doc = "Output directory in which to put all outputs" in
    Arg.(value & opt string "html" & info [ "output" ] ~doc)
  in
  let files = Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILE") in
  let doc = "Generate static files to serve a notebook" in
  let info = Cmd.info "generate" ~doc in
  Cmd.v info Term.(ret (const generate $ output_dir $ odoc_dir $ files))

let test_cmd =
  let files = Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILE") in
  let info = Cmd.info "test" ~doc:"Test an mld file" in
  Cmd.v info Term.(ret (const test $ files))

let main_cmd =
  let doc = "An odoc notebook tool" in
  let info = Cmd.info "odoc-notebook" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ generate_cmd; test_cmd ]

let () = exit (Cmd.eval main_cmd)
