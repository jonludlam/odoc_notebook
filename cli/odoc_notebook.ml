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

type breadcrumb = { name : string; href : string option; kind : string }
[@@deriving yojson]

type toc = { title : string; href : string; children : toc list }
[@@deriving yojson]

type node = { url : string option; kind : string option; content : string }
and 'a tree = { node : 'a; children : 'a tree list }
and sidebar = node tree list [@@deriving yojson]

type frontmatter = {
  other_config : (string * string) list;
} and as_json = {
  header : string;
  type_ : string; [@key "type"]
  uses_katex : bool;
  breadcrumbs : breadcrumb list;
  toc : toc list;
  preamble : string;
  source_anchor : string option;
  content : string;
  frontmatter : frontmatter; [@default { other_config = [] }]
}
[@@deriving yojson]


let generate_page parent_id odoc_dir mld =
  Odoc.compile ~output_dir:odoc_dir ~includes:Fpath.Set.empty
    ~input_file:(Fpath.v mld) ~parent_id ~warnings_tag:(Some "odoc_notebook")
    ~ignore_output:true

let generate_page_md parent_id odoc_dir mld =
  Odoc.compile_md ~output_dir:odoc_dir ~input_file:(Fpath.v mld) ~parent_id

let generate output_dir_str odoc_dir files =
  let verbose = true in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let () = Worker_pool.start_workers env sw 16 in
  let output_dir = Fpath.v output_dir_str in
  let lib_map =
    try
      let ic = open_in Fpath.(odoc_dir / "lib_map.json" |> Fpath.to_string) in
      let yojson = Yojson.Safe.from_channel ic in
      let lib_map = Yojson.Safe.Util.to_assoc yojson in
      let get_str x =
        Fpath.(odoc_dir // (Yojson.Safe.Util.to_string x |> Fpath.v))
      in
      List.fold_left
        (fun acc (k, v) -> Util.StringMap.add k (get_str v) acc)
        Util.StringMap.empty lib_map
    with _ ->
      Format.eprintf "Failed to read lib_map.json\n%!";
      Util.StringMap.singleton "stdlib" odoc_dir
  in
  let ( let* ) = Result.bind in
  let _ = Bos.OS.Dir.create ~path:true Fpath.(output_dir / "assets") |> Result.get_ok in
  let notebook_css =
    open_out Fpath.(output_dir / "assets" / "odoc-notebook.css" |> to_string)
  in
  Printf.fprintf notebook_css "%s" Notebook_css.notebook_css;
  close_out notebook_css;
  ignore (Odoc.support_files Fpath.(output_dir / "assets"));
  List.iter
    (fun mld ->
      let dir, file = Fpath.split_base (Fpath.v mld) in
      let parent_id = Odoc.Id.of_fpath Fpath.(normalize dir) in
      if Fpath.has_ext "mld" file then generate_page parent_id odoc_dir mld
      else if Fpath.has_ext "md" file then
        generate_page_md parent_id odoc_dir mld
      else ())
    files;
  List.iter
    (fun mld ->
      let dir, file = Fpath.split_base (Fpath.v mld) in
      let x = Fpath.set_ext ".odoc" file in
      let odoc_file = "page-" ^ Fpath.to_string x in
      let odoc_path = Fpath.(odoc_dir // dir) in
      let odoc_unit = Odoc_odoc.Odoc_file.load Fpath.(odoc_path / odoc_file) in
      let libs_list = match odoc_unit with
        | Ok {content=Page_content p; _} ->
          let frontmatter = p.frontmatter in
          Logs.debug (fun m -> m "Frontmatter other-config for page: %s = %a" (Fpath.to_string (Fpath.v mld)) (Fmt.Dump.list (Fmt.Dump.pair Fmt.Dump.string Fmt.Dump.string)) frontmatter.other_config);
          (* Extract libraries from the frontmatter *)
          let libs = try List.assoc "libs" frontmatter.other_config |> Astring.String.fields ~empty:false with Not_found -> [] in
          libs
        | Ok _ -> []
        | Error (`Msg x) -> Logs.err (fun m -> m "Failed to read frontmatter for page: %s - %s" (Fpath.to_string (Fpath.v mld)) x); []
      in
      Logs.debug (fun m -> m "Libraries for page: %s = %s" (Fpath.to_string (Fpath.v mld)) (String.concat ", " libs_list));
      let libs_list = "stdlib" :: libs_list in
      let libs =
        List.filter_map
          (fun lib_name -> try Some (lib_name, Util.StringMap.find lib_name lib_map) with _ -> Logs.err (fun m -> m "Failed to find library: %s" lib_name); None)
          libs_list
      in
      Odoc.link
        ~input_file:Fpath.(odoc_path / odoc_file)
        ~libs
        ~docs:[ ("site", odoc_dir) ]
        ~includes:[] ~ignore_output:true ~custom_layout:true
        ~warnings_tags:[ "odoc_notebook" ] ())
    files;

  Odoc.compile_index ~json:false ~roots:[ odoc_dir ] ~simplified:false
    ~wrap:false
    ~output_file:(Fpath.v "index.odoc-index")
    ();

  Odoc.sidebar_generate ~output_file:(Fpath.v "sidebar.json") ~json:true
    (Fpath.v "index.odoc-index")
    ();

  Odoc.sidebar_generate
    ~output_file:(Fpath.v "sidebar.odoc-sidebar")
    ~json:false
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

  let globaltoc cur_path =
    match sidebar with
    | [] ->
        Format.eprintf "No global sidebar found\n%!";
        ""
    | _ ->
        let rec aux tree =
          let children = List.map aux tree.children in
          let has_children = List.length children > 0 in
          let children =
            if has_children then
              Some (Printf.sprintf "<ul>%s</ul>" (String.concat "" children))
            else None
          in
          let content =
            match tree.node.url with
            | Some url ->
                Printf.sprintf "<a href=\"/%s\">%s</a>" url tree.node.content
            | None -> tree.node.content
          in
          let should_do_children =
            match tree.node.url with
            | None -> true
            | Some url -> 
              let parent = Fpath.parent (Fpath.v url) in
              (* Format.eprintf "Checking %a against %a\n%!" Fpath.pp parent Fpath.pp cur_path; *)
              let result =
                parent = Fpath.v "./" || Fpath.is_prefix parent cur_path in
              (* Format.eprintf "Result: %b\n%!" result; *)
              result
          in
          match children, should_do_children with
          | Some children, true ->
            Printf.sprintf "<li>%s%s</li>" content children
          | _, _ ->
            Printf.sprintf "<li>%s</li>" content
        in
        Printf.sprintf "<ul>%s</ul>" (String.concat "" (List.map aux sidebar))
  in

  (*let () =
    Mk_frontend.mk Util.StringSet.empty
      Fpath.(output_dir / "assets")
      "default_frontend"
  in*)
  let () = Out_channel.with_open_bin Fpath.(to_string (output_dir / "assets" / "default_frontend.js")) (fun oc ->
    Printf.fprintf oc "%s" (Frontend_crunch.read "main.bc.js" |> Option.get)) in

  List.iter
    (fun mld ->
      let dir, file = Fpath.split_base (Fpath.v mld) in
      let odoc_file =
        Fpath.(set_ext "odoc" file |> to_string |> (fun x -> "page-" ^ x) |> v)
      in
      let odocl_file = Fpath.(set_ext "odocl" odoc_file) in
      let odoc_dir = Fpath.(append odoc_dir dir) in
      match Fpath.(rem_ext ~multi:false file |> to_string) with
      | "index" ->
          Odoc.html_generate ~output_dir:output_dir_str
            ~input_file:(Fpath.append odoc_dir odocl_file)
            ~sidebar:(Fpath.v "sidebar.odoc-sidebar")
            ~as_json:false ()
      | _ ->
          let _ =
            Odoc.html_generate ~output_dir:output_dir_str
              ~input_file:(Fpath.append odoc_dir odocl_file)
              ~as_json:true ()
          in
          let json_file =
            Fpath.(
              append output_dir dir
              / (Fpath.set_ext ".html.json" file |> to_string))
          in
          let x = Fpath.rem_ext file |> Fpath.to_string in

          let ic = open_in (Fpath.to_string json_file) in
          let yojson = Yojson.Safe.from_channel ic in
          let _ =
            let json = as_json_of_yojson yojson in
            let json = match json with Ok x -> x | Error e -> failwith e in
            (* Format.eprintf "We've got the result - doing details\n%!"; *)
            let breadcrumbs = {|<a href="../index.html">Up</a> - notebooks|} in
            let localtoc =
              match json.toc with
              | [] -> None
              | _ ->
                  let rec aux (toc : toc) =
                    let children = List.map aux toc.children in
                    let children =
                      if List.length children > 0 then
                        Printf.sprintf "<ul>%s</ul>" (String.concat "" children)
                      else ""
                    in
                    Printf.sprintf "<li><a href=\"%s\">%s</a>%s</li>" toc.href
                      toc.title children
                  in
                  let toc = List.map aux json.toc in
                  Some (Printf.sprintf "<ul>%s</ul>" (String.concat "" toc))
            in
            let post_content = "" in
            Format.eprintf "Creating html page\n%!";
            let dir, _file = Fpath.split_base (Fpath.v mld) in
            let parent_id = Fpath.(normalize dir) in
            (* Format.eprintf "Parent id: %a\n%!" Fpath.pp parent_id; *)
            let globaltoc = globaltoc parent_id in
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
                    frontend = "default_frontend.js";
                    content = json.content;
                    post_content;
                  })
            in
            Format.eprintf "Created\n%!";
            let oc =
              open_out
                Fpath.(append output_dir dir / (x ^ ".html") |> to_string)
            in
            Printf.fprintf oc "%s" html;
            close_out oc;
            Ok ()
          in
          ())
    files;
  `Ok ()

(* let test files =
  match Odoc_notebook_lib.Test.run files with
  | Ok () -> `Ok ()
  | Error (`Msg m) ->
      Format.eprintf "Error: %s\n%!" m;
      `Error (false, m) *)

let fpath_arg =
  let print ppf v = Fpath.pp ppf v in
  Arg.conv (Fpath.of_string, print)

let odoc_dir =
  let doc = "Odoc directory from odoc_driver" in
  Arg.(required & opt (some fpath_arg) (Some (Fpath.v "_odoc")) & info [ "odoc-dir" ] ~doc)

let generate_cmd =
  let output_dir =
    let doc = "Output directory in which to put all outputs" in
    Arg.(value & opt string "html" & info [ "output" ] ~doc)
  in
  let files = Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILE") in
  let doc = "Generate static files to serve a notebook" in
  let info = Cmd.info "generate" ~doc in
  Cmd.v info Term.(ret (const generate $ output_dir $ odoc_dir $ files))

(* let test_cmd =
  let files = Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILE") in
  let info = Cmd.info "test" ~doc:"Test an mld file" in
  Cmd.v info Term.(ret (const test $ files)) *)

let main_cmd =
  let doc = "An odoc notebook tool" in
  let info = Cmd.info "odoc-notebook" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ generate_cmd ]

let () = exit (Cmd.eval main_cmd)
