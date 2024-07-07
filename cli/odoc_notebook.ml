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

type meta = {
  libs : string list; [@default []]
  html_scripts : string list; [@default []]
}
[@@deriving yojson]

type breadcrumb = { name : string; href : string; kind : string }
[@@deriving yojson]

type toc = { title : string; href : string; children : toc list }
[@@deriving yojson]

type as_json = {
  type_ : string; [@key "type"]
  uses_katex : bool;
  breadcrumbs : breadcrumb list;
  toc : toc list;
  preamble : string;
  source_anchor : string option;
  content : string;
}
[@@deriving yojson]

let meta_of_mld mld_file =
  let ic = open_in mld_file in
  let lines = Util.lines_of_channel ic in
  close_in ic;
  let text = String.concat "\n" lines in
  let location =
    { Lexing.pos_fname = mld_file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let parsed = Odoc_parser.parse_comment ~location ~text in
  let meta_block =
    List.filter_map
      (fun block ->
        let open Odoc_parser in
        match block with
        | { Loc.value = `Code_block c; _ } -> (
            match c.Ast.meta with
            | None -> None
            | Some m ->
                if m.Ast.language.value = "meta" then Some c.content.value
                else None)
        | _ -> None)
      (Odoc_parser.ast parsed)
  in
  let meta =
    match meta_block with
    | [] -> None
    | x :: _ :: _ ->
        Format.eprintf
          "Warning, more than one meta block found. Ignoring all but the first.";
        Some x
    | [ meta ] -> Some meta
  in
  match meta with
  | None -> None
  | Some meta -> (
      let y = Yojson.Safe.from_string meta in
      match meta_of_yojson y with Ok m -> Some m | _ -> None)

let cmi_files dir =
  Bos.OS.Dir.fold_contents ~traverse:`None ~elements:`Files
    (fun path acc ->
      if Fpath.has_ext ".cmi" path then Fpath.filename path :: acc else acc)
    [] dir

let gen_json cmis =
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
  let cmis =
    {
      Js_top_worker_rpc.Toplevel_api_gen.static_cmis = [];
      dynamic_cmis =
        [
          {
            dcs_url = "cmis/";
            dcs_toplevel_modules = List.map String.capitalize_ascii non_hidden;
            dcs_file_prefixes = Util.StringSet.to_list prefixes;
          };
        ];
    }
  in
  let rpc =
    Rpcmarshal.marshal Js_top_worker_rpc.Toplevel_api_gen.typ_of_cmis cmis
  in
  let json = Jsonrpc.to_string rpc in
  json

let generate_page mld =
  let cwd = Fpath.v "." in
  Odoc.compile ~output_dir:cwd ~includes:Fpath.Set.empty
    ~input_file:(Fpath.v mld) ~parent_id:"notebooks"

let generate output_dir_str files =
  let verbose = true in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let () = Worker_pool.start_workers env sw 16 in
  let output_dir = Fpath.v output_dir_str in
  let metas = List.filter_map (fun f -> meta_of_mld f) files in
  let libs =
    List.fold_left
      (fun acc meta -> Util.StringSet.(union (of_list meta.libs) acc))
      (Util.StringSet.singleton "stdlib")
      metas
  in
  let cmi_dirs =
    match Ocamlfind.deps (Util.StringSet.to_list libs) with
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
  let worker_oc = open_out Fpath.(output_dir / "assets" / "worker.js" |> to_string) in
  Printf.fprintf worker_oc "%s" (Odoc_notebook_crunch.read "worker.js" |> Option.get) ;
  close_out worker_oc;
  ignore (Odoc.support_files Fpath.(output_dir / "assets"));
  let init_json = gen_json cmis in
  List.iter (fun mld -> generate_page mld) files;
  List.iter
    (fun mld ->
      let x = String.sub mld 0 (String.length mld - 4) in
      let odoc_file = "page-" ^ x ^ ".odoc" in
      let odocl_file = odoc_file ^ "l" in
      let _ =
        Odoc.link
          ~input_file:Fpath.(v "notebooks" / odoc_file)
          ~includes:Fpath.Set.empty ()
      in
      let _ =
        Odoc.html_generate ~output_dir:output_dir_str
          ~input_file:Fpath.(v "notebooks" / odocl_file)
          ~as_json:true ()
      in
      let json_file =
        Fpath.(v "html" / "notebooks" / (x ^ ".html.json") |> to_string)
      in
      let ic = open_in json_file in
      let yojson = Yojson.Safe.from_channel ic in
      Format.eprintf "Here we go...\n%!";
      let _ =
        let* json = as_json_of_yojson yojson in
        let breadcrumbs = {|<a href="../index.html">Up</a> – notebooks|} in
        let toc =
          {|<ul>
        <li>
          <a href="#stsuff-x">Stsuff x</a>
          <ul>
            <li>
              <a href="#m-stuff">M stuff</a>
            </li>
            <li>
              <a href="#other-stuff">Other stuff</a>
            </li>
          </ul>
        </li>
      </ul>|}
        in
        Format.eprintf "Got here...\n%!";
        let post_content = Printf.sprintf {|
<script type="text/javascript" defer>
globalThis.cmis = %S;
</script>|} init_json in
        let* html =
          Html_page.(
            create
              {
                title = mld;
                breadcrumbs;
                toc;
                odoc_assets_path = "../assets";
                preamble = json.preamble;
                content = json.content;
                post_content;
              })
        in
        Format.eprintf "Alll good here!\n%!";
        let oc =
          open_out Fpath.(v "html" / "notebooks" / (x ^ ".html") |> to_string)
        in
        Printf.fprintf oc "%s" html;
        close_out oc;
        Ok ()
      in
      ())
    files;
  `Ok ()

let generate_cmd =
  let output_dir =
    let doc = "Output directory in which to put all outputs" in
    Arg.(value & opt string "html" & info [ "output" ] ~doc)
  in
  let files = Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILE") in
  let doc = "Generate static files to serve a notebook" in
  let info = Cmd.info "generate" ~doc in
  Cmd.v info Term.(ret (const generate $ output_dir $ files))

let main_cmd =
  let doc = "An odoc notebook tool" in
  let info = Cmd.info "odoc-notebook" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ generate_cmd ]

let () = exit (Cmd.eval main_cmd)
