(* Test an mld file. A mini mdx *)
open Lwt

let handle_ocaml (block : Blocks.Block.code_block) =
  Toplevel.eval () [ block.content_txt ] >>= function
  | Ok (mime, out) ->
      Format.eprintf "Evaluated some OCaml code: %s\n%!"
        (String.concat "\n" out);
      if List.length mime = 0 then Format.eprintf "No MIME values\n%!";
      let output = List.map (fun x -> `Text (Mime_printer.to_odoc x)) mime in
      Lwt.return { block with Blocks.Block.output }
  | Error e ->
      Format.eprintf "Got an error evaluating OCaml code\n%s%!"
        (String.concat "\n" e);
      Lwt.return block

let handle_toplevel (block : Blocks.Block.code_block) =
  let stripped, _ =
    Odoc_parser.codeblock_content block.code_block block.content_txt
  in
  Toplevel.eval_toplevel () stripped >>= fun res ->
  match res with
  | Ok { Js_top_worker_rpc.Toplevel_api_gen.script = content_txt; mime_vals; parts=_ } ->
      Format.eprintf "Evaluated some toplevel OCaml code: %s\n%!" content_txt;
      if List.length mime_vals = 0 then Format.eprintf "No MIME values\n%!";
      let changed = String.compare content_txt stripped in
      let output =
        List.map (fun x -> `Text (Mime_printer.to_odoc x)) mime_vals
      in
      Lwt.return { block with Blocks.Block.content_txt; output; changed = changed <> 0 }
  | Error _ ->
    Lwt.return block

let handle_block (block : Blocks.Block.code_block) =
  let open Blocks.Block in
  match language block with
  | OCaml ->
      let content_txt =
        Odoc_parser.codeblock_content block.code_block block.content_txt |> fst
      in
      handle_ocaml block >>= fun block -> Lwt.return
      { block with content_txt }
  | OCamlTop -> handle_toplevel block
  | Unknown _ ->
      let content_txt =
        Odoc_parser.codeblock_content block.code_block block.content_txt |> fst
      in
      Lwt.return { block with content_txt }

let rec print_block fmt (block : Blocks.Block.code_block) =
  let block =
    match (block.output, block.metadata) with
    | _ :: _, Some ({ delimiter = None; _ } as m) ->
        { block with metadata = Some { m with delimiter = Some "x" } }
    | _ -> block
  in
  let fmt_tags fmt tags =
    if tags <> [] then
      List.iter
        (fun (tag : Odoc_parser.Ast.code_block_tag) ->
          match tag with
          | `Tag t -> Format.fprintf fmt " %s" t.value
          | `Binding (k, v) -> Format.fprintf fmt " %s=%s" k.value v.value)
        tags
  in
  let fmt_meta fmt (meta : Blocks.Block.metadata option) =
    match meta with
    | None -> ()
    | Some meta ->
        Format.fprintf fmt "%s@%s%a"
          (Option.value ~default:"" meta.delimiter)
          meta.language_tag fmt_tags meta.tags
  in
  let indent_l = block.code_block.start.column in
  let indent = String.make indent_l ' ' in
  Format.fprintf fmt "{%a[\n" fmt_meta block.metadata;
  let lines = Astring.String.cuts ~sep:"\n" block.content_txt in
  let lines =
    match List.rev lines with "" :: tl -> List.rev tl | _ -> lines
  in
  List.iter (fun line -> Format.fprintf fmt "%s%s\n" indent line) lines;
  match (block.output, block.metadata) with
  | [], _ -> Format.fprintf fmt "%s]}" indent
  | _, Some { delimiter = Some m; _ } ->
      Format.fprintf fmt "]%s[\n" m;
      List.iter
        (fun x ->
          match x with
          | `Text t -> Format.fprintf fmt "%s%s\n" indent t
          | `Block b -> Format.fprintf fmt "%s%a\n" indent print_block b)
        block.output;
      Format.fprintf fmt "%s]}" indent
  | _ -> failwith "print_block: output without metadata"

let test file =
  let open Lwt.Infix in
  let mld = Mld.parse_mld file |> Result.get_ok in
  let blocks = Blocks.parse_mld mld |> Result.get_ok in
  let meta = Mld.frontmatter mld in
  let libs =
    try List.assoc "libs" meta.other_config |> Astring.String.fields ~empty:false with Not_found -> []
  in
  Toplevel.init ~verbose:false ~silent:true ~verbose_findlib:false
    ~directives:[] ~packages:libs ~predicates:[] () >>=
  function | Error e ->
    Lwt.return (Error e)
  |
    Ok () ->
  let results =
    Lwt_list.fold_left_s
      (fun acc block ->
        match block with
        | `Text _ -> Lwt.return (block :: acc)
        | `Block block ->
          let open Lwt.Infix in
          handle_block block >>= fun b -> Lwt.return ((`Block b) :: acc))
      [] blocks
  in
  results >>= fun results ->
  List.iter
    (fun x ->
      match x with
      | `Text t -> Format.printf "%s" t
      | `Block b -> Format.printf "%a" print_block b)
    (List.rev results);
  Lwt_result.return ()

let run files =
  Lwt_list.map_s test files >>= fun results ->
  match List.filter_map
    (function Ok _ -> None | Error e -> Some e)
    results with
  | [] -> Lwt_result.return ()
  | errs ->
      let msgs =
        List.map (function | Js_top_worker_rpc.Toplevel_api_gen.InternalError m -> m) errs |> String.concat "\n"
      in
      Lwt_result.fail (`Msg msgs)
