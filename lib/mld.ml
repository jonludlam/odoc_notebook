type t = { raw : string; parsed : Odoc_parser.t }

type meta = {
  libs : string list; [@default []]
  html_scripts : string list; [@default []]
}
[@@deriving yojson]

let parse_mld mld_file =
  let text = In_channel.(with_open_bin mld_file input_all) in
  let location =
    { Lexing.pos_fname = mld_file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  { raw = text; parsed = Odoc_parser.parse_comment ~location ~text }

let meta_of_parsed parsed =
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

let meta_of_mld mld_file =
  let { parsed; _ } = parse_mld mld_file in
  meta_of_parsed parsed
