module Block = struct
  type metadata = {
    language_tag : string;
    delimiter : string option;
    tags : Odoc_parser.Ast.code_block_tags;
  }

  type code_block = {
    metadata : metadata option;
    content_txt : string;
    code_block : Odoc_parser.Loc.span;
        (* Location of the enclosing code block *)
    location : Location.t; (* Location of the enclosing code block *)
    output : t list;
    changed : bool;
  }

  and t = [ `Text of string | `Block of code_block ]

  type language = OCaml | OCamlTop | Unknown of string option

  let language v =
    match v.metadata with
    | None -> Unknown None
    | Some m -> (
        match m.language_tag with
        | "ocaml" -> OCaml
        | "ocamltop" -> OCamlTop
        | _ -> Unknown (Some m.language_tag))
end

let output_of_line x =
  match String.trim x with "..." -> `Ellipsis | _ -> `Output x

let slice file_contents (loc : Location.t) =
  let start = loc.loc_start.pos_cnum in
  let len = loc.loc_end.pos_cnum - start in
  String.sub file_contents start len

(* Parse and extract code block metadata from an odoc formatted docstring.

   Code blocks are the only thing we're interested in. This function parses
   the given text and extracts the metadata and enough location information
   from the code blocks be able to String.sub them out of the original text.

   [location] is the location of this docstring within the original file
   (ie, the location of the contents of the documentation comment). This is
   required so we can splice out the code blocks from the original file.

   The results are prepended in reverse order onto [acc]. *)
let rec extract_code_block_info acc raw ~parsed ~ast =
  let module O = Odoc_parser in
  (* If odoc-parser produced any warnings, we raise them as errors here *)
  List.iter
    (fun error -> failwith (O.Warning.to_string error))
    (O.warnings parsed);

  (* Extract the useful info from what odoc has given us.

     Note, we don't use the contents of the code block that odoc has handed us
     as that has been stripped and we need all the relevant whitespace.
     Fortunately the location info give us enough info to be able to extract
     the code from the original text, whitespace and all.
  *)
  let rec handle_code_block : O.Loc.span -> _ -> Block.code_block =
    let convert_loc (sp : O.Loc.span) =
      Location.
        {
          loc_start = O.position_of_point parsed sp.start;
          loc_end = O.position_of_point parsed sp.end_;
          loc_ghost = false;
        }
    in
    fun location { O.Ast.meta; delimiter; content = { O.Loc.value; _ }; output }
      ->
      let metadata =
        Option.map
          (fun { O.Ast.language; tags } ->
            let language_tag = O.Loc.value language in
            Block.{ language_tag; delimiter; tags })
          meta
      in
      let content_txt = value in
      let code_block = location in
      let location = convert_loc location in
      let output_fn = parse_chunk raw parsed in
      let output =
        Option.map output_fn
          (output
            :> Odoc_parser.Ast.block_element Odoc_parser.Ast.with_location list
               option)
      in
      let output = Option.value ~default:[] output in
      { metadata; content_txt; location; code_block; output; changed = false }
  (* Fold over the results from odoc-parser, recurse where necessary
     and extract the code block metadata *)
  and fold_fn acc (elt : O.Ast.block_element O.Loc.with_location) =
    match elt with
    | { O.Loc.value = `Code_block c; location } ->
        handle_code_block location c :: acc
    | { O.Loc.value = `List (_, _, lists); _ } ->
        List.fold_left (List.fold_left fold_fn) acc (lists :> O.Ast.t list)
    | { O.Loc.value = `Tag tag; _ } -> (
        match tag with
        | `Deprecated blocks
        | `Param (_, blocks)
        | `Raise (_, blocks)
        | `Return blocks
        | `See (_, _, blocks)
        | `Before (_, blocks) ->
            List.fold_left fold_fn acc (blocks :> O.Ast.t)
        | _ -> acc)
    | _ -> acc
  in

  List.fold_left fold_fn acc ast

(* Given the locations of the code blocks within [file_contents], then slice it up into
   [Text] and [Block] parts by using the starts and ends of those blocks as
   boundaries. *)
and extract_blocks code_blocks file_contents =
  let cursor, tokens =
    List.fold_left
      (fun (cursor, code_blocks) (code_block : Block.code_block) ->
        let pre_text =
          `Text
            (String.sub file_contents cursor
               (code_block.location.loc_start.pos_cnum - cursor))
        in
        let block = `Block code_block in
        (* append them in reverse order, since this is a fold_left *)
        let code_blocks = block :: pre_text :: code_blocks in
        (code_block.location.loc_end.pos_cnum, code_blocks))
      (0, []) code_blocks
  in
  let tokens = List.rev tokens in
  if cursor < String.length file_contents then
    let remainder =
      String.sub file_contents cursor (String.length file_contents - cursor)
    in
    if not (String.equal remainder "") then tokens @ [ `Text remainder ]
    else tokens
  else tokens

and parse_chunk raw parsed ast =
  let code_blocks = extract_code_block_info [] raw ~parsed ~ast |> List.rev in
  extract_blocks code_blocks raw

let parse_mld : Mld.t -> (Block.t list, [ `Msg of string ]) Result.t =
 fun mld ->
  let { Mld.raw; parsed } = mld in
  let ast = Odoc_parser.ast parsed in
  Ok (parse_chunk raw parsed ast)
