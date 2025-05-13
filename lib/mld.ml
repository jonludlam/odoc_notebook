type t = { raw : string; parsed : Odoc_parser.t; model_ast : Odoc_model.Comment.docs; frontmatter : Odoc_model.Frontmatter.t }

let parse_mld mld_file =
  let raw = In_channel.(with_open_bin mld_file input_all) in
  let location =
    { Lexing.pos_fname = mld_file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let parsed = Odoc_parser.parse_comment ~location ~text:raw in
  let parent = Odoc_model.(Paths.Identifier.Mk.page (None, Names.PageName.make_std "Dummy")) in
  let r = Odoc_model.Error.raise_warnings (Odoc_loader.read_string parent mld_file raw) in
  match r with
  | Ok (model_ast, frontmatter) ->
    Ok { raw; parsed; model_ast; frontmatter }
  | Error _ ->    Error ("Failed to parse mld file")

let frontmatter parsed =
   parsed.frontmatter

let model_ast parsed =
  parsed.model_ast