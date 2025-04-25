open Brr
open Js_top_worker_rpc
module W = Js_top_worker_client_fut.W

let _ = Toploop.getvalue
let ocaml = Jv.get Jv.global "__CM__mllike" |> Stream.Language.of_jv
let ocaml = Stream.Language.define ocaml
let _ = Console.debug [ Jv.of_string "Starting" ]

type meta = {
  libs : string list; [@default []]
  html_scripts : string list; [@default []]
}
[@@deriving yojson]

let initialise requires s callback =
  let open Fut.Result_syntax in
  let rpc = Js_top_worker_client_fut.start s 100000 callback in
  let* () =
    W.init rpc { Toplevel_api_gen.path = "/static/cmis"; cmas = []; cmis={ static_cmis=[]; dynamic_cmis=[] }; findlib_index = "/_opam/findlib_index"; findlib_requires=requires; stdlib_dcs = "/_opam/ocaml/dynamic_cmis.json";
    }
  in
  Fut.return (Ok rpc)

let styled_text tstyle text =
  let lines = Astring.String.cuts ~sep:"\n" text in
  let non_empty = List.filter (fun x -> x <> "") lines in
  List.map
    (fun l -> El.(span ~at:[ At.style (Jstr.v tstyle) ] [ txt' (l ^ "\n") ]))
    non_empty

let mime_els id mime_vals =
  List.map
    (fun mime ->
      let l =
        Astring.String.cuts ~sep:"/" mime.Toplevel_api_gen.mime_type
      in
      match (l, mime.encoding) with
      | [ "image"; _ty ], Mime_printer.Base64 ->
          let src = "data:" ^ mime.mime_type ^ ";base64," ^ mime.data in
          [ El.img ~at:[ At.src (Jstr.of_string src) ] () ]
      | [ "image"; "svg" ], Noencoding ->
          let div = El.div [] in
          Jv.set (El.to_jv div) "innerHTML" (Jv.of_string mime.data);
          El.children div
      | [ "text"; "html" ], Noencoding ->
          let div = El.div [] in
          Jv.set (El.to_jv div) "innerHTML" (Jv.of_string mime.data);
          El.children div
      | _ -> [])
    mime_vals
  |> List.flatten


let append_to_output id output _mime_only =
  let el = Brr.Document.find_el_by_id Brr.G.document (Jstr.v id) in
  match el with
  | None -> ()
  | Some el ->
      let stdout =
        List.map Option.to_list
          Toplevel_api_gen.[ output.stdout; output.caml_ppf ]
        |> List.concat |> String.concat "\n" |> styled_text "color:white"
        |> El.pre ~at:[ At.class' (Jstr.v "stdout") ]
      in
      let mime_els = mime_els id output.mime_vals in 
      El.set_children el ([ stdout ] @ mime_els)

let basic_setup =
  Jv.get Jv.global "__CM__basic_setup" |> Code_mirror.Extension.of_jv

type block_ty = Deferred | OCaml | Toplevel | Other

let string_of_block_ty = function
  | Deferred -> "Deferred"
  | OCaml -> "OCaml"
  | Toplevel -> "Toplevel"
  | Other -> "Other"

let block_ty_of_string = function
  | "Deferred" -> Deferred
  | "OCaml" -> OCaml
  | "Toplevel" -> Toplevel
  | "Other" -> Other
  | _ -> failwith "Unknown block type"

type ostate = {
  id : string;
  output_id : string;
  hidden : bool;
  solution : string option;
  ty : block_ty;
  mime_only : bool;
  rpc : Js_top_worker_client_fut.rpc;
}

let to_jv : ostate -> Jv.t =
 fun x ->
  let res =
    Jv.obj
      [|
        ("id", Jv.of_string x.id);
        ("rpc", Jv.repr x.rpc);
        ("hidden", Jv.of_bool x.hidden);
        ("output_id", Jv.of_string x.output_id);
        ("ty", Jv.of_string (string_of_block_ty x.ty));
        ("mime_only", Jv.of_bool x.mime_only);
      |]
  in
  Jv.set_if_some res "solution" (Option.map Jv.of_string x.solution);
  res

let of_jv : Jv.t -> ostate =
 fun jv ->
  let id = Jv.get jv "id" |> Jv.to_string in
  let output_id = Jv.get jv "output_id" |> Jv.to_string in
  let (rpc : Js_top_worker_client_fut.rpc) = Jv.get jv "rpc" |> Obj.magic in
  let solution = Jv.find jv "solution" |> Option.map Jv.to_string in
  let ty = Jv.get jv "ty" |> Jv.to_string |> block_ty_of_string in
  let hidden = Jv.get jv "hidden" |> Jv.to_bool in
  let mime_only = Jv.get jv "mime_only" |> Jv.to_bool in
  { id; output_id; rpc; solution; ty; mime_only; hidden }

let oeffect = Code_mirror.State.StateEffect.define to_jv of_jv

let autorun_effect =
  Code_mirror.State.StateEffect.define
    Jv.of_bool
    Jv.to_bool

let autorun_state_field =
  let update cur t =
    let effects = Code_mirror.State.Transaction.effects t in
    List.fold_right
      (fun e cur ->
        if Code_mirror.State.StateEffect.is e autorun_effect then
          match Code_mirror.State.StateEffect.value e autorun_effect with
          | Some b -> b
          | _ -> cur
        else cur)
      effects cur in
  Code_mirror.State.StateField.define Jv.of_bool Jv.to_bool
  ~create:(fun _ -> false) ~update

let exec_js id _ =
  let fun_name = Printf.sprintf "%s(globalThis)" id in
  (* let output_div = Printf.sprintf "output-%d" id in *)
  let _f = Js_of_ocaml.Js.Unsafe.eval_string fun_name in
  let mime_entries = Mime_printer.get () in
  Console.log
    [
      Jv.of_string (Printf.sprintf "Got %d entries" (List.length mime_entries));
    ];
  List.iter
    (fun e -> Console.log [ Jv.of_string (Mime_printer.to_odoc e) ])
    mime_entries

let run_code st code v =
  (* let code =
        {|module Odoc_notebook_cell_|} ^ st.id ^ {| = struct |} ^ code
        ^ {| end;; |}
      in *)
  let toggle_autorun = Code_mirror.State.StateEffect.of_ autorun_effect false in

  match st.ty with
  | Other -> Fut.return (Ok ())
  | Deferred ->
      Fut.bind (W.compile_js st.rpc None code) (function
        | Ok res ->
            Console.(log [ str ("JS received: " ^ res) ]);
            let _ = Js_of_ocaml.Js.Unsafe.eval_string res in
            (* exec_js st.id (); *)
            let changes = Code_mirror.State.Transaction.create ~effects:[toggle_autorun] () in
            Code_mirror.View.EditorView.dispatch v changes;
            Fut.return (Ok ())
        | Error _e ->
            Console.(log [ str "error" ]);
            let changes = Code_mirror.State.Transaction.create ~effects:[toggle_autorun] () in
            Code_mirror.View.EditorView.dispatch v changes;
            Fut.return (Ok ()))
  | OCaml ->
      Fut.bind (W.exec st.rpc code) (fun res ->
          match res with
          | Ok res ->
              (* Console.(log [ str ("OCaml received: " ^ (Option.value ~default:"" res.stdout)) ]); *)
              (* append_to_output st.output_id res st.mime_only; *)
              (* Console.log [ Option.get (Option.map Jv.of_string res.stdout) ]; *)
              let changes = Code_mirror.State.Transaction.create ~effects:[toggle_autorun] () in
              Code_mirror.View.EditorView.dispatch v changes;
              let el = Brr.Document.find_el_by_id Brr.G.document (Jstr.v st.output_id) in
              (match el with
              | None -> Console.log [ Jv.of_string "No output element" ]
              | Some el ->
                  let mime_els = mime_els st.output_id res.mime_vals in 
                  El.set_children el (mime_els));
              Console.(log [ str "All finished" ]);
              Fut.return (Ok ())
          | Error _e ->
              let changes = Code_mirror.State.Transaction.create ~effects:[toggle_autorun] () in
              Code_mirror.View.EditorView.dispatch v changes;
              Console.log [ Jv.of_string "error" ];
              Fut.return (Error ()))
  | Toplevel ->
      Fut.bind (W.exec_toplevel st.rpc code) (fun res ->
          match res with
          | Error _ ->
            let changes = Code_mirror.State.Transaction.create ~effects:[toggle_autorun] () in
            Code_mirror.View.EditorView.dispatch v changes;
            Fut.return (Ok ())
          | Ok s ->
              let cur_length =
                Code_mirror.View.EditorView.state v
                |> Code_mirror.State.EditorState.doc
                |> Code_mirror.State.Text.length
              in
              let changes =
                {
                  Code_mirror.State.Transaction.from = 0;
                  to_ = Some cur_length;
                  insert = Some s.script;
                }
              in
              let changes = Code_mirror.State.Transaction.create ~changes ~effects:[toggle_autorun] () in
              Code_mirror.View.EditorView.dispatch v changes;
              let el = Brr.Document.find_el_by_id Brr.G.document (Jstr.v st.output_id) in
              (match el with
              | None -> Console.log [ Jv.of_string "No output element" ]
              | Some el ->
                  let mime_els = mime_els st.output_id s.mime_vals in 
                  El.set_children el (mime_els);
              );
              Fut.return (Ok ()))

let panel_constructor (st : ostate) (v : Code_mirror.View.EditorView.t) =
  let run_button =
    Brr.El.(button ~at:[ At.class' (Jstr.v "panel") ] [ txt (Jstr.v "Run") ]) in
  let run =
    match st.ty with
    | Other -> []
    | _ -> [run_button]
  in
  (* let autorun_txt autorun =
    if autorun
    then Jstr.v "Autorun: On"
    else Jstr.v "Autorun: Off"
  in *)
  let autorun = Code_mirror.State.EditorState.field (Code_mirror.View.EditorView.state v) autorun_state_field in 
  (* let autorun_span =
    Brr.El.(span [ txt (autorun_txt autorun) ])
  in *)
  let show_solution =
    match st.solution with
    | Some s ->
        let ss = Brr.El.(button [ txt (Jstr.v "Show Solution") ]) in
        let listener =
          Brr.Ev.listen Ev.click
            (fun _ ->
              Console.log [ Jv.of_string "clicked" ];
              let solution = Brr.El.find_by_class (Jstr.v s) in
              match solution with
              | [ e ] ->
                  Console.log [ Jv.of_string "Got a solution" ];
                  let soln =
                    El.txt_text
                      (El.children e |> List.hd |> El.children |> List.hd)
                    |> Jstr.to_string
                  in
                  Console.log [ Jv.of_string soln ];
                  let cur_length =
                    Code_mirror.View.EditorView.state v
                    |> Code_mirror.State.EditorState.doc
                    |> Code_mirror.State.Text.length
                  in
                  let changes =
                    {
                      Code_mirror.State.Transaction.from = 0;
                      to_ = Some cur_length;
                      insert = Some soln;
                    }
                  in
                  let changes =
                    Code_mirror.State.Transaction.create ~changes ()
                  in
                  Code_mirror.View.EditorView.dispatch v changes
              | _ -> Console.log [ "No solution found" ])
            (El.as_target ss)
        in
        ignore listener;
        [ ss ]
    | None -> []
  in
  let update v =
      let state = Code_mirror.View.EditorView.Update.state v in
      let _autorun = Code_mirror.State.EditorState.field state autorun_state_field in
      ()
      (* let text = autorun_txt autorun in *)
      (* El.set_children autorun_span El.[ txt text ] *)
  in


  let clickfn _ =
    let code =
      Code_mirror.(
        View.EditorView.state v |> State.EditorState.doc |> State.Text.to_string)
    in
    ignore @@ run_code st code v
  in
  ignore (Brr.Ev.listen Ev.click clickfn (El.as_target run_button));
  if autorun then
    ignore @@ clickfn ();
  (* let id = Brr.El.span ~at:[At.class' (Jstr.v "panel_id")] [ Brr.El.txt' st.id ] in *)
  let dom = Brr.El.(div (run @ show_solution)) in
  Code_mirror.View.Panel.create ~top:true ~update dom

let ostate =
  let provide field =
    Code_mirror.State.Facet.from' Code_mirror.View.showPanel field (fun st ->
        Some (panel_constructor st)) in

  let update cur t =
    let effects = Code_mirror.State.Transaction.effects t in
    List.fold_right
      (fun e cur ->
        if Code_mirror.State.StateEffect.is e oeffect then
          match Code_mirror.State.StateEffect.value e oeffect with
          | Some b -> b
          | _ -> cur
        else cur)
      effects cur in

  Code_mirror.State.StateField.define to_jv of_jv
    ~create:(fun _ -> failwith "unset")
    ~provide ~update


let init ?doc ?(exts = []) () =
  let open Code_mirror in
  (* let doc = Option.map Jv.of_string doc in *)
  let config =
    State.EditorStateConfig.create ?doc ~extensions:(basic_setup :: exts) ()
  in
  let state = State.EditorState.create ~config () in
  let config = View.EditorViewConfig.create ~state () in
  let view : View.EditorView.t = View.EditorView.create ~config () in
  (state, view)

let make_editor rpc id elt =
  let parent_exn elt =
    match El.parent elt with Some e -> e | None -> failwith "no parent"
  in
  let parent = parent_exn elt in
  Console.log [ Jstr.v "Got parent" ];
  Console.log [ parent_exn parent ];
  let output =
    match El.children (parent_exn parent) with
    | _ :: o :: _ -> Some o
    | _ -> None
  in
  let output_id = "output-" ^ string_of_int id in
  let _ = Option.iter (El.set_at (Jstr.v "id") (Some (Jstr.v output_id))) output in
  let doc = El.txt_text (El.children elt |> List.hd) |> Jstr.to_string in
  let classes_iter =
    Jv.call (Jv.get (Jv.repr parent) "classList") "values" [||]
  in
  let str_classes =
    Jv.It.fold Jv.to_string (fun x y -> x :: y) classes_iter []
  in
  let solution =
    List.fold_right
      (fun x y ->
        if Astring.String.is_prefix ~affix:"solution:" x then
          Some
            (String.sub x
               (String.length "solution:")
               (String.length x - String.length "solution:"))
        else y)
      str_classes None
  in
  let mime_only = List.mem "mime-only" str_classes in
  (* let is_deferred = List.mem "deferred-js" str_classes in *)
  let ty =
    if List.mem "language-ocaml" str_classes then Toplevel
    else if List.mem "language-ocamltop" str_classes then Toplevel
    else Other in
  let hidden = List.mem "hidden" str_classes in
  Console.log [ Jstr.v ("Classes: " ^ String.concat ", " str_classes) ];
  let autorun = List.mem "autorun" str_classes in
  let merlin_extensions =
    match ty with
    | OCaml | Deferred ->
        Merlin_codemirror.[ autocomplete rpc; linter rpc; tooltip_on_hover rpc ]
    | Toplevel -> []
    | Other -> []
  in
  let theme =
    Jv.get Jv.global "__CM__theme_dracula" |> Code_mirror.Extension.of_jv
  in
  let el = El.(div ~at:[ At.class' (Jstr.v "editor") ] []) in
  let ostate_extension =
    Code_mirror.State.StateField.init ostate (fun _ ->
        {
          id = "id_" ^ string_of_int (id + 1);
          output_id;
          rpc;
          solution;
          ty;
          mime_only;
          hidden;
        })
  in
  if hidden then
    El.set_at (Jstr.v "style") (Some (Jstr.v "display: none;")) (parent_exn parent);
  let autorun_extension =
    Code_mirror.State.StateField.init autorun_state_field (fun _ -> autorun)
  in
  let editor, view =
    init ~doc
      ~exts:(autorun_extension :: ostate_extension :: ocaml :: theme :: merlin_extensions)
      ()
  in
  El.append_children el [ Code_mirror.View.EditorView.dom view ];
  El.insert_siblings `After parent [ el ];
  El.remove parent;
  Some (editor, view)

let get_sections () =
  El.fold_find_by_selector
    (fun x y -> x :: y)
    (Jstr.v "header.odoc-preamble")
    []
  @ El.fold_find_by_selector
      (fun x y -> x :: y)
      (Jstr.v "div.odoc-content > section")
      []

let show_section n =
  let sections = get_sections () in
  let set_display el display =
    let style =
      Printf.sprintf "display: %s;" (if display then "block" else "none")
    in
    El.set_at (Jstr.v "style") (Some (Jstr.v style)) el
  in
  List.iteri (fun i s -> set_display s (i = n)) sections

let nav_setup () =
  let nav_elts =
    El.fold_find_by_selector (fun x y -> x :: y) (Jstr.v "nav > ul > li") []
  in
  List.iteri
    (fun i el ->
      let _ =
        Brr.Ev.listen Ev.click (fun _ -> show_section (i + 1)) (El.as_target el)
      in
      ())
    nav_elts

let init_page requires =
  let open Fut.Result_syntax in
  let* rpc =
    initialise requires "/assets/worker.js" (fun _ ->
        Console.(log [ str "Timeout" ]))
  in
  Console.log [ Jv.of_string "Initialised" ];
  let* _o = W.setup rpc () in
  let ocaml_elts =
    El.fold_find_by_selector
      (fun x y -> x :: y)
      (Jstr.v "pre code")
      []
    |> List.rev
  in
  let _editors = List.mapi (make_editor rpc) ocaml_elts in

  (* List.iteri (fun n x -> exec_js (Printf.sprintf "id_%d" (n + 1)) x) elts; *)
  (* let sections = get_sections () in
  let n = List.length sections in
  List.iteri
    (fun i s ->
      El.set_at (Jstr.v "style") (Some (Jstr.v "display: none;")) s;
      let prev = El.(button [ txt (Jstr.v "Prev") ]) in
      let next = El.(button [ txt (Jstr.v "Next") ]) in
      let _ =
        Brr.Ev.listen Ev.click
          (fun _ ->
            let next = (i + 1) mod n in
            show_section next)
          (El.as_target prev)
      in
      let _ =
        Brr.Ev.listen Ev.click
          (fun _ ->
            let prev = (i - 1 + n) mod n in
            show_section prev)
          (El.as_target next)
      in
      El.append_children s [ prev; next ])
    sections;
  show_section 0; *)
  (* nav_setup (); *)
  Fut.return (Ok ())

open Fut.Syntax

let main _requires =
  let meta_elts =
    El.fold_find_by_selector
        (fun elt y ->
          let parent_exn elt =
            match El.parent elt with Some e -> e | None -> failwith "no parent"
          in
          let parent = parent_exn elt in
          let classes_iter =
            Jv.call (Jv.get (Jv.repr parent) "classList") "values" [||]
          in
          let str_classes =
            Jv.It.fold Jv.to_string (fun x y -> x :: y) classes_iter []
          in
          if List.mem "language-meta" str_classes then
            let meta =
              let doc = El.txt_text (El.children elt |> List.hd) |> Jstr.to_string in
              let y = Yojson.Safe.from_string doc in
              [y]
            in
            meta
          else 
            y)
      (Jstr.v "pre code")
      []
    |> List.rev
  in
  let meta = (match meta_elts with
  | [] -> Console.log [ Jv.of_string "No meta elements found" ]; None
  | [x] ->
    (match meta_of_yojson x with Ok m -> Some m | _ -> None)

  | _ -> None) in
  let libs = match meta with | Some m -> m.libs | None -> [] in

  Console.(log [ str "DOM content loaded." ]);
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in
  Console.(log [ str "Resources loaded." ]);
  ignore (init_page libs);

  Fut.return ()

(* let _ =
  let _ = Console.debug [ Jv.of_string "global cmis = " ] in
  let _ = Console.debug [ Jv.get Jv.global "cmis" ] in
  main (Jv.to_string (Jv.get Jv.global "cmis")) *)
