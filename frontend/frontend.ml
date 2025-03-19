open Brr
open Js_top_worker_rpc
module W = Js_top_worker_client_fut.W

let _ = Toploop.getvalue
let ocaml = Jv.get Jv.global "__CM__mllike" |> Stream.Language.of_jv
let ocaml = Stream.Language.define ocaml
let _ = Console.debug [ Jv.of_string "Starting" ]

let initialise cmis s callback =
  let open Fut.Result_syntax in
  let rpc = Js_top_worker_client_fut.start s 100000 callback in
  let* () =
    W.init rpc { Toplevel_api_gen.path = "/static/cmis"; cmas = []; cmis }
  in
  Fut.return (Ok rpc)

let styled_text tstyle text =
  let lines = Astring.String.cuts ~sep:"\n" text in
  let non_empty = List.filter (fun x -> x <> "") lines in
  List.map
    (fun l -> El.(span ~at:[ At.style (Jstr.v tstyle) ] [ txt' (l ^ "\n") ]))
    non_empty

let append_to_output id output _mime_only =
  let el = Brr.Document.find_el_by_id Brr.G.document (Jstr.v id) in
  match el with
  | None -> ()
  | Some el ->
      let stdout =
        List.map Option.to_list
          Toplevel_api_gen.[ output.stdout; output.caml_ppf ]
        |> List.concat |> String.concat "\n" |> styled_text "color:black"
        |> El.pre
      in
      let mime =
        List.map
          (fun mime ->
            let l =
              Astring.String.cuts ~sep:"/" mime.Toplevel_api_gen.mime_type
            in
            match (l, mime.encoding) with
            | [ "image"; _ty ], Mime_printer.Base64 ->
                let src = "data:" ^ mime.mime_type ^ ";base64," ^ mime.data in
                [ El.img ~at:[ At.src (Jstr.of_string src) ] () ]
            | [ "text"; "html" ], Noencoding ->
                let div = El.div [] in
                Jv.set (El.to_jv div) "innerHTML" (Jv.of_string mime.data);
                [ div ]
            | _ -> [])
          output.mime_vals
        |> List.flatten
      in
      El.set_children el ([ stdout ] @ mime)

let basic_setup =
  Jv.get Jv.global "__CM__basic_setup" |> Code_mirror.Extension.of_jv

type state = {
  id : string;
  output_id : string;
  solution : string option;
  is_deferred : bool;
  mime_only : bool;
  rpc : Js_top_worker_client_fut.rpc;
}

let to_jv : state -> Jv.t =
 fun x ->
  let res =
    Jv.obj
      [|
        ("id", Jv.of_string x.id);
        ("rpc", Jv.repr x.rpc);
        ("output_id", Jv.of_string x.output_id);
        ("is_deferred", Jv.of_bool x.is_deferred);
        ("mime_only", Jv.of_bool x.mime_only);
      |]
  in
  Jv.set_if_some res "solution" (Option.map Jv.of_string x.solution);
  res

let of_jv : Jv.t -> state =
 fun jv ->
  let id = Jv.get jv "id" |> Jv.to_string in
  let output_id = Jv.get jv "output_id" |> Jv.to_string in
  let (rpc : Js_top_worker_client_fut.rpc) = Jv.get jv "rpc" |> Obj.magic in
  let solution = Jv.find jv "solution" |> Option.map Jv.to_string in
  let is_deferred = Jv.get jv "is_deferred" |> Jv.to_bool in
  let mime_only = Jv.get jv "mime_only" |> Jv.to_bool in
  { id; output_id; rpc; solution; is_deferred; mime_only }

let effect = Code_mirror.State.StateEffect.define to_jv of_jv

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

let panel_constructor (st : state) (v : Code_mirror.View.EditorView.t) =
  let run = Brr.El.(button [ txt (Jstr.v "Run") ]) in
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

  let handle_click code =
    (* let code =
      {|module Odoc_notebook_cell_|} ^ st.id ^ {| = struct |} ^ code
      ^ {| end;; |}
    in *)
    if st.is_deferred then
      Fut.bind (W.compile_js st.rpc None code) (function
        | Ok res ->
            Console.(log [ str ("JS received: " ^ res) ]);
            let _ = Js_of_ocaml.Js.Unsafe.eval_string res in
            (* exec_js st.id (); *)
            Fut.return (Ok ())
        | Error _e ->
            Console.(log [ str "error" ]);
            Fut.return (Ok ()))
    else
      Fut.bind (W.exec st.rpc code) (fun res ->
          match res with
          | Ok res ->
              append_to_output st.output_id res st.mime_only;
              Console.log [ Option.get (Option.map Jv.of_string res.stdout) ];
              Fut.return (Ok ())
          | Error _e ->
              Console.log [ Jv.of_string "error" ];
              Fut.return (Error ()))
  in

  ignore
    (Brr.Ev.listen Ev.click
       (fun _ ->
         let code =
           Code_mirror.(
             View.EditorView.state v |> State.EditorState.doc
             |> State.Text.to_string)
         in
         ignore @@ handle_click code)
       (El.as_target run));
  let id = Brr.El.span [ Brr.El.txt' st.id ] in
  let dom = Brr.El.(div (run :: id :: show_solution)) in
  Code_mirror.View.Panel.create ~top:true dom

let provide field =
  Code_mirror.State.Facet.from' Code_mirror.View.showPanel field (fun st ->
      Some (panel_constructor st))

let update cur t =
  let effects = Code_mirror.State.Transaction.effects t in
  List.fold_right
    (fun e cur ->
      match Code_mirror.State.StateEffect.value e effect with
      | Some b -> b
      | _ -> cur)
    effects cur

let state =
  Code_mirror.State.StateField.define to_jv of_jv
    ~create:(fun _ -> failwith "unset")
    ~provide ~update

let init ?doc ?(exts = []) parent =
  let open Code_mirror in
  (* let doc = Option.map Jv.of_string doc in *)
  let config =
    State.EditorStateConfig.create ?doc ~extensions:(basic_setup :: exts) ()
  in
  let state = State.EditorState.create ~config () in
  let config = View.EditorViewConfig.create ~state ~parent () in
  let view : View.EditorView.t = View.EditorView.create ~config () in
  (state, view)

let make_editor rpc id elt =
  let parent_exn elt =
    match El.parent elt with Some e -> e | None -> failwith "no parent"
  in
  let parent = parent_exn elt in
  let output_id = "output-" ^ string_of_int id in
  let output = El.(div ~at:[ At.(id (Jstr.v output_id)) ] []) in
  let grandparent = parent_exn parent in
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
  let is_deferred = List.mem "deferred-js" str_classes in
  let noshow = List.mem "noshow" str_classes in
  let _autorun = List.mem "autorun" str_classes in
  let merlin_extensions =
    Merlin_codemirror.[ autocomplete rpc; linter rpc; tooltip_on_hover rpc ]
  in

  if noshow then (
    El.set_at (Jstr.v "style") (Some (Jstr.v "display: none;")) parent;
    El.append_children grandparent [ output ];
    ())
  else
    let state_extension =
      Code_mirror.State.StateField.init state (fun _ ->
          {
            id = "id_" ^ string_of_int (id + 1);
            output_id;
            rpc;
            solution;
            is_deferred;
            mime_only;
          })
    in
    let _, _ =
      init ~doc
        ~exts:(state_extension :: ocaml :: merlin_extensions)
        grandparent
    in
    El.remove parent;
    El.append_children grandparent [ output ];
    ()

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

let init_page cmis =
  let open Fut.Result_syntax in
  let* rpc =
    initialise cmis "/assets/worker.js" (fun _ -> Console.(log [ str "Timeout" ]))
  in
  Console.log [ Jv.of_string "Initialised" ];
  let* _o = W.setup rpc () in
  let elts =
    El.fold_find_by_selector
      (fun x y -> x :: y)
      (Jstr.v "pre.language-ocaml code")
      []
    |> List.rev
  in
  (* ignore make_editor; *)
  List.iteri (make_editor rpc) elts;
  (* List.iteri (fun n x -> exec_js (Printf.sprintf "id_%d" (n + 1)) x) elts; *)
  let sections = get_sections () in
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
  show_section 0;
  nav_setup ();
  Fut.return (Ok ())

open Fut.Syntax

let main cmis =
  Console.(log [ str "DOM content loaded." ]);
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in
  Console.(log [ str "Resources loaded." ]);
  ignore (init_page cmis);

  Fut.return ()

(* let _ =
  let _ = Console.debug [ Jv.of_string "global cmis = " ] in
  let _ = Console.debug [ Jv.get Jv.global "cmis" ] in
  main (Jv.to_string (Jv.get Jv.global "cmis")) *)
