open Js_top_worker_rpc
module M = Idl.IdM (* Server is synchronous *)
module IdlM = Idl.Make (M)
module Client = Toplevel_api_gen.Make (IdlM.GenClient ())
module Cmds = Toplevel_api_gen.Make (Cmdlinergen.Gen ())

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc path (call : Rpc.call) : Rpc.response =
  let sockaddr = Unix.ADDR_UNIX path in
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect s sockaddr;
  let ic = Unix.in_channel_of_descr s in
  let oc = Unix.out_channel_of_descr s in
  let msg_buf = Jsonrpc.string_of_call call in
  let len = Printf.sprintf "%016d" (String.length msg_buf) in
  output_string oc len;
  output_string oc msg_buf;
  flush oc;
  let len_buf = Bytes.make 16 '\000' in
  really_input ic len_buf 0 16;
  let len = int_of_string (Bytes.unsafe_to_string len_buf) in
  let msg_buf = Bytes.make len '\000' in
  really_input ic msg_buf 0 len;
  let (response : Rpc.response) =
    Jsonrpc.response_of_string (Bytes.unsafe_to_string msg_buf)
  in
  response
(*
   let server_cmd =
   let doc = "Start the server" in
   Cmdliner.(Cmd.v
    (Cmd.info "server" ~doc )
    (Term.(const Example2_server.start_server $ const ())))
*)

let cli () =
  let default =
    Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ()))
  in
  let info = Cmdliner.Cmd.info "cli" ~version:"1.6.1" ~doc:"a cli for an API" in
  let rpc = binary_rpc Toplevel_api_gen.sockpath in
  let cmds =
    (* server_cmd ::  *)
    List.map
      (fun t ->
        let term, info = t rpc in
        Cmdliner.(Cmd.v info Term.(term $ const ())))
      (Cmds.implementation ())
  in
  let cmd = Cmdliner.Cmd.group ~default info cmds in
  exit (Cmdliner.Cmd.eval cmd)

let () = cli ()
