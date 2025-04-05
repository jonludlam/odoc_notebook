let _ =
  let open Js_top_worker_rpc.Toplevel_api_gen in
  Printf.printf "testing\n%!";
  let cmis = { dynamic_cmis = []; static_cmis = [] } in
  Frontend.init_page cmis
