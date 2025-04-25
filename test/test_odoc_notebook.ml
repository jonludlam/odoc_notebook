let _ =
  let open Js_top_worker_rpc.Toplevel_api_gen in
  Printf.printf "testing\n%!";
  Frontend.init_page []
