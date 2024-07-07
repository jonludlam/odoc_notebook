let _ =
  let open Js_top_worker_rpc.Toplevel_api_gen in
  let cmis = {
    dynamic_cmis = [];
    static_cmis = [];
  }
  in
  Frontend.Main.init_page cmis
