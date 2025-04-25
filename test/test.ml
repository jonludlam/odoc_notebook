let _ =
  let document = Html.window##.document in
  document##getElementById (Js.string "hello")
