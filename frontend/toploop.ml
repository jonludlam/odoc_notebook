let toplevel_value_bindings = ref []

let getvalue name =
  try List.assoc name !toplevel_value_bindings
  with Not_found -> failwith (name ^ " unbound at toplevel")

let setvalue name v =
  toplevel_value_bindings := (name, v) :: !toplevel_value_bindings
