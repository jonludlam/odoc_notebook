(* Main *)

open Brr

let _ =
  let _elts =
    El.fold_find_by_selector (fun x y -> x :: y) (Jstr.v "nav > ul > li") []
  in
  Console.log [ Jv.of_int (List.length _elts) ]
