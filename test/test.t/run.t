  $ odoc_notebook test test_page.mld 2> /dev/null
  {0 Test of test}
  
  {@ocamltop[
  # Printf.printf "Hello, world\n%!";;
    Hello, world
    - : unit = ()
  ]}
  
  $ odoc_notebook test test_page2.mld 2> /dev/null
  {0 Test 2 - libraries}
  
  @libs astring
  
  {@ocamltop[
  # Astring.String.fields "this is a test";;
    - : string list = ["this"; "is"; "a"; "test"]
  ]}
  
 
  $ odoc_notebook test test_page3.mld 2> /dev/null
  {0 No libs, should error}
  
  {@ocamltop[
  # Astring.String.fields "this is a test";;
    Line 1, characters 0-21:
    Error: Unbound module "Astring"
    Hint: Did you mean "String"?
  ]}
  

