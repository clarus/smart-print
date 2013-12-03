open Document

let print_document (d : Document.t) : unit =
  let width = 25 in
  to_stdout 160 @@ Debug.pp_document_after_rendering width d;
  print_newline ();
  print_endline @@ String.make width '*';
  print_endline @@ to_string width d;
  print_endline @@ String.make width '*'

let main () =
  print_document (!^ "hello" ^^ !^ "world" ^^ newline ^^ nest 2 (!^ "gre" ^^ nest_all 2 (!^ "arg" ^^ !^ "arg")));
  Example.es |> List.iter (fun e ->
    print_document @@ Example.pp false e);
  print_document (sub_string "hfgs_kjl_df" 5 3);
  print_document (words "Lorem     ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.");
  print_document (lines "adipisicing elit,\nsed do eiusmod tempor\nincididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud ")

;;main ()
