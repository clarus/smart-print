open Document

let main () =
  let d = !^ "hello" ^^ !^ "world" ^^ new_line ^^ nest 2 (!^ "gre" ^^ nest_all 2 (!^ "arg" ^^ !^ "arg")) in
  print_endline (to_string d);
  print_endline (String.make 20 '*');
  Example.es |> List.iter (fun e ->
    print_endline @@ to_string @@ Example.pp false e)

;;main ()
