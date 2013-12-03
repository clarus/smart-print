open Document

let print_document (d : Document.t) : unit =
  let width = 25 in
  to_stdout 160 @@ Debug.pp_document d;
  print_newline ();
  print_endline @@ String.make width '*';
  print_endline @@ to_string width d;
  print_endline @@ String.make width '*'

let main () =
  let d = !^ "hello" ^^ !^ "world" ^^ newline ^^ nest 2 (!^ "gre" ^^ nest_all 2 (!^ "arg" ^^ !^ "arg")) in
  print_document d;
  Example.es |> List.iter (fun e ->
    print_document @@ Example.pp false e)

;;main ()
