open Document

let print_document (d : Document.t) : unit =
  let width = 25 in
  let a = Atom.render width @@ flatten d in
  print_endline (String.make width '*');
  print_endline @@ Atom.to_string @@ Atom.render 160 @@ flatten @@ Debug.pp a;
  print_endline (String.make width '*');
  print_endline @@ Atom.to_string a

let main () =
  let d = !^ "hello" ^^ !^ "world" ^^ new_line ^^ nest 2 (!^ "gre" ^^ nest_all 2 (!^ "arg" ^^ !^ "arg")) in
  print_document d;
  Example.es |> List.iter (fun e ->
    print_document @@ Example.pp false e)

;;main ()
