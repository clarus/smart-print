open Document

let main () =
  let d = !^ "hello" ^^ !^ "world" ^^ new_line ^^ nest 2 (!^ "gre") in
  print_endline (to_string d)

;;main ()
