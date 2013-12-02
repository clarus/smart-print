open Document

let main () =
  let d = !^ "hello" ^^ !^ "world" ^^ new_line ^^ nest 2 (!^ "gre" ^^ nest_all 2 (!^ "arg" ^^ !^ "arg")) in
  print_string (to_string d)

;;main ()
