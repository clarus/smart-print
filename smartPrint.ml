open Document

let main () =
  let d = !^ "hello" ^^ !^ "world" in
  print_endline (to_string d)

;;main ()
