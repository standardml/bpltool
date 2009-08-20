(* file: main.ml *)
(* Assumes the parser file is "SICparser.mly" and the lexer file is "SIClex.mll". *)
let main () =
  let lexbuf = Lexing.from_channel (open_in "SICtest.txt") in
  SICparser.input SIClex.token lexbuf;;

let _ = Printexc.print main();;

