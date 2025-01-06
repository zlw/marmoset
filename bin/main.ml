let rec repl () =
  Printf.printf ">> ";
  flush stdout;

  let input = input_line stdin in

  if input = "exit" then
    print_endline "Goodbye!"
  else
    Marmoset.Lexer.lex input
    |> List.iter (fun t -> Printf.printf "%s\n" (Marmoset.Token.show_token t));

  repl ()

let () =
  print_endline
    "Welcome to the REPL of Marmoset (Monkey) programming language, written in OCaml! 🐵🐫";
  repl ()
