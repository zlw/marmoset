let rec repl env =
  Printf.printf ">> ";
  flush stdout;

  let input = input_line stdin in

  if input = "exit" then
    print_endline "Goodbye!"
  else
    let _, program = Marmoset.Lexer.init input |> Marmoset.Parser.init |> Marmoset.Parser.parse_program in
    let value, env' = Marmoset.Eval.eval program env in
    let str = Marmoset.Value.to_string value in

    print_endline str;
    repl env'

let () =
  print_endline "Welcome to the REPL of Marmoset (Monkey) programming language, written in OCaml! 🐵🐫";

  repl (Marmoset.Env.init ())
