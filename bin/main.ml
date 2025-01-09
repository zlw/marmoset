let rec repl env =
  Printf.printf "(marmoset:repl) >> ";
  flush stdout;

  let input = input_line stdin in

  if input = "exit" then
    print_endline "Goodbye!"
  else
    match Marmoset.Parser.parse input with
    | Error msgs ->
        List.iter (fun msg -> print_endline msg) msgs;
        repl env
    | Ok program ->
        let value, env' = Marmoset.Eval.eval program env in
        let str = Marmoset.Value.to_string value in
        print_endline ("=> " ^ str);
        repl env'

let () =
  print_endline "Welcome to the REPL of Marmoset (Monkey) programming language, written in OCaml! 🐫";
  print_endline "Enter 'exit' to quit the REPL. 🙈 🙉 🙊";

  repl (Marmoset.Env.init ())
