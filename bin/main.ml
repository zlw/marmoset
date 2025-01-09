let run_repl () =
  let rec loop env =
    Printf.printf "(marmoset:repl) >> ";
    flush stdout;

    let input = input_line stdin in

    if input = "exit" then
      print_endline "Goodbye!"
    else
      match Marmoset.Parser.parse input with
      | Error msgs ->
          List.iter (fun msg -> print_endline msg) msgs;
          loop env
      | Ok program ->
          let value, env' = Marmoset.Eval.eval program env in
          let str = Marmoset.Value.to_string value in
          print_endline ("=> " ^ str);
          loop env'
  in
  print_endline "Welcome to the REPL of Marmoset (Monkey) programming language, written in OCaml! 🐫";
  print_endline "Enter 'exit' to quit the REPL. 🙈 🙉 🙊";

  loop (Marmoset.Env.init ())

let run_file () =
  let read_file filename =
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file ->
        close_in ic;
        String.concat "\n" (List.rev acc)
    in
    read_lines []
  in

  let filename = Sys.argv.(1) in
  let input = read_file filename in
  match Marmoset.Parser.parse input with
  | Error msgs -> List.iter (fun msg -> print_endline msg) msgs
  | Ok program ->
      let env = Marmoset.Env.init () in
      Marmoset.Eval.eval program env |> ignore

let () =
  let arg_len = Array.length Sys.argv in

  if arg_len - 1 = 0 then
    run_repl ()
  else if arg_len - 1 = 1 then
    run_file ()
  else
    print_endline "Usage: marmoset [filename]"
