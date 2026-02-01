type engine =
  | Eval
  | Vm

type config = {
  engine : engine;
  benchmark : bool;
  filename : string option;
}

let parse_args () : config =
  let engine = ref Eval in
  let benchmark = ref false in
  let filename = ref None in

  let args = Array.to_list Sys.argv |> List.tl in

  let rec parse = function
    | [] -> ()
    | "--engine=eval" :: rest ->
        engine := Eval;
        parse rest
    | "--engine=vm" :: rest ->
        engine := Vm;
        parse rest
    | "--benchmark" :: rest ->
        benchmark := true;
        parse rest
    | arg :: rest when String.length arg > 0 && arg.[0] <> '-' ->
        filename := Some arg;
        parse rest
    | arg :: _ ->
        Printf.eprintf "Unknown argument: %s\n" arg;
        Printf.eprintf "Usage: marmoset [--engine=eval|vm] [--benchmark] [filename]\n";
        exit 1
  in
  parse args;
  { engine = !engine; benchmark = !benchmark; filename = !filename }

(* Run program with tree-walking interpreter *)
let run_eval program =
  let env = Marmoset.Env.init () in
  let value, _ = Marmoset.Eval.eval program env in
  value

(* Run program with bytecode compiler + VM *)
let run_vm program =
  let compiler = Marmoset.Compiler.init () in
  match Marmoset.Compiler.compile compiler program with
  | Error msg -> Marmoset.Value.Error msg
  | Ok compiler' -> (
      let bytecode = Marmoset.Compiler.bytecode compiler' in
      let vm = Marmoset.Vm.create bytecode in
      match Marmoset.Vm.run vm with
      | Error msg -> Marmoset.Value.Error msg
      | Ok () -> Marmoset.Vm.last_popped_stack_elem vm)

(* Time a function and return (result, duration_in_seconds) *)
let time_it f =
  let start = Sys.time () in
  let result = f () in
  let stop = Sys.time () in
  (result, stop -. start)

let run_repl config =
  let engine_name =
    match config.engine with
    | Eval -> "eval"
    | Vm -> "vm"
  in

  (* For VM REPL, we need to maintain state across inputs *)
  (* For now, each line is independent (no persistent globals in VM mode) *)
  let rec loop_eval env =
    Printf.printf "(marmoset:%s) >> " engine_name;
    flush stdout;

    let input = input_line stdin in

    if input = "exit" then
      print_endline "Goodbye!"
    else
      match Marmoset.Parser.parse input with
      | Error msgs ->
          List.iter (fun msg -> print_endline msg) msgs;
          loop_eval env
      | Ok program ->
          let value, env' = Marmoset.Eval.eval program env in
          let str = Marmoset.Value.to_string value in
          print_endline ("=> " ^ str);
          loop_eval env'
  in

  let rec loop_vm () =
    Printf.printf "(marmoset:%s) >> " engine_name;
    flush stdout;

    let input = input_line stdin in

    if input = "exit" then
      print_endline "Goodbye!"
    else
      match Marmoset.Parser.parse input with
      | Error msgs ->
          List.iter (fun msg -> print_endline msg) msgs;
          loop_vm ()
      | Ok program ->
          let value = run_vm program in
          let str = Marmoset.Value.to_string value in
          print_endline ("=> " ^ str);
          loop_vm ()
  in

  print_endline "Welcome to the REPL of Marmoset (Monkey) programming language, written in OCaml!";
  Printf.printf "Engine: %s | Enter 'exit' to quit.\n" engine_name;

  match config.engine with
  | Eval -> loop_eval (Marmoset.Env.init ())
  | Vm -> loop_vm ()

let run_file config filename =
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

  let input = read_file filename in
  match Marmoset.Parser.parse input with
  | Error msgs -> List.iter (fun msg -> print_endline msg) msgs
  | Ok program -> (
      let run_fn =
        match config.engine with
        | Eval -> fun () -> run_eval program
        | Vm -> fun () -> run_vm program
      in

      if config.benchmark then
        let engine_name =
          match config.engine with
          | Eval -> "eval"
          | Vm -> "vm"
        in
        let value, duration = time_it run_fn in
        match value with
        | Marmoset.Value.Error msg -> Printf.printf "ERROR: %s\n" msg
        | _ ->
            Printf.printf "engine=%s, result=%s, duration=%.4fs\n" engine_name (Marmoset.Value.to_string value)
              duration
      else
        match run_fn () with
        | Marmoset.Value.Error msg -> print_endline ("ERROR: " ^ msg)
        | _ -> ())

let () =
  let config = parse_args () in
  match config.filename with
  | None -> run_repl config
  | Some filename -> run_file config filename
