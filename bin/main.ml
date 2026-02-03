type engine =
  | Eval
  | Vm

type command =
  | Repl of { engine : engine }
  | Run of {
      engine : engine;
      benchmark : bool;
      filename : string;
    }
  | Build of {
      input : string;
      output : string option;
      emit_go : string option;
    }

(* Type check a program, returning Ok type_string or Error with message *)
let typecheck source program =
  let env = Marmoset.Lib.Builtins.prelude_env () in
  match Marmoset.Lib.Checker.check_program_with_annotations ~env program with
  | Ok { result_type; _ } -> Ok (Marmoset.Lib.Types.to_string_pretty result_type)
  | Error err -> Error (Marmoset.Lib.Checker.format_error_with_context source err)

let parse_args () : command =
  let args = Array.to_list Sys.argv |> List.tl in

  match args with
  (* marmoset build <input> [-o output] [--emit-go dir] *)
  | "build" :: rest -> (
      let input = ref None in
      let output = ref None in
      let emit_go = ref None in

      let rec parse = function
        | [] -> ()
        | "-o" :: out :: rest ->
            output := Some out;
            parse rest
        | "--emit-go" :: dir :: rest ->
            emit_go := Some dir;
            parse rest
        | arg :: rest when String.length arg > 0 && arg.[0] <> '-' ->
            input := Some arg;
            parse rest
        | arg :: _ ->
            Printf.eprintf "Unknown build argument: %s\n" arg;
            Printf.eprintf "Usage: marmoset build <input.mr> [-o output] [--emit-go dir]\n";
            exit 1
      in
      parse rest;

      match !input with
      | None ->
          Printf.eprintf "Usage: marmoset build <input.mr> [-o output] [--emit-go dir]\n";
          exit 1
      | Some input -> Build { input; output = !output; emit_go = !emit_go })
  (* marmoset [run] [options] [filename] *)
  | _ -> (
      let engine = ref Eval in
      let benchmark = ref false in
      let filename = ref None in

      let rec parse = function
        | [] -> ()
        | "run" :: rest -> parse rest (* 'run' is optional *)
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
            Printf.eprintf "Usage: marmoset [run] [--engine=eval|vm] [--benchmark] [filename]\n";
            Printf.eprintf "       marmoset build <input.mr> [-o output] [--emit-go dir]\n";
            exit 1
      in
      parse args;

      match !filename with
      | None -> Repl { engine = !engine }
      | Some filename -> Run { engine = !engine; benchmark = !benchmark; filename })

(* Run program with tree-walking interpreter *)
let run_eval program =
  let env = Marmoset.Lib.Env.init () in
  let value, _ = Marmoset.Lib.Eval.eval program env in
  value

(* Run program with bytecode compiler + VM *)
let run_vm program =
  let compiler = Marmoset.Lib.Compiler.init () in
  match Marmoset.Lib.Compiler.compile compiler program with
  | Error msg -> Marmoset.Lib.Value.Error msg
  | Ok compiler' -> (
      let bytecode = Marmoset.Lib.Compiler.bytecode compiler' in
      let vm = Marmoset.Lib.Machine.create bytecode in
      match Marmoset.Lib.Machine.run vm with
      | Error msg -> Marmoset.Lib.Value.Error msg
      | Ok () -> Marmoset.Lib.Machine.last_popped_stack_elem vm)

(* Time a function and return (result, duration_in_seconds) *)
let time_it f =
  let start = Sys.time () in
  let result = f () in
  let stop = Sys.time () in
  (result, stop -. start)

let run_repl engine =
  let engine_name =
    match engine with
    | Eval -> "eval"
    | Vm -> "vm"
  in

  let rec loop_eval env =
    Printf.printf "(marmoset:%s) >> " engine_name;
    flush stdout;

    let input = input_line stdin in

    if input = "exit" then
      print_endline "Goodbye!"
    else
      match Marmoset.Lib.Parser.parse input with
      | Error msgs ->
          List.iter (fun msg -> print_endline msg) msgs;
          loop_eval env
      | Ok program -> (
          match typecheck input program with
          | Error msg ->
              Printf.printf "Type error: %s\n" msg;
              loop_eval env
          | Ok type_str ->
              let value, env' = Marmoset.Lib.Eval.eval program env in
              let str = Marmoset.Lib.Value.to_string value in
              print_endline ("=> " ^ str ^ " : " ^ type_str);
              loop_eval env')
  in

  let rec loop_vm () =
    Printf.printf "(marmoset:%s) >> " engine_name;
    flush stdout;

    let input = input_line stdin in

    if input = "exit" then
      print_endline "Goodbye!"
    else
      match Marmoset.Lib.Parser.parse input with
      | Error msgs ->
          List.iter (fun msg -> print_endline msg) msgs;
          loop_vm ()
      | Ok program -> (
          match typecheck input program with
          | Error msg ->
              Printf.printf "Type error: %s\n" msg;
              loop_vm ()
          | Ok type_str ->
              let value = run_vm program in
              let str = Marmoset.Lib.Value.to_string value in
              print_endline ("=> " ^ str ^ " : " ^ type_str);
              loop_vm ())
  in

  print_endline "Welcome to the REPL of Marmoset (Monkey) programming language, written in OCaml!";
  Printf.printf "Engine: %s | Enter 'exit' to quit.\n" engine_name;

  match engine with
  | Eval -> loop_eval (Marmoset.Lib.Env.init ())
  | Vm -> loop_vm ()

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

let run_file engine benchmark filename =
  let input = read_file filename in
  match Marmoset.Lib.Parser.parse input with
  | Error msgs -> List.iter (fun msg -> print_endline msg) msgs
  | Ok program -> (
      match typecheck input program with
      | Error msg ->
          Printf.eprintf "Type error: %s\n" msg;
          exit 1
      | Ok _ -> (
          let run_fn =
            match engine with
            | Eval -> fun () -> run_eval program
            | Vm -> fun () -> run_vm program
          in

          if benchmark then
            let engine_name =
              match engine with
              | Eval -> "eval"
              | Vm -> "vm"
            in
            let value, duration = time_it run_fn in
            match value with
            | Marmoset.Lib.Value.Error msg -> Printf.printf "ERROR: %s\n" msg
            | _ ->
                Printf.printf "engine=%s, result=%s, duration=%.4fs\n" engine_name
                  (Marmoset.Lib.Value.to_string value) duration
          else
            match run_fn () with
            | Marmoset.Lib.Value.Error msg -> print_endline ("ERROR: " ^ msg)
            | _ -> ()))

(* Build command - compile Marmoset to Go binary *)
let run_build input output_opt emit_go_opt =
  (* Read source file *)
  let source = read_file input in

  (* Compile to Go *)
  match Marmoset.Lib.Go_emitter.compile_to_build source with
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Ok build_output ->
      (* Determine output name *)
      let output =
        match output_opt with
        | Some o -> o
        | None ->
            (* Remove .mr extension if present, default to input name *)
            let base = Filename.remove_extension input in
            Filename.basename base
      in

      (* Create temp directory for Go files in current working directory *)
      let temp_dir = ".marmoset-build" in
      ignore (Sys.command ("mkdir -p " ^ temp_dir));

      (* Write Go files *)
      let main_go_path = Filename.concat temp_dir "main.go" in
      let runtime_go_path = Filename.concat temp_dir "runtime.go" in

      let write_file path content =
        let oc = open_out path in
        output_string oc content;
        close_out oc
      in

      write_file main_go_path build_output.main_go;
      write_file runtime_go_path build_output.runtime_go;

      (* If --emit-go, copy files to that directory *)
      (match emit_go_opt with
      | Some dir ->
          ignore (Sys.command ("mkdir -p " ^ dir));
          write_file (Filename.concat dir "main.go") build_output.main_go;
          write_file (Filename.concat dir "runtime.go") build_output.runtime_go;
          Printf.printf "Go source written to %s/\n" dir
      | None -> ());

      (* Initialize Go module *)
      let go_mod_path = Filename.concat temp_dir "go.mod" in
      write_file go_mod_path "module marmoset_out\n\ngo 1.18\n";

      (* Get absolute path for output *)
      let output_abs =
        if Filename.is_relative output then
          Filename.concat (Sys.getcwd ()) output
        else
          output
      in

      (* Run go build from the temp directory *)
      let cmd = Printf.sprintf "cd %s && go build -o %s ." temp_dir output_abs in
      let exit_code = Sys.command cmd in

      (* Clean up temp directory *)
      (try
         Sys.remove main_go_path;
         Sys.remove runtime_go_path;
         Sys.remove go_mod_path;
         ignore (Sys.command ("rmdir " ^ temp_dir))
       with _ -> ());

      if exit_code <> 0 then (
        Printf.eprintf "Go build failed\n";
        exit 1)
      else
        Printf.printf "Built: %s\n" output

let () =
  match parse_args () with
  | Repl { engine } -> run_repl engine
  | Run { engine; benchmark; filename } -> run_file engine benchmark filename
  | Build { input; output; emit_go } -> run_build input output emit_go
