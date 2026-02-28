type command =
  | Run of {
      benchmark : bool;
      filename : string;
    }
  | Build of {
      input : string;
      output : string option;
      emit_go : string option;
      release : bool;
    }
  | Check of { filename : string }
  | Lsp

let print_usage () =
  Printf.eprintf "Usage:\n";
  Printf.eprintf "  marmoset run [--benchmark] <input.mr>\n";
  Printf.eprintf "  marmoset build [--release] <input.mr> [-o output] [-go dir]\n";
  Printf.eprintf "  marmoset check <input.mr>\n";
  Printf.eprintf "  marmoset lsp\n"

let parse_args () : command =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "build" :: rest -> (
      let input = ref None in
      let output = ref None in
      let emit_go = ref None in
      let release = ref false in
      let rec parse = function
        | [] -> ()
        | "-o" :: out :: tail ->
            output := Some out;
            parse tail
        | ("-go" | "--emit-go") :: dir :: tail ->
            emit_go := Some dir;
            parse tail
        | "--release" :: tail ->
            release := true;
            parse tail
        | arg :: tail when String.length arg > 0 && arg.[0] <> '-' ->
            input := Some arg;
            parse tail
        | _ ->
            print_usage ();
            exit 1
      in
      parse rest;
      match !input with
      | Some input -> Build { input; output = !output; emit_go = !emit_go; release = !release }
      | None ->
          print_usage ();
          exit 1)
  | "run" :: rest -> (
      let benchmark = ref false in
      let input = ref None in
      let rec parse = function
        | [] -> ()
        | "--benchmark" :: tail ->
            benchmark := true;
            parse tail
        | arg :: tail when String.length arg > 0 && arg.[0] <> '-' ->
            input := Some arg;
            parse tail
        | _ ->
            print_usage ();
            exit 1
      in
      parse rest;
      match !input with
      | Some filename -> Run { benchmark = !benchmark; filename }
      | None ->
          print_usage ();
          exit 1)
  | "check" :: rest -> (
      let input = ref None in
      let rec parse = function
        | [] -> ()
        | arg :: tail when String.length arg > 0 && arg.[0] <> '-' ->
            input := Some arg;
            parse tail
        | _ ->
            print_usage ();
            exit 1
      in
      parse rest;
      match !input with
      | Some filename -> Check { filename }
      | None ->
          print_usage ();
          exit 1)
  | [ "lsp" ] -> Lsp
  | [ filename ] -> Run { benchmark = false; filename }
  | _ ->
      print_usage ();
      exit 1

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

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let output_absolute_path (output : string) : string =
  if Filename.is_relative output then
    Filename.concat (Sys.getcwd ()) output
  else
    output

let compile_to_binary
    ~(input_file : string)
    ~(source : string)
    ~(output_bin : string)
    ~(emit_go_dir : string option)
    ~(release : bool) : (unit, string) result =
  match Marmoset.Lib.Go_emitter.compile_to_build ~file_id:input_file source with
  | Error msg -> Error msg
  | Ok build_output ->
      let temp_dir = ".marmoset-build" in
      ignore (Sys.command ("mkdir -p " ^ temp_dir));

      let main_go_path = Filename.concat temp_dir "main.go" in
      let runtime_go_path = Filename.concat temp_dir "runtime.go" in
      let go_mod_path = Filename.concat temp_dir "go.mod" in

      write_file main_go_path build_output.main_go;
      write_file runtime_go_path build_output.runtime_go;
      write_file go_mod_path "module marmoset_out\n\ngo 1.18\n";

      (match emit_go_dir with
      | Some dir ->
          ignore (Sys.command ("mkdir -p " ^ dir));
          write_file (Filename.concat dir "main.go") build_output.main_go;
          write_file (Filename.concat dir "runtime.go") build_output.runtime_go
      | None -> ());

      let output_abs = output_absolute_path output_bin in
      let go_flags =
        if release then
          "-ldflags=\"-s -w\" -trimpath"
        else
          ""
      in
      let cmd = Printf.sprintf "cd %s && go build %s -o %s ." temp_dir go_flags output_abs in
      let exit_code = Sys.command cmd in

      (try
         Sys.remove main_go_path;
         Sys.remove runtime_go_path;
         Sys.remove go_mod_path;
         ignore (Sys.command ("rmdir " ^ temp_dir))
       with _ -> ());

      if exit_code = 0 then
        Ok ()
      else
        Error "Go build failed"

let run_build input output_opt emit_go_opt release =
  let source = read_file input in
  let output =
    match output_opt with
    | Some o -> o
    | None -> Filename.basename (Filename.remove_extension input)
  in
  match compile_to_binary ~input_file:input ~source ~output_bin:output ~emit_go_dir:emit_go_opt ~release with
  | Ok () ->
      (match emit_go_opt with
      | Some dir -> Printf.printf "Go source written to %s/\n" dir
      | None -> ());
      Printf.printf "Built: %s\n" output
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1

let run_file ~(benchmark : bool) ~(filename : string) =
  let source = read_file filename in
  let tmp_bin = Filename.temp_file "marmoset-run" "" in
  (try Sys.remove tmp_bin with _ -> ());

  let release = benchmark in
  match compile_to_binary ~input_file:filename ~source ~output_bin:tmp_bin ~emit_go_dir:None ~release with
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Ok () ->
      let start = Sys.time () in
      let exit_code = Sys.command tmp_bin in
      let stop = Sys.time () in
      (try Sys.remove tmp_bin with _ -> ());
      if benchmark then
        Printf.eprintf "duration=%.4fs\n" (stop -. start);
      if exit_code <> 0 then
        exit exit_code

let run_check filename =
  let source = read_file filename in
  Marmoset_lsp.Doc_state.reset_globals ();
  let env = Marmoset.Lib.Builtins.prelude_env () in
  match Marmoset.Lib.Checker.check_string_with_annotations ~env ~file_id:filename source with
  | Ok _ -> Printf.printf "OK\n"
  | Error err ->
      let msg = Marmoset.Lib.Checker.format_error_with_context source err in
      Printf.eprintf "%s\n" msg;
      exit 1

let () =
  match parse_args () with
  | Run { benchmark; filename } -> run_file ~benchmark ~filename
  | Build { input; output; emit_go; release } -> run_build input output emit_go release
  | Check { filename } -> run_check filename
  | Lsp -> Marmoset_lsp.Server.run ()
