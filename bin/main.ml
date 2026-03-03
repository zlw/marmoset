let version_string = Printf.sprintf "marmoset %s (built %s)" Build_info.git_hash Build_info.build_time

module Diagnostic = Marmoset.Lib.Diagnostic

type command =
  | Run of {
      benchmark : bool;
      filename : string;
    }
  | Build of {
      input : string;
      output : string option;
      emit_go : string option;
    }
  | Release of {
      input : string;
      output : string option;
    }
  | Check of { filename : string }
  | Lsp
  | Version

let print_usage () =
  Printf.eprintf "Usage:\n";
  Printf.eprintf "  marmoset run [--benchmark] <input.mr>\n";
  Printf.eprintf "  marmoset build <input.mr> [-o output] [-go dir]\n";
  Printf.eprintf "  marmoset release <input.mr> [-o output]\n";
  Printf.eprintf "  marmoset check <input.mr>\n";
  Printf.eprintf "  marmoset lsp\n";
  Printf.eprintf "  marmoset --version\n"

let parse_args () : command =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "build" :: rest -> (
      let input = ref None in
      let output = ref None in
      let emit_go = ref None in
      let rec parse = function
        | [] -> ()
        | "-o" :: out :: tail ->
            output := Some out;
            parse tail
        | ("-go" | "--emit-go") :: dir :: tail ->
            emit_go := Some dir;
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
      | Some input -> Build { input; output = !output; emit_go = !emit_go }
      | None ->
          print_usage ();
          exit 1)
  | "release" :: rest -> (
      let input = ref None in
      let output = ref None in
      let rec parse = function
        | [] -> ()
        | "-o" :: out :: tail ->
            output := Some out;
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
      | Some input -> Release { input; output = !output }
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
  | [ "--version" ] | [ "-v" ] | [ "version" ] -> Version
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

let read_all_lines (ic : in_channel) : string =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  Buffer.contents buf

let run_command_capture_combined_output (cmd : string) : int * string =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let output = read_all_lines ic |> String.trim in
  let status = Unix.close_process_in ic in
  let exit_code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal
  in
  (exit_code, output)

let print_diagnostics ~(file_id : string) ~(source : string) (diags : Diagnostic.t list) : unit =
  let source_lookup candidate_file_id =
    if String.equal candidate_file_id file_id then
      Some source
    else
      None
  in
  Printf.eprintf "%s\n" (Diagnostic.render_many_cli ~source_lookup diags)

let compile_to_binary
    ~(input_file : string)
    ~(source : string)
    ~(output_bin : string)
    ~(emit_go_dir : string option)
    ~(release : bool) : (unit, Diagnostic.t list) result =
  match Marmoset.Lib.Go_emitter.compile_to_build ~file_id:input_file source with
  | Error diags -> Error diags
  | Ok build_output ->
      let temp_dir = ".marmoset/build/" ^ string_of_int (Unix.getpid ()) in
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
      let exit_code, go_output = run_command_capture_combined_output cmd in

      (try ignore (Sys.command ("rm -rf " ^ temp_dir)) with _ -> ());

      if exit_code = 0 then
        Ok ()
      else
        Error [ Marmoset.Lib.Go_emitter.classify_go_build_failure ~exit_code ~output:go_output ]

let run_build input output_opt emit_go_opt =
  let source = read_file input in
  let output =
    match output_opt with
    | Some o -> o
    | None -> Filename.basename (Filename.remove_extension input)
  in
  match
    compile_to_binary ~input_file:input ~source ~output_bin:output ~emit_go_dir:emit_go_opt ~release:false
  with
  | Ok () ->
      (match emit_go_opt with
      | Some dir -> Printf.printf "Go source written to %s/\n" dir
      | None -> ());
      Printf.printf "Built: %s\n" output
  | Error diags ->
      print_diagnostics ~file_id:input ~source diags;
      exit 1

let run_release input output_opt =
  let source = read_file input in
  let output =
    match output_opt with
    | Some o -> o
    | None -> Filename.basename (Filename.remove_extension input)
  in
  match compile_to_binary ~input_file:input ~source ~output_bin:output ~emit_go_dir:None ~release:true with
  | Ok () -> Printf.printf "Built (release): %s\n" output
  | Error diags ->
      print_diagnostics ~file_id:input ~source diags;
      exit 1

let run_file ~(benchmark : bool) ~(filename : string) =
  let source = read_file filename in
  let tmp_bin = Filename.temp_file "marmoset-run" "" in
  (try Sys.remove tmp_bin with _ -> ());

  let release = benchmark in
  match compile_to_binary ~input_file:filename ~source ~output_bin:tmp_bin ~emit_go_dir:None ~release with
  | Error diags ->
      print_diagnostics ~file_id:filename ~source diags;
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
  | Error errs ->
      print_diagnostics ~file_id:filename ~source errs;
      exit 1

let () =
  match parse_args () with
  | Run { benchmark; filename } -> run_file ~benchmark ~filename
  | Build { input; output; emit_go } -> run_build input output emit_go
  | Release { input; output } -> run_release input output
  | Check { filename } -> run_check filename
  | Lsp ->
      Printf.eprintf "[marmoset-lsp] %s\n%!" version_string;
      Marmoset_lsp.Server.run ()
  | Version -> Printf.printf "%s\n" version_string
