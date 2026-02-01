(* Instructions is the final, immutable bytecode *)
type instructions = bytes

and opcode =
  | OpConstant
  | OpAdd

and definition = {
  name : string;
  operand_widths : int list;
}

let to_int = function
  | OpConstant -> 0
  | OpAdd -> 1

let to_definition = function
  | OpConstant -> { name = "OpConstant"; operand_widths = [ 2 ] }
  | OpAdd -> { name = "OpAdd"; operand_widths = [] }

let make (op : opcode) (operands : int list) : bytes =
  let def = to_definition op in
  let instruction_length = 1 + List.fold_left ( + ) 0 def.operand_widths in
  let instruction = Bytes.create instruction_length in

  Bytes.set instruction 0 (Char.chr (op |> to_int));

  let rec fill_instruction offset operands widths =
    match (operands, widths) with
    | [], [] -> ()
    | o :: os, w :: ws ->
        (match w with
        | 2 ->
            let b1 = Char.chr ((o lsr 8) land 0xFF) in
            let b2 = Char.chr (o land 0xFF) in
            Bytes.set instruction offset b1;
            Bytes.set instruction (offset + 1) b2
        | _ -> ());
        fill_instruction (offset + w) os ws
    | _ -> ()
  in
  fill_instruction 1 operands def.operand_widths;
  instruction

(* Helper to concatenate multiple instructions into a single instruction sequence *)
let concat (instrs : bytes list) : instructions =
  let buf = Buffer.create 256 in
  List.iter (Buffer.add_bytes buf) instrs;
  Buffer.to_bytes buf

module Test = struct
  type test = {
    input : opcode;
    operands : int list;
    expected : bytes;
  }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun test ->
           let instruction = make test.input test.operands in
           instruction = test.expected)

  let%test "test_make" =
    [
      { input = OpConstant; operands = [ 65534 ]; expected = Bytes.of_string "\000\255\254" };
      { input = OpAdd; operands = []; expected = Bytes.of_string "\001" };
    ]
    |> run

  let%test "test_dissasemble" =
    (* TODO: implement instruction disassembly *)
    true
end
