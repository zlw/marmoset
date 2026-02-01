(* Instructions is the final, immutable bytecode *)
type instructions = bytes

and opcode =
  | OpConstant
  | OpAdd
  | OpPop
  | OpSub
  | OpMul
  | OpDiv
  | OpTrue
  | OpFalse
  | OpEqual
  | OpNotEqual
  | OpGreaterThan
  | OpMinus
  | OpBang

and definition = {
  name : string;
  operand_widths : int list;
}

let to_int = function
  | OpConstant -> 0
  | OpAdd -> 1
  | OpPop -> 2
  | OpSub -> 3
  | OpMul -> 4
  | OpDiv -> 5
  | OpTrue -> 6
  | OpFalse -> 7
  | OpEqual -> 8
  | OpNotEqual -> 9
  | OpGreaterThan -> 10
  | OpMinus -> 11
  | OpBang -> 12

let of_int = function
  | 0 -> Some OpConstant
  | 1 -> Some OpAdd
  | 2 -> Some OpPop
  | 3 -> Some OpSub
  | 4 -> Some OpMul
  | 5 -> Some OpDiv
  | 6 -> Some OpTrue
  | 7 -> Some OpFalse
  | 8 -> Some OpEqual
  | 9 -> Some OpNotEqual
  | 10 -> Some OpGreaterThan
  | 11 -> Some OpMinus
  | 12 -> Some OpBang
  | _ -> None

let to_definition = function
  | OpConstant -> { name = "OpConstant"; operand_widths = [ 2 ] }
  | OpAdd -> { name = "OpAdd"; operand_widths = [] }
  | OpPop -> { name = "OpPop"; operand_widths = [] }
  | OpSub -> { name = "OpSub"; operand_widths = [] }
  | OpMul -> { name = "OpMul"; operand_widths = [] }
  | OpDiv -> { name = "OpDiv"; operand_widths = [] }
  | OpTrue -> { name = "OpTrue"; operand_widths = [] }
  | OpFalse -> { name = "OpFalse"; operand_widths = [] }
  | OpEqual -> { name = "OpEqual"; operand_widths = [] }
  | OpNotEqual -> { name = "OpNotEqual"; operand_widths = [] }
  | OpGreaterThan -> { name = "OpGreaterThan"; operand_widths = [] }
  | OpMinus -> { name = "OpMinus"; operand_widths = [] }
  | OpBang -> { name = "OpBang"; operand_widths = [] }

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

(* Read a big-endian uint16 from instructions at offset *)
let read_uint16 (ins : instructions) (offset : int) : int =
  let b1 = Char.code (Bytes.get ins offset) in
  let b2 = Char.code (Bytes.get ins (offset + 1)) in
  (b1 lsl 8) lor b2

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
