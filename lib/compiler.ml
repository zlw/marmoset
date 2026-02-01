open Ast

let ( let* ) res f = Result.bind res f
let buffer_size = 256

(* Compiler uses Buffer.t and Dynarray.t internally for efficient building *)
type compiler = {
  instructions : Buffer.t;
  constants : Value.value Dynarray.t;
}

(* Bytecode is the final, immutable result *)
type bytecode = {
  instructions : Code.instructions; (* bytes *)
  constants : Value.value array;
}

let init : compiler = { instructions = Buffer.create buffer_size; constants = Dynarray.create () }

(* Add a constant to the pool, returns new compiler and the constant's index *)
let add_constant (c : compiler) (obj : Value.value) : compiler * int =
  let index = Dynarray.length c.constants in
  Dynarray.add_last c.constants obj;
  (c, index)

(* Emit an instruction into the buffer, returns new compiler and the instruction's position *)
let emit (c : compiler) (op : Code.opcode) (operands : int list) : compiler * int =
  let ins = Code.make op operands in
  let pos = Buffer.length c.instructions in
  Buffer.add_bytes c.instructions ins;
  (c, pos)

let rec compile (c : compiler) (s : AST.program) : (compiler, string) result =
  (* Create fresh mutable structures starting with c's existing state *)
  (* This encapsulates mutation: each compile call gets its own buffers *)
  let buf = Buffer.create (max buffer_size (Buffer.length c.instructions)) in
  Buffer.add_buffer buf c.instructions;

  let consts = Dynarray.create () in
  Dynarray.append_seq consts (Dynarray.to_seq c.constants);

  let working_c : compiler = { instructions = buf; constants = consts } in

  let rec loop c s =
    match s with
    | [] -> Ok c
    | hd :: tl -> (
        match compile_statement c hd with
        | Ok c' -> loop c' tl
        | Error e -> Error e)
  in
  loop working_c s

and compile_statement (c : compiler) (s : AST.statement) : (compiler, string) result =
  match s with
  | AST.Expression e ->
      let* c' = compile_expression c e in
      let c'', _pos = emit c' Code.OpPop [] in
      Ok c''
  | _ -> failwith "Not implemented"

and compile_expression (c : compiler) (e : AST.expression) : (compiler, string) result =
  match e with
  | AST.Infix (left, op, right) ->
      let* c' = compile_expression c left in
      let* c'' = compile_expression c' right in
      let opcode =
        match op with
        | "+" -> Ok Code.OpAdd
        | "-" -> Ok Code.OpSub
        | "*" -> Ok Code.OpMul
        | "/" -> Ok Code.OpDiv
        | _ -> Error (Printf.sprintf "unknown operator %s" op)
      in
      let* opcode = opcode in
      let c''', _pos = emit c'' opcode [] in
      Ok c'''
  | AST.Integer i ->
      let c', index = add_constant c (Value.Integer i) in
      let c'', _pos = emit c' Code.OpConstant [ index ] in
      Ok c''
  | AST.Float f ->
      let c', index = add_constant c (Value.Float f) in
      let c'', _pos = emit c' Code.OpConstant [ index ] in
      Ok c''
  | AST.Boolean b ->
      let opcode =
        if b then
          Code.OpTrue
        else
          Code.OpFalse
      in
      let c', _pos = emit c opcode [] in
      Ok c'
  | _ -> failwith "Not implemented"

(* Convert the working compiler to final immutable bytecode *)
let bytecode (c : compiler) : bytecode =
  { instructions = Buffer.to_bytes c.instructions; constants = Dynarray.to_array c.constants }

module Test = struct
  type test = {
    input : string;
    expectedConstants : Value.value array;
    expectedInstructions : Buffer.t;
  }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun test ->
           let p = Parser.parse test.input in
           match p with
           | Ok p -> (
               let c = compile init p in
               match c with
               | Ok c' ->
                   let b = bytecode c' in
                   let expected_instructions = Buffer.to_bytes test.expectedInstructions in
                   b.constants = test.expectedConstants && b.instructions = expected_instructions
               | Error _ -> false)
           | Error _ -> false)

  let%test "test_integer_arithmetic" =
    let expected_instructions =
      let c : compiler = { instructions = Buffer.create 32; constants = Dynarray.create () } in
      let c, _ = emit c Code.OpConstant [ 0 ] in
      let c, _ = emit c Code.OpConstant [ 1 ] in
      let c, _ = emit c Code.OpAdd [] in
      let c, _ = emit c Code.OpPop [] in
      c.instructions
    in
    [
      {
        input = "1 + 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions = expected_instructions;
      };
    ]
    |> run
end
