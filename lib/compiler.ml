open Ast

let ( let* ) res f = Result.bind res f
let buffer_size = 256

(* Track emitted instructions for backpatching *)
type emitted_instruction = {
  opcode : Code.opcode;
  position : int;
}

(* Compiler uses Buffer.t and Dynarray.t internally for efficient building *)
type compiler = {
  instructions : Buffer.t;
  constants : Value.value Dynarray.t;
  symbol_table : Symbol_table.t;
  mutable last_instruction : emitted_instruction option;
  mutable previous_instruction : emitted_instruction option;
}

(* Bytecode is the final, immutable result *)
type bytecode = {
  instructions : Code.instructions; (* bytes *)
  constants : Value.value array;
}

let init () : compiler =
  {
    instructions = Buffer.create buffer_size;
    constants = Dynarray.create ();
    symbol_table = Symbol_table.create ();
    last_instruction = None;
    previous_instruction = None;
  }

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
  (* Track last and previous instructions for backpatching *)
  c.previous_instruction <- c.last_instruction;
  c.last_instruction <- Some { opcode = op; position = pos };
  (c, pos)

(* Helper to get current instruction position (next byte to be written) *)
let current_instructions_len (c : compiler) : int = Buffer.length c.instructions

(* Check if the last emitted instruction is OpPop *)
let last_instruction_is_pop (c : compiler) : bool =
  match c.last_instruction with
  | Some { opcode = Code.OpPop; _ } -> true
  | _ -> false

(* Remove the last OpPop instruction (used when compiling if/else blocks) *)
let remove_last_pop (c : compiler) : unit =
  match c.last_instruction with
  | Some { opcode = Code.OpPop; position } ->
      (* Truncate the buffer to remove the OpPop *)
      let contents = Buffer.to_bytes c.instructions in
      Buffer.reset c.instructions;
      Buffer.add_subbytes c.instructions contents 0 position;
      c.last_instruction <- c.previous_instruction
  | _ -> ()

(* Change the operand of an already-emitted instruction at the given position.
   Used for backpatching jump addresses. *)
let change_operand (c : compiler) (op_position : int) (operand : int) : unit =
  (* Write the new operand as big-endian uint16 at position + 1 (after the opcode byte) *)
  let b1 = Char.chr ((operand lsr 8) land 0xFF) in
  let b2 = Char.chr (operand land 0xFF) in
  let buf_bytes = Buffer.to_bytes c.instructions in
  Bytes.set buf_bytes (op_position + 1) b1;
  Bytes.set buf_bytes (op_position + 2) b2;
  (* Replace buffer contents *)
  Buffer.reset c.instructions;
  Buffer.add_bytes c.instructions buf_bytes

let rec compile (c : compiler) (s : AST.program) : (compiler, string) result =
  (* Create fresh mutable structures starting with c's existing state *)
  (* This encapsulates mutation: each compile call gets its own buffers *)
  let buf = Buffer.create (max buffer_size (Buffer.length c.instructions)) in
  Buffer.add_buffer buf c.instructions;

  let consts = Dynarray.create () in
  Dynarray.append_seq consts (Dynarray.to_seq c.constants);

  let working_c : compiler =
    {
      instructions = buf;
      constants = consts;
      symbol_table = c.symbol_table;
      last_instruction = c.last_instruction;
      previous_instruction = c.previous_instruction;
    }
  in

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
  | AST.Block stmts ->
      (* Compile each statement in the block *)
      let rec loop c = function
        | [] -> Ok c
        | stmt :: rest ->
            let* c' = compile_statement c stmt in
            loop c' rest
      in
      loop c stmts
  | AST.Let (name, value) ->
      (* Compile the value expression *)
      let* c' = compile_expression c value in
      (* Define the symbol and get its index *)
      let symbol = Symbol_table.define c'.symbol_table name in
      (* Emit OpSetGlobal to store the value *)
      let c'', _pos = emit c' Code.OpSetGlobal [ symbol.index ] in
      Ok c''
  | _ -> failwith "Not implemented"

and compile_expression (c : compiler) (e : AST.expression) : (compiler, string) result =
  match e with
  | AST.Infix (left, op, right) ->
      (* For "<", we swap operands and use OpGreaterThan instead *)
      (* e.g., "3 < 5" becomes: compile(5), compile(3), OpGreaterThan *)
      let* c' =
        if op = "<" then
          compile_expression c right
        else
          compile_expression c left
      in
      let* c'' =
        if op = "<" then
          compile_expression c' left
        else
          compile_expression c' right
      in
      let opcode =
        match op with
        | "+" -> Ok Code.OpAdd
        | "-" -> Ok Code.OpSub
        | "*" -> Ok Code.OpMul
        | "/" -> Ok Code.OpDiv
        | "==" -> Ok Code.OpEqual
        | "!=" -> Ok Code.OpNotEqual
        | ">" -> Ok Code.OpGreaterThan
        | "<" -> Ok Code.OpGreaterThan (* swapped operands above! *)
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
  | AST.Prefix (op, right) ->
      (* First compile the operand, then emit the prefix operation *)
      let* c' = compile_expression c right in
      let opcode =
        match op with
        | "-" -> Ok Code.OpMinus
        | "!" -> Ok Code.OpBang
        | _ -> Error (Printf.sprintf "unknown prefix operator %s" op)
      in
      let* opcode = opcode in
      let c'', _pos = emit c' opcode [] in
      Ok c''
  | AST.If (condition, consequence, alternative) ->
      (* Compile the condition *)
      let* c' = compile_expression c condition in

      (* Emit OpJumpNotTruthy with placeholder address *)
      let c'', jump_not_truthy_pos = emit c' Code.OpJumpNotTruthy [ 9999 ] in

      (* Compile consequence block *)
      let* c''' = compile_statement c'' consequence in

      (* Remove the trailing OpPop from consequence (we want the value) *)
      if last_instruction_is_pop c''' then
        remove_last_pop c''';

      (* Emit OpJump with placeholder (to skip alternative) *)
      let c4, jump_pos = emit c''' Code.OpJump [ 9999 ] in

      (* Now we know where the alternative starts - backpatch OpJumpNotTruthy *)
      let after_consequence_pos = current_instructions_len c4 in
      change_operand c4 jump_not_truthy_pos after_consequence_pos;

      (* Compile alternative (or emit OpNull if none) *)
      let* c5 =
        match alternative with
        | None ->
            let c5, _pos = emit c4 Code.OpNull [] in
            Ok c5
        | Some alt ->
            let* c5 = compile_statement c4 alt in
            (* Remove the trailing OpPop from alternative (we want the value) *)
            if last_instruction_is_pop c5 then
              remove_last_pop c5;
            Ok c5
      in

      (* Backpatch OpJump to skip over alternative *)
      let after_alternative_pos = current_instructions_len c5 in
      change_operand c5 jump_pos after_alternative_pos;

      Ok c5
  | AST.Identifier name -> (
      (* Look up the identifier in the symbol table *)
      match Symbol_table.resolve c.symbol_table name with
      | Some symbol ->
          let c', _pos = emit c Code.OpGetGlobal [ symbol.index ] in
          Ok c'
      | None -> Error (Printf.sprintf "undefined variable %s" name))
  | _ -> failwith "Not implemented"

(* Convert the working compiler to final immutable bytecode *)
let bytecode (c : compiler) : bytecode =
  { instructions = Buffer.to_bytes c.instructions; constants = Dynarray.to_array c.constants }

module Test = struct
  type test = {
    input : string;
    expectedConstants : Value.value array;
    expectedInstructions : Code.instructions;
  }

  (* Helper to build expected instructions from a list of (opcode, operands) *)
  let make_instructions (ops : (Code.opcode * int list) list) : Code.instructions =
    Code.concat (List.map (fun (op, operands) -> Code.make op operands) ops)

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun test ->
           let p = Parser.parse test.input in
           match p with
           | Ok p -> (
               let c = compile (init ()) p in
               match c with
               | Ok c' ->
                   let b = bytecode c' in
                   b.constants = test.expectedConstants && b.instructions = test.expectedInstructions
               | Error _ -> false)
           | Error _ -> false)

  let%test "test_integer_arithmetic" =
    [
      {
        input = "1 + 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpAdd, []); (Code.OpPop, []) ];
      };
      {
        input = "1; 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpPop, []); (Code.OpConstant, [ 1 ]); (Code.OpPop, []) ];
      };
      {
        input = "1 - 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpSub, []); (Code.OpPop, []) ];
      };
      {
        input = "1 * 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpMul, []); (Code.OpPop, []) ];
      };
      {
        input = "2 / 1";
        expectedConstants = [| Value.Integer 2L; Value.Integer 1L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpDiv, []); (Code.OpPop, []) ];
      };
      {
        input = "-1";
        expectedConstants = [| Value.Integer 1L |];
        expectedInstructions =
          make_instructions [ (Code.OpConstant, [ 0 ]); (Code.OpMinus, []); (Code.OpPop, []) ];
      };
    ]
    |> run

  let%test "test_boolean_expressions" =
    [
      {
        input = "true";
        expectedConstants = [||];
        expectedInstructions = make_instructions [ (Code.OpTrue, []); (Code.OpPop, []) ];
      };
      {
        input = "false";
        expectedConstants = [||];
        expectedInstructions = make_instructions [ (Code.OpFalse, []); (Code.OpPop, []) ];
      };
      {
        input = "1 > 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpGreaterThan, []); (Code.OpPop, []) ];
      };
      {
        input = "1 < 2";
        (* Note: operands are swapped for < *)
        expectedConstants = [| Value.Integer 2L; Value.Integer 1L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpGreaterThan, []); (Code.OpPop, []) ];
      };
      {
        input = "1 == 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpEqual, []); (Code.OpPop, []) ];
      };
      {
        input = "1 != 2";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpNotEqual, []); (Code.OpPop, []) ];
      };
      {
        input = "true == false";
        expectedConstants = [||];
        expectedInstructions =
          make_instructions [ (Code.OpTrue, []); (Code.OpFalse, []); (Code.OpEqual, []); (Code.OpPop, []) ];
      };
      {
        input = "true != false";
        expectedConstants = [||];
        expectedInstructions =
          make_instructions [ (Code.OpTrue, []); (Code.OpFalse, []); (Code.OpNotEqual, []); (Code.OpPop, []) ];
      };
      {
        input = "!true";
        expectedConstants = [||];
        expectedInstructions = make_instructions [ (Code.OpTrue, []); (Code.OpBang, []); (Code.OpPop, []) ];
      };
    ]
    |> run

  let%test "test_conditionals" =
    [
      (* if (true) { 10 }; 3333; *)
      {
        input = "if (true) { 10 }; 3333;";
        expectedConstants = [| Value.Integer 10L; Value.Integer 3333L |];
        expectedInstructions =
          make_instructions
            [
              (* 0000 *)
              (Code.OpTrue, []);
              (* 0001 *)
              (Code.OpJumpNotTruthy, [ 10 ]);
              (* 0004 *)
              (Code.OpConstant, [ 0 ]);
              (* 0007 *)
              (Code.OpJump, [ 11 ]);
              (* 0010 *)
              (Code.OpNull, []);
              (* 0011 *)
              (Code.OpPop, []);
              (* 0012 *)
              (Code.OpConstant, [ 1 ]);
              (* 0015 *)
              (Code.OpPop, []);
            ];
      };
      (* if (true) { 10 } else { 20 }; 3333; *)
      {
        input = "if (true) { 10 } else { 20 }; 3333;";
        expectedConstants = [| Value.Integer 10L; Value.Integer 20L; Value.Integer 3333L |];
        expectedInstructions =
          make_instructions
            [
              (* 0000 *)
              (Code.OpTrue, []);
              (* 0001 *)
              (Code.OpJumpNotTruthy, [ 10 ]);
              (* 0004 *)
              (Code.OpConstant, [ 0 ]);
              (* 0007 *)
              (Code.OpJump, [ 13 ]);
              (* 0010 *)
              (Code.OpConstant, [ 1 ]);
              (* 0013 *)
              (Code.OpPop, []);
              (* 0014 *)
              (Code.OpConstant, [ 2 ]);
              (* 0017 *)
              (Code.OpPop, []);
            ];
      };
    ]
    |> run

  let%test "test_global_let_statements" =
    [
      (* let one = 1; *)
      {
        input = "let one = 1;";
        expectedConstants = [| Value.Integer 1L |];
        expectedInstructions = make_instructions [ (Code.OpConstant, [ 0 ]); (Code.OpSetGlobal, [ 0 ]) ];
      };
      (* let one = 1; let two = 2; *)
      {
        input = "let one = 1; let two = 2;";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpSetGlobal, [ 0 ]);
              (Code.OpConstant, [ 1 ]);
              (Code.OpSetGlobal, [ 1 ]);
            ];
      };
      (* let one = 1; one; *)
      {
        input = "let one = 1; one;";
        expectedConstants = [| Value.Integer 1L |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpSetGlobal, [ 0 ]); (Code.OpGetGlobal, [ 0 ]); (Code.OpPop, []) ];
      };
      (* let one = 1; let two = one + one; two; *)
      {
        input = "let one = 1; let two = one + one; two;";
        expectedConstants = [| Value.Integer 1L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpSetGlobal, [ 0 ]);
              (Code.OpGetGlobal, [ 0 ]);
              (Code.OpGetGlobal, [ 0 ]);
              (Code.OpAdd, []);
              (Code.OpSetGlobal, [ 1 ]);
              (Code.OpGetGlobal, [ 1 ]);
              (Code.OpPop, []);
            ];
      };
    ]
    |> run
end
