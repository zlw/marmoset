open Ast

let ( let* ) res f = Result.bind res f
let buffer_size = 256

(* Track emitted instructions for backpatching *)
type emitted_instruction = {
  opcode : Code.opcode;
  position : int;
}

(* Compilation scope tracks per-function compilation state *)
type compilation_scope = {
  instructions : Buffer.t;
  mutable last_instruction : emitted_instruction option;
  mutable previous_instruction : emitted_instruction option;
}

(* Compiler uses compilation scopes for nested function compilation *)
type compiler = {
  constants : Value.value Dynarray.t;
  mutable symbol_table : Symbol_table.t;
  scopes : compilation_scope Dynarray.t;
  mutable scope_index : int;
}

(* Bytecode is the final, immutable result *)
type bytecode = {
  instructions : Code.instructions; (* bytes *)
  constants : Value.value array;
}

(* Get the current compilation scope *)
let current_scope (c : compiler) : compilation_scope = Dynarray.get c.scopes c.scope_index

(* Get the current instructions buffer *)
let current_instructions (c : compiler) : Buffer.t = (current_scope c).instructions

let init () : compiler =
  let main_scope =
    { instructions = Buffer.create buffer_size; last_instruction = None; previous_instruction = None }
  in
  let scopes = Dynarray.create () in
  Dynarray.add_last scopes main_scope;
  let symbol_table = Symbol_table.create () in
  (* Pre-populate builtins in the symbol table *)
  List.iteri (fun i name -> ignore (Symbol_table.define_builtin symbol_table i name)) Symbol_table.builtin_names;
  { constants = Dynarray.create (); symbol_table; scopes; scope_index = 0 }

(* Enter a new compilation scope for compiling a function *)
let enter_scope (c : compiler) : unit =
  let scope =
    { instructions = Buffer.create buffer_size; last_instruction = None; previous_instruction = None }
  in
  Dynarray.add_last c.scopes scope;
  c.scope_index <- c.scope_index + 1;
  c.symbol_table <- Symbol_table.create_enclosed c.symbol_table

(* Leave the current scope and return its compiled instructions *)
let leave_scope (c : compiler) : bytes =
  let scope = current_scope c in
  let instructions = Buffer.to_bytes scope.instructions in
  (* Pop the scope *)
  let _ = Dynarray.pop_last c.scopes in
  c.scope_index <- c.scope_index - 1;
  (* Restore outer symbol table *)
  (match c.symbol_table.outer with
  | Some outer -> c.symbol_table <- outer
  | None -> ());
  instructions

(* Add a constant to the pool, returns new compiler and the constant's index *)
let add_constant (c : compiler) (obj : Value.value) : compiler * int =
  let index = Dynarray.length c.constants in
  Dynarray.add_last c.constants obj;
  (c, index)

(* Emit an instruction into the current scope's buffer *)
let emit (c : compiler) (op : Code.opcode) (operands : int list) : compiler * int =
  let ins = Code.make op operands in
  let scope = current_scope c in
  let pos = Buffer.length scope.instructions in
  Buffer.add_bytes scope.instructions ins;
  (* Track last and previous instructions for backpatching *)
  scope.previous_instruction <- scope.last_instruction;
  scope.last_instruction <- Some { opcode = op; position = pos };
  (c, pos)

(* Helper to get current instruction position (next byte to be written) *)
let current_instructions_len (c : compiler) : int = Buffer.length (current_instructions c)

(* Check if the last emitted instruction is OpPop *)
let last_instruction_is_pop (c : compiler) : bool =
  match (current_scope c).last_instruction with
  | Some { opcode = Code.OpPop; _ } -> true
  | _ -> false

(* Check if the last emitted instruction matches the given opcode *)
let last_instruction_is (c : compiler) (op : Code.opcode) : bool =
  match (current_scope c).last_instruction with
  | Some { opcode; _ } -> opcode = op
  | None -> false

(* Remove the last OpPop instruction (used when compiling if/else blocks) *)
let remove_last_pop (c : compiler) : unit =
  let scope = current_scope c in
  match scope.last_instruction with
  | Some { opcode = Code.OpPop; position } ->
      (* Truncate the buffer to remove the OpPop *)
      let contents = Buffer.to_bytes scope.instructions in
      Buffer.reset scope.instructions;
      Buffer.add_subbytes scope.instructions contents 0 position;
      scope.last_instruction <- scope.previous_instruction
  | _ -> ()

(* Replace the last instruction - used to convert OpPop to OpReturnValue *)
let replace_last_pop_with_return (c : compiler) : unit =
  let scope = current_scope c in
  match scope.last_instruction with
  | Some { opcode = Code.OpPop; position } ->
      (* Replace OpPop with OpReturnValue *)
      let contents = Buffer.to_bytes scope.instructions in
      Bytes.set contents position (Char.chr (Code.to_int Code.OpReturnValue));
      Buffer.reset scope.instructions;
      Buffer.add_bytes scope.instructions contents;
      scope.last_instruction <- Some { opcode = Code.OpReturnValue; position }
  | _ -> ()

(* Change the operand of an already-emitted instruction at the given position.
   Used for backpatching jump addresses. *)
let change_operand (c : compiler) (op_position : int) (operand : int) : unit =
  let scope = current_scope c in
  (* Write the new operand as big-endian uint16 at position + 1 (after the opcode byte) *)
  let b1 = Char.chr ((operand lsr 8) land 0xFF) in
  let b2 = Char.chr (operand land 0xFF) in
  let buf_bytes = Buffer.to_bytes scope.instructions in
  Bytes.set buf_bytes (op_position + 1) b1;
  Bytes.set buf_bytes (op_position + 2) b2;
  (* Replace buffer contents *)
  Buffer.reset scope.instructions;
  Buffer.add_bytes scope.instructions buf_bytes

(* Compile a named function literal (for let statements) to enable recursion.
   This is separate from compile_expression's AST.Function case because we need
   to define the function name inside the function scope BEFORE compiling the body. *)
let rec compile_function_literal
    (c : compiler) (name : string) (params : AST.expression list) (body : AST.statement) :
    (compiler, string) result =
  (* Enter a new compilation scope for the function *)
  enter_scope c;
  (* Define the function name inside the scope with FunctionScope to enable recursion *)
  ignore (Symbol_table.define_function_name c.symbol_table name);
  (* Define each parameter in the symbol table (they become locals) *)
  List.iter
    (fun param ->
      match param with
      | AST.Identifier pname -> ignore (Symbol_table.define c.symbol_table pname)
      | _ -> failwith "invalid function parameter")
    params;
  (* Compile the function body *)
  let* c' = compile_statement c body in
  (* If the last instruction is OpPop, replace it with OpReturnValue *)
  if last_instruction_is_pop c' then
    replace_last_pop_with_return c';
  (* If there's no return, emit implicit OpReturn (returns null) *)
  (if not (last_instruction_is c' Code.OpReturnValue) then
     let c'', _pos = emit c' Code.OpReturn [] in
     ignore c'');
  (* Get the number of locals defined in this scope *)
  let num_locals = c.symbol_table.num_definitions in
  (* Get the free symbols that this function needs to capture *)
  let free_symbols = Dynarray.to_array c.symbol_table.free_symbols in
  (* Leave the scope and get the compiled instructions *)
  let instructions = leave_scope c in
  (* Create the CompiledFunction value *)
  let compiled_fn = Value.CompiledFunction { instructions; num_locals; num_parameters = List.length params } in
  (* Add to constants *)
  let c'', fn_index = add_constant c compiled_fn in
  (* Before emitting OpClosure, emit instructions to load free variables *)
  let c''' =
    Array.fold_left
      (fun c sym ->
        let c', _pos =
          match sym.Symbol_table.scope with
          | Symbol_table.LocalScope -> emit c Code.OpGetLocal [ sym.index ]
          | Symbol_table.FreeScope -> emit c Code.OpGetFree [ sym.index ]
          | _ -> failwith "unexpected scope for free variable"
        in
        c')
      c'' free_symbols
  in
  (* Emit OpClosure with function index and number of free variables *)
  let c4, _pos = emit c''' Code.OpClosure [ fn_index; Array.length free_symbols ] in
  Ok c4

and compile (c : compiler) (s : AST.program) : (compiler, string) result =
  let rec loop c s =
    match s with
    | [] -> Ok c
    | hd :: tl -> (
        match compile_statement c hd with
        | Ok c' -> loop c' tl
        | Error e -> Error e)
  in
  loop c s

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
      (* For function literals, we need to define the function name inside
         the function scope to enable recursion *)
      let* c' =
        match value with
        | AST.Function (params, body) ->
            (* Special handling for function literals to support recursion *)
            compile_function_literal c name params body
        | _ -> compile_expression c value
      in
      (* Define the symbol and get its index *)
      let symbol = Symbol_table.define c'.symbol_table name in
      (* Emit appropriate set opcode based on scope *)
      let c'', _pos =
        match symbol.scope with
        | Symbol_table.GlobalScope -> emit c' Code.OpSetGlobal [ symbol.index ]
        | Symbol_table.LocalScope -> emit c' Code.OpSetLocal [ symbol.index ]
        | Symbol_table.BuiltinScope -> failwith "cannot reassign builtin"
        | Symbol_table.FreeScope -> failwith "cannot reassign free variable"
        | Symbol_table.FunctionScope -> failwith "cannot reassign function"
      in
      Ok c''
  | AST.Return expr ->
      (* Compile the return value expression *)
      let* c' = compile_expression c expr in
      (* Emit OpReturnValue *)
      let c'', _pos = emit c' Code.OpReturnValue [] in
      Ok c''

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
  | AST.String s ->
      let c', index = add_constant c (Value.String s) in
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
          let c', _pos =
            match symbol.scope with
            | Symbol_table.GlobalScope -> emit c Code.OpGetGlobal [ symbol.index ]
            | Symbol_table.LocalScope -> emit c Code.OpGetLocal [ symbol.index ]
            | Symbol_table.BuiltinScope -> emit c Code.OpGetBuiltin [ symbol.index ]
            | Symbol_table.FreeScope -> emit c Code.OpGetFree [ symbol.index ]
            | Symbol_table.FunctionScope -> emit c Code.OpCurrentClosure []
          in
          Ok c'
      | None -> Error (Printf.sprintf "undefined variable %s" name))
  | AST.Array elements ->
      (* Compile each element expression *)
      let rec compile_elements c = function
        | [] -> Ok c
        | el :: rest ->
            let* c' = compile_expression c el in
            compile_elements c' rest
      in
      let* c' = compile_elements c elements in
      (* Emit OpArray with the number of elements *)
      let c'', _pos = emit c' Code.OpArray [ List.length elements ] in
      Ok c''
  | AST.Hash pairs ->
      (* Compile each key-value pair: key, value, key, value, ... *)
      let rec compile_pairs c = function
        | [] -> Ok c
        | (key, value) :: rest ->
            let* c' = compile_expression c key in
            let* c'' = compile_expression c' value in
            compile_pairs c'' rest
      in
      let* c' = compile_pairs c pairs in
      (* Emit OpHash with total number of elements (2 * num_pairs) *)
      let c'', _pos = emit c' Code.OpHash [ List.length pairs * 2 ] in
      Ok c''
  | AST.Index (left, index) ->
      (* Compile the object being indexed *)
      let* c' = compile_expression c left in
      (* Compile the index expression *)
      let* c'' = compile_expression c' index in
      (* Emit OpIndex *)
      let c''', _pos = emit c'' Code.OpIndex [] in
      Ok c'''
  | AST.Function (params, body) ->
      (* Enter a new compilation scope for the function *)
      enter_scope c;
      (* Define each parameter in the symbol table (they become locals) *)
      List.iter
        (fun param ->
          match param with
          | AST.Identifier name -> ignore (Symbol_table.define c.symbol_table name)
          | _ -> failwith "invalid function parameter")
        params;
      (* Note: For named functions (recursive), the function name should be defined
         with FunctionScope BEFORE compiling the body. This is handled by 
         compile_function_literal when called from let statements. *)
      (* Compile the function body *)
      let* c' = compile_statement c body in
      (* If the last instruction is OpPop, replace it with OpReturnValue *)
      if last_instruction_is_pop c' then
        replace_last_pop_with_return c';
      (* If there's no return, emit implicit OpReturn (returns null) *)
      (if not (last_instruction_is c' Code.OpReturnValue) then
         let c'', _pos = emit c' Code.OpReturn [] in
         ignore c'');
      (* Get the number of locals defined in this scope *)
      let num_locals = c.symbol_table.num_definitions in
      (* Get the free symbols that this function needs to capture *)
      let free_symbols = Dynarray.to_array c.symbol_table.free_symbols in
      (* Leave the scope and get the compiled instructions *)
      let instructions = leave_scope c in
      (* Create the CompiledFunction value *)
      let compiled_fn =
        Value.CompiledFunction { instructions; num_locals; num_parameters = List.length params }
      in
      (* Add to constants *)
      let c'', fn_index = add_constant c compiled_fn in
      (* Before emitting OpClosure, emit instructions to load free variables *)
      let c''' =
        Array.fold_left
          (fun c sym ->
            let c', _pos =
              match sym.Symbol_table.scope with
              | Symbol_table.LocalScope -> emit c Code.OpGetLocal [ sym.index ]
              | Symbol_table.FreeScope -> emit c Code.OpGetFree [ sym.index ]
              | _ -> failwith "unexpected scope for free variable"
            in
            c')
          c'' free_symbols
      in
      (* Emit OpClosure with function index and number of free variables *)
      let c4, _pos = emit c''' Code.OpClosure [ fn_index; Array.length free_symbols ] in
      Ok c4
  | AST.Call (fn, args) ->
      (* Compile the function expression *)
      let* c' = compile_expression c fn in
      (* Compile each argument *)
      let rec compile_args c = function
        | [] -> Ok c
        | arg :: rest ->
            let* c' = compile_expression c arg in
            compile_args c' rest
      in
      let* c'' = compile_args c' args in
      (* Emit OpCall with argument count *)
      let c''', _pos = emit c'' Code.OpCall [ List.length args ] in
      Ok c'''

(* Convert the working compiler to final immutable bytecode *)
let bytecode (c : compiler) : bytecode =
  { instructions = Buffer.to_bytes (current_instructions c); constants = Dynarray.to_array c.constants }

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

  let%test "test_string_expressions" =
    [
      (* Simple string literal *)
      {
        input = "\"monkey\"";
        expectedConstants = [| Value.String "monkey" |];
        expectedInstructions = make_instructions [ (Code.OpConstant, [ 0 ]); (Code.OpPop, []) ];
      };
      (* String concatenation *)
      {
        input = "\"mon\" + \"key\"";
        expectedConstants = [| Value.String "mon"; Value.String "key" |];
        expectedInstructions =
          make_instructions
            [ (Code.OpConstant, [ 0 ]); (Code.OpConstant, [ 1 ]); (Code.OpAdd, []); (Code.OpPop, []) ];
      };
    ]
    |> run

  let%test "test_array_literals" =
    [
      (* Empty array *)
      {
        input = "[]";
        expectedConstants = [||];
        expectedInstructions = make_instructions [ (Code.OpArray, [ 0 ]); (Code.OpPop, []) ];
      };
      (* Array with integers *)
      {
        input = "[1, 2, 3]";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L; Value.Integer 3L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpConstant, [ 1 ]);
              (Code.OpConstant, [ 2 ]);
              (Code.OpArray, [ 3 ]);
              (Code.OpPop, []);
            ];
      };
      (* Array with expression *)
      {
        input = "[1 + 2, 3 * 4]";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L; Value.Integer 3L; Value.Integer 4L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpConstant, [ 1 ]);
              (Code.OpAdd, []);
              (Code.OpConstant, [ 2 ]);
              (Code.OpConstant, [ 3 ]);
              (Code.OpMul, []);
              (Code.OpArray, [ 2 ]);
              (Code.OpPop, []);
            ];
      };
    ]
    |> run

  let%test "test_hash_literals" =
    [
      (* Empty hash *)
      {
        input = "{}";
        expectedConstants = [||];
        expectedInstructions = make_instructions [ (Code.OpHash, [ 0 ]); (Code.OpPop, []) ];
      };
      (* Hash with integer keys *)
      {
        input = "{1: 2, 3: 4}";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L; Value.Integer 3L; Value.Integer 4L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpConstant, [ 1 ]);
              (Code.OpConstant, [ 2 ]);
              (Code.OpConstant, [ 3 ]);
              (Code.OpHash, [ 4 ]);
              (Code.OpPop, []);
            ];
      };
      (* Hash with expressions *)
      {
        input = "{1: 2 + 3}";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L; Value.Integer 3L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpConstant, [ 1 ]);
              (Code.OpConstant, [ 2 ]);
              (Code.OpAdd, []);
              (Code.OpHash, [ 2 ]);
              (Code.OpPop, []);
            ];
      };
    ]
    |> run

  let%test "test_index_expressions" =
    [
      (* Array indexing *)
      {
        input = "[1, 2, 3][1]";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L; Value.Integer 3L; Value.Integer 1L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpConstant, [ 1 ]);
              (Code.OpConstant, [ 2 ]);
              (Code.OpArray, [ 3 ]);
              (Code.OpConstant, [ 3 ]);
              (Code.OpIndex, []);
              (Code.OpPop, []);
            ];
      };
      (* Hash indexing *)
      {
        input = "{1: 2}[1]";
        expectedConstants = [| Value.Integer 1L; Value.Integer 2L; Value.Integer 1L |];
        expectedInstructions =
          make_instructions
            [
              (Code.OpConstant, [ 0 ]);
              (Code.OpConstant, [ 1 ]);
              (Code.OpHash, [ 2 ]);
              (Code.OpConstant, [ 2 ]);
              (Code.OpIndex, []);
              (Code.OpPop, []);
            ];
      };
    ]
    |> run
end
