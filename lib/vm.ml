let stack_size = 2048
let globals_size = 65536
let max_frames = 1024

(* Array of built-in functions, indexed by their builtin index *)
let builtins : Value.value array =
  [|
    Value.BuiltinFunction Builtins.builtin_len;
    Value.BuiltinFunction Builtins.builtin_puts;
    Value.BuiltinFunction Builtins.builtin_first;
    Value.BuiltinFunction Builtins.builtin_last;
    Value.BuiltinFunction Builtins.builtin_rest;
    Value.BuiltinFunction Builtins.builtin_push;
  |]

(* A frame represents a function call - holds a closure *)
type frame = {
  cl : Value.closure;
  mutable ip : int; (* Instruction pointer within this frame's function *)
  base_pointer : int; (* Where this frame's locals start on the stack *)
}

type vm = {
  constants : Value.value array;
  stack : Value.value array;
  globals : Value.value array;
  frames : frame array;
  mutable frame_index : int; (* Index of current frame *)
  mutable sp : int; (* Stack pointer: always points to next free slot. Top is stack[sp-1] *)
}

(* Get the current frame *)
let current_frame (vm : vm) : frame = vm.frames.(vm.frame_index)

(* Push a new frame *)
let push_frame (vm : vm) (f : frame) : unit =
  vm.frame_index <- vm.frame_index + 1;
  vm.frames.(vm.frame_index) <- f

(* Pop the current frame *)
let pop_frame (vm : vm) : frame =
  let f = vm.frames.(vm.frame_index) in
  vm.frame_index <- vm.frame_index - 1;
  f

(* Get current instructions from current frame *)
let current_instructions (vm : vm) : bytes = (current_frame vm).cl.fn.instructions

let create (bytecode : Compiler.bytecode) : vm =
  (* Wrap the main program in a "main" closure *)
  let main_fn : Value.compiled_function =
    { instructions = bytecode.instructions; num_locals = 0; num_parameters = 0 }
  in
  let main_closure : Value.closure = { fn = main_fn; free = [||] } in
  let main_frame : frame = { cl = main_closure; ip = 0; base_pointer = 0 } in
  let frames = Array.make max_frames main_frame in
  {
    constants = bytecode.constants;
    stack = Array.make stack_size Value.Null;
    globals = Array.make globals_size Value.Null;
    frames;
    frame_index = 0;
    sp = 0;
  }

(* Create VM with existing globals (for REPL continuity) *)
let create_with_globals (bytecode : Compiler.bytecode) (globals : Value.value array) : vm =
  let main_fn : Value.compiled_function =
    { instructions = bytecode.instructions; num_locals = 0; num_parameters = 0 }
  in
  let main_closure : Value.closure = { fn = main_fn; free = [||] } in
  let main_frame : frame = { cl = main_closure; ip = 0; base_pointer = 0 } in
  let frames = Array.make max_frames main_frame in
  {
    constants = bytecode.constants;
    stack = Array.make stack_size Value.Null;
    globals;
    frames;
    frame_index = 0;
    sp = 0;
  }

let push (vm : vm) (value : Value.value) : (unit, string) result =
  if vm.sp >= stack_size then
    Error "stack overflow"
  else (
    vm.stack.(vm.sp) <- value;
    vm.sp <- vm.sp + 1;
    Ok ())

let pop (vm : vm) : Value.value =
  let value = vm.stack.(vm.sp - 1) in
  vm.sp <- vm.sp - 1;
  value

(* For testing: get the last popped element (still on stack, just below sp) *)
let last_popped_stack_elem (vm : vm) : Value.value = vm.stack.(vm.sp)

let execute_binary_integer_op (op : Code.opcode) (l : int64) (r : int64) : int64 =
  match op with
  | Code.OpAdd -> Int64.add l r
  | Code.OpSub -> Int64.sub l r
  | Code.OpMul -> Int64.mul l r
  | Code.OpDiv -> Int64.div l r
  | _ -> 0L

let execute_binary_float_op (op : Code.opcode) (l : float) (r : float) : float =
  match op with
  | Code.OpAdd -> l +. r
  | Code.OpSub -> l -. r
  | Code.OpMul -> l *. r
  | Code.OpDiv -> l /. r
  | _ -> 0.0

let execute_binary_op (vm : vm) (op : Code.opcode) : unit =
  let right = pop vm in
  let left = pop vm in
  match (left, right) with
  | Value.Integer l, Value.Integer r ->
      let result = execute_binary_integer_op op l r in
      let _ = push vm (Value.Integer result) in
      ()
  | Value.Float l, Value.Float r ->
      let result = execute_binary_float_op op l r in
      let _ = push vm (Value.Float result) in
      ()
  | Value.Integer l, Value.Float r ->
      let result = execute_binary_float_op op (Int64.to_float l) r in
      let _ = push vm (Value.Float result) in
      ()
  | Value.Float l, Value.Integer r ->
      let result = execute_binary_float_op op l (Int64.to_float r) in
      let _ = push vm (Value.Float result) in
      ()
  | Value.String l, Value.String r when op = Code.OpAdd ->
      let _ = push vm (Value.String (l ^ r)) in
      ()
  | _ -> ()

(* Execute comparison operations - always returns a boolean *)
let execute_comparison (vm : vm) (op : Code.opcode) : unit =
  let right = pop vm in
  let left = pop vm in
  let result =
    match (left, right) with
    | Value.Integer l, Value.Integer r -> (
        match op with
        | Code.OpEqual -> l = r
        | Code.OpNotEqual -> l <> r
        | Code.OpGreaterThan -> l > r
        | _ -> false)
    | Value.Float l, Value.Float r -> (
        match op with
        | Code.OpEqual -> l = r
        | Code.OpNotEqual -> l <> r
        | Code.OpGreaterThan -> l > r
        | _ -> false)
    | Value.Integer l, Value.Float r -> (
        let l = Int64.to_float l in
        match op with
        | Code.OpEqual -> l = r
        | Code.OpNotEqual -> l <> r
        | Code.OpGreaterThan -> l > r
        | _ -> false)
    | Value.Float l, Value.Integer r -> (
        let r = Int64.to_float r in
        match op with
        | Code.OpEqual -> l = r
        | Code.OpNotEqual -> l <> r
        | Code.OpGreaterThan -> l > r
        | _ -> false)
    | Value.Boolean l, Value.Boolean r -> (
        match op with
        | Code.OpEqual -> l = r
        | Code.OpNotEqual -> l <> r
        | _ -> false)
    | _ -> false
  in
  let _ = push vm (Value.Boolean result) in
  ()

let run (vm : vm) : (unit, string) result =
  while vm.frame_index >= 0 && (current_frame vm).ip < Bytes.length (current_instructions vm) do
    let frame = current_frame vm in
    let ins = frame.cl.fn.instructions in
    let ip = frame.ip in
    let op_byte = Char.code (Bytes.get ins ip) in

    (* Use unsafe_of_int for fast opcode dispatch - we trust the compiler emitted valid opcodes *)
    (match Code.unsafe_of_int op_byte with
    | Code.OpConstant ->
        let const_index = Code.read_uint16 ins (ip + 1) in
        frame.ip <- frame.ip + 2;
        let _ = push vm vm.constants.(const_index) in
        ()
    | Code.OpAdd -> execute_binary_op vm Code.OpAdd
    | Code.OpSub -> execute_binary_op vm Code.OpSub
    | Code.OpMul -> execute_binary_op vm Code.OpMul
    | Code.OpDiv -> execute_binary_op vm Code.OpDiv
    | Code.OpPop ->
        let _ = pop vm in
        ()
    | Code.OpTrue ->
        let _ = push vm (Value.Boolean true) in
        ()
    | Code.OpFalse ->
        let _ = push vm (Value.Boolean false) in
        ()
    | Code.OpEqual -> execute_comparison vm Code.OpEqual
    | Code.OpNotEqual -> execute_comparison vm Code.OpNotEqual
    | Code.OpGreaterThan -> execute_comparison vm Code.OpGreaterThan
    | Code.OpMinus -> (
        (* Negate the top of the stack - works for Integer and Float *)
        let operand = pop vm in
        match operand with
        | Value.Integer i ->
            let _ = push vm (Value.Integer (Int64.neg i)) in
            ()
        | Value.Float f ->
            let _ = push vm (Value.Float (-.f)) in
            ()
        | _ -> ())
    | Code.OpBang ->
        (* Logical NOT - uses truthiness rules *)
        let operand = pop vm in
        let result = not (Value.is_truthy operand) in
        let _ = push vm (Value.Boolean result) in
        ()
    | Code.OpJump ->
        (* Unconditional jump - read 2-byte target address *)
        let target = Code.read_uint16 ins (ip + 1) in
        (* Set ip to target - 1 because the loop will do +1 *)
        frame.ip <- target - 1
    | Code.OpJumpNotTruthy ->
        (* Conditional jump - jump only if top of stack is falsy *)
        let target = Code.read_uint16 ins (ip + 1) in
        let condition = pop vm in
        if not (Value.is_truthy condition) then
          (* Jump: set ip to target - 1 *)
          frame.ip <- target - 1
        else
          (* Don't jump: skip over the 2-byte operand *)
          frame.ip <- frame.ip + 2
    | Code.OpNull ->
        let _ = push vm Value.Null in
        ()
    | Code.OpGetGlobal ->
        let global_index = Code.read_uint16 ins (ip + 1) in
        frame.ip <- frame.ip + 2;
        let _ = push vm vm.globals.(global_index) in
        ()
    | Code.OpSetGlobal ->
        let global_index = Code.read_uint16 ins (ip + 1) in
        frame.ip <- frame.ip + 2;
        vm.globals.(global_index) <- pop vm
    | Code.OpArray ->
        let num_elements = Code.read_uint16 ins (ip + 1) in
        frame.ip <- frame.ip + 2;
        (* Pop num_elements from stack, build array *)
        (* Elements are on stack in order: first element pushed first, so at bottom *)
        let elements = Array.init num_elements (fun _ -> Value.Null) in
        for i = num_elements - 1 downto 0 do
          elements.(i) <- pop vm
        done;
        let _ = push vm (Value.Array (Array.to_list elements)) in
        ()
    | Code.OpHash ->
        let num_elements = Code.read_uint16 ins (ip + 1) in
        frame.ip <- frame.ip + 2;
        (* num_elements is total count (keys + values), so num_pairs = num_elements / 2 *)
        (* Pop pairs in reverse order: val, key, val, key, ... *)
        let hash = Hashtbl.create (num_elements / 2) in
        let pairs = Array.init num_elements (fun _ -> Value.Null) in
        for i = num_elements - 1 downto 0 do
          pairs.(i) <- pop vm
        done;
        (* pairs is now [key0; val0; key1; val1; ...] *)
        let rec add_pairs i =
          if i >= num_elements then
            ()
          else
            let key = pairs.(i) in
            let value = pairs.(i + 1) in
            Hashtbl.replace hash key value;
            add_pairs (i + 2)
        in
        add_pairs 0;
        let _ = push vm (Value.Hash hash) in
        ()
    | Code.OpIndex -> (
        let index = pop vm in
        let obj = pop vm in
        match (obj, index) with
        | Value.Array elements, Value.Integer i ->
            let len = List.length elements in
            let idx = Int64.to_int i in
            let result =
              if idx >= 0 && idx < len then
                List.nth elements idx
              else if idx < 0 && idx + len >= 0 then
                List.nth elements (len + idx)
              else
                Value.Null
            in
            let _ = push vm result in
            ()
        | Value.Hash hash, key ->
            let result =
              match Hashtbl.find_opt hash key with
              | Some v -> v
              | None -> Value.Null
            in
            let _ = push vm result in
            ()
        | Value.String s, Value.Integer i ->
            let len = String.length s in
            let idx = Int64.to_int i in
            let result =
              if idx >= 0 && idx < len then
                Value.String (String.make 1 s.[idx])
              else if idx < 0 && idx + len >= 0 then
                Value.String (String.make 1 s.[len + idx])
              else
                Value.Null
            in
            let _ = push vm result in
            ()
        | _ ->
            let _ = push vm Value.Null in
            ())
    | Code.OpCall -> (
        (* Read number of arguments *)
        let num_args = Code.read_uint8 ins (ip + 1) in
        frame.ip <- frame.ip + 1;
        (* Get the function from the stack (it's below the arguments) *)
        let fn_value = vm.stack.(vm.sp - 1 - num_args) in
        match fn_value with
        | Value.Closure cl ->
            (* Verify argument count *)
            if num_args <> cl.fn.num_parameters then
              failwith (Printf.sprintf "wrong number of arguments: want=%d, got=%d" cl.fn.num_parameters num_args)
            else
              (* Create new frame with closure *)
              let base_pointer = vm.sp - num_args in
              let new_frame = { cl; ip = -1; base_pointer } in
              push_frame vm new_frame;
              (* Reserve space for locals on the stack *)
              vm.sp <- base_pointer + cl.fn.num_locals
        | Value.BuiltinFunction builtin_fn ->
            (* Collect arguments from stack *)
            let args = Array.init num_args (fun i -> vm.stack.(vm.sp - num_args + i)) in
            let args_list = Array.to_list args in
            (* Pop the arguments and the function *)
            vm.sp <- vm.sp - num_args - 1;
            (* Call the builtin function *)
            let result = builtin_fn args_list in
            (* Push the result *)
            let _ = push vm result in
            ()
        | _ -> failwith "calling non-function")
    | Code.OpReturnValue ->
        (* Pop the return value *)
        let return_value = pop vm in
        (* Pop the frame *)
        let old_frame = pop_frame vm in
        (* Restore sp to before the call (remove function and args) *)
        vm.sp <- old_frame.base_pointer - 1;
        (* Push the return value *)
        let _ = push vm return_value in
        ()
    | Code.OpReturn ->
        (* Pop the frame (return null implicitly) *)
        let old_frame = pop_frame vm in
        (* Restore sp to before the call (remove function and args) *)
        vm.sp <- old_frame.base_pointer - 1;
        (* Push null *)
        let _ = push vm Value.Null in
        ()
    | Code.OpGetLocal ->
        (* Read local index (1 byte) *)
        let local_index = Code.read_uint8 ins (ip + 1) in
        frame.ip <- frame.ip + 1;
        (* Get local from stack relative to base_pointer *)
        let _ = push vm vm.stack.(frame.base_pointer + local_index) in
        ()
    | Code.OpSetLocal ->
        (* Read local index (1 byte) *)
        let local_index = Code.read_uint8 ins (ip + 1) in
        frame.ip <- frame.ip + 1;
        (* Set local on stack relative to base_pointer *)
        vm.stack.(frame.base_pointer + local_index) <- pop vm
    | Code.OpGetBuiltin ->
        (* Read builtin index (1 byte) *)
        let builtin_index = Code.read_uint8 ins (ip + 1) in
        frame.ip <- frame.ip + 1;
        (* Push the builtin function onto the stack *)
        let _ = push vm builtins.(builtin_index) in
        ()
    | Code.OpClosure ->
        (* Read constant index (2 bytes) and number of free variables (1 byte) *)
        let const_index = Code.read_uint16 ins (ip + 1) in
        let num_free = Code.read_uint8 ins (ip + 3) in
        frame.ip <- frame.ip + 3;
        (* Get the compiled function from constants *)
        let fn =
          match vm.constants.(const_index) with
          | Value.CompiledFunction fn -> fn
          | _ -> failwith "OpClosure: expected CompiledFunction"
        in
        (* Pop free variables from stack (they were pushed in order) *)
        let free = Array.init num_free (fun i -> vm.stack.(vm.sp - num_free + i)) in
        vm.sp <- vm.sp - num_free;
        (* Create closure and push it *)
        let closure : Value.closure = { fn; free } in
        let _ = push vm (Value.Closure closure) in
        ()
    | Code.OpGetFree ->
        (* Read free variable index (1 byte) *)
        let free_index = Code.read_uint8 ins (ip + 1) in
        frame.ip <- frame.ip + 1;
        (* Get the free variable from the current closure *)
        let _ = push vm frame.cl.free.(free_index) in
        ()
    | Code.OpCurrentClosure ->
        (* Push the current closure onto the stack (for recursive calls) *)
        let _ = push vm (Value.Closure frame.cl) in
        ());

    (current_frame vm).ip <- (current_frame vm).ip + 1
  done;

  Ok ()

module Test = struct
  type vm_test = {
    input : string;
    expected : Value.value;
  }

  let run_vm_test (test : vm_test) : bool =
    match Parser.parse test.input with
    | Error _ -> false
    | Ok program -> (
        match Compiler.compile (Compiler.init ()) program with
        | Error _ -> false
        | Ok compiler -> (
            let bytecode = Compiler.bytecode compiler in
            let vm = create bytecode in
            match run vm with
            | Error _ -> false
            | Ok () ->
                let stack_elem = last_popped_stack_elem vm in
                stack_elem = test.expected))

  let%test "test_integer_arithmetic" =
    [
      { input = "1"; expected = Value.Integer 1L };
      { input = "2"; expected = Value.Integer 2L };
      { input = "1 + 2"; expected = Value.Integer 3L };
      { input = "1 - 2"; expected = Value.Integer (-1L) };
      { input = "2 * 3"; expected = Value.Integer 6L };
      { input = "6 / 2"; expected = Value.Integer 3L };
      { input = "50 / 2 * 2 + 10 - 5"; expected = Value.Integer 55L };
      { input = "5 * (2 + 10)"; expected = Value.Integer 60L };
    ]
    |> List.for_all run_vm_test

  let%test "test_float_arithmetic" =
    [
      { input = "1.5"; expected = Value.Float 1.5 };
      { input = "1.5 + 2.5"; expected = Value.Float 4.0 };
      { input = "3.0 - 1.5"; expected = Value.Float 1.5 };
      { input = "2.0 * 3.0"; expected = Value.Float 6.0 };
      { input = "6.0 / 2.0"; expected = Value.Float 3.0 };
      (* Mixed integer/float *)
      { input = "1 + 2.5"; expected = Value.Float 3.5 };
      { input = "2.5 + 1"; expected = Value.Float 3.5 };
    ]
    |> List.for_all run_vm_test

  let%test "test_boolean_expressions" =
    [ { input = "true"; expected = Value.Boolean true }; { input = "false"; expected = Value.Boolean false } ]
    |> List.for_all run_vm_test

  let%test "test_comparison_expressions" =
    [
      (* Integer comparisons *)
      { input = "1 == 1"; expected = Value.Boolean true };
      { input = "1 == 2"; expected = Value.Boolean false };
      { input = "1 != 2"; expected = Value.Boolean true };
      { input = "1 != 1"; expected = Value.Boolean false };
      { input = "1 > 2"; expected = Value.Boolean false };
      { input = "2 > 1"; expected = Value.Boolean true };
      { input = "1 < 2"; expected = Value.Boolean true };
      { input = "2 < 1"; expected = Value.Boolean false };
      (* Boolean comparisons *)
      { input = "true == true"; expected = Value.Boolean true };
      { input = "true == false"; expected = Value.Boolean false };
      { input = "true != false"; expected = Value.Boolean true };
      { input = "false != false"; expected = Value.Boolean false };
      (* Float comparisons (Marmoset addition) *)
      { input = "1.5 == 1.5"; expected = Value.Boolean true };
      { input = "1.5 > 1.0"; expected = Value.Boolean true };
      { input = "1.0 < 1.5"; expected = Value.Boolean true };
      (* Mixed int/float comparisons (Marmoset addition) *)
      { input = "1 == 1.0"; expected = Value.Boolean true };
      { input = "2 > 1.5"; expected = Value.Boolean true };
    ]
    |> List.for_all run_vm_test

  let%test "test_prefix_expressions" =
    [
      (* Minus - integer negation *)
      { input = "-5"; expected = Value.Integer (-5L) };
      { input = "-10"; expected = Value.Integer (-10L) };
      { input = "--5"; expected = Value.Integer 5L };
      { input = "-50 + 100 + -50"; expected = Value.Integer 0L };
      (* Minus - float negation (Marmoset addition) *)
      { input = "-1.5"; expected = Value.Float (-1.5) };
      { input = "--2.5"; expected = Value.Float 2.5 };
      (* Bang - logical NOT *)
      { input = "!true"; expected = Value.Boolean false };
      { input = "!false"; expected = Value.Boolean true };
      { input = "!!true"; expected = Value.Boolean true };
      { input = "!!false"; expected = Value.Boolean false };
      (* Bang with non-boolean values (truthiness) *)
      { input = "!5"; expected = Value.Boolean false };
      { input = "!!5"; expected = Value.Boolean true };
    ]
    |> List.for_all run_vm_test

  let%test "test_conditionals" =
    [
      (* Basic if with truthy condition *)
      { input = "if (true) { 10 }"; expected = Value.Integer 10L };
      { input = "if (true) { 10 } else { 20 }"; expected = Value.Integer 10L };
      { input = "if (false) { 10 } else { 20 }"; expected = Value.Integer 20L };
      (* If without else when condition is falsy returns null *)
      { input = "if (false) { 10 }"; expected = Value.Null };
      (* Truthy integer conditions *)
      { input = "if (1) { 10 }"; expected = Value.Integer 10L };
      { input = "if (1 < 2) { 10 }"; expected = Value.Integer 10L };
      { input = "if (1 > 2) { 10 }"; expected = Value.Null };
      { input = "if (1 > 2) { 10 } else { 20 }"; expected = Value.Integer 20L };
      (* Complex conditions *)
      { input = "if (1 < 2) { 10 } else { 20 }"; expected = Value.Integer 10L };
      { input = "if (1 == 1) { 10 }"; expected = Value.Integer 10L };
      { input = "if (1 != 1) { 10 } else { 20 }"; expected = Value.Integer 20L };
      (* Nested expressions in consequence/alternative *)
      { input = "if (true) { 1 + 2 }"; expected = Value.Integer 3L };
      { input = "if (false) { 1 } else { 2 + 3 }"; expected = Value.Integer 5L };
    ]
    |> List.for_all run_vm_test

  let%test "test_global_let_statements" =
    [
      (* Simple binding *)
      { input = "let one = 1; one"; expected = Value.Integer 1L };
      { input = "let one = 1; let two = 2; one + two"; expected = Value.Integer 3L };
      { input = "let one = 1; let two = one + one; one + two"; expected = Value.Integer 3L };
      (* Float bindings (Marmoset) *)
      { input = "let x = 1.5; x"; expected = Value.Float 1.5 };
      { input = "let x = 1.5; let y = 2.5; x + y"; expected = Value.Float 4.0 };
      (* Boolean bindings *)
      { input = "let t = true; t"; expected = Value.Boolean true };
      { input = "let f = false; !f"; expected = Value.Boolean true };
      (* Expression in binding *)
      { input = "let x = 5 * 5; x"; expected = Value.Integer 25L };
      { input = "let x = if (true) { 10 } else { 20 }; x"; expected = Value.Integer 10L };
    ]
    |> List.for_all run_vm_test

  let%test "test_string_expressions" =
    [
      (* Simple string literals *)
      { input = "\"monkey\""; expected = Value.String "monkey" };
      { input = "\"mon\" + \"key\""; expected = Value.String "monkey" };
      { input = "\"mon\" + \"key\" + \"banana\""; expected = Value.String "monkeybanana" };
      (* String in let binding *)
      { input = "let s = \"hello\"; s"; expected = Value.String "hello" };
      { input = "let a = \"hello\"; let b = \" world\"; a + b"; expected = Value.String "hello world" };
    ]
    |> List.for_all run_vm_test

  let%test "test_array_literals" =
    [
      (* Empty array *)
      { input = "[]"; expected = Value.Array [] };
      (* Array with integers *)
      { input = "[1, 2, 3]"; expected = Value.Array [ Value.Integer 1L; Value.Integer 2L; Value.Integer 3L ] };
      (* Array with expressions *)
      {
        input = "[1 + 2, 3 * 4, 5 + 6]";
        expected = Value.Array [ Value.Integer 3L; Value.Integer 12L; Value.Integer 11L ];
      };
      (* Array with mixed types *)
      {
        input = "[1, \"two\", true]";
        expected = Value.Array [ Value.Integer 1L; Value.String "two"; Value.Boolean true ];
      };
      (* Nested arrays *)
      {
        input = "[[1, 2], [3, 4]]";
        expected =
          Value.Array
            [
              Value.Array [ Value.Integer 1L; Value.Integer 2L ];
              Value.Array [ Value.Integer 3L; Value.Integer 4L ];
            ];
      };
    ]
    |> List.for_all run_vm_test

  (* Helper to create expected hash values *)
  let make_hash (pairs : (Value.value * Value.value) list) : Value.value =
    let h = Hashtbl.create (List.length pairs) in
    List.iter (fun (k, v) -> Hashtbl.replace h k v) pairs;
    Value.Hash h

  let%test "test_hash_literals" =
    [
      (* Empty hash *)
      { input = "{}"; expected = make_hash [] };
      (* Hash with integer keys *)
      { input = "{1: 2}"; expected = make_hash [ (Value.Integer 1L, Value.Integer 2L) ] };
      (* Hash with multiple pairs *)
      {
        input = "{1: 2, 3: 4}";
        expected = make_hash [ (Value.Integer 1L, Value.Integer 2L); (Value.Integer 3L, Value.Integer 4L) ];
      };
      (* Hash with expressions as values *)
      { input = "{1: 2 + 3}"; expected = make_hash [ (Value.Integer 1L, Value.Integer 5L) ] };
      (* Hash with string keys *)
      { input = "{\"name\": \"monkey\"}"; expected = make_hash [ (Value.String "name", Value.String "monkey") ] };
    ]
    |> List.for_all run_vm_test

  let%test "test_index_expressions" =
    [
      (* Array indexing - basic *)
      { input = "[1, 2, 3][0]"; expected = Value.Integer 1L };
      { input = "[1, 2, 3][1]"; expected = Value.Integer 2L };
      { input = "[1, 2, 3][2]"; expected = Value.Integer 3L };
      (* Array indexing - out of bounds returns null *)
      { input = "[1, 2, 3][99]"; expected = Value.Null };
      (* Array indexing - negative indices (Marmoset extension) *)
      { input = "[1, 2, 3][-1]"; expected = Value.Integer 3L };
      { input = "[1, 2, 3][-2]"; expected = Value.Integer 2L };
      { input = "[1, 2, 3][-3]"; expected = Value.Integer 1L };
      { input = "[1, 2, 3][-4]"; expected = Value.Null };
      (* Array with expressions *)
      { input = "let arr = [1, 2, 3]; arr[0]"; expected = Value.Integer 1L };
      { input = "let arr = [1, 2, 3]; arr[1 + 1]"; expected = Value.Integer 3L };
      (* Hash indexing *)
      { input = "{1: 2}[1]"; expected = Value.Integer 2L };
      { input = "{1: 2, 3: 4}[3]"; expected = Value.Integer 4L };
      { input = "{1: 2}[0]"; expected = Value.Null };
      { input = "{\"foo\": \"bar\"}[\"foo\"]"; expected = Value.String "bar" };
      (* String indexing (Marmoset extension) *)
      { input = "\"hello\"[0]"; expected = Value.String "h" };
      { input = "\"hello\"[1]"; expected = Value.String "e" };
      { input = "\"hello\"[4]"; expected = Value.String "o" };
      { input = "\"hello\"[5]"; expected = Value.Null };
      (* String negative indexing (Marmoset extension) *)
      { input = "\"hello\"[-1]"; expected = Value.String "o" };
      { input = "\"hello\"[-5]"; expected = Value.String "h" };
      { input = "\"hello\"[-6]"; expected = Value.Null };
    ]
    |> List.for_all run_vm_test

  let%test "test_calling_functions_without_arguments" =
    [
      (* Simple function that returns constant *)
      { input = "let fivePlusTen = fn() { 5 + 10 }; fivePlusTen();"; expected = Value.Integer 15L };
      (* Function assigned to another variable *)
      { input = "let one = fn() { 1 }; let two = fn() { 2 }; one() + two()"; expected = Value.Integer 3L };
      (* Nested function calls *)
      {
        input = "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c()";
        expected = Value.Integer 3L;
      };
    ]
    |> List.for_all run_vm_test

  let%test "test_functions_with_return_statement" =
    [
      (* Early return *)
      { input = "let earlyReturn = fn() { return 99; 100; }; earlyReturn();"; expected = Value.Integer 99L };
      (* Return in if *)
      { input = "let f = fn() { if (true) { return 10; } return 20; }; f();"; expected = Value.Integer 10L };
    ]
    |> List.for_all run_vm_test

  let%test "test_functions_without_return_value" =
    [
      (* Empty function body returns null *)
      { input = "let noReturn = fn() { }; noReturn();"; expected = Value.Null };
      (* Expression statement still becomes the return value in Monkey *)
      { input = "let f = fn() { 1; }; f();"; expected = Value.Integer 1L };
    ]
    |> List.for_all run_vm_test

  let%test "test_first_class_functions" =
    [
      (* Return function from function *)
      {
        input =
          "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();";
        expected = Value.Integer 1L;
      };
    ]
    |> List.for_all run_vm_test

  let%test "test_calling_functions_with_bindings" =
    [
      (* Local bindings in function *)
      { input = "let one = fn() { let one = 1; one }; one();"; expected = Value.Integer 1L };
      {
        input = "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();";
        expected = Value.Integer 3L;
      };
      {
        input =
          "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; let threeAndFour = fn() { let three = 3; let four = 4; three + four; }; oneAndTwo() + threeAndFour();";
        expected = Value.Integer 10L;
      };
      (* Local shadows global *)
      {
        input =
          "let globalSeed = 50; let minusOne = fn() { let num = 1; globalSeed - num; }; let minusTwo = fn() { let num = 2; globalSeed - num; }; minusOne() + minusTwo();";
        expected = Value.Integer 97L;
      };
    ]
    |> List.for_all run_vm_test

  let%test "test_calling_functions_with_arguments_and_bindings" =
    [
      (* Single argument *)
      { input = "let identity = fn(a) { a; }; identity(4);"; expected = Value.Integer 4L };
      (* Multiple arguments *)
      { input = "let sum = fn(a, b) { a + b; }; sum(1, 2);"; expected = Value.Integer 3L };
      { input = "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);"; expected = Value.Integer 3L };
      (* Mixing args and locals *)
      { input = "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2) + sum(3, 4);"; expected = Value.Integer 10L };
      (* Nested function with arguments *)
      {
        input =
          "let globalNum = 10; let sum = fn(a, b) { let c = a + b; c + globalNum; }; let outer = fn() { sum(1, 2) + sum(3, 4) + globalNum; }; outer() + globalNum;";
        expected = Value.Integer 50L;
      };
    ]
    |> List.for_all run_vm_test

  let%test "test_calling_functions_with_wrong_arguments" =
    (* This should fail - test that it raises an exception *)
    let input = "let f = fn(a) { a; }; f();" in
    match Parser.parse input with
    | Error _ -> false
    | Ok program -> (
        match Compiler.compile (Compiler.init ()) program with
        | Error _ -> false
        | Ok compiler -> (
            let bytecode = Compiler.bytecode compiler in
            let vm = create bytecode in
            try
              let _ = run vm in
              false (* Should have raised *)
            with _ -> true))

  (* Note: Recursive functions require Chapter 9's closure support (FunctionScope + OpCurrentClosure).
     The following test demonstrates that calling a previously defined function works. *)
  let%test "test_calling_previously_defined_functions" =
    [
      (* Function calling another function defined earlier *)
      { input = "let base = fn(n) { n }; let f = fn(n) { base(n - 1) }; f(5);"; expected = Value.Integer 4L };
      { input = "let a = fn() { 1 }; let b = fn() { a() + 1 }; b();"; expected = Value.Integer 2L };
    ]
    |> List.for_all run_vm_test

  let%test "test_builtin_functions" =
    [
      (* len *)
      { input = "len(\"\")"; expected = Value.Integer 0L };
      { input = "len(\"four\")"; expected = Value.Integer 4L };
      { input = "len(\"hello world\")"; expected = Value.Integer 11L };
      { input = "len([1, 2, 3])"; expected = Value.Integer 3L };
      { input = "len([])"; expected = Value.Integer 0L };
      (* first *)
      { input = "first([1, 2, 3])"; expected = Value.Integer 1L };
      { input = "first([])"; expected = Value.Null };
      (* last *)
      { input = "last([1, 2, 3])"; expected = Value.Integer 3L };
      { input = "last([])"; expected = Value.Null };
      (* rest *)
      { input = "rest([1, 2, 3])"; expected = Value.Array [ Value.Integer 2L; Value.Integer 3L ] };
      { input = "rest([])"; expected = Value.Null };
      (* push *)
      { input = "push([], 1)"; expected = Value.Array [ Value.Integer 1L ] };
      {
        input = "push([1, 2], 3)";
        expected = Value.Array [ Value.Integer 1L; Value.Integer 2L; Value.Integer 3L ];
      };
    ]
    |> List.for_all run_vm_test

  let%test "test_builtin_functions_in_expressions" =
    [
      (* Using builtins in more complex expressions *)
      { input = "len([1, 2, 3]) + 5"; expected = Value.Integer 8L };
      { input = "let arr = [1, 2, 3]; len(arr)"; expected = Value.Integer 3L };
      { input = "let arr = [1, 2, 3]; first(rest(arr))"; expected = Value.Integer 2L };
      {
        input = "let arr = push([], 1); push(arr, 2)";
        expected = Value.Array [ Value.Integer 1L; Value.Integer 2L ];
      };
    ]
    |> List.for_all run_vm_test

  let%test "test_closures" =
    [
      (* Simple closure - inner function captures outer parameter *)
      {
        input = "let newClosure = fn(a) { fn() { a; }; }; let closure = newClosure(99); closure();";
        expected = Value.Integer 99L;
      };
      (* Classic newAdder example *)
      {
        input = "let newAdder = fn(a) { fn(b) { a + b; }; }; let addTwo = newAdder(2); addTwo(3);";
        expected = Value.Integer 5L;
      };
      (* Multiple closures from same outer function *)
      {
        input =
          "let newAdder = fn(a) { fn(b) { a + b; }; }; let addTwo = newAdder(2); let addThree = newAdder(3); addTwo(1) + addThree(1);";
        expected = Value.Integer 7L;
      };
      (* Nested closures - deeper nesting *)
      {
        input =
          "let newAdderOuter = fn(a) { fn(b) { fn(c) { a + b + c; }; }; }; let newAdderInner = newAdderOuter(1); let adder = newAdderInner(2); adder(3);";
        expected = Value.Integer 6L;
      };
      (* Closure capturing local variable *)
      {
        input = "let newClosure = fn(a) { let b = 10; fn() { a + b; }; }; let closure = newClosure(5); closure();";
        expected = Value.Integer 15L;
      };
    ]
    |> List.for_all run_vm_test

  let%test "test_recursive_functions" =
    [
      (* Recursive countdown *)
      {
        input = "let countDown = fn(x) { if (x == 0) { return 0; } countDown(x - 1); }; countDown(5);";
        expected = Value.Integer 0L;
      };
      (* Recursive fibonacci *)
      {
        input = "let fib = fn(n) { if (n < 2) { return n; } fib(n - 1) + fib(n - 2); }; fib(10);";
        expected = Value.Integer 55L;
      };
      (* Wrapper function calling recursive inner function *)
      {
        input =
          "let wrapper = fn() { let countDown = fn(x) { if (x == 0) { return 0; } countDown(x - 1); }; countDown(1); }; wrapper();";
        expected = Value.Integer 0L;
      };
    ]
    |> List.for_all run_vm_test
end
