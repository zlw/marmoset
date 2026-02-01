let stack_size = 2048

type vm = {
  constants : Value.value array;
  instructions : Code.instructions;
  stack : Value.value array;
  mutable sp : int; (* Stack pointer: always points to next free slot. Top is stack[sp-1] *)
  mutable ip : int; (* Instruction pointer: current position in instructions *)
}

let create (bytecode : Compiler.bytecode) : vm =
  {
    instructions = bytecode.instructions;
    constants = bytecode.constants;
    stack = Array.make stack_size Value.Null;
    sp = 0;
    ip = 0;
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
  let len = Bytes.length vm.instructions in
  vm.ip <- 0;

  while vm.ip < len do
    let op_byte = Char.code (Bytes.get vm.instructions vm.ip) in

    (match Code.of_int op_byte with
    | Some Code.OpConstant ->
        let const_index = Code.read_uint16 vm.instructions (vm.ip + 1) in
        vm.ip <- vm.ip + 2;
        let _ = push vm vm.constants.(const_index) in
        ()
    | Some Code.OpAdd -> execute_binary_op vm Code.OpAdd
    | Some Code.OpSub -> execute_binary_op vm Code.OpSub
    | Some Code.OpMul -> execute_binary_op vm Code.OpMul
    | Some Code.OpDiv -> execute_binary_op vm Code.OpDiv
    | Some Code.OpPop ->
        let _ = pop vm in
        ()
    | Some Code.OpTrue ->
        let _ = push vm (Value.Boolean true) in
        ()
    | Some Code.OpFalse ->
        let _ = push vm (Value.Boolean false) in
        ()
    | Some Code.OpEqual -> execute_comparison vm Code.OpEqual
    | Some Code.OpNotEqual -> execute_comparison vm Code.OpNotEqual
    | Some Code.OpGreaterThan -> execute_comparison vm Code.OpGreaterThan
    | Some Code.OpMinus -> (
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
    | Some Code.OpBang ->
        (* Logical NOT - uses truthiness rules *)
        let operand = pop vm in
        let result = not (Value.is_truthy operand) in
        let _ = push vm (Value.Boolean result) in
        ()
    | None -> ());

    vm.ip <- vm.ip + 1
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
        match Compiler.compile Compiler.init program with
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
end
