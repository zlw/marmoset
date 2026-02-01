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
    | Some Code.OpAdd -> (
        let right = pop vm in
        let left = pop vm in
        match (left, right) with
        | Value.Integer l, Value.Integer r ->
            let _ = push vm (Value.Integer (Int64.add l r)) in
            ()
        | _ -> ())
    | Some Code.OpPop ->
        let _ = pop vm in
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
    ]
    |> List.for_all run_vm_test
end
