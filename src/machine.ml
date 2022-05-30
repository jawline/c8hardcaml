open! Core
open! Hardcaml
open! Signal
open Global

module States = struct
  type t =
    | Fetch_op
    | Execute
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t =
    { program : 'a [@bits 1]
    ; program_pc : 'a [@bits 12]
    ; program_write_enable : 'a [@bits 1]
    ; program_address : 'a [@bits 16]
    ; program_data : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { main_pc : 'a [@bits 12]
    ; op : 'a [@bits 16]
    ; in_execute : 'a [@bits 1]
    ; program_read_address : 'a [@bits 16]
    ; program_read_data : 'a [@bits 8]
    ; executor_state : 'a Executor.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

let create (i : 'a I.t) : 'a O.t =
  let open Always in
  let program_counter = Variable.reg ~enable:vdd ~width:12 r_sync in
  let program_pointer = Variable.reg ~enable:vdd ~width:12 r_sync in
  let op_first = Variable.reg ~enable:vdd ~width:8 r_sync in
  let op_second = Variable.reg ~enable:vdd ~width:8 r_sync in
  let op = Variable.wire ~default:(Signal.of_int ~width:16 0) in
  let sp = Variable.reg ~enable:vdd ~width:32 r_sync in
  let executor_enable = Variable.wire ~default:(Signal.of_int ~width:1 0) in
  let program_registers =
    Sequence.range 0 16
    |> Sequence.map ~f:(fun _ -> Variable.reg ~enable:vdd ~width:8 r_sync)
    |> Sequence.to_list
  in
  let executor =
    Executor.create
      { Executor.I.input_pc = program_counter.value
      ; input_i = program_pointer.value
      ; input_sp = sp.value
      ; input_registers = List.map ~f:(fun v -> v.value) program_registers
      ; begin_ = executor_enable.value
      ; opcode = op.value
      }
  in
  let write_enable = Variable.reg ~enable:vdd ~width:1 r_sync in
  let write_address = Variable.reg ~enable:vdd ~width:word_size r_sync in
  let write_data = Variable.reg ~enable:vdd ~width:byte_size r_sync in
  let read_address = Variable.wire ~default:(Signal.of_int ~width:16 0) in
  let read_data =
    machine_ram
      ~write_enable:write_enable.value
      ~write_address:write_address.value
      ~write_data:write_data.value
      ~read_address:read_address.value
  in
  let state = State_machine.create (module States) ~enable:vdd r_sync in
  let program_read_data = Variable.reg ~enable:vdd ~width:8 r_sync in
  let fetch_cycle = Variable.reg ~enable:vdd ~width:2 r_sync in
  let runner =
    [ program_read_data <-- read_data
    ; op <-- concat_msb [ op_first.value; op_second.value ]
    ; state.switch
        [ ( Fetch_op
          , [ when_
                (fetch_cycle.value ==:. 0)
                [ read_address <-- uresize program_counter.value 16 ]
            ; when_ (fetch_cycle.value ==:. 1) [ op_first <-- read_data ]
            ; when_
                (fetch_cycle.value ==:. 2)
                [ read_address <-- uresize (program_counter.value +:. 1) 16 ]
            ; when_
                (fetch_cycle.value ==:. 3)
                [ op_second <-- read_data; state.set_next Execute ]
            ; fetch_cycle <-- fetch_cycle.value +:. 1
            ] )
        ; ( Execute
          , [ executor_enable <--. 1
            ; when_
                executor.done_
                [ program_counter <-- executor.pc
                ; program_pointer <-- executor.i
                ; sp <-- executor.sp
                ; proc
                    (List.zip_exn program_registers executor.registers
                    |> List.map ~f:(fun (register, result) -> register <-- result))
                ; state.set_next Fetch_op
                ]
            ] )
        ]
    ]
  in
  let programmer =
    [ if_
        (i.program ==:. 1)
        [ program_read_data <-- read_data
        ; read_address <-- i.program_address
        ; write_enable <-- i.program_write_enable
        ; write_address <-- i.program_address
        ; write_data <-- i.program_data
        ; program_counter <-- i.program_pc
        ]
        runner
    ]
  in
  compile programmer;
  { O.program_read_data = program_read_data.value
  ; program_read_address = read_address.value
  ; op = op.value
  ; in_execute = executor_enable.value
  ; main_pc = program_counter.value
  ; executor_state = executor
  }
;;

module Test = struct
  let sim_set_write_ram sim (i : _ I.t) (o : _ O.t) addr data =
    i.program := Bits.of_int ~width:1 1;
    i.program_write_enable := Bits.of_int ~width:1 1;
    i.program_address := Bits.of_int ~width:word_size addr;
    i.program_data := Bits.of_int ~width:byte_size data;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    print_s [%message (Bits.to_int !(o.program_read_data) : int)]
  ;;

  let sim_read_addr sim (i : _ I.t) (o : _ O.t) addr =
    i.program := Bits.of_int ~width:1 1;
    i.program_write_enable := Bits.of_int ~width:1 0;
    i.program_address := Bits.of_int ~width:word_size addr;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    print_s [%message (Bits.to_int !(o.program_read_data) : int)]
  ;;

  let dump v = Bits.to_int !(v)

  let sim_cycle_not_programming sim (i : _ I.t) (o : _ O.t) =
    i.program := Bits.of_int ~width:1 0;
    Cyclesim.cycle sim;
    print_s
      [%message
        ""
          ~op:(Bits.to_int !(o.op) : int)
          ~pc:(Bits.to_int !(o.main_pc) : int)
          ~program_read_data:(Bits.to_int !(o.program_read_data) : int)
          ~program_read_address:(Bits.to_int !(o.program_read_address) : int)
          ~in_execute:(dump o.in_execute : int)
          ~executor_pc:(dump o.executor_state.pc : int)
          ~executor_i:(dump o.executor_state.i : int)
          ~executor_done:(dump o.executor_state.done_ : int)
          ~executor_registers:(List.map o.executor_state.registers ~f:dump : int list)
    ]
  ;;

  let test ~create =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    sim_set_write_ram sim inputs outputs 0 50;
    sim_set_write_ram sim inputs outputs 1 48;
    sim_set_write_ram sim inputs outputs 2 1;
    sim_set_write_ram sim inputs outputs 3 1;
    sim_set_write_ram sim inputs outputs 4 51;
    sim_read_addr sim inputs outputs 0;
    sim_read_addr sim inputs outputs 1;
    sim_read_addr sim inputs outputs 2;
    sim_read_addr sim inputs outputs 3;
    sim_read_addr sim inputs outputs 4;
    sim_read_addr sim inputs outputs 5;
    (* Simulate the program we just wrote running for 100 cycles *)
    Sequence.range 0 100 |> Sequence.iter ~f:(fun _ ->
            sim_cycle_not_programming sim inputs outputs);
    ()
  ;;

  let%expect_test "program memory" =
    test ~create;
    [%expect {|
      ("Bits.to_int (!(o.program_read_data))" 50)
      ("Bits.to_int (!(o.program_read_data))" 48)
      ("Bits.to_int (!(o.program_read_data))" 1)
      ("Bits.to_int (!(o.program_read_data))" 1)
      ("Bits.to_int (!(o.program_read_data))" 51)
      ("Bits.to_int (!(o.program_read_data))" 50)
      ("Bits.to_int (!(o.program_read_data))" 48)
      ("Bits.to_int (!(o.program_read_data))" 1)
      ("Bits.to_int (!(o.program_read_data))" 1)
      ("Bits.to_int (!(o.program_read_data))" 51)
      ("Bits.to_int (!(o.program_read_data))" 0)
      ((op 0) (pc 0) (program_read_data 0) (program_read_address 0) (in_execute 0)
       (executor_pc 0) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 12800) (pc 0) (program_read_data 50) (program_read_address 1)
       (in_execute 0) (executor_pc 0) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 12800) (pc 0) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 0) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 12848) (pc 0) (program_read_data 48) (program_read_address 0)
       (in_execute 1) (executor_pc 0) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 12848) (pc 0) (program_read_data 50) (program_read_address 0)
       (in_execute 1) (executor_pc 0) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 12848) (pc 0) (program_read_data 50) (program_read_address 0)
       (in_execute 1) (executor_pc 2) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 12848) (pc 2) (program_read_data 50) (program_read_address 2)
       (in_execute 0) (executor_pc 0) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 12848) (pc 2) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 2) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 304) (pc 2) (program_read_data 1) (program_read_address 3)
       (in_execute 0) (executor_pc 2) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 304) (pc 2) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 2) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 2) (program_read_data 1) (program_read_address 0)
       (in_execute 1) (executor_pc 2) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 2) (program_read_data 50) (program_read_address 2)
       (in_execute 0) (executor_pc 2) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 2) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 2) (program_read_data 1) (program_read_address 3)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 2) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 2) (program_read_data 1) (program_read_address 0)
       (in_execute 1) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 4) (program_read_data 50) (program_read_address 4)
       (in_execute 0) (executor_pc 2) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 257) (pc 4) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13057) (pc 4) (program_read_data 51) (program_read_address 5)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13057) (pc 4) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 4) (program_read_data 0) (program_read_address 0)
       (in_execute 1) (executor_pc 4) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 4) (program_read_data 50) (program_read_address 4)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 4) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 4) (program_read_data 51) (program_read_address 5)
       (in_execute 0) (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 4) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 4) (program_read_data 0) (program_read_address 0)
       (in_execute 1) (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 8) (program_read_data 50) (program_read_address 8)
       (in_execute 0) (executor_pc 4) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 13056) (pc 8) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 0) (program_read_address 9) (in_execute 0)
       (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 50) (program_read_address 0) (in_execute 0)
       (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 8) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 50) (program_read_address 8) (in_execute 0)
       (executor_pc 8) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 50) (program_read_address 0) (in_execute 0)
       (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 0) (program_read_address 9) (in_execute 0)
       (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 50) (program_read_address 0) (in_execute 0)
       (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 8) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 50) (program_read_address 10)
       (in_execute 0) (executor_pc 8) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 0) (program_read_address 11)
       (in_execute 0) (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 10) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 50) (program_read_address 10)
       (in_execute 0) (executor_pc 10) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 0) (program_read_address 11)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 10) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 50) (program_read_address 12)
       (in_execute 0) (executor_pc 10) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 0) (program_read_address 13)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 12) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 50) (program_read_address 12)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 0) (program_read_address 13)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 12) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 50) (program_read_address 14)
       (in_execute 0) (executor_pc 12) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 0) (program_read_address 15)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 14) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 50) (program_read_address 14)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 0) (program_read_address 15)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 14) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 50) (program_read_address 16)
       (in_execute 0) (executor_pc 14) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 0) (program_read_address 17)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 16) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 50) (program_read_address 16)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 0) (program_read_address 17)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 16) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 50) (program_read_address 18)
       (in_execute 0) (executor_pc 16) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 0) (program_read_address 19)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 18) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 50) (program_read_address 18)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 0) (program_read_address 19)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 18) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 50) (program_read_address 20)
       (in_execute 0) (executor_pc 18) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 0) (program_read_address 21)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 20) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 50) (program_read_address 20)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 22) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 0) (program_read_address 21)
       (in_execute 0) (executor_pc 22) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 22) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 20) (program_read_data 0) (program_read_address 0) (in_execute 1)
       (executor_pc 22) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 22) (program_read_data 50) (program_read_address 22)
       (in_execute 0) (executor_pc 20) (executor_i 0) (executor_done 0)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 22) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 22) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 22) (program_read_data 0) (program_read_address 23)
       (in_execute 0) (executor_pc 22) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      ((op 0) (pc 22) (program_read_data 50) (program_read_address 0)
       (in_execute 0) (executor_pc 22) (executor_i 0) (executor_done 1)
       (executor_registers (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))) |}]
  ;;
end
