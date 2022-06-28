open Core
open Hardcaml
open C8.Programmable_cpu_core
open C8.Global
open Helper

let no_op = 0b0000_0000_0000_0000
let assign_v0_2 = 0b0110000000000010
let assign_v0_3 = 0b0110000000000011
let assign_v1_1 = 0b0110000100000001
let xor_v1_random = 0b1100000100000000
let add_v0_5 = 0b0111000000000101
let assign_v6_255 = 0b0110011011111111
let assign_v0_five = 0b0110_0000_00000101
let assign_v0_four = 0b0110_0000_00000100
let assign_v1_five = 0b0110_0001_00000101
let skip_if_v0_five = 0b0011_0000_00000101
let skip_if_v0_not_five = 0b0100_0000_00000101
let skip_if_v0_eq_v1 = 0b0101_0000_0001_0000
let assign_v0_v1 = 0b1000_0000_0001_0000
let or_v0_v1 = 0b1000_0000_0001_0001
let and_v0_v1 = 0b1000_0000_0001_0010
let xor_v0_v1 = 0b1000_0000_0001_0011
let jump_to_512 = 0b0001001000000000
let jump_to_1024 = 0b0001010000000000
let jump_to_1 = 0b0001000000000001
let add_i_by_v1 = 0b1111000100011110
let assign_v0_1 = 0b0110000000000001
let assign_v1_2 = 0b0110000100000010
let assign_v2_3 = 0b0110001000000011
let assign_v3_4 = 0b0110001100000100
let assign_v4_5 = 0b0110010000000101
let assign_v0_0 = 0b0110000000000000
let assign_v1_0 = 0b0110000100000000
let assign_v2_0 = 0b0110001000000000
let assign_v3_0 = 0b0110001100000000
let assign_v4_0 = 0b0110100000000000
let assign_v0_9 = 0b0110000000001001
let reg_dump_all_registers = 0b1111_1111_01010101
let reg_dump_first_three_registers = 0b1111_0010_01010101
let reg_load_all_registers = 0b1111_1111_01100101
let load_sprite_to_i = 0b1111_0000_00101001

let bounded_standard_stop ?(max = 1000) () =
  let executed = ref 0 in
  fun error done_ ->
    executed := !executed + 1;
    !executed = max || error <> 0 || done_ <> 0
;;

let test_with_range ~opcodes ~stop_when ~range:(range_start, range_stop) =
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let sim = Simulator.create (create ~spec:r_sync) in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  let print = false in
  let rom = make_rom_of_opcodes ~opcodes in
  if print
  then (
    List.iter ~f:(printf "%x ") rom;
    printf "\n");
  sim_program_rom sim inputs ~rom;
  List.iter opcodes ~f:(fun _ ->
      let step () =
        sim_cycle_not_programming sim inputs outputs ~print;
        stop_when
          (Bits.to_int !(outputs.core.executor_error))
          (Bits.to_int !(outputs.core.executor_done))
      in
      let rec until ~f = if f () then () else until ~f in
      until ~f:step);
  (* When we stop on done the registers have just been set so run one more cycle so that their new state is reflected in output *)
  sim_cycle_not_programming sim inputs outputs ~print;
  let range = sim_read_memory_range sim inputs outputs range_start range_stop in
  ( !(outputs.core.registers.pc)
  , !(outputs.core.executor_error)
  , outputs.core.registers
  , range )
;;

let test ~opcodes ~stop_when =
  let pc, error, registers, _ = test_with_range ~opcodes ~stop_when ~range:(0, 0) in
  pc, error, registers
;;

let print_registers ~(registers : _ C8.Registers.In_circuit.t) =
  let as_strings =
    [ sprintf "I:%s" (Bits.to_string !(registers.i)) ]
    @ List.mapi registers.registers ~f:(fun i register ->
          sprintf "V%i:%s" i (Bits.to_string !register))
  in
  Core.print_s [%message "" ~_:(as_strings : string list)]
;;

let%expect_test "step (enabled)" =
  let pc, _error, _registers =
    test ~opcodes:[ no_op ] ~stop_when:(bounded_standard_stop ())
  in
  let pc = Bits.to_int pc in
  Core.print_s [%message (pc : int)];
  [%expect {|
    "TODO: Clear screen (make a Memcpy module?)"
    (pc 514) |}]
;;

let%expect_test "step (jump) to 1024" =
  let pc, error, _ =
    test ~opcodes:[ jump_to_1024 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  [%expect
    {|
    "TODO: Clear screen (make a Memcpy module?)"
    ((pc 1024) (error 0)) |}]
;;

let%expect_test "step (jump) to 512" =
  let pc, error, _ =
    test ~opcodes:[ jump_to_512 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  [%expect
    {|
    "TODO: Clear screen (make a Memcpy module?)"
    ((pc 512) (error 0)) |}]
;;

let%expect_test "step (jump) to 1" =
  let pc, error, _ = test ~opcodes:[ jump_to_1 ] ~stop_when:(bounded_standard_stop ()) in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  [%expect {|
    "TODO: Clear screen (make a Memcpy module?)"
    ((pc 1) (error 0)) |}]
;;

let%expect_test "assign V0 to 1" =
  let pc, error, registers =
    test ~opcodes:[ assign_v0_1 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 514) (error 0))
      (I:000000000000 V0:00000001 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign V1 to 2" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_2 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 514) (error 0))
      (I:000000000000 V0:00000000 V1:00000010 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign V2 to 3" =
  let pc, error, registers =
    test ~opcodes:[ assign_v2_3 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 514) (error 0))
      (I:000000000000 V0:00000000 V1:00000000 V2:00000011 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign V6 to max_int (255)" =
  let pc, error, registers =
    test ~opcodes:[ assign_v6_255 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 514) (error 0))
      (I:000000000000 V0:00000000 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:11111111 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test skip if equal on equal value" =
  let pc, error, registers =
    test
      ~opcodes:[ assign_v0_five; skip_if_v0_five ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 518) (error 0))
      (I:000000000000 V0:00000101 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test skip if equal on non-equal value" =
  (* We expect a PC of 4 because the skip path should
     not be taken and there are 2 (size of 4) preceding
     instructions. *)
  let pc, error, registers =
    test
      ~opcodes:[ assign_v0_four; skip_if_v0_five ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0))
      (I:000000000000 V0:00000100 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test skip if not equal on equal value" =
  (* We expect a PC of 4 because the skip path should
     not be taken and there are 2 (size of 4) preceding
     instructions. *)
  let pc, error, registers =
    test
      ~opcodes:[ assign_v0_five; skip_if_v0_not_five ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0))
      (I:000000000000 V0:00000101 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test skip if v0 = v1 when v0 = v1" =
  (* We expect a PC of 8 because the skip path should
     be taken and there are 3 (size of 6) preceding
     instructions. *)
  let pc, error, registers =
    test
      ~opcodes:[ assign_v0_five; assign_v1_five; skip_if_v0_eq_v1 ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 520) (error 0))
      (I:000000000000 V0:00000101 V1:00000101 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test skip if v0 = v1 when v0 <> v1" =
  (* We expect a PC of 6 because the skip path should
     be taken and there are 3 (size of 6) preceding
     instructions. *)
  let pc, error, registers =
    test
      ~opcodes:[ assign_v0_four; assign_v1_five; skip_if_v0_eq_v1 ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 518) (error 0))
      (I:000000000000 V0:00000100 V1:00000101 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "add 5 to v0 twices" =
  let pc, error, registers =
    test ~opcodes:[ add_v0_5; add_v0_5 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0))
      (I:000000000000 V0:00001010 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign 4 to v0 then add 5 to v0" =
  let pc, error, registers =
    test ~opcodes:[ assign_v0_four; add_v0_5 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0))
      (I:000000000000 V0:00001001 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign 2 to v1 then assign v0 to v1" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_2; assign_v0_v1 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0))
      (I:000000000000 V0:00000010 V1:00000010 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign 2 to v1 then assign 1 to v0 then or them" =
  let pc, error, registers =
    test
      ~opcodes:[ assign_v1_1; assign_v0_2; or_v0_v1 ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 518) (error 0))
      (I:000000000000 V0:00000011 V1:00000001 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign 2 to v1 then assign 3 to v0 then and them" =
  let pc, error, registers =
    test
      ~opcodes:[ assign_v1_1; assign_v0_3; and_v0_v1 ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 518) (error 0))
      (I:000000000000 V0:00000001 V1:00000001 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign 1 to v1 then assign 3 to v0 then xor them" =
  let pc, error, registers =
    test
      ~opcodes:[ assign_v1_1; assign_v0_3; xor_v0_v1 ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 518) (error 0))
      (I:000000000000 V0:00000010 V1:00000001 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "assign 1 to v1 then increment i by v1" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_1; add_i_by_v1 ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0))
      (I:000000000001 V0:00000000 V1:00000001 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test random state" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_1; xor_v1_random ] ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0))
      (I:000000000000 V0:00000000 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test dump all registers" =
  let pc, error, registers, range =
    test_with_range
      ~range:(0, 32)
      ~opcodes:
        [ assign_v0_1
        ; assign_v1_2
        ; assign_v2_3
        ; assign_v3_4
        ; assign_v4_5
        ; reg_dump_all_registers
        ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int) (range : int array)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 524) (error 0)
       (range (1 2 3 4 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      (I:000000000000 V0:00000001 V1:00000010 V2:00000011 V3:00000100 V4:00000101
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test dump the first three registers" =
  let pc, error, registers, range =
    test_with_range
      ~range:(0, 32)
      ~opcodes:
        [ assign_v0_1
        ; assign_v1_2
        ; assign_v2_3
        ; assign_v3_4
        ; assign_v4_5
        ; add_i_by_v1
        ; reg_dump_first_three_registers
        ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int) (range : int array)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 526) (error 0)
       (range (0 0 1 2 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      (I:000000000010 V0:00000001 V1:00000010 V2:00000011 V3:00000100 V4:00000101
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test dump and then load registers" =
  let pc, error, registers, range =
    test_with_range
      ~range:(0, 32)
      ~opcodes:
        [ assign_v0_1
        ; assign_v1_2
        ; assign_v2_3
        ; assign_v3_4
        ; assign_v4_5
        ; reg_dump_all_registers
        ; assign_v0_0
        ; assign_v1_0
        ; assign_v2_0
        ; assign_v3_0
        ; assign_v4_0
        ; reg_load_all_registers
        ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int) (range : int array)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 536) (error 0)
       (range (1 2 3 4 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      (I:000000000000 V0:00000001 V1:00000010 V2:00000011 V3:00000100 V4:00000101
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

let%expect_test "test sprite load" =
  let pc, error, registers, range =
    test_with_range
      ~range:(0, 32)
      ~opcodes:[ assign_v0_9; load_sprite_to_i ]
      ~stop_when:(bounded_standard_stop ())
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int) (range : int array)];
  print_registers ~registers;
  [%expect
    {|
      "TODO: Clear screen (make a Memcpy module?)"
      ((pc 516) (error 0)
       (range (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      (I:000000101101 V0:00001001 V1:00000000 V2:00000000 V3:00000000 V4:00000000
       V5:00000000 V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000
       V11:00000000 V12:00000000 V13:00000000 V14:00000000 V15:00000000) |}]
;;

(** TODO: Stack pointer CALL and RET tests *)

(** TODO: Test get/set delay *)

(** TODO: test key press instructions *)
