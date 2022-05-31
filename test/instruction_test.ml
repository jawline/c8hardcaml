(* {[

open Core
open Hardcaml
open C8.Cpu_core

let standard_stop error done_ = error <> 0 || done_ <> 0
let assign_v0_1 = Bits.of_string "16'b0110000000000001"
let assign_v0_2 = Bits.of_string "16'b0110000000000010"
let assign_v0_3 = Bits.of_string "16'b0110000000000011"
let assign_v1_1 = Bits.of_string "16'b0110000100000001"
let xor_v1_random = Bits.of_string "16'b1100000100000000"
let assign_v1_2 = Bits.of_string "16'b0110000100000010"
let assign_v2_3 = Bits.of_string "16'b0110001000000011"
let add_v0_5 = Bits.of_string "16'b0111000000000101"
let assign_v6_255 = Bits.of_string "16'b0110011011111111"
let assign_v0_five = Bits.of_string "16'b0110_0000_00000101"
let assign_v0_four = Bits.of_string "16'b0110_0000_00000100"
let assign_v1_five = Bits.of_string "16'b0110_0001_00000101"
let skip_if_v0_five = Bits.of_string "16'b0011_0000_00000101"
let skip_if_v0_not_five = Bits.of_string "16'b0100_0000_00000101"
let skip_if_v0_eq_v1 = Bits.of_string "16'b0101_0000_0001_0000"
let assign_v0_v1 = Bits.of_string "16'b1000_0000_0001_0000"
let or_v0_v1 = Bits.of_string "16'b1000_0000_0001_0001"
let and_v0_v1 = Bits.of_string "16'b1000_0000_0001_0010"
let xor_v0_v1 = Bits.of_string "16'b1000_0000_0001_0011"

let copy_outputs_to_inputs (inputs : _ I.t) (outputs : _ O.t) =
  inputs.input_pc := !(outputs.pc);
  inputs.input_i := !(outputs.i);
  inputs.input_sp := !(outputs.sp);
  List.iter
    ~f:(fun (a, b) -> a := !b)
    (List.zip_exn inputs.input_registers outputs.registers)
;;

let test ~create ~opcodes ~stop_when =
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  List.iter opcodes ~f:(fun opcode ->
      inputs.begin_ := Bits.of_int ~width:1 1;
      inputs.opcode := opcode;
      let step () =
        Cyclesim.cycle sim;
        stop_when (Bits.to_int !(outputs.error)) (Bits.to_int !(outputs.done_))
      in
      let rec until ~f = if f () then () else until ~f in
      until ~f:step;
      copy_outputs_to_inputs inputs outputs);
  ( !(outputs.pc)
  , !(outputs.error)
  , List.map outputs.registers ~f:(fun register -> !register) )
;;

let print_registers ~registers =
  let as_strings =
    List.mapi registers ~f:(fun i register ->
        sprintf "V%i:%s" i (Bits.to_string register))
  in
  Core.print_s [%message (as_strings : string list)]
;;

let%expect_test "step (disabled)" =
  let pc, _, _ =
    test ~opcodes:[ Bits.of_int ~width:16 0 ] ~create ~stop_when:standard_stop
  in
  let pc = Bits.to_int pc in
  Core.print_s [%message (pc : int)];
  [%expect {| (pc 2) |}]
;;

let%expect_test "step (enabled)" =
  let pc, _, _ =
    test ~opcodes:[ Bits.of_int ~width:16 0 ] ~create ~stop_when:standard_stop
  in
  let pc = Bits.to_int pc in
  Core.print_s [%message (pc : int)];
  [%expect {| (pc 2) |}]
;;

let%expect_test "step (jump) to 1024" =
  let jump_to_addr = Bits.of_string "16'b0001010000000000" in
  let pc, error, _ = test ~opcodes:[ jump_to_addr ] ~create ~stop_when:standard_stop in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  [%expect {| ((pc 1024) (error 0)) |}]
;;

let%expect_test "step (jump) to 512" =
  let jump_to_addr = Bits.of_string "16'b0001001000000000" in
  let pc, error, _ = test ~opcodes:[ jump_to_addr ] ~create ~stop_when:standard_stop in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  [%expect {| ((pc 512) (error 0)) |}]
;;

let%expect_test "step (jump) to 1" =
  let jump_to_addr = Bits.of_string "16'b0001000000000001" in
  let pc, error, _ = test ~opcodes:[ jump_to_addr ] ~create ~stop_when:standard_stop in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  [%expect {| ((pc 1) (error 0)) |}]
;;

let%expect_test "assign V0 to 1" =
  let pc, error, registers =
    test ~opcodes:[ assign_v0_1 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 2) (error 0))
      (as_strings
       (V0:00000001 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign V1 to 2" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_2 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 2) (error 0))
      (as_strings
       (V0:00000000 V1:00000010 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign V2 to 3" =
  let pc, error, registers =
    test ~opcodes:[ assign_v2_3 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 2) (error 0))
      (as_strings
       (V0:00000000 V1:00000000 V2:00000011 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign V6 to max_int (255)" =
  let pc, error, registers =
    test ~opcodes:[ assign_v6_255 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 2) (error 0))
      (as_strings
       (V0:00000000 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:11111111 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "test skip if equal on equal value" =
  let pc, error, registers =
    test ~opcodes:[ assign_v0_five; skip_if_v0_five ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 6) (error 0))
      (as_strings
       (V0:00000101 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "test skip if equal on non-equal value" =
  let pc, error, registers =
    test ~opcodes:[ assign_v0_four; skip_if_v0_five ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 4) (error 0))
      (as_strings
       (V0:00000100 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "test skip if not equal on equal value" =
  let pc, error, registers =
    test ~opcodes:[ assign_v0_five; skip_if_v0_not_five ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 4) (error 0))
      (as_strings
       (V0:00000101 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "test skip if v0 = v1 when v0 = v1" =
  let pc, error, registers =
    test
      ~opcodes:[ assign_v0_five; assign_v1_five; skip_if_v0_eq_v1 ]
      ~create
      ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 8) (error 0))
      (as_strings
       (V0:00000101 V1:00000101 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "test skip if v0 = v1 when v0 <> v1" =
  let pc, error, registers =
    test
      ~opcodes:[ assign_v0_four; assign_v1_five; skip_if_v0_eq_v1 ]
      ~create
      ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 6) (error 0))
      (as_strings
       (V0:00000100 V1:00000101 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "add 5 to v0 twices" =
  let pc, error, registers =
    test ~opcodes:[ add_v0_5; add_v0_5 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 4) (error 0))
      (as_strings
       (V0:00001010 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign 4 to v0 then add 5 to v0" =
  let pc, error, registers =
    test ~opcodes:[ assign_v0_four; add_v0_5 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 4) (error 0))
      (as_strings
       (V0:00001001 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign 2 to v1 then assign v0 to v1" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_2; assign_v0_v1 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 4) (error 0))
      (as_strings
       (V0:00000010 V1:00000010 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign 2 to v1 then assign 1 to v0 then or them" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_1; assign_v0_2; or_v0_v1 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 6) (error 0))
      (as_strings
       (V0:00000011 V1:00000001 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign 2 to v1 then assign 3 to v0 then and them" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_1; assign_v0_3; and_v0_v1 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 6) (error 0))
      (as_strings
       (V0:00000001 V1:00000001 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "assign 2 to v1 then assign 3 to v0 then xor them" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_1; assign_v0_3; xor_v0_v1 ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 6) (error 0))
      (as_strings
       (V0:00000010 V1:00000001 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;

let%expect_test "test random state" =
  let pc, error, registers =
    test ~opcodes:[ assign_v1_1; xor_v1_random ] ~create ~stop_when:standard_stop
  in
  let pc, error = Bits.to_int pc, Bits.to_int error in
  Core.print_s [%message (pc : int) (error : int)];
  print_registers ~registers;
  [%expect
    {|
      ((pc 4) (error 0))
      (as_strings
       (V0:00000000 V1:10110111 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
;;
]} *)
