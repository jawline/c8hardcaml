open! Core
open! Hardcaml
open! Signal

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()

module Counter = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; incr : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { dout : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~w (i : _ I.t) =
    { O.dout =
        reg_fb
          (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
          ~enable:i.incr
          ~w
          (fun d -> d +:. 1)
    }
  ;;

  let test ~create ~iterations =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let step ~clear ~incr _ =
      inputs.clear := if clear = 1 then Bits.vdd else Bits.gnd;
      inputs.incr := if incr = 1 then Bits.vdd else Bits.gnd;
      Cyclesim.cycle sim
    in
    Sequence.iter (Sequence.range 0 iterations) ~f:(step ~clear:0 ~incr:1);
    !(outputs.dout)
  ;;

  let%expect_test "8 bit counter 256 increments" =
    Core.print_s [%message "8 bit counter"];
    let result = test ~create:(create ~w:8) ~iterations:256 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "8 bit counter"
      00000000 |}]
  ;;

  let%expect_test "8 bit counter 255 increments" =
    Core.print_s [%message "8 bit counter"];
    let result = test ~create:(create ~w:8) ~iterations:255 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "8 bit counter"
      11111111 |}]
  ;;

  let%expect_test "8 bit counter 257 increments" =
    Core.print_s [%message "8 bit counter"];
    let result = test ~create:(create ~w:8) ~iterations:257 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "8 bit counter"
      00000001 |}]
  ;;

  let%expect_test "16 bit counter 256 increments" =
    Core.print_s [%message "16 bit counter"];
    let result = test ~create:(create ~w:16) ~iterations:256 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "16 bit counter"
      0000000100000000 |}]
  ;;

  let%expect_test "16 bit counter 255 increments" =
    Core.print_s [%message "16 bit counter"];
    let result = test ~create:(create ~w:16) ~iterations:255 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "16 bit counter"
      0000000011111111 |}]
  ;;

  let%expect_test "16 bit counter 257 increments" =
    Core.print_s [%message "16 bit counter"];
    let result = test ~create:(create ~w:16) ~iterations:257 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "16 bit counter"
      0000000100000001 |}]
  ;;
end

module Registers = struct
  type 'a t =
    { pc : 'a
    ; i : 'a
    ; v : 'a list
    }
end

(* This module executes a single instruction *)
module Executor = struct
  module States = struct
    type t =
      | Wait
      | Executing
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; begin_ : 'a [@bits 1]
      ; opcode : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { pc : 'a
      ; error : 'a
      ; registers : 'a list [@length 16] [@bits 8]
      ; done_ : 'a [@bits 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  (* This updates a register using a register index value by one hot assignment.
   * If no register matches the target index then none are updated but that should
   * be impossible since the index circuit is sized for the number of registers. *)
  let onehot_assign_register registers index_circuit value =
    let open Always in
    proc
      (List.mapi registers ~f:(fun i register ->
           proc [ when_ (index_circuit ==:. i) [ register <-- value ] ]))
  ;;

  let create (i : _ I.t) =
    let open Always in
    let open Variable in
    let state = Always.State_machine.create (module States) ~enable:vdd r_sync in
    let pc = reg ~enable:vdd ~width:12 r_sync in
    let _i = reg ~enable:vdd ~width:12 r_sync in
    let error = reg ~enable:vdd ~width:8 r_sync in
    let executing_opcode = reg ~enable:vdd ~width:16 r_sync in
    let done_ = reg ~enable:vdd ~width:1 r_sync in
    let primary_op = select executing_opcode.value 15 12 in
    let jump_location = select executing_opcode.value 11 0 in
    let target_immediate = select executing_opcode.value 7 0 in
    let registers = List.init 16 ~f:(fun _ -> reg ~enable:vdd ~width:8 r_sync) in
    (* A with-valid list of the target register for one-hot encoding *)
    let target_register_index = select executing_opcode.value 11 8 in
    let target_second_register_index = select executing_opcode.value 7 4 in
    let target_register ~register_index =
      List.mapi registers ~f:(fun idx register ->
          let target_register = lsb (register_index ==:. idx) in
          { With_valid.valid = target_register; value = register.value })
      |> onehot_select
    in
    let first_target_register = target_register ~register_index:target_register_index in
    let second_target_register =
      target_register ~register_index:target_second_register_index
    in
    let ok = proc [ error <--. 0; done_ <--. 1; state.set_next Wait ] in
    compile
      [ state.switch
          [ ( Wait
            , [ when_
                  (i.begin_ ==:. 1)
                  [ state.set_next Executing
                  ; executing_opcode <-- i.opcode
                  ; done_ <--. 0
                  ]
              ] )
          ; ( Executing
            , [ (* This error state will become 0 if any op is matched *)
                error <--. 1
              ; (* We use 0 (originally native code call) as a No-op *)
                when_ (primary_op ==:. 0) [ pc <-- pc.value +:. 2; ok ]
              ; (* Jump to the 12 bits at the end of the opcode *)
                when_ (primary_op ==:. 1) [ pc <-- jump_location; ok ]
              ; (* Skip the next instruction of register is equal to immediate *)
                when_
                  (primary_op ==:. 3)
                  [ if_
                      (first_target_register ==: target_immediate)
                      [ (* Skip the next instruction *) pc <-- pc.value +:. 4 ]
                      [ (* We do not skip the next instruction *) pc <-- pc.value +:. 2 ]
                  ; ok
                  ]
              ; (* Skip the next instruction of register is not equal to immediate (dual of the opcode above) *)
                when_
                  (primary_op ==:. 4)
                  [ if_
                      (first_target_register <>: target_immediate)
                      [ (* Skip the next instruction *) pc <-- pc.value +:. 4 ]
                      [ (* We do not skip the next instruction *) pc <-- pc.value +:. 2 ]
                  ; ok
                  ]
              ; (* Skip the next instruction of register is equal to the second target register *)
                when_
                  (primary_op ==:. 3)
                  [ if_
                      (first_target_register ==: second_target_register)
                      [ (* Skip the next instruction *) pc <-- pc.value +:. 4 ]
                      [ (* We do not skip the next instruction *) pc <-- pc.value +:. 2 ]
                  ; ok
                  ]
              ; (* Assigns the register pointer to by the second nibble of the opcode to the value stored in the final byte of the opcode *)
                when_
                  (primary_op ==:. 6)
                  [ onehot_assign_register
                      registers
                      target_register_index
                      target_immediate
                  ; pc <-- pc.value +:. 2
                  ; ok
                  ]
              ] )
          ]
      ];
    { O.pc = pc.value
    ; done_ = done_.value
    ; error = error.value
    ; registers = List.map registers ~f:(fun register -> register.value)
    }
  ;;

  let standard_stop error done_ = error <> 0 || done_ <> 0

  module Test = struct
    let assign_v0_1 = Bits.of_string "16'b0110000000000001"
    let assign_v1_2 = Bits.of_string "16'b0110000100000010"
    let assign_v2_3 = Bits.of_string "16'b0110001000000011"
    let assign_v6_255 = Bits.of_string "16'b0110011011111111"
    let assign_v0_five = Bits.of_string "16'b0110000000000111"
    let assign_v0_four = Bits.of_string "16'b0110000000000110"
    let assign_v1_five = Bits.of_string "16'b0110000100000111"
    let skip_if_v0_five = Bits.of_string "16'b0011000000000111"
    let skip_if_v0_not_five = Bits.of_string "16'b0100000000000111"
    let skip_if_v0_eq_v1 = Bits.of_string "16'b0100_0000_0001_0000"

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
          until ~f:step);
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
      let pc, error, _ =
        test ~opcodes:[ jump_to_addr ] ~create ~stop_when:standard_stop
      in
      let pc, error = Bits.to_int pc, Bits.to_int error in
      Core.print_s [%message (pc : int) (error : int)];
      [%expect {| ((pc 1024) (error 0)) |}]
    ;;

    let%expect_test "step (jump) to 512" =
      let jump_to_addr = Bits.of_string "16'b0001001000000000" in
      let pc, error, _ =
        test ~opcodes:[ jump_to_addr ] ~create ~stop_when:standard_stop
      in
      let pc, error = Bits.to_int pc, Bits.to_int error in
      Core.print_s [%message (pc : int) (error : int)];
      [%expect {| ((pc 512) (error 0)) |}]
    ;;

    let%expect_test "step (jump) to 1" =
      let jump_to_addr = Bits.of_string "16'b0001000000000001" in
      let pc, error, _ =
        test ~opcodes:[ jump_to_addr ] ~create ~stop_when:standard_stop
      in
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
       (V0:00000111 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
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
       (V0:00000110 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
    ;;

    let%expect_test "test skip if not equal on equal value" =
      let pc, error, registers =
        test
          ~opcodes:[ assign_v0_five; skip_if_v0_not_five ]
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
       (V0:00000111 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
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
      ((pc 6) (error 0))
      (as_strings
       (V0:00000111 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
    ;;

    let%expect_test "test skip if v0 = v1 when v0 <> v1" =
      (* TODO: Broken *)
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
       (V0:00000111 V1:00000000 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
    ;;
  end
end
