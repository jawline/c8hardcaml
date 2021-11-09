open! Core
open! Hardcaml
open! Signal

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()

(* This module executes a single instruction *)
  module States = struct
    type t =
      | Wait
      | Executing
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; input_pc : 'a [@bits 12]
      ; input_i : 'a [@bits 12]
      ; input_sp : 'a [@bits 32]
      ; input_registers : 'a list [@length 16] [@bits 8]
      ; begin_ : 'a [@bits 1]
      ; opcode : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { pc : 'a
      ; i : 'a
      ; sp : 'a
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

  (* A record that stores a signal of a register index and a one hot encoding of it's value *)
  module TargetRegister = struct
    type t =
      { value : Signal.t
      ; index : Signal.t
      ; (* We store a reference to the registers so we can 1-hot assign later *)
        registers : Always.Variable.t list
      }

    let create ~(registers : Always.Variable.t list) index =
      let value =
        List.mapi registers ~f:(fun idx register ->
            let target_register = lsb (index ==:. idx) in
            { With_valid.valid = target_register; value = register.value })
        |> onehot_select
      in
      { value; index; registers }
    ;;

    let assign { index; registers; _ } value =
      let open Always in
      proc
        (List.mapi registers ~f:(fun i register ->
             proc [ when_ (index ==:. i) [ register <-- value ] ]))
    ;;
  end

  module ExecutorInternal = struct
    type t =
      { pc : Always.Variable.t
      ; i : Always.Variable.t
      ; sp : Always.Variable.t
      ; executing_opcode : Always.Variable.t
      ; (* The first nibble of the executing opcode *)
        primary_op : Signal.t
      ; registers : Always.Variable.t list
      ; register_zero : Always.Variable.t
      ; flag_register : Always.Variable.t
      ; state : States.t Always.State_machine.t
      ; (* If this opcode has a 12-bit pointer this signal will be equal to it *)
        opcode_address : Signal.t
      ; (* If the opcode contains an immediate value in the final 8 bits this signal will be equal to it *)
        opcode_immediate : Signal.t
      ; (* The final nibble in the opcode *)
        opcode_final_nibble : Signal.t
      ; (* If the opcode refers to a register in the second nibble this signal will be equal to it *)
        opcode_first_register : TargetRegister.t
      ; (* If the opcode refers to a register in the third nibble this signal will be equal to it *)
        opcode_second_register : TargetRegister.t
      ; (* 9-bit sized target registers to hold the carry TODO: Could be improved / merged into target register *)
        opcode_first_register_9bit : Signal.t
      ; opcode_second_register_9bit : Signal.t
      }

    let create () =
      let open Always in
      let open Variable in
      let state = State_machine.create (module States) ~enable:vdd r_sync in
      let pc = reg ~enable:vdd ~width:12 r_sync in
      let i = reg ~enable:vdd ~width:12 r_sync in
      let sp = reg ~enable:vdd ~width:32 r_sync in
      let executing_opcode = reg ~enable:vdd ~width:16 r_sync in
      let primary_op = select executing_opcode.value 15 12 in
      let registers = List.init 16 ~f:(fun _ -> reg ~enable:vdd ~width:8 r_sync) in
      let register_zero = List.nth_exn registers 0x0 in
      let flag_register = List.nth_exn registers 0xF in
      let opcode_address = select executing_opcode.value 11 0 in
      let opcode_immediate = select executing_opcode.value 7 0 in
      let opcode_final_nibble = select executing_opcode.value 3 0 in
      let opcode_first_register =
        TargetRegister.create ~registers (select executing_opcode.value 11 8)
      in
      let opcode_second_register =
        TargetRegister.create ~registers (select executing_opcode.value 7 4)
      in
      let opcode_first_register_9bit = uresize opcode_first_register.value 9 in
      let opcode_second_register_9bit = uresize opcode_second_register.value 9 in
      { pc
      ; i
      ; sp
      ; executing_opcode
      ; primary_op
      ; registers
      ; register_zero
      ; flag_register
      ; state
      ; opcode_address
      ; opcode_immediate
      ; opcode_final_nibble
      ; opcode_first_register
      ; opcode_second_register
      ; opcode_first_register_9bit
      ; opcode_second_register_9bit
      }
    ;;
  end

  let register_instructions
      ok
      { ExecutorInternal.pc
      ; opcode_first_register
      ; flag_register
      ; opcode_second_register
      ; opcode_first_register_9bit
      ; opcode_second_register_9bit
      ; opcode_final_nibble
      ; _
      }
    =
    let open Always in
    let open Variable in
    (* Stores the result of a 9-bit add of the two registers *)
    let add_result = opcode_first_register_9bit +: opcode_second_register_9bit in
    let sub_result = opcode_first_register_9bit -: opcode_second_register_9bit in
    let inv_sub_result = opcode_second_register_9bit -: opcode_first_register_9bit in
    proc
      [ (* Assign a register to another register *)
        when_
          (opcode_final_nibble ==:. 0)
          [ TargetRegister.assign opcode_first_register opcode_second_register.value
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* Or a register with another register *)
        when_
          (opcode_final_nibble ==:. 1)
          [ TargetRegister.assign
              opcode_first_register
              (opcode_first_register.value |: opcode_second_register.value)
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* And a register with another register *)
        when_
          (opcode_final_nibble ==:. 2)
          [ TargetRegister.assign
              opcode_first_register
              (opcode_first_register.value &: opcode_second_register.value)
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* Xor a register with another register *)
        when_
          (opcode_final_nibble ==:. 3)
          [ TargetRegister.assign
              opcode_first_register
              (opcode_first_register.value ^: opcode_second_register.value)
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* Add a register to another *)
        when_
          (opcode_final_nibble ==:. 4)
          [ TargetRegister.assign opcode_first_register (select add_result 7 0)
          ; (* Copy over the carry flag *)
            flag_register <-- uresize (select add_result 8 8) 8
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* Sub a register to another *)
        when_
          (opcode_final_nibble ==:. 5)
          [ TargetRegister.assign opcode_first_register (select sub_result 7 0)
          ; (* TODO: I don't think this correctly calculates a carry *)
            flag_register <-- uresize (select sub_result 8 8) 8
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* Shr register by 1 *)
        when_
          (opcode_final_nibble ==:. 6)
          [ TargetRegister.assign
              opcode_first_register
              (srl opcode_first_register.value 1)
          ; flag_register <-- uresize (lsb opcode_first_register.value) 8
            (* It doesn't matter that this happens after because registers update after the cycle *)
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* Same as sub but the operand order is reversed  *)
        when_
          (opcode_final_nibble ==:. 7)
          [ TargetRegister.assign opcode_first_register (select inv_sub_result 7 0)
          ; (* TODO: I don't think this correctly calculates a carry *)
            flag_register <-- uresize (select inv_sub_result 8 8) 8
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ; (* Sll register by 1 *)
        when_
          (opcode_final_nibble ==:. 8)
          [ TargetRegister.assign
              opcode_first_register
              (sll opcode_first_register.value 1)
          ; flag_register <-- uresize (msb opcode_first_register.value) 8
          ; pc <-- pc.value +:. 2
          ; ok
          ]
      ]
  ;;

  let no_op ok { ExecutorInternal.pc; _ } =
    let open Always in
    proc [ pc <-- pc.value +:. 2; ok ]
  ;;

  let assign_address ?(mutate = Fn.id) register ok { ExecutorInternal.opcode_address; _ } =
    let open Always in
    proc [ register <-- mutate opcode_address; ok ]
  ;;

  let skip_imm_inv
      invariant
      ok
      { ExecutorInternal.pc; opcode_first_register; opcode_immediate; _ }
    =
    let open Always in
    proc
      [ if_
          (invariant opcode_first_register.value opcode_immediate)
          [ (* Skip the next instruction *) pc <-- pc.value +:. 4 ]
          [ (* We do not skip the next instruction *) pc <-- pc.value +:. 2 ]
      ; ok
      ]
  ;;

  let skip_reg_inv
      invariant
      ok
      { ExecutorInternal.pc; opcode_first_register; opcode_second_register; _ }
    =
    let open Always in
    proc
      [ if_
          (invariant opcode_first_register.value opcode_second_register.value)
          [ (* Skip the next instruction *) pc <-- pc.value +:. 4 ]
          [ (* We do not skip the next instruction *) pc <-- pc.value +:. 2 ]
      ; ok
      ]
  ;;

  let combine_register_imm
      ~f
      ok
      { ExecutorInternal.pc; opcode_first_register; opcode_immediate; _ }
    =
    let open Always in
    proc
      [ TargetRegister.assign
          opcode_first_register
          (f opcode_first_register.value opcode_immediate)
      ; pc <-- pc.value +:. 2
      ; ok
      ]
  ;;

  let create (i : _ I.t) =
    let open Always in
    let open Variable in
    let error = reg ~enable:vdd ~width:8 r_sync in
    let done_ = reg ~enable:vdd ~width:1 r_sync in
    let internal = ExecutorInternal.create () in
    let state = internal.state in
    let ok = proc [ error <--. 0; done_ <--. 1; state.set_next Wait ] in
    compile
      [ state.switch
          [ ( Wait
            , [ when_
                  (i.begin_ ==:. 1)
                  [ state.set_next Executing
                  ; internal.executing_opcode <-- i.opcode
                  ; internal.pc <-- i.input_pc
                  ; internal.i <-- i.input_i
                  ; internal.sp <-- i.input_sp
                  ; proc
                      (List.map
                         (List.zip_exn internal.registers i.input_registers)
                         ~f:(fun (register, input) -> register <-- input))
                  ; done_ <--. 0
                  ]
              ] )
          ; ( Executing
            , [ (* This error state will become 0 if any op is matched *)
                error <--. 1
              ; (* We use 0 (originally native code call) as a No-op *)
                when_ (internal.primary_op ==:. 0) [ no_op ok internal ]
              ; (* Jump to the 12 bits at the end of the opcode *)
                when_
                  (internal.primary_op ==:. 1)
                  [ assign_address internal.pc ok internal ]
              ; (* Skip the next instruction of register is equal to immediate *)
                when_ (internal.primary_op ==:. 3) [ skip_imm_inv ( ==: ) ok internal ]
              ; (* Skip the next instruction of register is not equal to immediate (dual of the opcode above) *)
                when_ (internal.primary_op ==:. 4) [ skip_imm_inv ( <>: ) ok internal ]
              ; (* Skip the next instruction of register is equal to the second target register *)
                when_ (internal.primary_op ==:. 5) [ skip_reg_inv ( ==: ) ok internal ]
              ; (* Assigns the register pointed to by the second nibble of the opcode to the value stored in the final byte of the opcode *)
                when_
                  (internal.primary_op ==:. 6)
                  [ combine_register_imm ~f:(fun _ x -> x) ok internal ]
              ; (* Accumulate an immediate into the register addressed to by the second nibble of the opcode *)
                when_
                  (internal.primary_op ==:. 7)
                  [ combine_register_imm ~f:(fun x y -> x +: y) ok internal ]
              ; (* 8__x opcodes are register operations *)
                when_ (internal.primary_op ==:. 8) [ register_instructions ok internal ]
              ; (* Skip the next instruction of register is not equal to the second target register *)
                when_ (internal.primary_op ==:. 9) [ skip_reg_inv ( <>: ) ok internal ]
              ; (* Set the i register to the opcode address *)
                when_
                  (internal.primary_op ==:. 10)
                  [ assign_address internal.i ok internal ]
              ; (* Set the pc register to a fixed address + V0 *)
                when_
                  (internal.primary_op ==:. 11)
                  [ assign_address
                      ~mutate:(fun pc -> uresize internal.register_zero.value 12 +: pc)
                      internal.pc
                      ok
                      internal
                  ]
              ] )
          ]
      ];
    { O.pc = internal.pc.value
    ; i = internal.i.value
    ; sp = internal.sp.value
    ; done_ = done_.value
    ; error = error.value
    ; registers = List.map internal.registers ~f:(fun register -> register.value)
    }
  ;;

  module Test = struct
    let standard_stop error done_ = error <> 0 || done_ <> 0
    let assign_v0_1 = Bits.of_string "16'b0110000000000001"
    let assign_v0_2 = Bits.of_string "16'b0110000000000010"
    let assign_v0_3 = Bits.of_string "16'b0110000000000011"
    let assign_v1_1 = Bits.of_string "16'b0110000100000001"
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
        test
          ~opcodes:[ assign_v1_1; assign_v0_2; or_v0_v1 ]
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
       (V0:00000011 V1:00000001 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
    ;;

    let%expect_test "assign 2 to v1 then assign 3 to v0 then and them" =
      let pc, error, registers =
        test
          ~opcodes:[ assign_v1_1; assign_v0_3; and_v0_v1 ]
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
       (V0:00000001 V1:00000001 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
    ;;

    let%expect_test "assign 2 to v1 then assign 3 to v0 then xor them" =
      let pc, error, registers =
        test
          ~opcodes:[ assign_v1_1; assign_v0_3; xor_v0_v1 ]
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
       (V0:00000010 V1:00000001 V2:00000000 V3:00000000 V4:00000000 V5:00000000
        V6:00000000 V7:00000000 V8:00000000 V9:00000000 V10:00000000 V11:00000000
        V12:00000000 V13:00000000 V14:00000000 V15:00000000)) |}]
    ;;
  end
