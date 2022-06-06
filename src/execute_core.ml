open! Core
open! Hardcaml
open! Signal
open Global

type t =
  { registers : Registers.t
  ; (* The first nibble of the executing opcode *)
    primary_op : Signal.t
  ; register_zero : Always.Variable.t
  ; flag_register : Always.Variable.t
  ; (* If this opcode has a 12-bit pointer this signal will be equal to it *)
    opcode_address : Signal.t
  ; (* If the opcode contains an immediate value in the final 8 bits this signal will be equal to it *)
    opcode_immediate : Signal.t
  ; (* The final nibble in the opcode *)
    opcode_final_nibble : Signal.t
  ; (* If the opcode refers to a register in the second nibble this signal will be equal to it *)
    opcode_first_register : Target_register.t
  ; (* If the opcode refers to a register in the third nibble this signal will be equal to it *)
    opcode_second_register : Target_register.t
  ; (* 9-bit sized target registers to hold the carry TODO: Could be improved / merged into target register *)
    opcode_first_register_9bit : Signal.t
  ; opcode_second_register_9bit : Signal.t
  }

let create ~spec ~executing_opcode () =
  let registers = Registers.create ~spec in
  let primary_op = select executing_opcode 15 12 in
  let register_zero = List.nth_exn registers.registers 0x0 in
  let flag_register = List.nth_exn registers.registers 0xF in
  let opcode_address = select executing_opcode 11 0 in
  let opcode_immediate = select executing_opcode 7 0 in
  let opcode_final_nibble = select executing_opcode 3 0 in
  let opcode_first_register =
    Target_register.create ~registers:registers.registers (select executing_opcode 11 8)
  in
  let opcode_second_register =
    Target_register.create ~registers:registers.registers (select executing_opcode 7 4)
  in
  let opcode_first_register_9bit = uresize opcode_first_register.value 9 in
  let opcode_second_register_9bit = uresize opcode_second_register.value 9 in
  { registers
  ; register_zero
  ; flag_register
  ; primary_op
  ; opcode_address
  ; opcode_immediate
  ; opcode_final_nibble
  ; opcode_first_register
  ; opcode_second_register
  ; opcode_first_register_9bit
  ; opcode_second_register_9bit
  }
;;

let register_instructions
    ok
    { registers = { pc; _ }
    ; flag_register
    ; opcode_first_register
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
        [ Target_register.assign opcode_first_register opcode_second_register.value
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* Or a register with another register *)
      when_
        (opcode_final_nibble ==:. 1)
        [ Target_register.assign
            opcode_first_register
            (opcode_first_register.value |: opcode_second_register.value)
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* And a register with another register *)
      when_
        (opcode_final_nibble ==:. 2)
        [ Target_register.assign
            opcode_first_register
            (opcode_first_register.value &: opcode_second_register.value)
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* Xor a register with another register *)
      when_
        (opcode_final_nibble ==:. 3)
        [ Target_register.assign
            opcode_first_register
            (opcode_first_register.value ^: opcode_second_register.value)
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* Add a register to another *)
      when_
        (opcode_final_nibble ==:. 4)
        [ Target_register.assign opcode_first_register (select add_result 7 0)
        ; (* Copy over the carry flag *)
          flag_register <-- uresize (select add_result 8 8) 8
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* Sub a register to another *)
      when_
        (opcode_final_nibble ==:. 5)
        [ Target_register.assign opcode_first_register (select sub_result 7 0)
        ; (* TODO: I don't think this correctly calculates a carry *)
          flag_register <-- uresize (select sub_result 8 8) 8
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* Shr register by 1 *)
      when_
        (opcode_final_nibble ==:. 6)
        [ Target_register.assign opcode_first_register (srl opcode_first_register.value 1)
        ; flag_register <-- uresize (lsb opcode_first_register.value) 8
          (* It doesn't matter that this happens after because registers update after the cycle *)
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* Same as sub but the operand order is reversed  *)
      when_
        (opcode_final_nibble ==:. 7)
        [ Target_register.assign opcode_first_register (select inv_sub_result 7 0)
        ; (* TODO: I don't think this correctly calculates a carry *)
          flag_register <-- uresize (select inv_sub_result 8 8) 8
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; (* Sll register by 1 *)
      when_
        (opcode_final_nibble ==:. 8)
        [ Target_register.assign opcode_first_register (sll opcode_first_register.value 1)
        ; flag_register <-- uresize (msb opcode_first_register.value) 8
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ]
;;

let no_op ok { registers = { pc; _ }; _ } =
  let open Always in
  proc [ pc <-- pc.value +:. 2; ok ]
;;

let assign_address ?(mutate = Fn.id) register ok { opcode_address; _ } =
  let open Always in
  proc [ register <-- mutate opcode_address; ok ]
;;

let skip_imm_inv
    invariant
    ok
    { registers = { pc; _ }; opcode_first_register; opcode_immediate; _ }
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
    { registers = { pc; _ }; opcode_first_register; opcode_second_register; _ }
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
    { registers = { pc; _ }; opcode_first_register; opcode_immediate; _ }
  =
  let open Always in
  proc
    [ Target_register.assign
        opcode_first_register
        (f opcode_first_register.value opcode_immediate)
    ; pc <-- pc.value +:. 2
    ; ok
    ]
;;

let execute_instruction
    ~clock
    ~clear
    ~error
    ~done_
    ~ram
    ~(random_state : _ Xor_shift.O.t)
    ({ registers = { pc; i; sp; _ }
     ; primary_op
     ; opcode_first_register
     ; opcode_second_register
     ; opcode_final_nibble
     ; register_zero
     ; _
     } as t)
  =
  let open Always in
  let draw_enable = wire_false () in
  let draw_implementation, draw_wiring =
    Main_memory.circuit_with_memory ram ~f:(fun ~memory ->
        let i = i.value in
        let x = opcode_first_register.value in
        let y = opcode_second_register.value in
        let n = opcode_final_nibble in
        let o =
          Draw.create
            ~spec:r_sync
            { Draw.I.clock; clear; enable = draw_enable.value; x; y; n; i; memory }
        in
        o, o.memory)
  in
  Core.print_s [%message "WARN: REMOVE ME WHEN SP IS USED"];
  compile [ sp <--. 0 ];
  let ok = proc [ error <--. 0; done_ <--. 1 ] in
  (* TODO: Replace hardcoded constants with names *)
  proc
    [ (* This error state will become 0 if any op is matched *)
      error <--. 1
    ; (* We use 0 (originally native code call) as a No-op *)
      when_ (primary_op ==:. 0) [ no_op ok t ]
    ; (* Jump to the 12 bits at the end of the opcode *)
      when_ (primary_op ==:. 1) [ assign_address pc ok t ]
    ; (* Skip the next instruction of register is equal to immediate *)
      when_ (primary_op ==:. 3) [ skip_imm_inv ( ==: ) ok t ]
    ; (* Skip the next instruction of register is not equal to immediate (dual of the opcode above) *)
      when_ (primary_op ==:. 4) [ skip_imm_inv ( <>: ) ok t ]
    ; (* Skip the next instruction of register is equal to the second target register *)
      when_ (primary_op ==:. 5) [ skip_reg_inv ( ==: ) ok t ]
    ; (* Assigns the register pointed to by the second nibble of the opcode to the value stored in the final byte of the opcode *)
      when_ (primary_op ==:. 6) [ combine_register_imm ~f:(fun _ x -> x) ok t ]
    ; (* Accumulate an immediate into the register addressed to by the second nibble of the opcode *)
      when_ (primary_op ==:. 7) [ combine_register_imm ~f:(fun x y -> x +: y) ok t ]
    ; (* 8__x opcodes are register operations *)
      when_ (primary_op ==:. 8) [ register_instructions ok t ]
    ; (* Skip the next instruction of register is not equal to the second target register *)
      when_ (primary_op ==:. 9) [ skip_reg_inv ( <>: ) ok t ]
    ; (* Set the i register to the opcode address *)
      when_ (primary_op ==:. 10) [ assign_address i ok t; pc <-- pc.value +:. 2 ]
    ; (* Set the pc register to a fixed address + V0 *)
      when_
        (primary_op ==:. 11)
        [ assign_address ~mutate:(fun pc -> uresize register_zero.value 12 +: pc) pc ok t
        ]
    ; (* XOR the first register with the state of the PRNG *)
      when_
        (primary_op ==:. 12)
        [ Target_register.assign
            opcode_first_register
            (opcode_first_register.value ^: select random_state.pseudo_random 7 0)
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ; when_
        (primary_op ==:. 13)
        [ draw_enable <--. 1
        ; draw_wiring
        ; error <--. 0
        ; when_ draw_implementation.finished [ ok; pc <-- pc.value +:. 2 ]
        ]
    ; when_
        (primary_op ==:. 14)
        [ (* TODO: Key press instructions *) pc <-- pc.value +:. 2; ok ]
    ]
;;
