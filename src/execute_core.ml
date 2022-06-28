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
        (** If this opcode has a 12-bit pointer this signal will be equal to it *)
  ; opcode_address : Signal.t
  ; (* If the opcode contains an immediate value in the final 8 bits this signal will be equal to it *)
    opcode_immediate : Signal.t
  ; opcode_second_nibble : Signal.t
  ; opcode_final_nibble : Signal.t
  ; (* If the opcode refers to a register in the second nibble this signal will be equal to it *)
    opcode_first_register : Target_register.t
  ; (* If the opcode refers to a register in the third nibble this signal will be equal to it *)
    opcode_second_register : Target_register.t
  ; (* 9-bit sized target registers to hold the carry TODO: Could be improved / merged into target register *)
    opcode_first_register_9bit : Signal.t
  ; opcode_second_register_9bit : Signal.t
  ; keys : Signal.t Keys.t
  }

let create ~spec ~keys ~executing_opcode () =
  let registers = Registers.create ~spec in
  let primary_op = select executing_opcode 15 12 in
  let register_zero = List.nth_exn registers.registers 0x0 in
  let flag_register = List.nth_exn registers.registers 0xF in
  let opcode_address = select executing_opcode 11 0 in
  let opcode_immediate = select executing_opcode 7 0 in
  let opcode_final_nibble = select executing_opcode 3 0 in
  let opcode_second_nibble = select executing_opcode 11 8 in
  let opcode_first_register =
    Target_register.create registers (select executing_opcode 11 8)
  in
  let opcode_second_register =
    Target_register.create registers (select executing_opcode 7 4)
  in
  let opcode_first_register_9bit = uresize opcode_first_register.value 9 in
  let opcode_second_register_9bit = uresize opcode_second_register.value 9 in
  { registers
  ; register_zero
  ; flag_register
  ; primary_op
  ; opcode_address
  ; opcode_immediate
  ; opcode_second_nibble
  ; opcode_final_nibble
  ; opcode_first_register
  ; opcode_second_register
  ; opcode_first_register_9bit
  ; opcode_second_register_9bit
  ; keys
  }
;;

let key_instructions
    ok
    done_with_instruction
    { registers = { pc; _ }; opcode_first_register; opcode_immediate; keys; _ }
  =
  let open Always in
  let open Variable in
  let key_under_vx = wire_false () in
  proc
    [ (* One-hot encode the key under Vx *)
      List.mapi keys.state ~f:(fun i key ->
          proc [ when_ (opcode_first_register.value ==:. i) [ key_under_vx <-- key ] ])
      |> proc
    ; when_
        (opcode_immediate ==:. 0x9E)
        [ (* Skip if Keys[Vx] is pressed *)
          ok
        ; done_with_instruction
        ; if_
            (key_under_vx.value ==:. 1)
            [ pc <-- pc.value +:. 4 ]
            [ pc <-- pc.value +:. 2 ]
        ]
    ; when_
        (opcode_immediate ==:. 0xA1)
        [ (* Skip if Keys[Vx] is not pressed *)
          ok
        ; done_with_instruction
        ; if_
            (key_under_vx.value ==:. 1)
            [ pc <-- pc.value +:. 2 ]
            [ pc <-- pc.value +:. 4 ]
        ]
    ]
;;

let memory_instructions
    ~spec
    ~clock
    ~clear
    ~(ram : Main_memory.t)
    ok
    done_with_instruction
    { registers = { pc; i; registers; _ }
    ; opcode_first_register
    ; opcode_second_nibble
    ; opcode_immediate
    ; _
    }
  =
  let open Always in
  let open Variable in
  let reg_memory_step = reg ~width:5 spec in
  let reg_memory_op ~round_fn ~offset_register_by_one_cycle =
    let round_logic =
      Sequence.range
        0
        (List.length registers + if offset_register_by_one_cycle then 1 else 0)
      |> Sequence.to_list
      |> List.map ~f:(fun n ->
             let register_n = if offset_register_by_one_cycle then n - 1 else n in
             let round_register = List.nth registers register_n in
             let i = i.value +:. n in
             proc [ when_ (reg_memory_step.value ==:. n) [ round_fn i round_register ] ])
      |> proc
    in
    let final_step =
      uresize opcode_second_nibble 5 +:. if offset_register_by_one_cycle then 1 else 0
    in
    proc
      [ ok
      ; reg_memory_step <-- reg_memory_step.value +:. 1
      ; round_logic
      ; when_
          (reg_memory_step.value ==: final_step)
          [ reg_memory_step <--. 0; pc <-- pc.value +:. 2; done_with_instruction ]
      ]
  in
  let reg_dump =
    reg_memory_op ~offset_register_by_one_cycle:false ~round_fn:(fun i register ->
        let register = Option.value_exn register in
        proc
          [ ram.write_enable <--. 1
          ; ram.write_address <-- to_main_addr i
          ; ram.write_data <-- register.value
          ])
  in
  let reg_load =
    reg_memory_op ~offset_register_by_one_cycle:true ~round_fn:(fun i register ->
        let read_i_for_next_round = proc [ ram.read_address <-- to_main_addr i ] in
        let store_last_round_in_register =
          match register with
          | Some v -> proc [ v <-- ram.read_data ]
          | None -> proc []
        in
        proc [ read_i_for_next_round; store_last_round_in_register ])
  in
  (* TODO: Use a slower algorithm instead of wasting so many gates for bcd ?
     To save on time we implement BCD in hardware through a lookup table *)
  let bcd = Bcd.create { Bcd.I.clock; clear; input = opcode_first_register.value } in
  let bcd_logic =
    proc
      [ ok
      ; reg_memory_step <-- reg_memory_step.value +:. 1
      ; when_
          (reg_memory_step.value ==:. 0)
          [ Main_memory.write ram i.value (to_byte bcd.digit1) ]
      ; when_
          (reg_memory_step.value ==:. 1)
          [ Main_memory.write ram (i.value +:. 1) (to_byte bcd.digit2) ]
      ; when_
          (reg_memory_step.value ==:. 2)
          [ reg_memory_step <--. 0
          ; done_with_instruction
          ; pc <-- pc.value +:. 2
          ; Main_memory.write ram (i.value +:. 2) (to_byte bcd.digit3)
          ]
      ]
  in
  let delay_register = reg ~width:16 spec in
  proc
    [ (* Add VX to I without changing the flags register *)
      when_
        (opcode_immediate ==:. 0x1E)
        [ i <-- i.value +: to_addr opcode_first_register.value
        ; pc <-- pc.value +:. 2
        ; done_with_instruction
        ]
    ; when_
        (opcode_immediate ==:. 0x29)
        [ i
          <-- select
                (to_addr opcode_first_register.value *: Signal.of_int ~width:12 5)
                11
                0
        ; pc <-- pc.value +:. 2
        ; done_with_instruction
        ]
    ; when_ (opcode_immediate ==:. 0x33) [ bcd_logic ]
    ; when_ (opcode_immediate ==:. 0x55) [ reg_dump ]
    ; when_ (opcode_immediate ==:. 0x65) [ reg_load ]
    ; (* TODO: This delay register is not precise; it should tick at 60Hz but instead we tick it every 8 cycles. *)
      when_
        (delay_register.value <>:. 0)
        [ delay_register <-- delay_register.value -:. 1 ]
    ; when_
        (opcode_immediate ==:. 0x07)
        [ Target_register.assign
            opcode_first_register
            (sel_bottom (srl delay_register.value 3) 8)
        ; ok
        ; done_with_instruction
        ; pc <-- pc.value +:. 2
        ]
    ; when_
        (opcode_immediate ==:. 0x15)
        [ delay_register <-- sll (uresize opcode_first_register.value 16) 3
        ; ok
        ; done_with_instruction
        ; pc <-- pc.value +:. 2
        ]
    ]
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
    ; (* Sll register by 1 and store msb in carry *)
      when_
        (opcode_final_nibble ==:. 0xE)
        [ Target_register.assign opcode_first_register (sll opcode_first_register.value 1)
        ; flag_register <-- uresize (msb opcode_first_register.value) 8
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    ]
;;

let ret ~spec ~(ram : Main_memory.t) ok { registers = { pc; sp; _ }; _ } =
  let open Always in
  let prev_sp = sp.value -:. 2 in
  let step = Variable.reg ~width:2 spec in
  let first_read = Variable.reg ~width:8 spec in
  let next_pc = pc.value +:. 2 in
  proc
    [ step <-- step.value +:. 1
    ; when_ (step.value ==:. 0) [ ram.read_address <-- to_main_addr prev_sp; ok ]
    ; when_
        (step.value ==:. 1)
        [ ram.read_address <-- to_main_addr (prev_sp +:. 1); first_read <-- ram.read_data ]
    ; when_ (step.value ==:. 2) [ step <--. 0; sp <-- sp.value -:. 2; pc <-- next_pc; ok ]
    ]
;;

let no_op ~spec ~ram ok ({ registers = { pc; _ }; opcode_immediate; _ } as t) =
  let open Always in
  print_s [%message "TODO: Clear screen (make a Memcpy module?)"];
  (* Strictly the CHIP-8 doesn't have a no-op but this would 
     be a host machine call and it is useful for testing. *)
  proc
    [ when_ (opcode_immediate ==:. 0) [ pc <-- pc.value +:. 2; ok ]
    ; when_ (opcode_immediate ==:. 0xEE) [ ret ~spec ~ram ok t ]
    ; when_ (opcode_immediate ==:. 0xE0) [ pc <-- pc.value +:. 2 ; ok ]
    ]
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

let call_instruction
    ~spec
    ~(ram : Main_memory.t)
    ok
    { registers = { pc; sp; _ }; opcode_address; _ }
  =
  let open Always in
  let step = Variable.reg ~width:1 spec in
  let next_pc = pc.value +:. 2 in
  proc
    [ step <-- step.value +:. 1
    ; when_
        (step.value ==:. 0)
        [ ram.write_enable <--. 1
        ; ram.write_address <-- to_main_addr (sp.value +:. Main_memory.stack_start)
        ; ram.write_data <-- select next_pc 11 4
        ]
    ; when_
        (step.value ==:. 1)
        [ ram.write_enable <--. 1
        ; ram.write_address <-- to_main_addr (sp.value +:. (Main_memory.stack_start + 1))
        ; ram.write_data <-- uresize (select next_pc 3 0) (wsz `Byte)
        ; sp <-- sp.value +:. 2
        ; pc <-- opcode_address
        ; ok
        ]
    ]
;;

module Opcode_first_nibble = struct
  type t =
    | No_op_cls_or_ret
    | Immediate_jump
    | Immediate_call
    | Skip_equal_imm
    | Skip_not_equal_imm
    | Skip_equal_reg
    | Load_immediate
    | Add_immediate
    | Register_instructions
    | Skip_not_equal_reg
    | Load_i_imm
    | Jump_imm_plus_v0
    | Random
    | Draw
    | Keyboard
    | Memory_instructions
  [@@deriving enumerate]
end

let execute_instruction
    ~spec
    ~clock
    ~clear
    ~error
    ~done_
    ~ram
    ~(random_state : _ Xor_shift.O.t)
    ({ registers = { pc; i; _ }
     ; primary_op
     ; opcode_first_register
     ; opcode_second_register
     ; opcode_final_nibble
     ; opcode_immediate
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
  let ok = proc [ error <--. 0; done_ <--. 1 ] in
  let implementation_of_opcode_first_nibble ~opcode =
    let open Always in
    match opcode with
    | Opcode_first_nibble.No_op_cls_or_ret -> no_op ~spec ~ram ok t
    | Immediate_jump -> assign_address pc ok t
    | Immediate_call -> proc [ error <--. 0; call_instruction ~spec ~ram ok t ]
    | Skip_equal_imm -> skip_imm_inv ( ==: ) ok t
    | Skip_not_equal_imm -> skip_imm_inv ( <>: ) ok t
    | Skip_equal_reg -> skip_reg_inv ( ==: ) ok t
    | Load_immediate -> combine_register_imm ~f:(fun _ x -> x) ok t
    | Add_immediate -> combine_register_imm ~f:(fun x y -> x +: y) ok t
    | Register_instructions -> register_instructions ok t
    | Skip_not_equal_reg -> skip_reg_inv ( <>: ) ok t
    | Load_i_imm -> proc [ assign_address i ok t; pc <-- pc.value +:. 2 ]
    | Jump_imm_plus_v0 ->
      assign_address ~mutate:(fun pc -> uresize register_zero.value 12 +: pc) pc ok t
    | Random ->
      proc
        [ Target_register.assign
            opcode_first_register
            (opcode_immediate &: select random_state.pseudo_random 7 0)
        ; pc <-- pc.value +:. 2
        ; ok
        ]
    | Draw ->
      proc
        [ draw_enable <--. 1
        ; draw_wiring
        ; error <--. 0
        ; when_ draw_implementation.finished [ ok; pc <-- pc.value +:. 2 ]
        ]
    | Keyboard ->
      proc
        [ key_instructions (proc [ error <--. 0 ]) (proc [ error <--. 0; done_ <--. 1 ]) t
        ]
    | Memory_instructions ->
      proc
        [ memory_instructions
            ~clock
            ~clear
            ~spec
            ~ram
            (proc [ error <--. 0 ])
            (proc [ error <--. 0; done_ <--. 1 ])
            t
        ]
  in
  let opcode_implementations =
    List.mapi
      ~f:(fun opcode_index opcode ->
        proc
          [ when_
              (primary_op ==:. opcode_index)
              [ implementation_of_opcode_first_nibble ~opcode ]
          ])
      Opcode_first_nibble.all
    |> proc
  in
  (* TODO: Replace hardcoded constants with names *)
  proc
    [ (* This error state will become 0 if any op is matched *)
      error <--. 1
    ; opcode_implementations
    ]
;;
