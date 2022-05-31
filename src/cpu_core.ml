open! Core
open! Hardcaml
open! Signal
open Global

let prng_seed = 2341940502312319249

module States = struct
  type t =
    | Startup
    | Fetch_op
    | Execute
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; program : 'a [@bits 1]
    ; program_pc : 'a [@bits 12]
    ; program_write_enable : 'a [@bits 1]
    ; program_address : 'a [@bits 12]
    ; program_data : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

module Registers = struct
  type 'a t =
    { pc : 'a [@bits 12]
    ; i : 'a [@bits 12]
    ; sp : 'a [@bits 32]
    ; error : 'a [@bits 8]
    ; registers : 'a list [@length 16] [@bits 8]
    ; done_ : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { in_execute : 'a [@bits 1]
    ; program_read_address : 'a [@bits 12]
    ; program_read_data : 'a [@bits 8]
    ; op : 'a [@bits 16]
    ; registers : 'a Registers.t
    }
  [@@deriving sexp_of, hardcaml]
end

(* This updates a register using a register index value by one hot assignment.
   If no register matches the target index then none are updated but that should
   be impossible since the index circuit is sized for the number of registers. *)
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
        [ TargetRegister.assign opcode_first_register (srl opcode_first_register.value 1)
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
        [ TargetRegister.assign opcode_first_register (sll opcode_first_register.value 1)
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

let fetch
    ~(state : States.t Always.State_machine.t)
    ~(done_ : Always.Variable.t)
    ~(pc : Always.Variable.t)
    ~(executing_opcode : Always.Variable.t)
    ~(ram : Main_memory.t)
  =
  let open Always in
  let fetch_cycle = Variable.reg ~enable:vdd ~width:2 r_sync in
  let op_first = Variable.reg ~enable:vdd ~width:8 r_sync in
  let op_second = Variable.reg ~enable:vdd ~width:8 r_sync in
  let op = concat_msb [ op_first.value; op_second.value ] in
  [ done_ <--. 0
  ; when_ (fetch_cycle.value ==:. 0) [ ram.read_address <-- pc.value ]
  ; when_ (fetch_cycle.value ==:. 1) [ op_first <-- ram.read_data ]
  ; when_ (fetch_cycle.value ==:. 2) [ ram.read_address <-- (pc.value +:. 1) ]
  ; when_
      (fetch_cycle.value ==:. 3)
      [ op_second <-- ram.read_data; executing_opcode <-- op; state.set_next Execute ]
  ; fetch_cycle <-- fetch_cycle.value +:. 1
  ]
;;

let startup
    ~random_state_seed
    ~(internal : ExecutorInternal.t)
    ~(state : States.t Always.State_machine.t)
  =
  let open Always in
  [ (* Seed the PRNG on the first cycle. Since this is fixed machines will always behave identically. *)
    random_state_seed <--. prng_seed
  ; internal.pc <--. 0
  ; internal.sp <--. 0
  ; internal.i <--. 0
  ; state.set_next Fetch_op
  ]
;;

let create (i : _ I.t) : _ O.t =
  let open Always in
  let open Variable in
  let ram = Main_memory.create () in
  let random_state_seed = wire ~default:(Signal.of_int ~width:64 0) in
  let random_state =
    Xor_shift.create
      { Xor_shift.I.clock = i.clock; clear = i.clear; seed = random_state_seed.value }
  in
  let error = reg ~enable:vdd ~width:8 r_sync in
  let done_ = reg ~enable:vdd ~width:1 r_sync in
  let in_execute = wire ~default:(Signal.of_int ~width:1 0) in
  let internal = ExecutorInternal.create () in
  let state = internal.state in
  let ok = proc [ error <--. 0; done_ <--. 1; state.set_next Fetch_op ] in
  let main_execution =
    [ state.switch
        [ Startup, startup ~random_state_seed ~internal ~state
        ; ( Fetch_op
          , fetch
              ~done_
              ~pc:internal.pc
              ~executing_opcode:internal.executing_opcode
              ~ram
              ~state )
        ; ( Execute
          , [ in_execute <--. 1
            ; (* This error state will become 0 if any op is matched *)
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
                [ assign_address internal.i ok internal
                ; internal.pc <-- internal.pc.value +:. 2
                ]
            ; (* Set the pc register to a fixed address + V0 *)
              when_
                (internal.primary_op ==:. 11)
                [ assign_address
                    ~mutate:(fun pc -> uresize internal.register_zero.value 12 +: pc)
                    internal.pc
                    ok
                    internal
                ]
            ; (* XOR the first register with the state of the PRNG *)
              when_
                (internal.primary_op ==:. 12)
                [ TargetRegister.assign
                    internal.opcode_first_register
                    (internal.opcode_first_register.value
                    ^: select random_state.pseudo_random 7 0)
                ; internal.pc <-- internal.pc.value +:. 2
                ; ok
                ]
            ] )
        ]
    ]
  in
  compile
    [ if_
        (i.program ==:. 1)
        [ ram.read_address <-- i.program_address
        ; ram.write_enable <-- i.program_write_enable
        ; ram.write_address <-- i.program_address
        ; ram.write_data <-- i.program_data
        ; internal.pc <-- i.program_pc
        ]
        main_execution
    ];
  { O.in_execute = in_execute.value
  ; program_read_data = ram.read_data
  ; program_read_address = ram.read_address.value
  ; op = internal.executing_opcode.value
  ; registers =
      { Registers.pc = internal.pc.value
      ; i = internal.i.value
      ; sp = internal.sp.value
      ; done_ = done_.value
      ; error = error.value
      ; registers = List.map internal.registers ~f:(fun register -> register.value)
      }
  }
;;
