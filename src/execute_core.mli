open! Core
open! Hardcaml
open! Signal

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

val create
  :  spec:Reg_spec.t
  -> keys:Signal.t Keys.t
  -> executing_opcode:Signal.t
  -> unit
  -> t

val execute_instruction
  :  spec:Reg_spec.t
  -> clock:Signal.t
  -> clear:Signal.t
  -> error:Always.Variable.t
  -> done_:Always.Variable.t
  -> ram:Main_memory.t
  -> random_state:Signal.t Xor_shift.O.t
  -> t
  -> Always.t
