open! Core
open! Hardcaml
open! Signal

(* Foundation of a CHIP-8 core. Manages the fetch execute cycle
e  and maintains a random state for opcodes to use. *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.I.t
    ; keys : 'a Keys.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { in_execute : 'a [@bits 1]
    ; in_fetch : 'a [@bits 1]
    ; op : 'a [@bits 16]
    ; working_op : 'a [@bits 16]
    ; registers : 'a Registers.In_circuit.t
    ; fetch_finished : 'a [@bits 1]
    ; fetch_cycle : 'a [@bits 2]
    ; last_op : 'a [@bits 16]
    ; executor_done : 'a [@bits 1]
    ; executor_error : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.O.t
    ; random_state : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

val create : spec:Reg_spec.t -> Signal.t I.t -> Signal.t O.t
