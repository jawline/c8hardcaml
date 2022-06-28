open! Core
open! Hardcaml
open! Signal

(* Foundation of a CHIP-8 core. Manages the fetch execute cycle
   and maintains a random state for opcodes to use. *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; program : 'a [@bits 1]
    ; program_pc : 'a [@bits 12]
    ; program_write_enable : 'a [@bits 1]
    ; program_address : 'a [@bits 16]
    ; program_data : 'a [@bits 8]
    ; keys : 'a Keys.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { core : 'a Cpu_core.O.t
    ; read_data : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

val create : spec:Reg_spec.t -> Signal.t I.t -> Signal.t O.t
