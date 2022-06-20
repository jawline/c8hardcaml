open! Core
open! Hardcaml
open! Signal

(** An implementation of fetch that takes 2 cycles
    to read a 16-bit opcode from main memory. *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; program_counter : 'a [@bits 12]
    ; in_fetch : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { finished : 'a [@bits 1]
    ; fetch_cycle : 'a [@bits 2]
    ; opcode : 'a [@bits 16]
    ; memory : 'a Main_memory.In_circuit.O.Just_read.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : spec:Reg_spec.t -> O.Of_signal.comb I.t -> O.Of_signal.comb O.t
