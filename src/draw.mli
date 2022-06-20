open! Core
open! Hardcaml
open! Signal

(** An implementation of the draw instruction in hardware. There are five
    cycles per bit as we need to do an unaligned write into a bitvector. *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; x : 'a [@bits 8]
    ; y : 'a [@bits 8]
    ; n : 'a [@bits 4]
    ; i : 'a [@bits 12]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { finished : 'a [@bits 1]
    ; step : 'a [@bits 12]
    ; memory : 'a Main_memory.In_circuit.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : spec:Reg_spec.t -> O.Of_signal.comb I.t -> O.Of_signal.comb O.t
