open! Core
open! Hardcaml
open! Signal

(** An implementation of cycle by cycle memset for use with clearing the screen. *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; address : 'a [@bits 16]
    ; size : 'a [@bits 8]
    ; write_data : 'a [@bits 8]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { finished : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : spec:Reg_spec.t -> O.Of_signal.comb I.t -> O.Of_signal.comb O.t
