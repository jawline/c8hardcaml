open! Core
open! Hardcaml
open! Signal

(** An implementation of a simple pseudorandom number generator
    (XOR-shift 64) in hardware for the random opcodes.
    
    If the seed input is not zero then the current state
    will be overwritten with the new seed, otherwise a new random
    number will be generated each cycle. *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; seed : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { pseudo_random : 'a [@bits 64] } [@@deriving sexp_of, hardcaml]
end

val create : O.Of_signal.comb I.t -> O.Of_signal.comb O.t
