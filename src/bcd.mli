open! Core
open! Hardcaml
open! Signal

(** An implementation of binary coded decimals by lookup table *)

module I : sig
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; input : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { digit1 : 'a [@bits 4]
    ; digit2 : 'a [@bits 4]
    ; digit3 : 'a [@bits 4]
    }
  [@@deriving sexp_of, hardcaml]
end

val create : O.Of_signal.comb I.t -> O.Of_signal.comb O.t
