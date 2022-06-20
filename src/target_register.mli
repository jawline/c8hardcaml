open! Core
open! Hardcaml
open! Signal

(** A record that stores a signal of a register index and a one hot encoding of it's value *)
type t =
  { value : Signal.t
  ; index : Signal.t
  ; registers : Registers.t
  }

val create : Registers.t -> Signal.t -> t
val assign : t -> Signal.t -> Always.t
