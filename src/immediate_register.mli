open! Core
open! Hardcaml
open! Signal

(** Creates a wire defaulting to a
   register.

   On cycles where [t] is assigned
   the wire and register are set so that the change is
   immediately reflected in the value, but on succeeding
   cycles it is persisted. *)

type t =
  { register : Always.Variable.t
  ; wire : Always.Variable.t
  ; value : Signal.t
  }

val create : width:int -> spec:Reg_spec.t -> t
val ( <-- ) : t -> Signal.t -> Always.t
val ( <--. ) : t -> int -> Always.t
