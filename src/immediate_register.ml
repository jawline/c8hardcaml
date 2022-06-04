open! Core
open! Hardcaml
open! Signal

(* Creates a wire defaulting to a
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

let create ~width ~spec =
  let register = Always.Variable.reg ~width spec in
  let wire = Always.Variable.wire ~default:register.value in
  { register; wire; value = wire.value }
;;

let ( <-- ) t v =
  let open Always in
  proc [ t.register <-- v; t.wire <-- v ]
;;

let ( <--. ) t v =
  let open Always in
  proc [ t.register <--. v; t.wire <--. v ]
;;

(* TODO: Automated tests *)
