open! Core
open! Hardcaml
open! Signal

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
