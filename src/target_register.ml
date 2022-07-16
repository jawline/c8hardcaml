open! Core
open! Hardcaml
open! Signal

type t =
  { value : Signal.t
  ; index : Signal.t
  ; registers : Registers.t
  }

let create (registers : Registers.t) index =
  let value =
    List.mapi registers.registers ~f:(fun idx register ->
      let target_register = lsb (index ==:. idx) in
      { With_valid.valid = target_register; value = register.value })
    |> onehot_select
  in
  { value; index; registers }
;;

let assign { index; registers = { Registers.registers; _ }; _ } value =
  let open Always in
  proc
    (List.mapi registers ~f:(fun i register ->
       proc [ when_ (index ==:. i) [ register <-- value ] ]))
;;
