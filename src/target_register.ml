open! Core
open! Hardcaml
open! Signal

(* This updates a register using a register index value by one hot assignment.
   If no register matches the target index then none are updated but that should
   be impossible since the index circuit is sized for the number of registers. *)
let onehot_assign_register registers index_circuit value =
  let open Always in
  proc
    (List.mapi registers ~f:(fun i register ->
         proc [ when_ (index_circuit ==:. i) [ register <-- value ] ]))
;;

(* A record that stores a signal of a register index and a one hot encoding of it's value *)
type t =
  { value : Signal.t
  ; index : Signal.t
  ; (* We store a reference to the registers so we can 1-hot assign later *)
    registers : Always.Variable.t list
  }

let create ~(registers : Always.Variable.t list) index =
  let value =
    List.mapi registers ~f:(fun idx register ->
        let target_register = lsb (index ==:. idx) in
        { With_valid.valid = target_register; value = register.value })
    |> onehot_select
  in
  { value; index; registers }
;;

let assign { index; registers; _ } value =
  let open Always in
  proc
    (List.mapi registers ~f:(fun i register ->
         proc [ when_ (index ==:. i) [ register <-- value ] ]))
;;
