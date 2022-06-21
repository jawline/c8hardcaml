open! Core
open! Hardcaml
open! Signal

module In_circuit : sig
  (* TODO: Rename the programmable registers to Vn or some other name to separate it from the module name? *)
  type 'a t =
    { pc : 'a [@bits 12]
    ; i : 'a [@bits 12]
    ; sp : 'a [@bits 12]
    ; registers : 'a list [@length 16] [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

type t =
  { pc : Always.Variable.t
  ; i : Always.Variable.t
  ; sp : Always.Variable.t
  ; registers : Always.Variable.t list
  }
[@@deriving fields]

val create : spec:Reg_spec.t -> t
val to_circuit : t -> In_circuit.Of_signal.comb In_circuit.t

(* TODO: Rename make_assign_of_circuit to make it clear that the Always.t returned does the assigning? *)
val assign_of_circuit : t -> In_circuit.Of_signal.comb In_circuit.t -> Always.t
