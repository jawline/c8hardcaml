open! Core
open! Hardcaml
open! Signal

module In_circuit = struct
  type 'a t =
    { pc : 'a [@bits 12]
    ; i : 'a [@bits 12]
    ; sp : 'a
          [@bits 32]
          (* Rename these to Vn or some other name to separate it from the module name? *)
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

let create ~spec =
  Always.(
    Variable.(
      let pc = reg ~enable:vdd ~width:12 spec in
      let i = reg ~enable:vdd ~width:12 spec in
      let sp = reg ~enable:vdd ~width:32 spec in
      let registers = List.init 16 ~f:(fun _ -> reg ~enable:vdd ~width:8 spec) in
      { pc; i; sp; registers }))
;;

let to_circuit t =
  { In_circuit.pc = t.pc.value
  ; i = t.i.value
  ; sp = t.sp.value
  ; registers = List.map t.registers ~f:(fun r -> r.value)
  }
;;

let assign_of_circuit
    ({ pc; i; sp; registers } : t)
    ({ pc = new_pc; i = new_i; sp = new_sp; registers = new_registers } : _ In_circuit.t)
  =
  let open Always in
  let assign_registers =
    List.map (List.zip_exn registers new_registers) ~f:(fun (register, new_register) ->
        register <-- new_register)
    |> proc
  in
  proc [ pc <-- new_pc; i <-- new_i; sp <-- new_sp; assign_registers ]
;;
