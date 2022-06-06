open! Core
open! Hardcaml
open! Signal
open Global

(* Foundation of a CHIP-8 core. Manages the fetch execute cycle
   and maintains a random state for opcodes to use. *)

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; program : 'a [@bits 1]
    ; program_pc : 'a [@bits 12]
    ; program_write_enable : 'a [@bits 1]
    ; program_address : 'a [@bits 16]
    ; program_data : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { core : 'a Cpu_core.O.t
    ; read_data : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

let programming_mode ~(ram : Main_memory.t) (i : _ I.t) =
  let open Always in
  proc
    [ ram.read_address <-- i.program_address
    ; ram.write_enable <-- i.program_write_enable
    ; ram.write_address <-- i.program_address
    ; ram.write_data <-- i.program_data
    ]
;;

let create (i : _ I.t) : _ O.t =
  let open Always in
  let ram = Main_memory.create () in
  let enable = wire_false () in
  let core, core_wiring =
    Main_memory.circuit_with_memory ram ~f:(fun ~memory ->
        let core =
          Cpu_core.create { Cpu_core.I.clear; clock; enable = enable.value; memory }
        in
        core, core.memory)
  in
  compile
    [ if_ (is_set i.program) [ programming_mode ~ram i ] [ core_wiring; set_high enable ]
    ];
  { core; read_data = ram.read_data }
;;
