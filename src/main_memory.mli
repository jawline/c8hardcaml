open! Core
open Hardcaml

(* A helper module to propagate wires that set the read and write
    data lines in board RAM. *)

(** The amount of program addressable memory on the system *)
val ram_size : int

(** The stack stores return locations only. A size of 2x128 = 128 nested calls before
    a stack overflow. The stack is not program addressable. *)
val stack_size : int

(** The framebuffer size in bytes. (width / 8) * height because a single bit is used per pixel. *)
val framebuffer_size : int

(** The total memory size for the system *)
val memory_size : int

(** The start address of the stack *)
val stack_start : int

(** The start address of the framebuffer *)
val framebuffer_start : int

(** This is the start address where the machine ROM (font characters) go in memory. *)
val machine_rom_offset : int

(** This ROM should be programmed into main memory before enabling the CPU. It contains font data *)
val machine_rom : int array

type main_memory =
  { write_enable : Always.Variable.t
  ; write_address : Always.Variable.t
  ; write_data : Always.Variable.t
  ; read_address : Always.Variable.t
  ; read_data : Signal.t
  }

type t = main_memory

module In_circuit : sig
  module I : sig
    type 'a t = { read_data : 'a [@bits 8] } [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    module Just_read : sig
      type 'a t = { read_address : 'a [@bits 16] } [@@deriving sexp_of, hardcaml]

      val always_create : read_address:Always.Variable.t -> Of_signal.comb t
      val t_of_main_memory : main_memory -> Of_signal.comb t
    end

    type 'a t =
      { read_address : 'a [@bits 16]
      ; write_enable : 'a [@bits 1]
      ; write_address : 'a [@bits 16]
      ; write_data : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]

    val create
      :  read_address:Signal.t
      -> write_enable:Signal.t
      -> write_address:Signal.t
      -> write_data:Signal.t
      -> Of_signal.comb t

    val always_create
      :  read_address:Always.Variable.t
      -> write_enable:Always.Variable.t
      -> write_address:Always.Variable.t
      -> write_data:Always.Variable.t
      -> Of_signal.comb t

    val t_of_main_memory : main_memory -> Of_signal.comb t
  end
end

module Wires : sig
  (** An intermediate store of main memory state for propagation back to the root. *)
  type t =
    { write_enable : Always.Variable.t
    ; write_address : Always.Variable.t
    ; write_data : Always.Variable.t
    ; read_address : Always.Variable.t
    }

  val create : unit -> t
  val to_main_memory : read_data:In_circuit.O.Of_signal.comb -> t -> main_memory
  val to_output : t -> In_circuit.O.Of_signal.comb In_circuit.O.t
end

val create : unit -> t

val circuit_with_memory
  :  t
  -> f:
       (memory:In_circuit.O.Of_signal.comb In_circuit.I.t
        -> 'a * In_circuit.O.Of_signal.comb In_circuit.O.t)
  -> 'a * Always.t

val circuit_with_just_read_memory
  :  t
  -> f:
       (memory:In_circuit.O.Of_signal.comb In_circuit.I.t
        -> 'a * In_circuit.O.Of_signal.comb In_circuit.O.Just_read.t)
  -> 'a * Always.t

val write : t -> Signal.t -> Signal.t -> Always.t
