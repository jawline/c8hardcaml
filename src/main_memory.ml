open Core
open Hardcaml
open Signal
open Always
open Global

let ram_size = 4096
let stack_size = 2 * 128
let framebuffer_size = screen_width * screen_height / 8
let memory_size = ram_size + stack_size + framebuffer_size
let stack_start = ram_size
let framebuffer_start = ram_size + stack_size
let machine_rom_offset = 0

let machine_rom =
  [| 0xF0
   ; 0x90
   ; 0x90
   ; 0x90
   ; 0xF0
   ; 0x20
   ; 0x60
   ; 0x20
   ; 0x20
   ; 0x70
   ; 0xF0
   ; 0x10
   ; 0xF0
   ; 0x10
   ; 0xF0
   ; 0xF0
   ; 0x10
   ; 0xF0
   ; 0x10
   ; 0xF0
   ; 0x90
   ; 0x90
   ; 0xF0
   ; 0x10
   ; 0x10
   ; 0xF0
   ; 0x80
   ; 0xF0
   ; 0x10
   ; 0xF0
   ; 0xF0
   ; 0x80
   ; 0xF0
   ; 0x90
   ; 0xF0
   ; 0xF0
   ; 0x10
   ; 0x20
   ; 0x40
   ; 0x40
   ; 0xF0
   ; 0x90
   ; 0xF0
   ; 0x90
   ; 0xF0
   ; 0xF0
   ; 0x90
   ; 0xF0
   ; 0x10
   ; 0xF0
   ; 0xF0
   ; 0x90
   ; 0xF0
   ; 0x90
   ; 0x90
   ; 0xE0
   ; 0x90
   ; 0xE0
   ; 0x90
   ; 0xE0
   ; 0xF0
   ; 0x80
   ; 0x80
   ; 0x80
   ; 0xF0
   ; 0xE0
   ; 0x90
   ; 0x90
   ; 0x90
   ; 0xE0
   ; 0xF0
   ; 0x80
   ; 0xF0
   ; 0x80
   ; 0xF0
   ; 0xF0
   ; 0x80
   ; 0xF0
   ; 0x80
   ; 0x80
  |]
;;

type main_memory =
  { write_enable : Always.Variable.t
  ; write_address : Always.Variable.t
  ; write_data : Always.Variable.t
  ; read_address : Always.Variable.t
  ; read_data : Signal.t
  }

type t = main_memory

module In_circuit = struct
  module I = struct
    type 'a t = { read_data : 'a [@bits 8] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    module Just_read = struct
      type 'a t = { read_address : 'a [@bits 16] } [@@deriving sexp_of, hardcaml]

      let always_create ~(read_address : Always.Variable.t) =
        { read_address = read_address.value }
      ;;

      let t_of_main_memory ({ read_address; _ } : main_memory) =
        always_create ~read_address
      ;;
    end

    type 'a t =
      { read_address : 'a [@bits 16]
      ; write_enable : 'a [@bits 1]
      ; write_address : 'a [@bits 16]
      ; write_data : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]

    let create ~read_address ~write_enable ~write_address ~write_data =
      { read_address; write_enable; write_address; write_data }
    ;;

    let always_create
        ~(read_address : Always.Variable.t)
        ~(write_enable : Always.Variable.t)
        ~(write_address : Always.Variable.t)
        ~(write_data : Always.Variable.t)
      =
      create
        ~read_address:read_address.value
        ~write_enable:write_enable.value
        ~write_address:write_address.value
        ~write_data:write_data.value
    ;;

    let t_of_main_memory
        ({ read_address; write_enable; write_address; write_data; read_data = _ } :
          main_memory)
      =
      always_create ~read_address ~write_enable ~write_address ~write_data
    ;;
  end
end

let machine_ram ~write_enable ~write_address ~write_data ~read_address =
  let read_ports =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:memory_size
      ~write_ports:[| { write_enable; write_address; write_data; write_clock = clock } |]
      ~read_ports:[| { read_enable = vdd; read_address; read_clock = clock } |]
      ()
  in
  Array.get read_ports 0
;;

module Wires = struct
  type t =
    { write_enable : Always.Variable.t
    ; write_address : Always.Variable.t
    ; write_data : Always.Variable.t
    ; read_address : Always.Variable.t
    }

  (* This creates t of wires for use as an intermediate store
           of main memory assignments. Main memory is not wired to it. *)
  let create () =
    let write_enable =
      Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Bit) 0)
    in
    let write_address =
      Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0)
    in
    let write_data = Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Byte) 0) in
    let read_address =
      Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0)
    in
    { write_enable; write_address; write_data; read_address }
  ;;

  (** Convert this to a mock main memory which is not necessarily wired into the main memory *)
  let to_main_memory ~read_data { write_enable; write_address; write_data; read_address } =
    { write_enable; write_address; write_data; read_address; read_data }
  ;;

  let to_output { write_enable; write_address; write_data; read_address } =
    { In_circuit.O.write_enable = write_enable.value
    ; write_address = write_address.value
    ; write_data = write_data.value
    ; read_address = read_address.value
    }
  ;;
end

let create () =
  let ({ Wires.write_enable; write_address; write_data; read_address } as wires) =
    Wires.create ()
  in
  let read_data =
    machine_ram
      ~write_enable:write_enable.value
      ~write_address:write_address.value
      ~write_data:write_data.value
      ~read_address:read_address.value
  in
  Wires.to_main_memory ~read_data wires
;;

let circuit_with_memory (t : t) ~f =
  let open Always in
  let output, { In_circuit.O.write_enable; write_address; write_data; read_address } =
    f ~memory:{ In_circuit.I.read_data = t.read_data }
  in
  let connect_outputs_to_ram =
    proc
      [ t.write_enable <-- write_enable
      ; t.write_address <-- write_address
      ; t.write_data <-- write_data
      ; t.read_address <-- read_address
      ]
  in
  output, connect_outputs_to_ram
;;

let circuit_with_just_read_memory (t : t) ~f =
  let open Always in
  let output, { In_circuit.O.Just_read.read_address } =
    f ~memory:{ In_circuit.I.read_data = t.read_data }
  in
  let connect_outputs_to_ram = proc [ t.read_address <-- read_address ] in
  output, connect_outputs_to_ram
;;

let write t addr value =
  let open Always in
  proc
    [ t.write_enable <--. 1
    ; t.write_address <-- to_main_addr addr
    ; t.write_data <-- value
    ]
;;
