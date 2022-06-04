open Core
open Hardcaml
open Signal
open Always
open Global

let ram_size = 4096
let framebuffer_start = ram_size

(* One bit per pixel in a 64x32 display *)
let frame_buffer = screen_width * screen_height / 8

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
    end

    type 'a t =
      { read_address : 'a [@bits 16]
      ; write_enable : 'a [@bits 1]
      ; write_address : 'a [@bits 16]
      ; write_data : 'a [@bits 16]
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
  end
end

(** A helper module to wrap wires to read and write from memory in a default structure *)

type t =
  { write_enable : Always.Variable.t
  ; write_address : Always.Variable.t
  ; write_data : Always.Variable.t
  ; read_address : Always.Variable.t
  ; read_data : Signal.t
  }

let create_with_in_circuit (t : t) ~f =
  let open Always in
  let output, { In_circuit.O.write_enable; write_address; write_data; read_address } =
    f ~input:{ In_circuit.I.read_data = t.read_data }
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

let create_with_in_circuit_just_read (t : t) ~f =
  let open Always in
  let output, { In_circuit.O.Just_read.read_address } =
    f ~input:{ In_circuit.I.read_data = t.read_data }
  in
  let connect_outputs_to_ram = proc [ t.read_address <-- read_address ] in
  output, connect_outputs_to_ram
;;

let machine_ram ~write_enable ~write_address ~write_data ~read_address =
  let read_ports =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:(ram_size + frame_buffer)
      ~write_ports:[| { write_enable; write_address; write_data; write_clock = clock } |]
      ~read_ports:[| { read_enable = vdd; read_address; read_clock = clock } |]
      ()
  in
  Array.get read_ports 0
;;

let create () =
  let write_enable = Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Bit) 0) in
  let write_address =
    Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0)
  in
  let write_data = Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Byte) 0) in
  let read_address =
    Variable.wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0)
  in
  let read_data =
    machine_ram
      ~write_enable:write_enable.value
      ~write_address:write_address.value
      ~write_data:write_data.value
      ~read_address:read_address.value
  in
  { write_enable; write_address; write_data; read_address; read_data }
;;
