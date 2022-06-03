open Core
open Hardcaml
open Signal
open Always
open Global

let ram_size = 4096

(* One bit per pixel in a 64x32 display *)
let frame_buffer = screen_width * screen_height / 8

(** A helper module to wrap wires to read and write from memory in a default structure *)

type t =
  { write_enable : Always.Variable.t
  ; write_address : Always.Variable.t
  ; write_data : Always.Variable.t
  ; read_address : Always.Variable.t
  ; read_data : Signal.t
  ; framebuffer_start : int
  }

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
  { write_enable
  ; write_address
  ; write_data
  ; read_address
  ; read_data
  ; framebuffer_start = ram_size
  }
;;
