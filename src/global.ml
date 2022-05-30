open! Core
open! Hardcaml
open! Signal

let word_size = 16
let byte_size = 8

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1

let r_sync = Reg_spec.create ~clock ~clear ()

let machine_ram ~write_enable ~write_address ~write_data ~read_address =
  let read_ports =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:4096
      ~write_ports:[| { write_enable; write_address; write_data; write_clock = clock } |]
      ~read_ports:[| { read_enable = vdd; read_address; read_clock = clock } |]
      ()
  in
  Array.get read_ports 0
;;
