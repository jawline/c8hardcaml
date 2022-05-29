open! Core
open! Hardcaml
open! Signal
open Always
     open Variable 

let clock = Signal.input "clock" 1;;
let clear = Signal.input "clear" 1;;
let r_sync = Reg_spec.create ~clock ~clear ();;

let write = reg ~enable:vdd ~width:1 r_sync;;
let read = reg ~enable:vdd ~width:1 r_sync ;;

let read_address = reg ~enable:vdd ~width:16 r_sync ;;
let write_address = reg ~enable:vdd ~width:16 r_sync ;;

let write_data = reg ~enable:vdd ~width:8 r_sync ;;

let read_data =
   let read_ports = (Ram.create
  ~collision_mode:Read_before_write
  ~size:4096
  ~write_ports:[| { write_enable = write.value; write_address = write_address.value ; write_data = write_data.value  ; write_clock = clock } |]
  ~read_ports:[| { read_enable = read.value; read_address = read_address.value ; read_clock = clock } |]) in
   Array.get read_ports 0
