open! Core
open! Hardcaml
open! Signal

module Sized = struct
  let size t =
    match t with
    | `Bit -> 1
    | `Address -> 12
    | `Main_address -> 16
    | `Byte -> 8
  ;;
end

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()
let screen_width = 64
let screen_height = 32
let to_main_addr addr = uresize addr (Sized.size `Main_address)
let to_addr addr = uresize addr (Sized.size `Address)

let reg_false () = Always.Variable.wire ~default:(Signal.of_int ~width:1 0)
