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
    | `Opcode -> 16
  ;;
end

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()
let r_enabled ~enable = Reg_spec.create ~reset:(enable ==:. 0) ~clock ~clear ()
let screen_width = 64
let screen_height = 32
let to_byte v = uresize v (Sized.size `Byte)
let to_main_addr addr = uresize addr (Sized.size `Main_address)
let to_addr addr = uresize addr (Sized.size `Address)
let wire_false () = Always.Variable.wire ~default:(Signal.of_int ~width:1 0)
let is_set v = v ==:. 1
let set_high v = Always.(v <--. 1)
