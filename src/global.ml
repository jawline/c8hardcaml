open! Core
open! Hardcaml
open! Signal

module Size = struct
  type t =
    [ `Bit
    | `Address
    | `Main_address
    | `Byte
    | `Opcode
    ]
end

let wsz t =
  match t with
  | `Bit -> 1
  | `Address -> 12
  | `Main_address -> 16
  | `Byte -> 8
  | `Opcode -> 16
;;

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()
let r_enabled ~enable = Reg_spec.create ~reset:(enable ==:. 0) ~clock ~clear ()
let screen_width = 64
let screen_height = 32
let to_byte v = uresize v (wsz `Byte)
let to_main_addr addr = uresize addr (wsz `Main_address)
let to_addr addr = uresize addr (wsz `Address)
let wire_false () = Always.Variable.wire ~default:(Signal.of_int ~width:1 0)
let is_set v = v ==:. 1
let is_not_set v = v ==:. 0
let set_high v = Always.(v <--. 1)
