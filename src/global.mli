open! Core
open! Hardcaml
open! Signal

module Size : sig
  type t =
    [ `Bit
    | `Address
    | `Main_address
    | `Byte
    | `Opcode
    ]
end

val wsz : Size.t -> int
val clock : Signal.t
val clear : Signal.t
val r_sync : Reg_spec.t
val r_enabled : enable:Signal.t -> Reg_spec.t
val screen_width : int
val screen_height : int
val to_byte : Signal.t -> Signal.t
val to_main_addr : Signal.t -> Signal.t
val to_addr : Signal.t -> Signal.t
val wire_false : unit -> Always.Variable.t
val is_set : Signal.t -> Signal.t
val is_not_set : Signal.t -> Signal.t
val set_high : Always.Variable.t -> Always.t
