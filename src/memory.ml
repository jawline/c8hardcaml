open! Core
open! Hardcaml
open! Signal

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()

module I = struct
  type 'a t =
    { clock : 'a
    ; address : 'a [@bits 64]
    ; write: 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    {
     address: 'a [@bits 64]
   ; value: 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end
