open! Core
open! Hardcaml
open! Signal

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()

  module States = struct
    type t =
      | Wait
      | Fetch_op
      | Execute
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a [@bits 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

    let create ~(executor:'a Executor.I.t * 'a Executor.O.t) () =
      let open Always in
      let open Variable in
      let (executor_input, _executor_output) = executor in
      let done_ = reg ~enable:vdd ~width:1 r_sync in
      compile [ done_ <--. 1; executor_input.begin_ <--. 1; executor_input.opcode <--. 0 ];
     { O.done_ }
    ;;

  module Test = struct
  end
