open! Core
open! Hardcaml
open! Signal
open Global

module States = struct
  type t =
    | Wait
    | Fetch_op
    | Execute
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t = {
    start_: 'a [@bits 1]
  } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    done_ : 'a [@bits 1]
  } [@@deriving sexp_of, hardcaml]
end

let create (_i : _ I.t) =
  let open Always.Variable in
  let open Always in

  let state = State_machine.create (module States) ~enable:vdd r_sync in 

  let finished_ = reg ~enable:vdd ~width:1 r_sync in
  let memory_address = reg ~enable:vdd ~width:64 r_sync in

  compile
    [ state.switch
        [ (Wait, [])
        ; (Fetch_op, [])
        ; (Execute, [])
        ]
    ];

  Always.compile [ memory_address <--. 4; write <--. 1; finished_ <--. 1];
  { O.done_ = finished_.value };;

module Test = struct
  let test ~create =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let _inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    print_s [%message (outputs : _ O.t)];
    ()
  ;;

  let%expect_test "8 bit counter 256 increments" =
    test ~create;
    [%expect {| |}]
  ;;
end
