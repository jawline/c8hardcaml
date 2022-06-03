open! Core
open! Hardcaml
open! Signal
open Global

module Draw_state = struct
  type t =
    | Read
    | Write
  [@@deriving sexp_of, compare, enumerate]
end

(** An implementation of a simple pseudorandom number generator
    (XOR-shift 64) in hardware for the random opcodes.
    
    If the seed input is not zero then the current state
    will be overwritten with the new seed, otherwise a new random
    number will be generated each cycle. *)

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; x : 'a [@bits 8]
    ; y : 'a [@bits 8]
    ; n : 'a [@bits 8]
    ; i : 'a [@bits 16]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { finished : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

let create ~spec (i : _ I.t) =
  let open Always in
  let open Variable in
  (* TODO: Collision bit *)
  (* TODO: Test this instruction *)
  let state = State_machine.create (module Draw_state) ~enable:vdd spec in
  let x = to_main_addr i.x in
  let y = to_main_addr i.y in
  let n = to_addr i.n in
  let i_register = i.i in
  let finished = wire ~default:(Signal.of_int ~width:1 0) in
  let write_enable = wire ~default:(Signal.of_int ~width:1 0) in
  let write_address = wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0) in
  let write_data = wire ~default:(Signal.of_int ~width:(Sized.size `Byte) 0) in
  let read_address = wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0) in
  (* Step calculates the current depth into the draw operation *)
  let step = Variable.reg ~enable:vdd ~width:(Sized.size `Address) spec in
  let framebuffer_address =
    let step_value_as_address = to_main_addr step.value in
    (* Shifting y left by 3 is the same as multiply it by screen_width / 8 *)
    let row_offset = sll (y +: step_value_as_address) 3 in
    (* Shifting x right by three is the same as dividing it by 8 *)
    let framebuffer_offset = srl x 3 +: row_offset in
    framebuffer_offset +:. Main_memory.framebuffer_start
  in
  let read_step =
    [ read_address <-- (to_main_addr (i_register +: step.value)); state.set_next Write ]
  in
  let write_step =
    [ step <-- step.value +:. 1
    ; write_enable <--. 1
    ; write_address <-- framebuffer_address
    ; write_data <-- i.memory.read_data
    ; state.set_next Read
    ]
  in
  let set_finished_and_reset = proc [ state.set_next Read; step <--. 0 ; finished <--. 1 ] in
  let step_draw = proc [ state.switch [ Read, read_step; Write, write_step ] ] in
  compile [ when_ (i.enable) [ if_ (step.value +:. 1 ==: n) [ set_finished_and_reset ] [ step_draw ] ] ];
  { O.finished = finished.value
  ; memory =
      Main_memory.In_circuit.O.always_create
        ~read_address
        ~write_enable
        ~write_address
        ~write_data
  }
;;

module Test = struct
  let cycle sim (_o : _ O.t) = Cyclesim.cycle sim

  let test ~cycles:_ =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create ~spec:r_sync) in
    let _inputs : _ I.t = Cyclesim.inputs sim in
    let _outputs : _ O.t = Cyclesim.outputs sim in
    (* TODO: Draw tests *)
    ()
  ;;

  let%expect_test "seed and cycle" =
    test ~cycles:1000;
    [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    ("[+:] got inputs of different widths"
      ((wire (width 16) (data_in i))
        (register (width 12)
          ((clock clock) (clock_edge Rising) (clear clear) (clear_level High)
            (clear_to 0x000) (enable 0b1))
          (data_in wire))))
    Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
    Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
    Called from Hardcaml__Comb.Make.(+:) in file "src/comb.ml", line 491, characters 4-40
    Called from C8__Draw.create in file "src/draw.ml", line 68, characters 37-63
    Called from Hardcaml__Circuit.With_interface.create_exn in file "src/circuit.ml", line 380, characters 18-30
    Called from Hardcaml__Cyclesim.With_interface.create in file "src/cyclesim.ml", line 112, characters 18-81
    Called from C8__Draw.Test.test in file "src/draw.ml", line 96, characters 14-52
    Called from C8__Draw.Test.(fun) in file "src/draw.ml", line 104, characters 4-21
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
  ;;
end
