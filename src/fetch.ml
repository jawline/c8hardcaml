open! Core
open! Hardcaml
open! Signal
open Global

(** An implementation of fetch that takes 2 cycles
    to read a 16-bit opcode from main memory.

    When in_fetch is set fetch_cycle (a two bit counter)
    is incremented and read address is set to the
    program counter or program_counter + 1 depending
    on step.

    If step is one then finished is set which will begin
    opcode execution and [opcode] will be set to
    the correct opcode to execute.
    *)

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; program_counter : 'a [@bits 12]
    ; in_fetch : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { finished : 'a [@bits 1]
    ; opcode : 'a [@bits 16]
    ; memory : 'a Main_memory.In_circuit.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

let create ~spec (i : _ I.t) =
  let open Always in
  let open Variable in
  let write_enable = wire ~default:(Signal.of_int ~width:1 0) in
  let write_address = wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0) in
  let write_data = wire ~default:(Signal.of_int ~width:(Sized.size `Byte) 0) in
  let read_address = wire ~default:(Signal.of_int ~width:(Sized.size `Main_address) 0) in
  let fetch_cycle = Variable.reg ~enable:vdd ~width:2 spec in
  let op_first = Variable.reg ~enable:vdd ~width:8 spec in
  let op = concat_msb [ op_first.value; i.memory.read_data ] in
  (* Set up a read of the first byte *)
  let first_cycle = proc [ read_address <-- i.program_counter ] in
  (* Store the first read byte and set up the next read *)
  let second_cycle =
    proc [ op_first <-- i.memory.read_data; read_address <-- i.program_counter +:. 1 ]
  in
  let fetch_logic =
    proc
      [ (* Visible on the next cycle *)
        fetch_cycle <-- fetch_cycle.value +:. 1
      ; when_ (fetch_cycle.value ==:. 0) [ first_cycle ]
      ; when_ (fetch_cycle.value ==:. 1) [ second_cycle ]
      ; when_ (fetch_cycle.value ==:. 2) [ fetch_cycle <--. 0 ]
      ]
  in
  compile [ when_ (i.in_fetch ==:. 0) [ fetch_logic ] ];
  { O.finished = fetch_cycle.value ==:. 2
  ; opcode = op
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
    [@@expect.uncaught_exn
      {|
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
