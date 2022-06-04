open! Core
open! Hardcaml
open! Signal
open Global

(** An implementation of fetch that takes 2 cycles
    to read a 16-bit opcode from main memory. *)

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
    ; fetch_cycle : 'a [@bits 2]
    ; opcode : 'a [@bits 16]
    ; memory : 'a Main_memory.In_circuit.O.Just_read.t
    }
  [@@deriving sexp_of, hardcaml]
end

let create ~spec (i : _ I.t) =
  let open Always in
  let open Variable in
  let read_address = wire ~default:(Signal.of_int ~width:16 0) in
  let finished = wire ~default:(Signal.of_int ~width:1 0) in
  let fetch_cycle = reg ~enable:vdd ~width:2 spec in
  let op_first = reg ~enable:vdd ~width:8 spec in
  let op = concat_msb [ op_first.value; i.memory.read_data ] in
  (* Set up a read of the first byte *)
  let first_cycle = proc [ read_address <-- to_main_addr i.program_counter ] in
  (* Store the first read byte and set up the next read *)
  let second_cycle =
    proc
      [ op_first <-- i.memory.read_data
      ; read_address <-- to_main_addr (i.program_counter +:. 1)
      ]
  in
  let fetch_logic =
    proc
      [ when_
          (fetch_cycle.value ==:. 0)
          [ first_cycle; fetch_cycle <--. 1; finished <--. 0 ]
      ; when_ (fetch_cycle.value ==:. 1) [ second_cycle; fetch_cycle <--. 2 ]
      ; when_ (fetch_cycle.value ==:. 2) [ finished <--. 1; fetch_cycle <--. 0 ]
      ]
  in
  compile
    [ if_ (i.in_fetch ==:. 1) [ fetch_logic ] [ fetch_cycle <--. 0; finished <--. 0 ] ];
  { O.finished = finished.value
  ; fetch_cycle = fetch_cycle.value
  ; opcode = op
  ; memory = Main_memory.In_circuit.O.Just_read.always_create ~read_address
  }
;;

module Test = struct
  let cycle sim (_o : _ O.t) = Cyclesim.cycle sim

  let test () =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create ~spec:r_sync) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs, outputs, sim
  ;;

  let set_pc (i : _ I.t) v = i.program_counter := Bits.of_int ~width:12 v
  let set_in_fetch (i : _ I.t) = i.in_fetch := Bits.of_int ~width:1 1
  let set_read (i : _ I.t) v = i.memory.read_data := Bits.of_int ~width:8 v
  let to_int v = Bits.to_int !v

  let print_state (i : _ I.t) (o : _ O.t) =
    printf
      "Inputs: PC: %i in fetch %i read data %i\n"
      (to_int i.program_counter)
      (to_int i.in_fetch)
      (to_int i.memory.read_data);
    printf
      "Finished: %i Fetch_cycle: %i Opcode: %x Read_address: %i\n"
      (to_int o.finished)
      (to_int o.fetch_cycle)
      (Bits.to_int !(o.opcode))
      (Bits.to_int !(o.memory.read_address))
  ;;

  let%expect_test "seed and cycle" =
    let i, o, sim = test () in
    let cycle () =
      Cyclesim.cycle sim;
      print_state i o
    in
    cycle ();
    cycle ();
    [%expect
      {|
       Inputs: PC: 0 in fetch 0 read data 0
       Finished: 0 Fetch_cycle: 0 Opcode: 0 Read_address: 0
       Inputs: PC: 0 in fetch 0 read data 0
       Finished: 0 Fetch_cycle: 0 Opcode: 0 Read_address: 0 |}];
    set_pc i 543;
    cycle ();
    cycle ();
    [%expect
      {|
       Inputs: PC: 543 in fetch 0 read data 0
       Finished: 0 Fetch_cycle: 0 Opcode: 0 Read_address: 0
       Inputs: PC: 543 in fetch 0 read data 0
       Finished: 0 Fetch_cycle: 0 Opcode: 0 Read_address: 0 |}];
    printf "Pre-fetch-enabled\n";
    print_state i o;
    printf "Post-fetch-enabled\n";
    set_in_fetch i;
    cycle ();
    set_read i 1;
    cycle ();
    set_read i 2;
    cycle ();
    [%expect
      {|
       Pre-fetch-enabled
       Inputs: PC: 543 in fetch 0 read data 0
       Finished: 0 Fetch_cycle: 0 Opcode: 0 Read_address: 0
       Post-fetch-enabled
       Inputs: PC: 543 in fetch 1 read data 0
       Finished: 0 Fetch_cycle: 1 Opcode: 0 Read_address: 544
       Inputs: PC: 543 in fetch 1 read data 1
       Finished: 1 Fetch_cycle: 2 Opcode: 101 Read_address: 0
       Inputs: PC: 543 in fetch 1 read data 2
       Finished: 0 Fetch_cycle: 0 Opcode: 102 Read_address: 543 |}];
    cycle ();
    set_read i 3;
    cycle ();
    set_read i 4;
    cycle ();
    [%expect
      {|
       Inputs: PC: 543 in fetch 1 read data 2
       Finished: 0 Fetch_cycle: 1 Opcode: 102 Read_address: 544
       Inputs: PC: 543 in fetch 1 read data 3
       Finished: 1 Fetch_cycle: 2 Opcode: 303 Read_address: 0
       Inputs: PC: 543 in fetch 1 read data 4
       Finished: 0 Fetch_cycle: 0 Opcode: 304 Read_address: 543 |}]
  ;;
end
