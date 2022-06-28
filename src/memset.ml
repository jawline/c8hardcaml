open! Core
open! Hardcaml
open! Signal
open Global

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; address : 'a [@bits 16]
    ; size : 'a [@bits 8]
    ; write_data : 'a [@bits 8]
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
  let ram = Main_memory.Wires.create () in
  let set_in_progress = Variable.reg ~enable:vdd ~width:1 spec in
  let write_index = Variable.reg ~enable:vdd ~width:8 spec in
  let finished = wire_false () in
  compile
    [ if_
        i.enable
        [ ram.read_address <--. 0
        ; ram.write_enable <--. 1
        ; ram.write_data <-- i.write_data
        ; if_
            (is_set set_in_progress.value)
            [ ram.write_address <-- write_index.value
            ; write_index <-- write_index.value +:. 1
            ; when_
                (write_index.value ==: i.size -:. 1)
                [ finished <--. 1; set_in_progress <--. 0; write_index <--. 0 ]
            ]
            [ set_in_progress <--. 1
            ; write_index <--. 1
            ; ram.write_address <-- i.address
            ]
        ]
        [ set_in_progress <--. 0; write_index <--. 0 ]
    ];
  { O.finished = finished.value; memory = Main_memory.Wires.to_output ram }
;;

module Test = struct
  let test ~cycles =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create ~spec:r_sync) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let output : _ O.t = Cyclesim.outputs sim in
    (* TODO: Something weird is going on if draw is active on the first cycle *)
    inputs.enable := Bits.of_int ~width:1 0;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    inputs.enable := Bits.of_int ~width:1 1;
    inputs.address := Bits.of_int ~width:16 4096;
    inputs.size := Bits.of_int ~width:8 128;
    let print_outputs () =
      let pp v = Bits.to_int !v in
      printf
        "Finished: %i Read addr: %i  Write addr: %i %i %i\n"
        (pp output.finished)
        (pp output.memory.read_address)
        (pp output.memory.write_address)
        (pp output.memory.write_data)
        (pp output.memory.write_enable)
    in
    print_outputs ();
    printf "Starting main loop\n";
    Sequence.(
      range 0 cycles
      |> iter ~f:(fun _ ->
             Cyclesim.cycle sim;
             print_outputs ()));
    (* TODO: Draw tests *)
    ()
  ;;

  let%expect_test "seed and cycle" =
    test ~cycles:256;
    [%expect
      {|
      Finished: 0 Step: 0 Read addr: 0  Write addr: 0 0 0
      Starting main loop
      Finished: 0 Step: 0 Read addr: 4393  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 0 Read addr: 4394  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4394 0 1
      Finished: 0 Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 4401  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 1 Read addr: 4402  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4402 0 1
      Finished: 1 Step: 2 Read addr: 0  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 231  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 4393  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 0 Read addr: 4394  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4394 0 1
      Finished: 0 Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 4401  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 1 Read addr: 4402  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4402 0 1
      Finished: 1 Step: 2 Read addr: 0  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 231  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 4393  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 0 Read addr: 4394  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4394 0 1
      Finished: 0 Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 4401  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 1 Read addr: 4402  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4402 0 1
      Finished: 1 Step: 2 Read addr: 0  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 231  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 4393  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 0 Read addr: 4394  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4394 0 1
      Finished: 0 Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 4401  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 1 Read addr: 4402  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4402 0 1
      Finished: 1 Step: 2 Read addr: 0  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 231  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 4393  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 0 Read addr: 4394  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4394 0 1
      Finished: 0 Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 4401  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 1 Read addr: 4402  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4402 0 1
      Finished: 1 Step: 2 Read addr: 0  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 231  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 4393  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 0 Read addr: 4394  Write addr: 0 0 0
      Finished: 0 Step: 0 Read addr: 0  Write addr: 4394 0 1
      Finished: 0 Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 4401  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 1 Read addr: 4402  Write addr: 0 0 0
      Finished: 0 Step: 1 Read addr: 0  Write addr: 4402 0 1 |}]
  ;;
end
