open! Core
open! Hardcaml
open! Signal
open Global

(** An implementation of the draw instruction in hardware. We cycle through
    i to i + n writing the bits under I to the frame buffer. Every other cycle
    we read the current state of the frame buffer so that we can test collision
    until (2N) cycles has occured (one cycle to read the framebuffer and one
    to read I). *)

module Draw_state = struct
  type t =
    | Read
    | Write
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; x : 'a [@bits 8]
    ; y : 'a [@bits 8]
    ; n : 'a [@bits 4]
    ; i : 'a [@bits 12]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { finished : 'a [@bits 1]
    ; step : 'a [@bits 12]
    ; read : 'a [@bits 1]
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
  let read = wire ~default:(Signal.of_int ~width:1 0) in
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
    [ read <--. 1
    ; read_address <-- to_main_addr (i_register +: step.value)
    ; state.set_next Write
    ]
  in
  let write_step =
    [ step <-- step.value +:. 1
    ; write_enable <--. 1
    ; write_address <-- framebuffer_address
    ; write_data <-- i.memory.read_data
    ; state.set_next Read
    ]
  in
  (* TODO: Bits not bytes! *)
  (* TODO: Collision *)
  let set_finished_and_reset =
    proc [ state.set_next Read; step <--. 0; finished <--. 1 ]
  in
  let step_draw = proc [ state.switch [ Read, read_step; Write, write_step ] ] in
  compile
    [ when_
        i.enable
        [ if_ (step.value ==: n +:. 1) [ set_finished_and_reset ] [ step_draw ] ]
    ];
  { O.finished = finished.value
  ; step = step.value
  ; read = read.value
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

  let test ~cycles ~x ~y ~n ~i =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create ~spec:r_sync) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    (* TODO: Something weird is going on if draw is active on the first cycle *)
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    inputs.enable := Bits.of_int ~width:1 1;
    inputs.x := Bits.of_int ~width:8 x;
    inputs.y := Bits.of_int ~width:8 y;
    inputs.n := Bits.of_int ~width:4 n;
    inputs.i := Bits.of_int ~width:12 i;
    let output : _ O.t = Cyclesim.outputs sim in
    let print_outputs () =
      let pp v = Bits.to_int !v in
      printf
        "Finished: %i Step: %i Read Step: %i Read addr: %i  Write addr: %i %i %i\n"
        (pp output.finished)
        (pp output.step)
        (pp output.read)
        (pp output.memory.read_address)
        (pp output.memory.write_address)
        (pp output.memory.write_data)
        (pp output.memory.write_enable)
    in
    Sequence.(
      range 0 cycles
      |> iter ~f:(fun _ ->
             Cyclesim.cycle sim;
             print_outputs ()));
    (* TODO: Draw tests *)
    ()
  ;;

  let%expect_test "seed and cycle" =
    test ~cycles:16 ~x:12 ~y:5 ~n:4 ~i:231;
    [%expect
      {|
      Finished: 0 Step: 0 Read Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 1 Read Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read Step: 0 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 2 Read Step: 1 Read addr: 233  Write addr: 0 0 0
      Finished: 0 Step: 2 Read Step: 0 Read addr: 0  Write addr: 4409 0 1
      Finished: 0 Step: 3 Read Step: 1 Read addr: 234  Write addr: 0 0 0
      Finished: 0 Step: 3 Read Step: 0 Read addr: 0  Write addr: 4417 0 1
      Finished: 0 Step: 4 Read Step: 1 Read addr: 235  Write addr: 0 0 0
      Finished: 0 Step: 4 Read Step: 0 Read addr: 0  Write addr: 4425 0 1
      Finished: 1 Step: 5 Read Step: 0 Read addr: 0  Write addr: 0 0 0
      Finished: 0 Step: 0 Read Step: 1 Read addr: 231  Write addr: 0 0 0
      Finished: 0 Step: 0 Read Step: 0 Read addr: 0  Write addr: 4393 0 1
      Finished: 0 Step: 1 Read Step: 1 Read addr: 232  Write addr: 0 0 0
      Finished: 0 Step: 1 Read Step: 0 Read addr: 0  Write addr: 4401 0 1
      Finished: 0 Step: 2 Read Step: 1 Read addr: 233  Write addr: 0 0 0
      Finished: 0 Step: 2 Read Step: 0 Read addr: 0  Write addr: 4409 0 1 |}]
  ;;
end
