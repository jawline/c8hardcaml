open! Core
open! Hardcaml
open! Signal
open Global

module Draw_state = struct
  (** Writing a byte is a five cycle process because writes don't need to be
      aligned. 
      
      We read the byte we want to read then read in the left
      (framebuffer_addr) byte then write the cut of the number of bits of i
      that should be in the byte.
      
      We then do the same for the byte on the right.*)
  type t =
    | Read_i
    | Read_lhs
    | Write_lhs
    | Read_rhs
    | Write_rhs
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
    ; memory : 'a Main_memory.In_circuit.O.t
    ; flag : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

(** Because writes can be unaligned we need to write to either side of two bytes. *)
let draw_side
    ~(state : Draw_state.t Always.State_machine.t)
    ~(ram : Main_memory.Wires.t)
    ~read_data
    ~x
    ~side
    ~current_i
    ~framebuffer_address
    ~collision_accumulator
  =
  let open Always in
  let open Variable in
  let read_next_state =
    match side with
    | `Lhs -> Draw_state.Write_lhs
    | `Rhs -> Write_rhs
  in
  let read_step =
    [ ram.read_address <-- to_main_addr framebuffer_address
    ; state.set_next read_next_state
    ]
    |> proc
  in
  let write_next_state =
    match side with
    | `Lhs -> Draw_state.Read_rhs
    | `Rhs -> Read_i
  in
  let x_offset = sel_bottom x 3 in
  let selected_write_data = wire ~default:(Signal.of_int ~width:8 0) in
  let padded_value_to_write bit =
    match side with
    | `Lhs -> srl current_i bit
    | `Rhs -> sll current_i (7 - bit + 1)
  in
  (* We write the value from i, offset by bits XOR the current value of the byte *)
  let new_framebuffer_value bit = padded_value_to_write bit ^: read_data in
  let write_data =
    Sequence.range 0 8
    |> Sequence.map ~f:(fun bit ->
           proc
             [ when_
                 (x_offset ==:. bit)
                 [ selected_write_data <-- new_framebuffer_value bit ]
             ])
    |> Sequence.to_list
    |> proc
  in
  (* TODO: I think this could be cleaner *)
  let collided =
    selected_write_data.value &: read_data <>: read_data |: collision_accumulator.value
  in
  let write_step =
    [ write_data
    ; ram.write_enable <--. 1
    ; ram.write_address <-- framebuffer_address
    ; ram.write_data <-- selected_write_data.value
    ; state.set_next write_next_state
    ; collision_accumulator <-- collided
    ]
    |> proc
  in
  read_step, write_step, collided
;;

let create ~spec (i : _ I.t) =
  let open Always in
  let open Variable in
  let state = State_machine.create (module Draw_state) ~enable:vdd spec in
  let x = to_main_addr i.x in
  let y = to_main_addr i.y in
  let n = to_addr i.n in
  let i_register = i.i in
  let finished = wire ~default:(Signal.of_int ~width:1 0) in
  let ram = Main_memory.Wires.create () in
  (* Step calculates the current depth into the draw operation *)
  let step = reg ~enable:vdd ~width:(wsz `Address) spec in
  let collision_accumulator = reg ~enable:vdd ~width:1 spec in
  let last_step = step.value ==: n in
  let framebuffer_address =
    let step_value_as_address = to_main_addr step.value in
    (* Shifting y left by 3 is the same as multiply it by screen_width / 8 *)
    let row_offset = sll (y +: step_value_as_address) 3 in
    (* Shifting x right by three is the same as dividing it by 8 *)
    let framebuffer_offset = srl x 3 +: row_offset in
    framebuffer_offset +:. Main_memory.framebuffer_start
  in
  let current_i = Variable.reg ~enable:vdd ~width:8 spec in
  let read_i_step =
    [ ram.read_address <-- to_main_addr (i_register +: step.value)
    ; state.set_next Read_lhs
    ; current_i <--. 0
    ]
  in
  (* I will be read one cycle before read_lhs so we need to write it down
     when on that side. *)
  let read_data = i.memory.read_data in
  let read_lhs, write_lhs, _first_collide =
    draw_side
      ~read_data
      ~state
      ~ram
      ~x
      ~side:`Lhs
      ~current_i:current_i.value
      ~framebuffer_address
      ~collision_accumulator
  in
  let read_lhs = proc [ current_i <-- i.memory.read_data; read_lhs ] in
  let read_rhs, write_rhs, collided =
    draw_side
      ~read_data
      ~state
      ~ram
      ~x
      ~side:`Rhs
      ~current_i:current_i.value
      ~framebuffer_address:(framebuffer_address +:. 1)
      ~collision_accumulator
  in
  (* On write_rhs we should increment the step *)
  let write_rhs = proc [ write_rhs; step <-- step.value +:. 1 ] in
  (* TODO: Collision *)
  let set_finished_and_reset =
    proc
      [ state.set_next Read_i
      ; step <--. 0
      ; finished <--. 1
      ; current_i <--. 0
      ; collision_accumulator <--. 0
      ]
  in
  let step_draw =
    proc
      [ state.switch
          [ Read_i, read_i_step
          ; Read_lhs, [ read_lhs ]
          ; Write_lhs, [ write_lhs ]
          ; Read_rhs, [ read_rhs ]
          ; Write_rhs, [ write_rhs ]
          ]
      ]
  in
  compile
    [ if_
        i.enable
        [ if_ last_step [ set_finished_and_reset ] [ step_draw ] ]
        [ state.set_next Read_i ]
    ];
  { O.finished = finished.value
  ; step = step.value
  ; memory = Main_memory.Wires.to_output ram
  ; flag = collided
  }
;;

module Test = struct
  (* TODO: Test this instruction better. *)

  let test ~cycles ~x ~y ~n ~i =
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
    inputs.x := Bits.of_int ~width:8 x;
    inputs.y := Bits.of_int ~width:8 y;
    inputs.n := Bits.of_int ~width:4 n;
    inputs.i := Bits.of_int ~width:12 i;
    let print_outputs () =
      let pp v = Bits.to_int !v in
      printf
        "Finished: %i Step: %i Read addr: %i  Write addr: %i %i %i\n"
        (pp output.finished)
        (pp output.step)
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
    test ~cycles:64 ~x:12 ~y:5 ~n:2 ~i:231;
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
