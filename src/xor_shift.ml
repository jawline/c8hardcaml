open! Core
open! Hardcaml
open! Signal
open Global

(** An implementation of a simple pseudorandom number generator
    (XOR-shift 64) in hardware for the random opcodes.
    
    If the seed input is not zero then the current state
    will be overwritten with the new seed, otherwise a new random
    number will be generated each cycle. *)

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; seed : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { pseudo_random : 'a [@bits 64] } [@@deriving sexp_of, hardcaml]
end

let create (i : _ I.t) =
  let open Always in
  let open Variable in
  let pseudo_random = reg ~enable:vdd ~width:64 r_sync in
  let next = pseudo_random.value in
  let next = next ^: sll next 13 in
  let next = next ^: srl next 7 in
  let next = next ^: sll next 17 in
  let next_value = wire ~default:next in
  compile
    [ when_ (i.seed <>:. 0) [ next_value <-- i.seed ]
    ; pseudo_random <-- next_value.value
    ];
  { O.pseudo_random = next_value.value }
;;

module Test = struct
  let sim_set_seed ~seed (i : _ I.t) = i.seed := Bits.of_int ~width:64 seed

  let cycle sim (o : _ O.t) =
    Cyclesim.cycle sim;
    printf "Random: %i\n" (Bits.to_int !(o.pseudo_random))
  ;;

  let test ~seed ~cycles =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    sim_set_seed ~seed inputs;
    (* Cycle twice to show that the random number generator does not start
       when seed is set *)
    cycle sim outputs;
    cycle sim outputs;
    (* Reset seed to zero and generate ~cycle random values *)
    sim_set_seed ~seed:0 inputs;
    Sequence.range 0 cycles |> Sequence.iter ~f:(fun _ -> cycle sim outputs);
    ()
  ;;

  let%expect_test "seed and cycle" =
    test ~seed:967531223135 ~cycles:32;
    [%expect
      {|
      Random: 967531223135
      Random: 967531223135
      Random: 2482561710366037723
      Random: -2821728140033329866
      Random: 3381513551776863580
      Random: -2993722293645530378
      Random: 4249267510368762147
      Random: -1151875455053916399
      Random: -3545341970826856065
      Random: 4264953125363019597
      Random: -2812446833441639213
      Random: -4527595793815391238
      Random: -3784639190650379235
      Random: -340588717576092395
      Random: -1202714723245203873
      Random: 2191374455794140451
      Random: -3632721436417696023
      Random: 3569978878405248020
      Random: 3725712344880807196
      Random: 3180746587244554982
      Random: -159614491943408205
      Random: -2263717540960334936
      Random: -2632263047041606721
      Random: -3315540437908262584
      Random: 2700180976052056746
      Random: 2708387420503272271
      Random: -2184267637560135847
      Random: -1387021846703511481
      Random: 3436143133380580951
      Random: 91996190692375483
      Random: -436954958897863284
      Random: 154825853390479047
      Random: 2557878309111941818
      Random: -4103811135862328657 |}]
  ;;
end
