open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; input : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { digit1 : 'a [@bits 4]
    ; digit2 : 'a [@bits 4]
    ; digit3 : 'a [@bits 4]
    }
  [@@deriving sexp_of, hardcaml]
end

let create ({ input; _ } : _ I.t) =
  let open Always in
  let open Variable in
  let zero = Signal.of_int ~width:4 0 in
  let digit1 = wire ~default:zero in
  let digit2 = wire ~default:zero in
  let digit3 = wire ~default:zero in
  let bcd_lut =
    Sequence.range 0 256
    |> Sequence.map ~f:(fun i ->
           let i_as_str = Int.to_string i |> String.rev in
           let pos_to_int i =
             if i < String.length i_as_str
             then String.get i_as_str i |> Char.to_int
             else 0
           in
           let bcd_digit1 = pos_to_int 2 in
           let bcd_digit2 = pos_to_int 1 in
           let bcd_digit3 = pos_to_int 0 in
           proc
             [ when_
                 (input ==:. i)
                 [ digit1 <--. bcd_digit1
                 ; digit2 <--. bcd_digit2
                 ; digit3 <--. bcd_digit3
                 ]
             ])
    |> Sequence.to_list
    |> proc
  in
  compile [ bcd_lut ];
  { O.digit1 = digit1.value; digit2 = digit2.value; digit3 = digit3.value }
;;

module Test = struct
  let test ~inp =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    inputs.input := Bits.of_int ~width:8 inp;
    let outputs : _ O.t = Cyclesim.outputs sim in
    Cyclesim.cycle sim;
    print_s [%message (outputs : Bits.t ref O.t)]
  ;;

  let%expect_test "bcd tests" =
    test ~inp:127;
    test ~inp:255;
    test ~inp:0;
    test ~inp:6;
    test ~inp:55;
    [%expect
      {|
      (outputs ((digit1 0001) (digit2 0010) (digit3 0111)))
      (outputs ((digit1 0010) (digit2 0101) (digit3 0101)))
      (outputs ((digit1 0000) (digit2 0000) (digit3 0000)))
      (outputs ((digit1 0000) (digit2 0000) (digit3 0110)))
      (outputs ((digit1 0000) (digit2 0101) (digit3 0101))) |}]
  ;;
end
