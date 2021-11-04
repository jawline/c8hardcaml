open! Core
open! Hardcaml
open! Signal

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()

module Counter = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; incr : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { dout : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~w (i : _ I.t) =
    { O.dout =
        reg_fb
          (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
          ~enable:i.incr
          ~w
          (fun d -> d +:. 1)
    }
  ;;

  let test ~create ~iterations =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    let step ~clear ~incr _ =
      inputs.clear := if clear = 1 then Bits.vdd else Bits.gnd;
      inputs.incr := if incr = 1 then Bits.vdd else Bits.gnd;
      Cyclesim.cycle sim
    in
    Sequence.iter (Sequence.range 0 iterations) ~f:(step ~clear:0 ~incr:1);
    !(outputs.dout)
  ;;

  let%expect_test "8 bit counter 256 increments" =
    Core.print_s [%message "8 bit counter"];
    let result = test ~create:(create ~w:8) ~iterations:256 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "8 bit counter"
      00000000 |}]
  ;;

  let%expect_test "8 bit counter 255 increments" =
    Core.print_s [%message "8 bit counter"];
    let result = test ~create:(create ~w:8) ~iterations:255 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "8 bit counter"
      11111111 |}]
  ;;

  let%expect_test "8 bit counter 257 increments" =
    Core.print_s [%message "8 bit counter"];
    let result = test ~create:(create ~w:8) ~iterations:257 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "8 bit counter"
      00000001 |}]
  ;;

  let%expect_test "16 bit counter 256 increments" =
    Core.print_s [%message "16 bit counter"];
    let result = test ~create:(create ~w:16) ~iterations:256 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "16 bit counter"
      0000000100000000 |}]
  ;;

  let%expect_test "16 bit counter 255 increments" =
    Core.print_s [%message "16 bit counter"];
    let result = test ~create:(create ~w:16) ~iterations:255 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "16 bit counter"
      0000000011111111 |}]
  ;;

  let%expect_test "16 bit counter 257 increments" =
    Core.print_s [%message "16 bit counter"];
    let result = test ~create:(create ~w:16) ~iterations:257 |> Bits.to_string in
    Core.print_s [%message result];
    [%expect {|
      "16 bit counter"
      0000000100000001 |}]
  ;;
end
