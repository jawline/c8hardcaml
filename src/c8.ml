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

module Fibonacci = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; count : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; output : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module States = struct
    type t =
      | Wait_for_seed
      | Counting
      | Finished
    [@@deriving sexp_of, compare, enumerate]
  end

  let create ~w ~work_w (i : 'a I.t) : 'a O.t =
    let open Always in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let state_machine = State_machine.create (module States) ~enable:vdd r_sync in
    let prev_0 = Variable.reg ~width:w ~enable:vdd r_sync in
    let prev_1 = Variable.reg ~width:w ~enable:vdd r_sync in
    let counter = Variable.reg ~width:work_w ~enable:vdd r_sync in
    let done_ = Variable.wire ~default:gnd in
    let output = Variable.wire ~default:(zero w) in
    compile
      [ state_machine.switch
          [ ( Wait_for_seed
            , [ prev_0 <--. 1
              ; prev_1 <--. 1
              ; counter <-- i.count -:. 1
              ; state_machine.set_next Counting
              ] )
          ; ( Counting
            , [ if_
                  (counter.value ==:. 0)
                  [ state_machine.set_next Finished ]
                  [ prev_0 <-- prev_1.value
                  ; prev_1 <-- prev_0.value +: prev_1.value
                  ; counter <-- counter.value -:. 1
                  ]
              ] )
          ; ( Finished
            , [ output <-- prev_1.value
              ; done_ <--. 1
              ; state_machine.set_next Wait_for_seed
              ] )
          ]
      ];
    { O.done_ = done_.value; output = output.value }
  ;;

  let test ~create ~work_w ~count =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.count := Bits.of_int ~width:work_w count;
    let step () =
      Cyclesim.cycle sim;
      Bits.to_int !(outputs.done_) = 1
    in
    let rec until ~f = if f () then () else until ~f in
    until ~f:step;
    !(outputs.output)
  ;;

  let%expect_test "16 bit fib with 8 bit counter" =
    let result =
      test ~create:(create ~w:16 ~work_w:8) ~work_w:8 ~count:10
      |> Bits.to_int
      |> Int.to_string
    in
    Core.print_s [%message result];
    [%expect {| 89 |}]
  ;;
end

module Registers = struct
  type 'a t =
    { pc : 'a
    ; i : 'a
    ; v : 'a list
    }
end

module Executor = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; enable: 'a [@bits 1]
      ; opcode: 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      pc: 'a;
      done_: 'a [@bits 1]
    } [@@deriving sexp_of, hardcaml]
  end

  let create (i : _ I.t) =
    let open Always in
    let open Variable in
    let pc = reg ~enable:vdd ~width:12 r_sync in
    let _i = reg ~enable:vdd ~width:12 r_sync in
    let _v = Sequence.map (Sequence.range 0 16) ~f:(fun _ -> reg ~enable:vdd ~width:8 r_sync) in
    compile [
    when_ (i.enable) [
      pc <-- pc.value +:. 2;
    ]
    ];
    { O.pc = pc.value; done_ = vdd }
  ;;

  let test ~create ~enable ~stop_when =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create create in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    inputs.enable := Bits.of_int ~width:1 enable;
    let step () =
      Cyclesim.cycle sim;
      stop_when (Bits.to_int !(outputs.pc)) (Bits.to_int !(outputs.done_))
    in
    let rec until ~f = if f () then () else until ~f in
    until ~f:step;
    !(outputs.pc)
  ;;

  let%expect_test "step (disabled)" =
    let pc = test ~enable:0 ~create ~stop_when:(fun _ _ -> true) in
    let pc = Bits.to_int pc in
    Core.print_s [%message (pc : int)];
    [%expect {| (pc 0) |}]
  ;;

  let%expect_test "step (enabled)" =
    let pc = test ~enable:1 ~create ~stop_when:(fun _ _ -> true) in
    let pc = Bits.to_int pc in
    Core.print_s [%message (pc : int)];
    [%expect {| (pc 2) |}]
  ;;
end
