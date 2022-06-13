open Core
open Async
open Hardcaml
open C8.Programmable_cpu_core
open C8.Global
open Helper

(* Average opcode takes 3 cycles to execute (2 to fetch, one to execute) *)
let rough_cycles_per_second = 512 * 3

let test ~cycles ~rom_file ~create ~print_on_cycle ~print_on_exit =
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let sim = Simulator.create (create ~spec:r_sync) in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  let rand = Random.State.default in
  (* Write font data to main memory *)
  sim_program_machine_rom sim inputs;
  (* Write the test rom to main memory *)
  sim_program_rom sim inputs ~rom:(String.to_list rom_file |> List.map ~f:Char.to_int);
  (* Set all keys to low *)
  List.iter inputs.keys.state ~f:(fun key -> key := Bits.of_int ~width:1 0);
  (* Simulate the program we just wrote running for 100 cycles *)
  Sequence.range 0 cycles
  |> Sequence.iter ~f:(fun i ->
         (* Every second roughly randomly re-assign pressed keys *)
         if i % rough_cycles_per_second = 0
         then
           List.iter inputs.keys.state ~f:(fun key ->
               key := Bits.of_int ~width:1 (Random.State.int rand 1));
         sim_cycle_not_programming sim inputs outputs ~print:print_on_cycle);
  sim_cycle_not_programming sim inputs outputs ~print:print_on_exit;
  printf "%s\n" (frame_buffer_as_string sim inputs outputs);
  ()
;;

let%expect_test "Particle" =
  let%bind rom_file = Reader.file_contents "particle.ch8" in
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 10)
    ~rom_file
    ~create;
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 20)
    ~rom_file
    ~create;
  [%expect
    {|
      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣤⠀⣤⣿⠀⣿⠛⠛⣿⣿⠀⣤⠀⠛⠛⣿⣿⠛⠛⠀⠀⠛⠛⣿⣤⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⣿⣤⠀⠛⠛⠛⣿⠀⠛⠛⠛⠛⠀⠛⠛
      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣿⠀⠀⠛⠀⣿⠛⠛⣿⣿⠀⣿⠀⠀⠀⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⠛⠀⠀⠀⠛⠛⣿⠀⠀⠛⠛⠛⠀⣤⣿
      ⠀⠀⠛⠛⠀⠛⠛⠀⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠀⠀⠛⠛⠀⠛⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠛⠛⠀⠀⠛⠛⠀⠀⠛⠛⠛⠛⠛⠀⠀⠛⠛⠀⠛⠛⠛⠛⠀⠀⠀⠛⠛⠀⠀⠛

      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣤⠀⣤⣿⠀⣿⠛⠛⣿⣿⠀⣤⠀⠛⠛⣿⣿⠛⠛⠀⠀⠛⠛⣿⣤⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⣿⣤⠀⠛⠛⠛⣿⠀⠛⠛⠛⠛⠀⠛⠛
      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣿⠀⠀⠛⠀⣿⠛⠛⣿⣿⠀⣿⠀⠀⠀⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⠛⠀⠀⠀⠛⠛⣿⠀⠀⠛⠛⠛⠀⣤⣿
      ⠀⠀⠛⠛⠀⠛⠛⠀⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠀⠀⠛⠛⠀⠛⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠛⠛⠀⠀⠛⠛⠀⠀⠛⠛⠛⠛⠛⠀⠀⠛⠛⠀⠛⠛⠛⠛⠀⠀⠀⠛⠛⠀⠀⠛ |}];
  return ()
;;

let%expect_test "Maze" =
  let%bind rom_file = Reader.file_contents "maze.ch8" in
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 10)
    ~rom_file
    ~create;
  [%expect
    {|
    ⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤
    ⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀
    ⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤
    ⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀
    ⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤
    ⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀
    ⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤
    ⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀
    ⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤
    ⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀
    ⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤
    ⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀
    ⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤
    ⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀
    ⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠛⣤
    ⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⠀⠀⠀⣤⠀⠀ |}];
  return ()
;;

let%expect_test "Blinky" =
  let%bind rom_file = Reader.file_contents "blinky.ch8" in
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 5)
    ~rom_file
    ~create;
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 50)
    ~rom_file
    ~create;
  [%expect
    {|
      ⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⠀⣿⠀⠛⣤⠀
      ⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠛⠛⣤⣿⣿
      ⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⣤⠀⠀⠀⣤⣤

      ⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⠀⣿⠀⠛⣤⠀
      ⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠛⠛⣤⣿⣿
      ⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⣤⠀⠀⠀⣤⣤ |}];
  return ()
;;

let%expect_test "Trip 8" =
  let%bind rom_file = Reader.file_contents "trip8.ch8" in
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 2)
    ~rom_file
    ~create;
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 10)
    ~rom_file
    ~create;
  [%expect
    {|
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⣿
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⣤⠀⠀⠀⣿

      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣤⠛⠀⠀⠀⣿
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⣤⠀⠀⠀⣿ |}];
  return ()
;;

let%expect_test "Pong" =
  let%bind rom_file = Reader.file_contents "pong.ch8" in
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 3)
    ~rom_file
    ~create;
  test
    ~print_on_exit:false
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 10)
    ~rom_file
    ~create;
  [%expect
    {|
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠛⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣿⠛

      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠛
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣤⠀⠀⠀⠀
      ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⣤⠀⠀⠀⠛ |}];
  return ()
;;
