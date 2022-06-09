open Core
open Async
open Hardcaml
open C8.Programmable_cpu_core
open Helper

let test ~rom_file ~create =
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let sim = Simulator.create create in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  (* Write the test rom to main memory *)
  sim_program_rom sim inputs ~rom:(String.to_list rom_file |> List.map ~f:Char.to_int);
  (* Simulate the program we just wrote running for 100 cycles *)
  Sequence.range 0 90000
  |> Sequence.iter ~f:(fun _ -> sim_cycle_not_programming sim inputs outputs ~print:false);
  sim_cycle_not_programming sim inputs outputs ~print:true;
  printf "Framebuffer\n%s\n" (frame_buffer_as_string sim inputs outputs);
  ()
;;

let%expect_test "Maze" =
  let%bind rom_file =
    Reader.file_contents "../test_rom/Maze (alt) [David Winter, 199x].ch8"
  in
  test ~rom_file ~create;
  [%expect
    {|
    "WARN: REMOVE ME WHEN SP IS USED"
    ((core
      ((in_execute 0) (in_fetch 1) (op 0001001000011100)
       (working_op 0001001000000000)
       (registers
        ((pc 001000011100) (i 001000100010) (sp 00000000000000000000000000000000)
         (registers
          (00000000 00100000 00000001 00000000 00000000 00000000 00000000
           00000000 00000000 00000000 00000000 00000000 00000000 00000000
           00000000 00000000))))
       (fetch_finished 0) (fetch_cycle 00) (last_op 0001001000011100)
       (executor_done 0) (executor_error 0)
       (memory
        ((read_address 0000001000011100) (write_enable 0)
         (write_address 0000000000000000) (write_data 00000000)))
       (random_state 10111000)))
     (read_data 00000000))
    Framebuffer
                          *
           *       *       *       *
          *       *               *


           *       *       *       *
          *       *       *       *


           *       *       *       *
          *       *       *       *

          *       *               *
           *       *       *       *
                          *

                  *
           *       *       *       *
          *               *       *

                  *       *
           *       *       *       *
          *                       *

          *
           *       *       *       *
                  *       *       *

          *       *       *
           *       *       *       *
                                  * |}];
  return ()
;;

let%expect_test "Blinky" =
  let%bind rom_file =
    Reader.file_contents "/home/blake/chip8Hardcaml/test_rom/blinky.ch8"
  in
  test ~rom_file ~create;
  [%expect
    {|
      "WARN: REMOVE ME WHEN SP IS USED"
      ((core
        ((in_execute 1) (in_fetch 0) (op 1111000101010101)
         (working_op 1111000100000000)
         (registers
          ((pc 001000100000) (i 100011001000) (sp 00000000000000000000000000000000)
           (registers
            (00000000 00000000 00000000 00000000 00000000 00000000 00000000
             00000000 00000000 00000000 00000000 00000000 00000000 00000000
             00000000 00000000))))
         (fetch_finished 0) (fetch_cycle 00) (last_op 1111000101010101)
         (executor_done 0) (executor_error 1)
         (memory
          ((read_address 0000000000000000) (write_enable 0)
           (write_address 0000000000000000) (write_data 00000000)))
         (random_state 10111000)))
       (read_data 00000000))
      Framebuffer |}];
  return ()
;;
