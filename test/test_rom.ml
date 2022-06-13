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
  printf "Framebuffer\n%s\n" (frame_buffer_as_string sim inputs outputs);
  ()
;;

let%expect_test "Particle" =
  let%bind rom_file = Reader.file_contents "particle.ch8" in
  test
    ~print_on_exit:true
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 10)
    ~rom_file
    ~create;
  test
    ~print_on_exit:true
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 20)
    ~rom_file
    ~create;
  [%expect
    {|
      ((core
        ((in_execute 0) (in_fetch 1) (op 1111101000011110)
         (working_op 1111101000101011)
         (registers
          ((pc 001010100000) (i 001100011110) (sp 001000111110)
           (registers
            (01000000 00111100 11111001 11111010 00011111 00000000 00000101
             00000010 00000000 00000000 00011110 00000000 00000000 00000000
             00000000 00000000))))
         (fetch_finished 0) (fetch_cycle 00) (last_op 1111101000011110)
         (executor_done 0) (executor_error 0)
         (memory
          ((read_address 0000001010100000) (write_enable 0)
           (write_address 0000000000000000) (write_data 00000000)))
         (random_state 01010111)))
       (read_data 00101011))
      Framebuffer
      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣤⠀⣤⣿⠀⣿⠛⠛⣿⣿⠀⣤⠀⠛⠛⣿⣿⠛⠛⠀⠀⠛⠛⣿⣤⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⣿⣤⠀⠛⠛⠛⣿⠀⠛⠛⠛⠛⠀⠛⠛
      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣿⠀⠀⠛⠀⣿⠛⠛⣿⣿⠀⣿⠀⠀⠀⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⠛⠀⠀⠀⠛⠛⣿⠀⠀⠛⠛⠛⠀⣤⣿
      ⠀⠀⠛⠛⠀⠛⠛⠀⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠀⠀⠛⠛⠀⠛⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠛⠛⠀⠀⠛⠛⠀⠀⠛⠛⠛⠛⠛⠀⠀⠛⠛⠀⠛⠛⠛⠛⠀⠀⠀⠛⠛⠀⠀⠛

      ((core
        ((in_execute 1) (in_fetch 1) (op 0000000000000000)
         (working_op 0000000000000000)
         (registers
          ((pc 111010110010) (i 001100001101) (sp 001010101100)
           (registers
            (01000000 00111100 00000110 11111000 00011111 00000000 00000101
             00000010 00000000 00000000 00001110 00000000 00000000 00000000
             00000000 00000000))))
         (fetch_finished 1) (fetch_cycle 10) (last_op 0000000000000000)
         (executor_done 1) (executor_error 0)
         (memory
          ((read_address 0000000000000000) (write_enable 0)
           (write_address 0000000000000000) (write_data 00000000)))
         (random_state 01101110)))
       (read_data 00000000))
      Framebuffer
      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣤⠀⣤⣿⠀⣿⠛⠛⣿⣿⠀⣤⠀⠛⠛⣿⣿⠛⠛⠀⠀⠛⠛⣿⣤⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⣿⣤⠀⠛⠛⠛⣿⠀⠛⠛⠛⠛⠀⠛⠛
      ⠀⠛⣿⣿⠀⠛⠛⠛⠀⠛⠛⣿⣿⠀⠀⠛⠀⣿⠛⠛⣿⣿⠀⣿⠀⠀⠀⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⠀⠀⠛⠀⠀⠀⠛⠛⣿⠀⠀⠛⠛⠛⠀⣤⣿
      ⠀⠀⠛⠛⠀⠛⠛⠀⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠀⠀⠛⠛⠀⠛⠀⠀⠀⠛⠛⠀⠀⠀⠀⠛⠛⠛⠀⠀⠛⠛⠀⠀⠛⠛⠛⠛⠛⠀⠀⠛⠛⠀⠛⠛⠛⠛⠀⠀⠀⠛⠛⠀⠀⠛ |}];
  return ()
;;

let%expect_test "Maze" =
  let%bind rom_file = Reader.file_contents "maze.ch8" in
  test
    ~print_on_exit:true
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 10)
    ~rom_file
    ~create;
  [%expect
    {|
    ((core
      ((in_execute 0) (in_fetch 1) (op 0001001000011100)
       (working_op 0001001000010010)
       (registers
        ((pc 001000011100) (i 001000100010) (sp 000000000000)
         (registers
          (00000000 00100000 00000001 00000000 00000000 00000000 00000000
           00000000 00000000 00000000 00000000 00000000 00000000 00000000
           00000000 00000000))))
       (fetch_finished 0) (fetch_cycle 01) (last_op 0001001000011100)
       (executor_done 0) (executor_error 0)
       (memory
        ((read_address 0000001000011101) (write_enable 0)
         (write_address 0000000000000000) (write_data 00000000)))
       (random_state 01010111)))
     (read_data 00010010))
    Framebuffer
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
    ~print_on_exit:true
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 5)
    ~rom_file
    ~create;
  test
    ~print_on_exit:true
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 50)
    ~rom_file
    ~create;
  [%expect
    {|
      ((core
        ((in_execute 0) (in_fetch 1) (op 1000000100110100)
         (working_op 1000000100110001)
         (registers
          ((pc 100010110110) (i 100011001010) (sp 000000000100)
           (registers
            (11111110 00000001 00001111 11111111 00000000 00001010 00011000
             00000000 00000000 00000000 00000000 00000000 00000000 00000000
             00000000 00000001))))
         (fetch_finished 0) (fetch_cycle 01) (last_op 1000000100110100)
         (executor_done 0) (executor_error 0)
         (memory
          ((read_address 0000100010110111) (write_enable 0)
           (write_address 0000000000000000) (write_data 00000000)))
         (random_state 10001100)))
       (read_data 00110001))
      Framebuffer
      ⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⠀⣿⠀⠛⣤⠀
      ⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠛⠛⣤⣿⣿
      ⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⣤⠀⠀⠀⣤⣤

      ((core
        ((in_execute 1) (in_fetch 0) (op 1110010100000101)
         (working_op 1110010100100010)
         (registers
          ((pc 100101000010) (i 100011001010) (sp 000000000100)
           (registers
            (00000000 00010000 00001111 11111111 00000000 00000101 00011000
             00000000 00000000 00000000 00000000 00000000 00000000 00000000
             00000001 00000001))))
         (fetch_finished 0) (fetch_cycle 00) (last_op 1110010100000101)
         (executor_done 0) (executor_error 1)
         (memory
          ((read_address 0000000000000000) (write_enable 0)
           (write_address 0000000000000000) (write_data 00000000)))
         (random_state 11101111)))
       (read_data 00100010))
      Framebuffer
      ⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⣿⣤⣤⠀⠛⠀⠀⠀⠀⣿⠀⠛⣤⠀
      ⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠀⠛⠛⠛⠛⠀⠀⠛⠛⠛⣤⣿⣿
      ⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⠀⠀⠀⠛⣿⠛⠀⠀⣤⠀⠀⠀⣤⣤ |}];
  return ()
;;

let%expect_test "Trip 8" =
  let%bind rom_file = Reader.file_contents "trip8.ch8" in
  test
    ~print_on_exit:true
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 5)
    ~rom_file
    ~create;
  [%expect
    {|
      ((core
        ((in_execute 1) (in_fetch 0) (op 1111110100010101)
         (working_op 1111110111110000)
         (registers
          ((pc 001000011000) (i 000000000000) (sp 000000000000)
           (registers
            (00000000 00000000 00000000 00000000 00000000 00000000 00000000
             00000000 00000000 00000000 00000000 00000000 00000000 00100000
             00000000 00000000))))
         (fetch_finished 0) (fetch_cycle 00) (last_op 1111110100010101)
         (executor_done 0) (executor_error 1)
         (memory
          ((read_address 0000000000000000) (write_enable 0)
           (write_address 0000000000000000) (write_data 00000000)))
         (random_state 10001100)))
       (read_data 11110000))
      Framebuffer
      ⠀ |}];
  return ()
;;

let%expect_test "Pong" =
  let%bind rom_file = Reader.file_contents "pong.ch8" in
  test
    ~print_on_exit:true
    ~print_on_cycle:false
    ~cycles:(rough_cycles_per_second * 5)
    ~rom_file
    ~create;
  [%expect
    {|
      ((core
        ((in_execute 0) (in_fetch 1) (op 1111111000110011)
         (working_op 1111111011111110)
         (registers
          ((pc 001011010110) (i 001011110010) (sp 000000000010)
           (registers
            (00000000 00000000 00000000 00000000 00000000 00000000 00000000
             00000000 00000000 00000000 00000010 00001100 00111111 00001100
             00000000 00000000))))
         (fetch_finished 0) (fetch_cycle 01) (last_op 1111111000110011)
         (executor_done 0) (executor_error 0)
         (memory
          ((read_address 0000001011010111) (write_enable 0)
           (write_address 0000000000000000) (write_data 00000000)))
         (random_state 10001100)))
       (read_data 11111110))
      Framebuffer
      ⠀ |}];
  return ()
;;
