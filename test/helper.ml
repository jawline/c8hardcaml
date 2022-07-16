open Core
open Async
open Hardcaml
open C8.Global
open C8.Programmable_cpu_core

let program_start_offset = 0x200

let make_rom_of_opcodes ~opcodes =
  let opcode_to_bytes opcode =
    [ (opcode land 0b1111_1111_0000_0000) lsr 8; opcode land 0b1111_1111 ]
  in
  List.map ~f:opcode_to_bytes opcodes |> List.concat
;;

let pp v = Bits.to_int !v |> Int.to_string
let ppb v = Bits.to_string !v

let sim_set_write_ram sim (i : _ I.t) addr data =
  i.program := Bits.of_int ~width:(wsz `Bit) 1;
  i.program_write_enable := Bits.of_int ~width:(wsz `Bit) 1;
  i.program_address := Bits.of_int ~width:(wsz `Main_address) addr;
  i.program_data := Bits.of_int ~width:(wsz `Byte) data;
  i.program_pc := Bits.of_int ~width:(wsz `Address) 0;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim
;;

let sim_disable_programming (i : _ I.t) = i.program := Bits.of_int ~width:1 0

let sim_program_machine_rom sim (i : _ I.t) =
  (* Program font data into RAM before programming the ROM we read from disk *)
  Array.iteri C8.Main_memory.machine_rom ~f:(fun addr word ->
    sim_set_write_ram sim i (C8.Main_memory.machine_rom_offset + addr) word)
;;

let sim_program_rom sim (i : _ I.t) ~rom =
  List.iteri rom ~f:(fun addr value ->
    sim_set_write_ram sim i (addr + program_start_offset) value);
  sim_disable_programming i
;;

let sim_read_addr sim (i : _ I.t) (o : _ O.t) addr =
  i.program := Bits.of_int ~width:(wsz `Bit) 1;
  i.program_write_enable := Bits.of_int ~width:(wsz `Bit) 0;
  i.program_address := Bits.of_int ~width:(wsz `Main_address) addr;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Bits.to_int !(o.read_data)
;;

let sim_read_memory_range sim (i : _ I.t) (o : _ O.t) start sz =
  Sequence.range 0 sz
  |> Sequence.map ~f:(fun idx -> sim_read_addr sim i o (start + idx))
  |> Sequence.to_array
;;

let sim_read_framebuffer sim (i : _ I.t) (o : _ O.t) =
  sim_read_memory_range
    sim
    i
    o
    C8.Main_memory.framebuffer_start
    C8.Main_memory.framebuffer_size
;;

let draw_border_around_canvas ~canvas ~width ~height =
  Sequence.range 0 width
  |> Sequence.iter ~f:(fun x ->
       Drawille.set canvas { Drawille.x; y = 0 };
       Drawille.set canvas { Drawille.x; y = height - 1 });
  Sequence.range 0 height
  |> Sequence.iter ~f:(fun y ->
       Drawille.set canvas { Drawille.x = 0; y };
       Drawille.set canvas { Drawille.x = width - 1; y })
;;

let draw_framebuffer ~set sim (i : _ I.t) (o : _ O.t) =
  let frame_buffer = sim_read_framebuffer sim i o in
  let pixel x y =
    let y_offset = y * (screen_width / 8) in
    let x_offset = x / 8 in
    let bit = Int.shift_right 128 (x % 8) in
    Array.get frame_buffer (y_offset + x_offset) land bit
  in
  Sequence.range 0 screen_height
  |> Sequence.iter ~f:(fun y ->
       Sequence.range 0 screen_width
       |> Sequence.iter ~f:(fun x -> if pixel x y <> 0 then set x y else ()))
;;

let frame_buffer_as_string sim (i : _ I.t) (o : _ O.t) =
  let scale = 2 in
  (* Add two extra rows and columns to add a border *)
  let canvas_width = (screen_width * scale) + 2 in
  let canvas_height = (screen_height * scale) + 2 in
  let canvas = Drawille.create canvas_width canvas_height in
  (* Set with pixel scaling *)
  let set x y =
    let scaleseq = Sequence.range 0 scale in
    Sequence.iter scaleseq ~f:(fun xoff ->
      Sequence.iter scaleseq ~f:(fun yoff ->
        Drawille.set
          canvas
          { Drawille.x = (x * scale) + xoff + 1; y = (y * scale) + yoff + 1 }))
  in
  draw_border_around_canvas ~canvas ~width:canvas_width ~height:canvas_height;
  draw_framebuffer ~set sim i o;
  Drawille.frame canvas
;;

let sim_cycle_not_programming sim (i : _ I.t) (o : _ O.t) ~print =
  sim_disable_programming i;
  Cyclesim.cycle sim;
  if print then print_s [%sexp (o : Bits.t ref C8.Programmable_cpu_core.O.t)] else ()
;;

(* Average opcode takes 3 cycles to execute (2 to fetch, one to execute) *)
let rough_cycles_per_second = 512 * 3
let set_keys_unpressed = List.iter ~f:(fun key -> key := Bits.of_int ~width:1 0)

let assign_keys_randomly ~rand =
  List.iter ~f:(fun key -> key := Bits.of_int ~width:1 (Random.State.int rand 1))
;;

let test_bytes
  ?(print_on_cycle = false)
  ~run_for_cycles
  ~print_at_interval
  ~create
  ~rom
  ()
  =
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let sim = Simulator.create (create ~spec:r_sync) in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  let rand = Random.State.default in
  (* Write font data to main memory *)
  sim_program_machine_rom sim inputs;
  (* Write the test rom to main memory *)
  sim_program_rom sim inputs ~rom;
  set_keys_unpressed inputs.keys.state;
  Sequence.range 0 run_for_cycles
  |> Sequence.iter ~f:(fun cycle ->
       if (cycle + 1) % print_at_interval = 0
       then printf "%s\n" (frame_buffer_as_string sim inputs outputs);
       if cycle % (rough_cycles_per_second / 5) = 0
       then assign_keys_randomly ~rand inputs.keys.state;
       sim_cycle_not_programming sim inputs outputs ~print:print_on_cycle;
       if Bits.to_int !(outputs.core.executor_error) = 1
       then (
         (* Print registers and framebuffer on error *) printf "ERROR: ";
         sim_cycle_not_programming sim inputs outputs ~print:true;
         printf "%s" (frame_buffer_as_string sim inputs outputs);
         raise_s [%message "Error in ROM"]))
;;

let test_rom
  ?(print_on_cycle = false)
  ~run_for_cycles
  ~print_at_interval
  ~rom_file
  ~create
  ()
  =
  let rom = String.to_list rom_file |> List.map ~f:Char.to_int in
  test_bytes ~print_on_cycle ~run_for_cycles ~print_at_interval ~rom ~create ()
;;
