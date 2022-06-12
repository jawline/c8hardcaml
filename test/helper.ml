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
  i.program := Bits.of_int ~width:(Sized.size `Bit) 1;
  i.program_write_enable := Bits.of_int ~width:(Sized.size `Bit) 1;
  i.program_address := Bits.of_int ~width:(Sized.size `Main_address) addr;
  i.program_data := Bits.of_int ~width:(Sized.size `Byte) data;
  i.program_pc := Bits.of_int ~width:(Sized.size `Address) 0;
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
  i.program := Bits.of_int ~width:(Sized.size `Bit) 1;
  i.program_write_enable := Bits.of_int ~width:(Sized.size `Bit) 0;
  i.program_address := Bits.of_int ~width:(Sized.size `Main_address) addr;
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
    C8.Main_memory.frame_buffer_size
;;

let frame_buffer_as_string sim (i : _ I.t) (o : _ O.t) =
        let scale = 2 in
  let canvas = Drawille.create (screen_width * scale) ( screen_height * scale) in

  (* Set with pixel scaling *)
  let set x y =
          let scaleseq = Sequence.range 0 scale in
    Sequence.iter scaleseq ~f:(fun xoff ->
            Sequence.iter scaleseq ~f:(fun yoff ->
                    Drawille.set canvas { Drawille.x = (x * scale) + xoff ; y = (y * scale) + yoff }))
  in
  
  let frame_buffer = sim_read_framebuffer sim i o in
  let pixel x y =
    let y_offset = y * (screen_width / 8) in
    let x_offset = x / 8 in
    let bit = Int.shift_left 1 ((x % 8) - 1) in
    Array.get frame_buffer (y_offset + x_offset) land bit
  in
  Sequence.range 0 screen_height |> 
  Sequence.iter ~f:(fun y ->
         Sequence.range 0 screen_width
         |> Sequence.iter ~f:(fun x -> if pixel x y <> 0 then set x y else ()));
  Drawille.frame canvas
;;

let sim_cycle_not_programming sim (i : _ I.t) (o : _ O.t) ~print =
  sim_disable_programming i;
  Cyclesim.cycle sim;
  if print then print_s [%sexp (o : Bits.t ref C8.Programmable_cpu_core.O.t)] else ()
;;
