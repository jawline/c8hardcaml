open Core
open Async
open Hardcaml
open C8.Global
open C8.Cpu_core

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

let sim_program_rom sim (i : _ I.t) ~rom =
  List.iteri rom ~f:(fun addr value -> sim_set_write_ram sim i addr value);
  sim_disable_programming i
;;

let _sim_read_addr sim (i : _ I.t) (o : _ O.t) addr =
  i.program := Bits.of_int ~width:(Sized.size `Bit) 1;
  i.program_write_enable := Bits.of_int ~width:(Sized.size `Bit) 0;
  i.program_address := Bits.of_int ~width:(Sized.size `Main_address) addr;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  print_s [%message "" ~last_read:(pp o.read_data)]
;;

let sim_cycle_not_programming sim (i : _ I.t) (o : _ O.t) =
  sim_disable_programming i;
  Cyclesim.cycle sim;
  print_s
    [%message
      ""
        ~op:(ppb o.op)
        ~working_op:(ppb o.working_op)
        ~read_address:(pp o.read_address)
        ~read_data:(pp o.write_data)
        ~write_enable:(pp o.write_enable)
        ~write_address:(pp o.write_address)
        ~write_data:(pp o.write_data)
        ~in_execute:(pp o.in_execute)
        ~executor_pc:(pp o.registers.pc)
        ~executor_i:(pp o.registers.i)
        ~executor_done:(pp o.registers.done_)
        ~executor_error:(pp o.registers.error)
        ~executor_registers:(List.map o.registers.registers ~f:pp : string list)]
;;
