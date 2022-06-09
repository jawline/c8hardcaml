open! Core
open! Hardcaml
open! Signal
open Global

(* Foundation of a CHIP-8 core. Manages the fetch execute cycle
   and maintains a random state for opcodes to use. *)

let prng_seed = 3929319

module States = struct
  type t =
    | Startup
    | Fetch_op
    | Execute
  [@@deriving sexp_of, compare, enumerate]
end

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { in_execute : 'a [@bits 1]
    ; in_fetch : 'a [@bits 1]
    ; op : 'a [@bits 16]
    ; working_op : 'a [@bits 16]
    ; registers : 'a Registers.In_circuit.t
    ; fetch_finished : 'a [@bits 1]
    ; fetch_cycle : 'a [@bits 2]
    ; last_op : 'a [@bits 16]
    ; executor_done : 'a [@bits 1]
    ; executor_error : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.O.t
    ; random_state : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

let startup ~(registers : Registers.t) ~random_state_seed ~(state : States.t Always.State_machine.t) =
  let open Always in
  [ (* Seed the PRNG on the first cycle. Since this is fixed machines will always behave identically. *)
    random_state_seed <--. prng_seed
  ; state.set_next Fetch_op
  ; registers.pc <--. 0x200
  ]
;;

let create { I.clear; clock; enable; memory } : _ O.t =
  let open Always in
  let open Variable in
  let ram = Main_memory.Wires.t_of_in_circuit memory in
  let random_state_seed = wire ~default:(Signal.of_int ~width:64 0) in
  let random_state =
    Xor_shift.create { Xor_shift.I.clock; clear; seed = random_state_seed.value }
  in
  let executing_opcode =
    Immediate_register.create ~spec:r_sync ~width:(Sized.size `Opcode)
  in
  let error = reg ~enable:vdd ~width:1 r_sync in
  let done_ = wire_false () in
  let in_execute = wire ~default:(Signal.of_int ~width:1 0) in
  let in_fetch = wire ~default:(Signal.of_int ~width:1 0) in
  let internal =
    Execute_core.create ~executing_opcode:executing_opcode.value ~spec:r_sync ()
  in
  let state = State_machine.create (module States) ~enable:vdd r_sync in
  let last_op = reg ~enable:vdd ~width:16 r_sync in
  let fetch, fetch_wiring =
    Main_memory.circuit_with_just_read_memory ram ~f:(fun ~memory ->
        let o =
          Fetch.create
            ~spec:(r_enabled ~enable:in_fetch.value)
            { Fetch.I.clock
            ; clear
            ; in_fetch = in_fetch.value
            ; program_counter = internal.registers.pc.value
            ; memory
            }
        in
        o, o.memory)
  in
  let main_execution =
    [ state.switch
        [ Startup, startup ~random_state_seed ~state ~registers:internal.registers
        ; ( Fetch_op
          , [ fetch_wiring
            ; in_fetch <--. 1
            ; when_
                (fetch.finished ==:. 1)
                [ Immediate_register.(executing_opcode <-- fetch.opcode)
                ; in_execute <--. 1
                ; state.set_next Execute
                ]
            ] )
        ; Execute, [ in_execute <--. 1 ]
        ]
    ; when_
        (in_execute.value ==:. 1)
        [ last_op <-- executing_opcode.value
        ; Execute_core.execute_instruction
            ~clock
            ~clear
            ~error
            ~done_
            ~ram
            ~random_state
            internal
        ; when_ (done_.value ==:. 1) [ state.set_next Fetch_op ]
        ]
    ]
  in
  compile [ when_ (is_set enable) main_execution ];
  { O.in_execute = in_execute.value
  ; in_fetch = in_fetch.value
  ; op = executing_opcode.value
  ; working_op = fetch.opcode
  ; last_op = last_op.value
  ; executor_done = done_.value
  ; executor_error = error.value
  ; registers = Registers.to_circuit internal.registers
  ; fetch_cycle = fetch.fetch_cycle
  ; fetch_finished = fetch.finished
  ; memory = Main_memory.In_circuit.O.t_of_main_memory ram
  ; random_state = select  random_state.pseudo_random 7 0
  }
;;
