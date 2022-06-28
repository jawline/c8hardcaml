open! Core
open! Hardcaml
open! Signal
open Global

module I = struct
  type 'a t =
    { clock : 'a [@bits 1]
    ; clear : 'a [@bits 1]
    ; enable : 'a [@bits 1]
    ; address : 'a [@bits 16]
    ; size : 'a [@bits 8]
    ; write_value : 'a [@bits 8]
    ; memory : 'a Main_memory.In_circuit.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { finished : 'a [@bits 1]
    ; in_progress : 'a [@bits 1]
    ; memory : 'a Main_memory.In_circuit.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

let create ~spec (i : _ I.t) =
  let open Always in
  let open Variable in
  let ram = Main_memory.Wires.create () in
  let set_in_progress = Variable.reg ~enable:vdd ~width:1 spec in
  let write_index = Variable.reg ~enable:vdd ~width:8 spec in
  let finished = wire_false () in
  compile
    [ if_
        (is_set i.enable)
        [ ram.read_address <--. 0
        ; ram.write_enable <--. 1
        ; ram.write_data <-- i.write_value
        ; if_
            (is_set set_in_progress.value)
            [ ram.write_address <-- i.address +: to_main_addr write_index.value
            ; write_index <-- write_index.value +:. 1
            ; when_
                (write_index.value ==: i.size -:. 1)
                [ finished <--. 1; set_in_progress <--. 0; write_index <--. 0 ]
            ]
            [ set_in_progress <--. 1
            ; ram.write_address <-- i.address
            ; write_index <--. 1
            ]
        ]
        [ set_in_progress <--. 0; write_index <--. 0 ]
    ];
  { O.finished = finished.value
  ; in_progress = set_in_progress.value
  ; memory = Main_memory.Wires.to_output ram
  }
;;

module Test = struct
  let test ~cycles =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let sim = Simulator.create (create ~spec:r_sync) in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let output : _ O.t = Cyclesim.outputs sim in
    inputs.enable := Bits.of_int ~width:1 0;
    inputs.address := Bits.of_int ~width:16 4096;
    inputs.size := Bits.of_int ~width:8 128;
    inputs.write_value := Bits.of_int ~width:8 0xFF;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    inputs.enable := Bits.of_int ~width:1 1;
    let print_outputs () =
      let pp v = Bits.to_int !v in
      printf
        "In progress: %i Finished: %i Write addr: %i %i %i\n"
        (pp output.in_progress)
        (pp output.finished)
        (pp output.memory.write_address)
        (pp output.memory.write_data)
        (pp output.memory.write_enable)
    in
    print_outputs ();
    printf "Starting main loop\n";
    Sequence.(
      range 0 cycles
      |> iter ~f:(fun _ ->
             Cyclesim.cycle sim;
             print_outputs ()));
    ()
  ;;

  let%expect_test "seed and cycle" =
    test ~cycles:256;
    [%expect
      {|
      In progress: 0 Finished: 0 Write addr: 0 0 0
      Starting main loop
      In progress: 1 Finished: 0 Write addr: 4097 255 1
      In progress: 1 Finished: 0 Write addr: 4098 255 1
      In progress: 1 Finished: 0 Write addr: 4099 255 1
      In progress: 1 Finished: 0 Write addr: 4100 255 1
      In progress: 1 Finished: 0 Write addr: 4101 255 1
      In progress: 1 Finished: 0 Write addr: 4102 255 1
      In progress: 1 Finished: 0 Write addr: 4103 255 1
      In progress: 1 Finished: 0 Write addr: 4104 255 1
      In progress: 1 Finished: 0 Write addr: 4105 255 1
      In progress: 1 Finished: 0 Write addr: 4106 255 1
      In progress: 1 Finished: 0 Write addr: 4107 255 1
      In progress: 1 Finished: 0 Write addr: 4108 255 1
      In progress: 1 Finished: 0 Write addr: 4109 255 1
      In progress: 1 Finished: 0 Write addr: 4110 255 1
      In progress: 1 Finished: 0 Write addr: 4111 255 1
      In progress: 1 Finished: 0 Write addr: 4112 255 1
      In progress: 1 Finished: 0 Write addr: 4113 255 1
      In progress: 1 Finished: 0 Write addr: 4114 255 1
      In progress: 1 Finished: 0 Write addr: 4115 255 1
      In progress: 1 Finished: 0 Write addr: 4116 255 1
      In progress: 1 Finished: 0 Write addr: 4117 255 1
      In progress: 1 Finished: 0 Write addr: 4118 255 1
      In progress: 1 Finished: 0 Write addr: 4119 255 1
      In progress: 1 Finished: 0 Write addr: 4120 255 1
      In progress: 1 Finished: 0 Write addr: 4121 255 1
      In progress: 1 Finished: 0 Write addr: 4122 255 1
      In progress: 1 Finished: 0 Write addr: 4123 255 1
      In progress: 1 Finished: 0 Write addr: 4124 255 1
      In progress: 1 Finished: 0 Write addr: 4125 255 1
      In progress: 1 Finished: 0 Write addr: 4126 255 1
      In progress: 1 Finished: 0 Write addr: 4127 255 1
      In progress: 1 Finished: 0 Write addr: 4128 255 1
      In progress: 1 Finished: 0 Write addr: 4129 255 1
      In progress: 1 Finished: 0 Write addr: 4130 255 1
      In progress: 1 Finished: 0 Write addr: 4131 255 1
      In progress: 1 Finished: 0 Write addr: 4132 255 1
      In progress: 1 Finished: 0 Write addr: 4133 255 1
      In progress: 1 Finished: 0 Write addr: 4134 255 1
      In progress: 1 Finished: 0 Write addr: 4135 255 1
      In progress: 1 Finished: 0 Write addr: 4136 255 1
      In progress: 1 Finished: 0 Write addr: 4137 255 1
      In progress: 1 Finished: 0 Write addr: 4138 255 1
      In progress: 1 Finished: 0 Write addr: 4139 255 1
      In progress: 1 Finished: 0 Write addr: 4140 255 1
      In progress: 1 Finished: 0 Write addr: 4141 255 1
      In progress: 1 Finished: 0 Write addr: 4142 255 1
      In progress: 1 Finished: 0 Write addr: 4143 255 1
      In progress: 1 Finished: 0 Write addr: 4144 255 1
      In progress: 1 Finished: 0 Write addr: 4145 255 1
      In progress: 1 Finished: 0 Write addr: 4146 255 1
      In progress: 1 Finished: 0 Write addr: 4147 255 1
      In progress: 1 Finished: 0 Write addr: 4148 255 1
      In progress: 1 Finished: 0 Write addr: 4149 255 1
      In progress: 1 Finished: 0 Write addr: 4150 255 1
      In progress: 1 Finished: 0 Write addr: 4151 255 1
      In progress: 1 Finished: 0 Write addr: 4152 255 1
      In progress: 1 Finished: 0 Write addr: 4153 255 1
      In progress: 1 Finished: 0 Write addr: 4154 255 1
      In progress: 1 Finished: 0 Write addr: 4155 255 1
      In progress: 1 Finished: 0 Write addr: 4156 255 1
      In progress: 1 Finished: 0 Write addr: 4157 255 1
      In progress: 1 Finished: 0 Write addr: 4158 255 1
      In progress: 1 Finished: 0 Write addr: 4159 255 1
      In progress: 1 Finished: 0 Write addr: 4160 255 1
      In progress: 1 Finished: 0 Write addr: 4161 255 1
      In progress: 1 Finished: 0 Write addr: 4162 255 1
      In progress: 1 Finished: 0 Write addr: 4163 255 1
      In progress: 1 Finished: 0 Write addr: 4164 255 1
      In progress: 1 Finished: 0 Write addr: 4165 255 1
      In progress: 1 Finished: 0 Write addr: 4166 255 1
      In progress: 1 Finished: 0 Write addr: 4167 255 1
      In progress: 1 Finished: 0 Write addr: 4168 255 1
      In progress: 1 Finished: 0 Write addr: 4169 255 1
      In progress: 1 Finished: 0 Write addr: 4170 255 1
      In progress: 1 Finished: 0 Write addr: 4171 255 1
      In progress: 1 Finished: 0 Write addr: 4172 255 1
      In progress: 1 Finished: 0 Write addr: 4173 255 1
      In progress: 1 Finished: 0 Write addr: 4174 255 1
      In progress: 1 Finished: 0 Write addr: 4175 255 1
      In progress: 1 Finished: 0 Write addr: 4176 255 1
      In progress: 1 Finished: 0 Write addr: 4177 255 1
      In progress: 1 Finished: 0 Write addr: 4178 255 1
      In progress: 1 Finished: 0 Write addr: 4179 255 1
      In progress: 1 Finished: 0 Write addr: 4180 255 1
      In progress: 1 Finished: 0 Write addr: 4181 255 1
      In progress: 1 Finished: 0 Write addr: 4182 255 1
      In progress: 1 Finished: 0 Write addr: 4183 255 1
      In progress: 1 Finished: 0 Write addr: 4184 255 1
      In progress: 1 Finished: 0 Write addr: 4185 255 1
      In progress: 1 Finished: 0 Write addr: 4186 255 1
      In progress: 1 Finished: 0 Write addr: 4187 255 1
      In progress: 1 Finished: 0 Write addr: 4188 255 1
      In progress: 1 Finished: 0 Write addr: 4189 255 1
      In progress: 1 Finished: 0 Write addr: 4190 255 1
      In progress: 1 Finished: 0 Write addr: 4191 255 1
      In progress: 1 Finished: 0 Write addr: 4192 255 1
      In progress: 1 Finished: 0 Write addr: 4193 255 1
      In progress: 1 Finished: 0 Write addr: 4194 255 1
      In progress: 1 Finished: 0 Write addr: 4195 255 1
      In progress: 1 Finished: 0 Write addr: 4196 255 1
      In progress: 1 Finished: 0 Write addr: 4197 255 1
      In progress: 1 Finished: 0 Write addr: 4198 255 1
      In progress: 1 Finished: 0 Write addr: 4199 255 1
      In progress: 1 Finished: 0 Write addr: 4200 255 1
      In progress: 1 Finished: 0 Write addr: 4201 255 1
      In progress: 1 Finished: 0 Write addr: 4202 255 1
      In progress: 1 Finished: 0 Write addr: 4203 255 1
      In progress: 1 Finished: 0 Write addr: 4204 255 1
      In progress: 1 Finished: 0 Write addr: 4205 255 1
      In progress: 1 Finished: 0 Write addr: 4206 255 1
      In progress: 1 Finished: 0 Write addr: 4207 255 1
      In progress: 1 Finished: 0 Write addr: 4208 255 1
      In progress: 1 Finished: 0 Write addr: 4209 255 1
      In progress: 1 Finished: 0 Write addr: 4210 255 1
      In progress: 1 Finished: 0 Write addr: 4211 255 1
      In progress: 1 Finished: 0 Write addr: 4212 255 1
      In progress: 1 Finished: 0 Write addr: 4213 255 1
      In progress: 1 Finished: 0 Write addr: 4214 255 1
      In progress: 1 Finished: 0 Write addr: 4215 255 1
      In progress: 1 Finished: 0 Write addr: 4216 255 1
      In progress: 1 Finished: 0 Write addr: 4217 255 1
      In progress: 1 Finished: 0 Write addr: 4218 255 1
      In progress: 1 Finished: 0 Write addr: 4219 255 1
      In progress: 1 Finished: 0 Write addr: 4220 255 1
      In progress: 1 Finished: 0 Write addr: 4221 255 1
      In progress: 1 Finished: 0 Write addr: 4222 255 1
      In progress: 1 Finished: 1 Write addr: 4223 255 1
      In progress: 0 Finished: 0 Write addr: 4096 255 1
      In progress: 1 Finished: 0 Write addr: 4097 255 1
      In progress: 1 Finished: 0 Write addr: 4098 255 1
      In progress: 1 Finished: 0 Write addr: 4099 255 1
      In progress: 1 Finished: 0 Write addr: 4100 255 1
      In progress: 1 Finished: 0 Write addr: 4101 255 1
      In progress: 1 Finished: 0 Write addr: 4102 255 1
      In progress: 1 Finished: 0 Write addr: 4103 255 1
      In progress: 1 Finished: 0 Write addr: 4104 255 1
      In progress: 1 Finished: 0 Write addr: 4105 255 1
      In progress: 1 Finished: 0 Write addr: 4106 255 1
      In progress: 1 Finished: 0 Write addr: 4107 255 1
      In progress: 1 Finished: 0 Write addr: 4108 255 1
      In progress: 1 Finished: 0 Write addr: 4109 255 1
      In progress: 1 Finished: 0 Write addr: 4110 255 1
      In progress: 1 Finished: 0 Write addr: 4111 255 1
      In progress: 1 Finished: 0 Write addr: 4112 255 1
      In progress: 1 Finished: 0 Write addr: 4113 255 1
      In progress: 1 Finished: 0 Write addr: 4114 255 1
      In progress: 1 Finished: 0 Write addr: 4115 255 1
      In progress: 1 Finished: 0 Write addr: 4116 255 1
      In progress: 1 Finished: 0 Write addr: 4117 255 1
      In progress: 1 Finished: 0 Write addr: 4118 255 1
      In progress: 1 Finished: 0 Write addr: 4119 255 1
      In progress: 1 Finished: 0 Write addr: 4120 255 1
      In progress: 1 Finished: 0 Write addr: 4121 255 1
      In progress: 1 Finished: 0 Write addr: 4122 255 1
      In progress: 1 Finished: 0 Write addr: 4123 255 1
      In progress: 1 Finished: 0 Write addr: 4124 255 1
      In progress: 1 Finished: 0 Write addr: 4125 255 1
      In progress: 1 Finished: 0 Write addr: 4126 255 1
      In progress: 1 Finished: 0 Write addr: 4127 255 1
      In progress: 1 Finished: 0 Write addr: 4128 255 1
      In progress: 1 Finished: 0 Write addr: 4129 255 1
      In progress: 1 Finished: 0 Write addr: 4130 255 1
      In progress: 1 Finished: 0 Write addr: 4131 255 1
      In progress: 1 Finished: 0 Write addr: 4132 255 1
      In progress: 1 Finished: 0 Write addr: 4133 255 1
      In progress: 1 Finished: 0 Write addr: 4134 255 1
      In progress: 1 Finished: 0 Write addr: 4135 255 1
      In progress: 1 Finished: 0 Write addr: 4136 255 1
      In progress: 1 Finished: 0 Write addr: 4137 255 1
      In progress: 1 Finished: 0 Write addr: 4138 255 1
      In progress: 1 Finished: 0 Write addr: 4139 255 1
      In progress: 1 Finished: 0 Write addr: 4140 255 1
      In progress: 1 Finished: 0 Write addr: 4141 255 1
      In progress: 1 Finished: 0 Write addr: 4142 255 1
      In progress: 1 Finished: 0 Write addr: 4143 255 1
      In progress: 1 Finished: 0 Write addr: 4144 255 1
      In progress: 1 Finished: 0 Write addr: 4145 255 1
      In progress: 1 Finished: 0 Write addr: 4146 255 1
      In progress: 1 Finished: 0 Write addr: 4147 255 1
      In progress: 1 Finished: 0 Write addr: 4148 255 1
      In progress: 1 Finished: 0 Write addr: 4149 255 1
      In progress: 1 Finished: 0 Write addr: 4150 255 1
      In progress: 1 Finished: 0 Write addr: 4151 255 1
      In progress: 1 Finished: 0 Write addr: 4152 255 1
      In progress: 1 Finished: 0 Write addr: 4153 255 1
      In progress: 1 Finished: 0 Write addr: 4154 255 1
      In progress: 1 Finished: 0 Write addr: 4155 255 1
      In progress: 1 Finished: 0 Write addr: 4156 255 1
      In progress: 1 Finished: 0 Write addr: 4157 255 1
      In progress: 1 Finished: 0 Write addr: 4158 255 1
      In progress: 1 Finished: 0 Write addr: 4159 255 1
      In progress: 1 Finished: 0 Write addr: 4160 255 1
      In progress: 1 Finished: 0 Write addr: 4161 255 1
      In progress: 1 Finished: 0 Write addr: 4162 255 1
      In progress: 1 Finished: 0 Write addr: 4163 255 1
      In progress: 1 Finished: 0 Write addr: 4164 255 1
      In progress: 1 Finished: 0 Write addr: 4165 255 1
      In progress: 1 Finished: 0 Write addr: 4166 255 1
      In progress: 1 Finished: 0 Write addr: 4167 255 1
      In progress: 1 Finished: 0 Write addr: 4168 255 1
      In progress: 1 Finished: 0 Write addr: 4169 255 1
      In progress: 1 Finished: 0 Write addr: 4170 255 1
      In progress: 1 Finished: 0 Write addr: 4171 255 1
      In progress: 1 Finished: 0 Write addr: 4172 255 1
      In progress: 1 Finished: 0 Write addr: 4173 255 1
      In progress: 1 Finished: 0 Write addr: 4174 255 1
      In progress: 1 Finished: 0 Write addr: 4175 255 1
      In progress: 1 Finished: 0 Write addr: 4176 255 1
      In progress: 1 Finished: 0 Write addr: 4177 255 1
      In progress: 1 Finished: 0 Write addr: 4178 255 1
      In progress: 1 Finished: 0 Write addr: 4179 255 1
      In progress: 1 Finished: 0 Write addr: 4180 255 1
      In progress: 1 Finished: 0 Write addr: 4181 255 1
      In progress: 1 Finished: 0 Write addr: 4182 255 1
      In progress: 1 Finished: 0 Write addr: 4183 255 1
      In progress: 1 Finished: 0 Write addr: 4184 255 1
      In progress: 1 Finished: 0 Write addr: 4185 255 1
      In progress: 1 Finished: 0 Write addr: 4186 255 1
      In progress: 1 Finished: 0 Write addr: 4187 255 1
      In progress: 1 Finished: 0 Write addr: 4188 255 1
      In progress: 1 Finished: 0 Write addr: 4189 255 1
      In progress: 1 Finished: 0 Write addr: 4190 255 1
      In progress: 1 Finished: 0 Write addr: 4191 255 1
      In progress: 1 Finished: 0 Write addr: 4192 255 1
      In progress: 1 Finished: 0 Write addr: 4193 255 1
      In progress: 1 Finished: 0 Write addr: 4194 255 1
      In progress: 1 Finished: 0 Write addr: 4195 255 1
      In progress: 1 Finished: 0 Write addr: 4196 255 1
      In progress: 1 Finished: 0 Write addr: 4197 255 1
      In progress: 1 Finished: 0 Write addr: 4198 255 1
      In progress: 1 Finished: 0 Write addr: 4199 255 1
      In progress: 1 Finished: 0 Write addr: 4200 255 1
      In progress: 1 Finished: 0 Write addr: 4201 255 1
      In progress: 1 Finished: 0 Write addr: 4202 255 1
      In progress: 1 Finished: 0 Write addr: 4203 255 1
      In progress: 1 Finished: 0 Write addr: 4204 255 1
      In progress: 1 Finished: 0 Write addr: 4205 255 1
      In progress: 1 Finished: 0 Write addr: 4206 255 1
      In progress: 1 Finished: 0 Write addr: 4207 255 1
      In progress: 1 Finished: 0 Write addr: 4208 255 1
      In progress: 1 Finished: 0 Write addr: 4209 255 1
      In progress: 1 Finished: 0 Write addr: 4210 255 1
      In progress: 1 Finished: 0 Write addr: 4211 255 1
      In progress: 1 Finished: 0 Write addr: 4212 255 1
      In progress: 1 Finished: 0 Write addr: 4213 255 1
      In progress: 1 Finished: 0 Write addr: 4214 255 1
      In progress: 1 Finished: 0 Write addr: 4215 255 1
      In progress: 1 Finished: 0 Write addr: 4216 255 1
      In progress: 1 Finished: 0 Write addr: 4217 255 1
      In progress: 1 Finished: 0 Write addr: 4218 255 1
      In progress: 1 Finished: 0 Write addr: 4219 255 1
      In progress: 1 Finished: 0 Write addr: 4220 255 1
      In progress: 1 Finished: 0 Write addr: 4221 255 1
      In progress: 1 Finished: 0 Write addr: 4222 255 1
      In progress: 1 Finished: 1 Write addr: 4223 255 1
      In progress: 0 Finished: 0 Write addr: 4096 255 1 |}]
  ;;
end
