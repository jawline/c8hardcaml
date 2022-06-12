### c8hardcaml

An in-progress implementation of the CHIP-8 virtual machine in Hardcaml with the goal of running simple programs in simulator and on real hardware.

#### Working

- CPU Fetch & execute cycle
- Non-memory opcodes except for the random opcode
- XOR-shift based random number generator
- Test harness for a real ROM
- Frame buffer
- Memory opcodes

#### To Do

- Finish missing opcodes
- Get more applications working

#### Setting up

`$ opam pin drawille git@github.com:ksromanov/drawille.git ; dune runtest`
