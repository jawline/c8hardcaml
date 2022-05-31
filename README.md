### c8hardcaml

An in-progress implementation of the CHIP-8 virtual machine in Hardcaml with the goal of running simple programs in simulator and on real hardware.

#### Working

- CPU Fetch & execute cycle
- Non-memory opcodes except for the random opcode
- XOR-shift based random number generator
- Test harness for a real ROM

#### To Do

- Frame buffer
  - Maybe just serial output?
- Memory opcodes
  - Need to find a good abstraction level for the opcode logic
    when instructions might need access to memory.
