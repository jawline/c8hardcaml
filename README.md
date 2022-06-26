### c8hardcaml

An in-progress implementation of the CHIP-8 virtual machine in Hardcaml with the goal of running simple programs in simulator and on real hardware.

#### Testing

In addition to more conventional tests, this project includes a simulator which will simulate a real program for a number of cycles and then print the framebuffer to text using drawille. This allows us to visually inspect the output and acts as a very useful smoke test.

#### Setting up

`$ opam pin drawille git@github.com:ksromanov/drawille.git ; dune runtest`
