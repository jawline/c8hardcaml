open Core
open Async
open C8.Programmable_cpu_core
open Helper

let%expect_test "Biorhythm" =
  let%bind rom_file = Reader.file_contents "biorhythm.ch8" in
  test_rom
    ~run_for_instructions:(hz * 30)
    ~print_at_interval:(hz * 5)
    ~rom_file
    ~create
    ();
  [%expect
    {|
      WARN: Sound is not implemented
      ⣏⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
      ⣿⣏⣹⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⡏⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡰⠶⢆⡀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⢆⣀⡸⠇⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⢹⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣸⣇⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

      ⣏⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
      ⣿⣏⣹⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⡏⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡰⠶⢆⡀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⢆⣀⡸⠇⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⢹⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣸⣇⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

      ⣏⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
      ⣿⣏⣹⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⡏⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡰⠶⢆⡀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⢆⣀⡸⠇⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⢹⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣸⣇⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

      ⣏⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
      ⣿⣏⣹⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⡏⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡰⠶⢆⡀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⢆⣀⡸⠇⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⢹⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣸⣇⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

      ⣏⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
      ⣿⣏⣹⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⡏⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡰⠶⢆⡀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⢆⣀⡸⠇⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⢹⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣸⣇⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

      ⣏⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
      ⣿⣏⣹⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⡏⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡰⠶⢆⡀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣿⣏⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⢆⣀⡸⠇⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⢹⡏⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⣇⣸⣇⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
      ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚ |}];
  return ()
;;

let%expect_test "Missile" =
  let%bind rom_file = Reader.file_contents "missile.ch8" in
  test_rom
    ~run_for_instructions:(hz * 60)
    ~print_at_interval:(hz * 10)
    ~rom_file
    ~create
    ();
  [%expect
    {|
    WARN: Sound is not implemented
    ⡏⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⢹
    ⡇⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⢸
    ⡇⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣾⣿⣿⣷⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚⠛⠛⠛⠛⠛⠛⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

    ⡏⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⢹
    ⡇⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⢸
    ⡇⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣾⣿⣿⣷⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚⠛⠛⠛⠛⠛⠛⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

    ⡏⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⢹
    ⡇⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⢸
    ⡇⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣰⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⢀⣰⣾⣿⣿⣷⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⠓⠒⠒⠒⠚⠛⠛⠛⠛⠛⠛⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

    ⡏⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⢹
    ⡇⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⢸
    ⡇⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣾⣿⣿⣷⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚⠛⠛⠛⠛⠛⠛⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

    ⡏⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⢹
    ⡇⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⢸
    ⡇⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣾⣿⣿⣷⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚⠛⠛⠛⠛⠛⠛⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚

    ⡏⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⠉⠉⢉⣹⣏⡉⠉⠉⢹
    ⡇⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⠀⠀⠸⢿⡿⠇⠀⠀⢸
    ⡇⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣰⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⢀⣰⣾⣿⣿⣷⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⠓⠒⠒⠒⠒⠒⠒⠒⠚⠛⠛⠛⠛⠛⠛⠓⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠚ |}];
  return ()
;;

let%expect_test "Space Invaders" =
  let%bind rom_file = Reader.file_contents "space_invaders.ch8" in
  test_rom
    ~run_for_instructions:(hz * 120)
    ~print_at_interval:(hz * 10)
    ~rom_file
    ~create
    ();
  [%expect
    {|
    WARN: Sound is not implemented
    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⡉⠉⠉⠉⢉⣉⣉⣉⣉⣉⣉⡉⢉⡉⠉⠉⠉⠉⢉⡉⠉⢉⣉⣉⣉⣉⡉⠉⢉⣉⣉⣉⣉⣉⡉⠉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⢸⣏⣉⣉⣉⡉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⠀⠀⢸⡏⠉⠉⠉⢉⣹⡇⢸⡇⠀⠀⠀⠀⢸⡇⢀⣸⣏⣉⣉⣹⣇⡀⢸⣏⡉⠉⠉⠉⢱⡆⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⢸⣿⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⠀⠀⢸⡇⠀⠀⠀⢸⣿⡇⠸⠷⢶⣆⣰⡶⠾⠇⢸⡏⠉⠉⠉⢹⣿⡇⢸⣿⡇⠀⠀⠀⢸⡇⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠸⠿⠷⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠸⠇⠀⠀⠀⠸⠿⠇⠀⠀⠈⠹⠏⠁⠀⠀⠸⠇⠀⠀⠀⠸⠿⠇⠸⠿⠷⠶⠶⠶⠎⠁⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⢉⣉⣉⣉⣉⣉⣉⡉⠉⢉⣉⣉⣉⣉⡉⠉⢉⣉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⣉⡉⠉⢉⣉⣉⣉⣉⡉⠉⢉⣉⣉⡉⢉⣉⣉⡉⢉⣉⣉⡉⢉⣉⣉⡉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⢸⣏⣉⣉⣉⣹⣿⡇⢰⡎⠉⠉⠉⢉⣱⡆⢸⡏⢉⣉⣉⣉⣉⡁⢸⣏⣉⣉⣉⣹⣿⡇⢀⣸⣏⣉⣉⣹⣇⡀⢸⡏⠉⢱⡎⠉⢹⡇⢸⡏⠉⢱⡎⠉⢹⡇⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⢸⡏⠉⢹⣏⣉⡉⠁⢸⡇⠀⠀⠀⢸⣿⡇⢸⡇⠈⠉⠉⢹⣿⡇⢸⡏⠉⢹⣏⣉⡉⠁⢸⡏⠉⠉⠉⢹⣿⡇⢸⡇⠀⠈⠁⢰⣾⡇⢸⡇⠀⠈⠁⢰⣾⡇⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠸⠇⠀⠈⠉⠹⠇⠀⠈⠱⠶⠶⠶⠾⠏⠁⠸⠷⠶⠶⠶⠾⠿⠇⠸⠇⠀⠈⠉⠹⠇⠀⠸⠇⠀⠀⠀⠸⠿⠇⠸⠇⠀⠀⠀⠸⠿⠇⠸⠇⠀⠀⠀⠸⠿⠇⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⡉⠉⠉⢉⣉⣉⣉⣉⡉⠉⢉⡉⠉⠉⠉⠉⢉⡉⠉⠉⠉⢉⡉⠉⠉⠉⢉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⢸⣏⡉⠉⠉⠉⢱⡆⢀⣸⣏⣉⣉⣹⣇⡀⢸⡇⠀⠀⠀⠀⢸⡇⠀⠀⠀⢸⡇⠀⠀⠀⢸⣏⡉⠉⠉⠉⢱⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⡏⠉⠉⠉⢹⣿⡇⠸⠷⢶⣆⣰⡶⠾⠇⠀⠀⠀⢸⡇⠀⠀⠀⢸⣿⡇⠀⠀⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠷⠶⠶⠶⠎⠁⠸⠇⠀⠀⠀⠸⠿⠇⠀⠀⠈⠹⠏⠁⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠸⠿⠷⠶⠶⠶⠎⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⢸⣏⣉⣉⣉⣹⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⢸⡏⠉⢹⣏⣉⡉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠸⠇⠀⠈⠉⠹⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⢉⡉⠉⠉⠉⠉⢉⡉⢉⣉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⡉⠉⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⢸⣇⡀⠀⠀⠀⢸⡇⢸⣿⣏⣉⣉⣉⣉⡁⠈⠉⢉⣹⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⢸⣏⡉⠉⠉⠉⢱⡆⢸⣏⣉⣉⣉⡉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠈⠉⠉⠉⠉⠉⢹⡇⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠸⠿⠷⠶⠶⠶⠾⠇⠰⠶⠶⠶⠶⠶⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠷⠶⠶⠶⠎⠁⠸⠿⠷⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⢉⣉⣉⣉⣉⣉⣉⡉⢉⡉⠉⠉⠉⢉⣉⡉⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⠈⠉⢉⣹⡏⠉⠉⠁⢸⣇⣀⣀⣀⣸⣿⡇⢸⣏⣉⣉⣉⡉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⢸⣿⡇⠀⠀⠀⢸⡏⠉⠉⠉⢹⣿⡇⢸⣿⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠀⠀⠀⠸⠿⠇⠸⠿⠷⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⢉⣹⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⡉⠉⢉⣉⣉⣉⣉⣉⣉⡉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⢉⣹⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣏⡉⠉⠉⠉⢱⡆⢸⣏⣉⣉⣉⡉⠉⠁⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡏⠉⠉⠁⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠷⠶⠶⠶⠎⠁⠸⠿⠷⠶⠶⠶⠶⠆⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⠉⢉⣉⣉⡉⢉⣉⣉⡉⢉⣉⣉⡉⢉⣉⣉⡉⢉⣉⣉⣉⣉⣉⣉⡉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⢉⣹⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⢀⣸⣏⣉⣉⣹⣇⡀⢸⡏⠉⢱⡎⠉⢹⡇⢸⡏⠉⢱⡎⠉⢹⡇⢸⣏⣉⣉⣉⡉⠉⠁⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡏⠉⠉⠉⢹⣿⡇⢸⡇⠀⠈⠁⢰⣾⡇⢸⡇⠀⠈⠁⢰⣾⡇⢸⣿⡏⠉⠉⠁⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠸⠿⠇⠸⠇⠀⠀⠀⠸⠿⠇⠸⠇⠀⠀⠀⠸⠿⠇⠸⠿⠷⠶⠶⠶⠶⠆⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⠉⠉⠉⠉⠉⠉⠉⠉⢉⡉⠉⠉⠉⠉⢉⡉⢉⡉⠉⠉⠉⠉⢉⡉⠉⠉⠉⢉⡉⠉⠉⠉⢉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⡉⠉⠉⠉⢉⣉⡉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⢈⣱⡆⠰⢆⣰⣎⡁⢸⡇⠀⠀⠀⠀⢸⡇⠀⠀⠀⢸⡇⠀⠀⠀⢸⣏⡉⠉⠉⠉⢱⡆⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢀⡀⢸⣿⡇⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⢸⡏⢱⣶⡎⢹⣿⡇⠸⠷⢶⣆⣰⡶⠾⠇⠀⠀⠀⢸⡇⠀⠀⠀⢸⣿⡇⠀⠀⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⢸⡇⠈⢹⡇⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠸⠇⠸⠿⠇⠸⠿⠇⠀⠀⠈⠹⠏⠁⠀⠀⠀⠀⠀⠸⠇⠀⠀⠀⠸⠿⠷⠶⠶⠶⠎⠁⠀⠀⠀⠀⠀⠀⠀⠀⠸⠷⠶⠎⠱⠶⠾⠇⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⡉⠉⠉⠉⠉⢉⡉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⢉⣹⡏⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣇⣀⣀⣀⣀⣸⡇⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⢹⣿⡏⠉⠉⠁⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠇⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛

    ⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⢹⡇⢈⡉⠉⠉⠉⢹⡇⢈⡉⠉⠉⠉⠁⢈⡉⠉⠉⠉⠁⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⠀⠸⠷⠶⠶⢶⡆⢸⣷⣶⣶⣾⡇⢸⣷⣶⣶⣶⣾⡇⢸⡇⠀⠀⠀⠀⢸⡷⠆⠀⠀⠀⠀⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢸
    ⡇⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⠀⢰⣶⣶⣶⣾⡇⢸⡏⠉⠉⠉⠁⢸⡏⠉⠉⠉⢹⡇⢸⣷⣶⣶⣶⡆⢸⣷⣶⣶⣶⡆⠀⠰⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠆⢸
    ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠉⠁⠈⠁⠀⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠈⠉⠉⠉⠉⠁⠈⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠀⢰⡆⢰⡶⠶⠶⠶⢶⡆⢰⣶⡆⠀⠀⠀⢰⡆⠀⢰⡶⠶⠶⢶⡆⠀⢰⡶⠶⠶⠶⢆⡀⢰⡶⠶⠶⠶⠆⢰⡶⠶⠶⠶⢶⡆⢰⡶⠶⠶⠶⠶⠆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢀⣸⡇⢸⣇⡀⠀⠀⢸⡇⠸⢿⡇⠀⠀⢰⡾⠇⢰⣾⡷⠶⠶⠾⢷⡆⢸⣷⡆⠀⠀⢸⡇⢸⣷⡶⠶⠆⠀⢸⡷⢶⡶⠶⠾⠇⠸⠷⠶⠶⢶⣶⡆⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⢸⣿⡇⢸⣿⡇⠀⠀⢸⡇⠀⠸⢷⡆⢰⡾⠇⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡇⠀⠀⢸⡇⢸⣿⡇⠀⠀⠀⢸⡇⠸⠷⢶⣶⡆⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⢸
    ⡇⠀⠀⠀⠀⠀⠸⠿⠇⠸⠿⠇⠀⠀⠸⠇⠀⠀⠸⠷⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠸⠇⠸⠿⠷⠶⠶⠎⠁⠸⠿⠷⠶⠶⠆⠸⠇⠀⠀⠸⠿⠇⠰⠶⠶⠶⠾⠿⠇⠀⠀⠀⠀⠀⢸
    ⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⡀⠀⢸
    ⡇⠀⢸⡏⢉⣉⣉⣉⣉⣉⣉⡉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⣉⣉⣉⣉⣉⡉⠉⢉⣉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⣉⡉⢉⣉⣉⣉⣉⣉⣉⡉⠉⢹⡇⠀⢸
    ⡇⠀⢸⡇⢸⣿⣏⣉⣉⣉⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣏⡉⠉⠉⠉⢱⡆⢸⣏⣉⣉⣉⡉⠉⠁⢸⣿⣏⣉⣉⣉⣉⡁⠈⠉⢉⣹⡏⠉⠉⠁⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠈⠉⠉⠉⠉⠉⢹⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀⢸⡇⢸⣿⡏⠉⠉⠁⠀⠀⠈⠉⠉⠉⠉⠉⢹⡇⠀⠀⢸⣿⡇⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⢸⡇⠰⠶⠶⠶⠶⠶⠾⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠷⠶⠶⠶⠎⠁⠸⠿⠷⠶⠶⠶⠶⠆⠰⠶⠶⠶⠶⠶⠾⠇⠀⠀⠸⠿⠇⠀⠀⠀⠀⢸⡇⠀⢸
    ⡇⠀⠸⠷⢶⡶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⠶⢶⡶⠾⠇⠀⢸
    ⣇⣀⣀⣀⣸⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸⣇⣀⣀⣀⣸
    ⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛ |}];
  return ()
;;
