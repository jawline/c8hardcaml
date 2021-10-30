open! Core
open! Hardcaml
open! C8

let () =
  let open Fibonacci in
  let result =
    test ~create:(create ~w:16 ~work_w:8) ~work_w:8 ~count:10 |> Bits.to_string
  in
  Core.print_s [%message result]
;;
