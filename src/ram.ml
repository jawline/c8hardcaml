open! Core
open! Hardcaml
open! Signal

type t =
  { read : Ram.Read_port.t
  ; write : Ram.Write_port.t
  }
