open! Core
open! Hardcaml
open! Signal

type 'a t = { state : 'a list [@length 16] [@bits 1] } [@@deriving sexp_of, hardcaml]
