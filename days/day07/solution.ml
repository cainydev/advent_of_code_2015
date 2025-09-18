open Base
open Aoc_lib.Helpers

let day = 7

module StateMap = Map.M(String)

type atom =
  | Value of int
  | Variable of string
  [@@deriving sexp, compare, hash]

type gate =
  | Const of atom
  | And of atom * atom
  | Or of atom * atom
  | LShift of atom * atom
  | RShift of atom * atom
  | Not of atom
[@@deriving sexp, compare, hash]

module WireMap = Map.M(String)
type t = gate WireMap.t 

let parse_input (input: string): t =
  let add_wire map (wire, gate) = Map.set map ~key:wire ~data:gate in
  let parse_atom s = match Int.of_string_opt s with
    | Some n -> Value n
    | None -> Variable s
  in
  String.split_lines input
  |> List.map ~f:(fun line ->
      match String.split line ~on:' ' with
        [input; "->"; output] -> output, Const (parse_atom input)
      | ["NOT"; input; "->"; output] -> 
          output, Not (parse_atom input)
      | [input1; "AND"; input2; "->"; output] -> 
          output, And (parse_atom input1, parse_atom input2)
      | [input1; "OR"; input2; "->"; output] -> 
          output, Or (parse_atom input1, parse_atom input2)
      | [input; "LSHIFT"; shift; "->"; output] -> 
          output, LShift (parse_atom input, parse_atom shift)
      | [input; "RSHIFT"; shift; "->"; output] -> 
          output, RShift (parse_atom input, parse_atom shift)
      | _ -> failwith ("Parse error: " ^ line))
  |> List.fold ~init:(Map.empty (module String)) ~f:add_wire

let to_uint16 n = Int.(land) n 0xFFFF

let eval wires wire: int =
  let aux = memo_rec (fun self wire ->
    let get a = match a with | Value x -> x | Variable x -> self x in
    if not $ Map.mem wires wire then failwith $ "Can't eval: Wire not found: " ^ wire else
    match Map.find_exn wires wire with
      | Const v -> get v
      | Not a -> Int.bit_not (get a) |> to_uint16
      | And (a, b) -> Int.bit_and (get a) (get b) |> to_uint16
      | Or (a, b) -> Int.bit_or (get a) (get b) |> to_uint16
      | LShift (a, n) -> Int.(lsl) (get a) (get n) |> to_uint16
      | RShift (a, n) -> Int.(lsr) (get a) (get n) |> to_uint16
  )
  in aux wire

let part1 (wires: t): string = eval wires "a" |> Int.to_string

let part2 (wires: t): string =
  let res = eval wires "a" in eval (Map.set wires ~key:"b" ~data:(Const (Value res))) "a" |> Int.to_string
