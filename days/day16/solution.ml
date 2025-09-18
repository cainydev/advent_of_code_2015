open Base
open Aoc_lib.Helpers

let day = 16

module M = Map.M(String)
type t = int M.t array 

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(fun l ->
    (match String.filter ~f:(fun c -> Char.is_alphanum c || Char.is_whitespace c) l |> String.split ~on:' ' with
    | [_; i; k1; v1; k2; v2; k3; v3] ->
        Map.of_alist_exn (module String) [(k1, Int.of_string v1); (k2, Int.of_string v2); (k3, Int.of_string v3)]
    | _ -> failwith "Parse Error") 
  )
  |> List.to_array

let sue = Map.of_alist_exn (module String) [
  "children", (3, (=));
  "cats", (7, (>));
  "samoyeds", (2, (=));
  "pomeranians", (3, (<));
  "akitas", (0, (=));
  "vizslas", (0, (=));
  "goldfish", (5, (<));
  "trees", (3, (>));
  "cars", (2, (=));
  "perfumes", (1, (=))
]

let part1 (input: t): string =
  Array.findi_exn input ~f:(fun _ m ->
    Map.for_alli m ~f:(fun ~key ~data -> Map.find_exn sue key |> fst = data)
  ) |> fst |> (+) 1 |> Int.to_string

let part2 (input: t): string =
  Array.findi_exn input ~f:(fun _ m ->
    Map.for_alli m ~f:(fun ~key ~data ->
      let (n, op) = Map.find_exn sue key in op data n)
  ) |> fst |> (+) 1 |> Int.to_string
