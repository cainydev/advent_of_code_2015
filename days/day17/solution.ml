open Base
open Aoc_lib.Helpers

let day = 17
type t = int list

let parse_input (input: string): t = String.split_lines input |> List.map ~f:Int.of_string

let part1 (input: t): string =
  let rec aux cs target =
    match List.filter cs ~f:(fun c -> c <= target) with
    | [] -> if target = 0 then 1 else 0
    | c :: cs ->
        aux cs target +
        if c <= target then aux cs (target - c) else 0
  in aux input 150 |> Int.to_string

let part2 (input: t): string =
  let rec get_min cs target used =
    if target = 0 then used else
    match cs with
    | [] -> Int.max_value
    | c :: cs ->
        Int.min (get_min cs target used)
        (if c <= target then get_min cs (target - c) (used + 1) else Int.max_value)
  in
  let min_used = get_min input 150 0 in
  let rec aux cs target used =
    if target = 0 && used <= min_used then 1 else
    match cs with
    | [] -> 0
    | c :: cs -> (aux cs target used) + if c <= target then aux cs (target - c) (used + 1) else 0
  in aux input 150 0 |> Int.to_string
