open Base
open Aoc_lib.Helpers

let day = 1
type t = string

let parse_input (input: string): t = input

let part1 (input: t): string =
  let up = String.count ~f:(Char.equal '(') input
  and down = String.count ~f:(Char.equal ')') input
  in up - down |> Int.to_string

let part2 (input: t): string =
  let rec aux count floor list =
    if floor = -1 then count else match list with
    | '(' :: rest -> aux (count + 1) (floor + 1) rest
    | ')' :: rest -> aux (count + 1) (floor - 1) rest
    | _ -> failwith "Should not happen"
  in aux 0 0 (String.to_list input) |> Int.to_string
