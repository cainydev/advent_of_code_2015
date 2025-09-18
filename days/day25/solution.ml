open Base
open Aoc_lib.Helpers

let day = 25
type t = (int * int) 

let parse_input (input: string): t =
  match
  String.drop_prefix input 80
  |> String.filter ~f:(fun c -> Char.is_alphanum c || Char.is_whitespace c)
  |> String.split ~on:' '
  with
  | [row; _; col] -> (Int.of_string row, Int.of_string col)
  | _ -> failwith "Invalid input"

let part1 ((tr, tc): t): string =
  let n = ref 20151125 in
  let row = ref 1 in
  let col = ref 1 in
  while !row <> tr || !col <> tc do
    n := (!n * 252533) % 33554393;
    if !row = 1 then (row := !col + 1; col := 1)
    else (row := !row - 1; col := !col + 1)
  done;
  !n |> Int.to_string

let part2 (input: t): string = fst input |> Int.to_string
