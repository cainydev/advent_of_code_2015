open Base
open Aoc_lib.Helpers

let day = 20
type t = int

let parse_input (input: string): t = Int.of_string input

let part1 (input: t): string =
  let limit = input / 42 in  
  let houses = Array.create ~len:(limit + 1) 0 in
  for elf = 1 to limit do
    let gift = elf * 10 in
    let rec visit h =
      if h <= limit then (
        houses.(h) <- houses.(h) + gift;
        visit (h + elf)
      )
    in visit elf
  done;
  let rec find i =
    if i > limit then failwith "Limit to low"
    else if houses.(i) >= input then i else find (i + 1)
  in find 1 |> Int.to_string

let part2 (input: t): string =
  let limit = input / 42 in  
  let houses = Array.create ~len:(limit + 1) 0 in
  for elf = 1 to limit do
    let gift = elf * 11 in
    let rec visit h i =
      if h <= limit && i <= 50 then (
        houses.(h) <- houses.(h) + gift;
        visit (h + elf) (i + 1)
      )
    in visit elf 1
  done;
  let rec find i =
    if i > limit then failwith "Limit to low"
    else if houses.(i) >= input then i else find (i + 1)
  in find 1 |> Int.to_string
