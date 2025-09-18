open Base
open Aoc_lib.Helpers
open Yojson 

module Json = Basic

let day = 12
type t = Json.t 

let parse_input (input: string): t = Json.from_string input

let rec sum_ints json =
  match json with
  | `Int n -> n
  | `Assoc xs -> List.sum (module Int) ~f:(fun (_, v) -> sum_ints v) xs
  | `List xs -> List.sum (module Int) ~f:sum_ints xs 
  | _ -> 0

let part1 (input: t): string = sum_ints input |> Int.to_string

let rec sum_ints_2 json =
  match json with
  | `Int n -> n
  | `Assoc xs ->
      if List.exists xs ~f:(fun (_, v) ->
        match v with
        | `String s -> String.equal s "red" 
        | _ -> false)
      then 0
      else List.sum (module Int) ~f:(fun (_, v) -> sum_ints_2 v) xs
  | `List xs -> List.sum (module Int) ~f:sum_ints_2 xs 
  | _ -> 0

let part2 (input: t): string = sum_ints_2 input |> Int.to_string

