open Base
open Aoc_lib.Helpers

let day = 2
type t = (int * int * int) list

let parse_input (input: string): t =
  input |> String.split_lines |> List.map ~f:(fun line ->
    match String.split ~on:'x' line with
    | [a; b; c] -> ( 
        match List.sort [Int.of_string a; Int.of_string b; Int.of_string c] ~compare:(Int.compare) with
        | [s; m; l] -> (s, m, l)
        | _ -> failwith "Nonsense"
      )
    | _ -> failwith "Invalid input"
  )

let part1 (input: t): string =
  List.sum (module Int) ~f:(fun (a, b, c) -> 3*a*b + 2*a*c + 2*b*c) input |> Int.to_string

let part2 (input: t): string =
  List.sum (module Int) ~f:(fun (a, b, c) -> (2*a + 2*b) + (a*b*c)) input |> Int.to_string

