open Base
open Aoc_lib.Helpers

let day = 8
type t = string list

let parse_input (input: string): t = String.split_lines input

let memory_length s =
  let rec aux n = function
  | [] -> n
  | ('\\' :: 'x' :: a :: b :: rest) when Char.is_hex_digit a && Char.is_hex_digit b -> aux (n + 1) rest
  | ('\\' :: '"' :: rest) -> aux (n + 1) rest
  | ('\\' :: '\\' :: rest) -> aux (n + 1) rest
  | ('\"' :: rest) -> aux n rest
  | (_ :: rest) -> aux (n + 1) rest
  in aux 0 $ String.to_list s

let escaped_length s = String.sum (module Int) s ~f:(function | '\\' | '"' -> 2 | _ -> 1) + 2

let part1 (input: t): string =
  let literals = List.sum (module Int) input ~f:(String.length) in
  let memory = List.sum (module Int) input ~f:(memory_length) in literals - memory |> Int.to_string

let part2 (input: t): string =
  List.sum (module Int) input ~f:(fun s -> (escaped_length s) - String.length s) |> Int.to_string
