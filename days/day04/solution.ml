open Base
open Aoc_lib.Helpers

let day = 4
type t = string

let parse_input (input: string): t = input

let md5hex = Core.Md5.digest_string >> Core.Md5.to_hex

let part1 (input: t): string =
  Sequence.find_exn positive_ints ~f:(
    Int.to_string >> String.append input >> md5hex >> (String.is_prefix ~prefix:"00000")
  ) |> Int.to_string

let part2 (input: t): string =
  Sequence.find_exn positive_ints ~f:(
    Int.to_string >> String.append input >> md5hex >> (String.is_prefix ~prefix:"000000")
  ) |> Int.to_string
