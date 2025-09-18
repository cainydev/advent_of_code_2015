open Base
open Aoc_lib.Helpers

let day = 10
type t = char list

let parse_input (input: string): t = String.to_list input 

let look_and_say (input: char list) = 
  List.group ~break:(Char.(<>)) input
  |> List.concat_map ~f:(function
    | [] -> failwith "Error"
    | (c :: cs) ->
        let n = List.length cs + 1 |> Int.to_string in
        String.append n (String.of_char c) |> String.to_list)

let part1 (input: t): string =
  let cur = ref input in
  for i = 1 to 40 do cur := look_and_say !cur done;
  List.length !cur |> Int.to_string

let part2 (input: t): string =
  let cur = ref input in
  for i = 1 to 50 do cur := look_and_say !cur done;
  List.length !cur |> Int.to_string
