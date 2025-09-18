open Base
open Aoc_lib.Helpers

let day = 14
type t = (int * int * int) list

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(fun s -> match List.drop (String.split ~on:' ' s) 3 with
    | [d; _; _; t; _; _; _; _; _; _; r; _] -> (Int.of_string d, Int.of_string t, Int.of_string r)
    | _ -> failwith "Parse Error")

let pos_at n (d, t, r) = (n /% (t + r)) * d * t + (d * Int.min (n % (t + r)) t)

let part1 (input: t): string =
  let total_time = 2503 in
  List.map input ~f:(pos_at total_time)
  |> List.max_elt ~compare
  |> Option.value_exn
  |> Int.to_string

let part2 (input: t): string =
  let n = List.length input in
  let total_time = 2503 in
  let scores = Array.create ~len:n 0 in
  for i = 1 to total_time do
    let p = List.map input ~f:(pos_at i) |> List.to_array in
    let max = Array.max_elt ~compare p |> Option.value_exn in
    for i = 0 to n - 1 do
      if p.(i) = max then scores.(i) <- scores.(i) + 1
    done;
  done;
  Array.max_elt ~compare scores
  |> Option.value_exn
  |> Int.to_string
