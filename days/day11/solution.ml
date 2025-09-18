open Base
open Aoc_lib.Helpers

let day = 11
type t = char array

let parse_input (input: string): t = String.to_list_rev input |> List.to_array

let is_valid p =
  let n = Array.length p in
  let forbidden = ['i'; 'o'; 'l'] in
  let has_straight =
    let found = ref false in
    let current = ref 1 in
    for i = 1 to n - 1 do
      if Char.to_int p.(i) = Char.to_int p.(i-1) - 1 then
        current := !current + 1
      else
        current := 1;
      if !current >= 3 then found := true
    done;
    !found
  in
  let no_forbidden =
    Array.for_all p ~f:(fun c -> not (List.mem forbidden c ~equal:Char.equal))
  in
  let rec collect_pairs i seen =
    if i >= n - 1 then seen
    else if Char.equal p.(i) p.(i+1) && not (Set.mem seen p.(i)) then
      collect_pairs (i+2) (Set.add seen p.(i))
    else
      collect_pairs (i+1) seen
  in
  has_straight &&
  no_forbidden &&
  Set.length (collect_pairs 0 (Set.empty (module Char))) >= 2

let next c =
  match c with
  | 'z' -> 'a'
  | _ -> Char.of_int_exn (Char.to_int c + 1)

let increment_password arr =
  let rec aux idx =
    if idx >= (Array.length arr) then ()
    else
      let c = next arr.(idx) in
      arr.(idx) <- c;
      if Char.equal arr.(idx) 'a' then
        aux (idx + 1);
  in
  aux 0

let part1 (input : t) : string =
  while not (is_valid input) do
    increment_password input
  done;
  String.of_array input |> String.rev

let part2 (input: t): string =
  let input = part1 input |> String.to_list_rev |> List.to_array in
  increment_password input;
  while not (is_valid input) do
    increment_password input
  done;
  String.of_array input |> String.rev
