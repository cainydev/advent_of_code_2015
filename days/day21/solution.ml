open Base
open Aoc_lib.Helpers

let day = 21
type t = (int * int * int) 

let parse_input (input: string): t =
  match String.split_lines input with
  | [hit; dam; arm] ->
      let hit = String.split ~on:' ' hit |> List.last_exn |> Int.of_string in
      let dam = String.split ~on:' ' dam |> List.last_exn |> Int.of_string in
      let arm = String.split ~on:' ' arm |> List.last_exn |> Int.of_string in
      (hit, dam, arm)
  | _ -> failwith "Parse Error"

let winning (ph, pd, pa) (bh, bd, ba) =
  let real_pd = Int.max (pd - ba) 1 in
  let real_bd = Int.max (bd - pa) 1 in
  bh / real_pd <= ph / real_bd

let (+++) (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

let weapons = [|(8, 4, 0); (10, 5, 0); (25, 6, 0); (40, 7, 0); (74, 8, 0)|]
let armor = [|(0, 0, 0); (13, 0, 1); (31, 0, 2); (53, 0, 3); (75, 0, 4); (102, 0, 5)|]
let rings = [|(0, 0, 0); (25, 1, 0); (50, 2, 0); (100, 3, 0); (20, 0, 1); (40, 0, 2); (60, 0, 3); (75, 3, 0); (125, 4, 0); (45, 1, 1); (65, 1, 2); (85, 1, 3); (150, 5, 0); (70, 2, 1); (90, 2, 2); (110, 2, 3); (120, 3, 1); (140, 3, 2); (160, 3, 3); (60, 0, 3); (80, 0, 4); (100, 0, 5)|]

let part1 (boss: t): string =
  let min = ref Int.max_value in
  for w = 0 to Array.length weapons - 1 do 
    for a = 0 to Array.length armor - 1 do
      for r = 0 to Array.length rings - 1 do
        let (cost, pd, pa) = weapons.(w) +++ armor.(a) +++ rings.(r) in
        if winning (100, pd, pa) boss then min := Int.min !min cost
      done
    done
  done;
  !min |> Int.to_string

let part2 (boss: t): string =
  let max = ref Int.min_value in
  for w = 0 to Array.length weapons - 1 do 
    for a = 0 to Array.length armor - 1 do
      for r = 0 to Array.length rings - 1 do
        let (cost, pd, pa) = weapons.(w) +++ armor.(a) +++ rings.(r) in
        if not $ winning (100, pd, pa) boss then max := Int.max !max cost
      done
    done
  done;
  !max |> Int.to_string
