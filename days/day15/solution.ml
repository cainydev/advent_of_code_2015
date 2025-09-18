open Base
open Aoc_lib.Helpers

let day = 15
type t = (int * int * int * int * int) array

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(fun s ->
    match String.filter s ~f:(fun c -> Char.is_alphanum c || Char.is_whitespace c || Char.equal '-' c) |> String.split ~on:' ' with
    | [_; _; c; _; d; _; f; _; t; _; cal] ->
        (Int.of_string c, Int.of_string d, Int.of_string f, Int.of_string t, Int.of_string cal)
    | _ -> failwith "Parse Error")
  |> List.to_array

let part1 (i: t): string =
  let ac, ad, af, at, _ = i.(0) in
  let bc, bd, bf, bt, _ = i.(1) in
  let cc, cd, cf, ct, _ = i.(2) in
  let dc, dd, df, dt, _ = i.(3) in
  let m = ref 0 in
  for a = 0 to 100 do
    for b = 0 to 100 - a do
      for c = 0 to 100 - a - b do
        let d = 100 - a - b - c in
        let capacity = Int.max 0 (a * ac + b * bc + c * cc + d * dc)
        and durability = Int.max 0 (a * ad + b * bd + c * cd + d * dd)
        and flavor = Int.max 0 (a * af + b * bf + c * cf + d * df)
        and texture = Int.max 0 (a * at + b * bt + c * ct + d * dt)
        in let total = capacity * durability * flavor * texture in
        if total > !m then (
          m := total
        )
      done
    done
  done; !m |> Int.to_string

let part2 (i: t): string =
  let ac, ad, af, at, acal = i.(0) in
  let bc, bd, bf, bt, bcal = i.(1) in
  let cc, cd, cf, ct, ccal = i.(2) in
  let dc, dd, df, dt, dcal = i.(3) in
  let m = ref 0 in
  for a = 0 to 100 do
    for b = 0 to 100 - a do
      for c = 0 to 100 - a - b do
        let d = 100 - a - b - c in
        let capacity = Int.max 0 (a * ac + b * bc + c * cc + d * dc)
        and durability = Int.max 0 (a * ad + b * bd + c * cd + d * dd)
        and flavor = Int.max 0 (a * af + b * bf + c * cf + d * df)
        and texture = Int.max 0 (a * at + b * bt + c * ct + d * dt)
        and calories = a * acal + b * bcal + c * ccal + d * dcal in
        let total = capacity * durability * flavor * texture in
        if calories = 500 && total > !m then m := total
      done
    done
  done; !m |> Int.to_string

