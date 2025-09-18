open Base
open Aoc_lib.Helpers

let day = 18
type t = bool Grid.t

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(String.to_array >> Array.map ~f:(function | '.' -> false | '#' -> true | _ -> failwith "Parse Error"))
  |> List.to_array

let step m = Grid.map (fun p s ->
  let on = Grid.fold8 (fun _ ns count -> if ns then count + 1 else count) m p 0 in
  if s
  then if on = 2 || on = 3 then true else false
  else if not s && on = 3 then true else s
  ) m

let part1 (input: t): string =
  let transformed = Fn.apply_n_times ~n:100 step input in
  Grid.fold (fun _ s acc -> if s then acc + 1 else acc) transformed 0
  |> Int.to_string

let part2 (input: t): string =
  let (x, y) = Grid.size input in
  let fix_corners m =
    m.(0).(0) <- true;
    m.(0).(y - 1) <- true;
    m.(x - 1).(0) <- true;
    m.(x - 1).(y - 1) <- true;
    m
  in
  let transformed = Fn.apply_n_times ~n:100 (fix_corners >> step) input |> fix_corners in
  Grid.fold (fun _ s acc -> if s then acc + 1 else acc) transformed 0
  |> Int.to_string

