open Base
open Aoc_lib.Helpers

let day = 3
type t = (Grid.position -> Grid.position) array 

let parse_input (input: string): t = String.to_array input |> Array.map ~f:(function
  | '^' -> Grid.north
  | '<' -> Grid.west
  | '>' -> Grid.east
  | 'v' -> Grid.south
  | _ -> failwith "Parse error"
)

let part1 (input: t): string =
  Array.fold input ~init:((0, 0), Map.Poly.empty) ~f:(fun (pos, v) next ->
    next pos, Map.update v pos ~f:(function None -> 1 | Some x -> x + 1)
  ) |> snd |> Map.count ~f:(Int.(<) 0) |> Int.to_string 

let part2 (input: t): string =
  Array.fold input ~init:(false, (0, 0), (0, 0), Map.Poly.empty)
    ~f:(fun (switch, santa, robot, m) next ->
      let m1 = Map.update m santa ~f:(function None -> 1 | Some x -> x + 1) in
      let m2 = Map.update m1 robot ~f:(function None -> 1 | Some x -> x + 1) in
      if switch then (not switch, santa, next robot, m2) else (not switch, next santa, robot, m2)
    )
  |> frth4 |> Map.count ~f:(Int.(<) 0) |> Int.to_string 
