open Base
open Aoc_lib.Helpers
open Aoc_lib

let day = 13

module FloatTSP = Tsp.Make(Tsp.Float_weight) 
type t = Tsp.Float_weight.t array array 

let parse_input (input: string): t =
  let lines = String.split_lines input in
  let n = 8 in 
  let m = Array.make_matrix ~dimx:n ~dimy:n (Float.max_value) in
  let names = Hashtbl.create (module String) in
  let index_of s = Hashtbl.find_or_add names s ~default:(fun () -> Hashtbl.length names) in 
  List.iter lines ~f:(fun s ->
    match String.filter ~f:(fun c -> Char.is_alphanum c || Char.is_whitespace c) s |> String.split ~on:' ' with
    | [a; "would"; "gain"; c; "happiness"; "units"; "by"; "sitting"; "next"; "to"; b] ->
        m.(index_of a).(index_of b) <- Float.of_string c
    | [a; "would"; "lose"; c; "happiness"; "units"; "by"; "sitting"; "next"; "to"; b] ->
        m.(index_of a).(index_of b) <- -.(Float.of_string c)
    | _ -> failwith "Parse error"
  );
  for i = 0 to n - 1 do m.(i).(i) <- 0.0 done;
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let d = (m.(i).(j) +. m.(j).(i)) /. 2.0 in
      m.(i).(j) <- d;
      m.(j).(i) <- d
    done
  done; 
  m

let part1 (cost: t): string =
  FloatTSP.hamiltonian_cycle ~maximize:true ~cost |> fst |> Float.( * ) 2.0 |> Float.to_string

let part2 (cost: t): string =
  let n = Array.length cost in
  let ext_cost = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      ext_cost.(i).(j) <- cost.(i).(j)
    done
  done;
  FloatTSP.hamiltonian_cycle ~maximize:true ~cost:ext_cost |> fst |> Float.( * ) 2.0 |> Float.to_string

