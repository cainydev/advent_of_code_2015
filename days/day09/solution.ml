open Base
open Aoc_lib.Helpers
open Aoc_lib

let day = 9

module IntTSP = Tsp.Make(Tsp.Int_weight)
type t = Tsp.Int_weight.t array array 

let parse_input (input: string): t =
  let lines = String.split_lines input in
  let n = 1.0 +. Float.sqrt (1.0 +. 8.0 *. (Float.of_int $ List.length lines)) /. 2.0 |> Int.of_float in 
  let m = Array.make_matrix ~dimx:n ~dimy:n 0 in
  let names = Hashtbl.create (module String) in
  let index_of s = Hashtbl.find_or_add names s ~default:(fun () -> Hashtbl.length names) in 
  List.iter lines ~f:(fun s ->
    match String.split s ~on:' ' with
    | [a; "to"; b; "="; c] -> (
      m.(index_of a).(index_of b) <- Int.of_string c;
      m.(index_of b).(index_of a) <- Int.of_string c)
    | _ -> failwith "Parse error"
  ); m

let rec factorial n = n * (factorial (n - 1))

let part1 (cost: t): string = IntTSP.hamiltonian_path ~maximize:false ~cost |> fst |> Int.to_string
let part2 (cost: t): string = IntTSP.hamiltonian_path ~maximize:true ~cost |> fst |> Int.to_string
